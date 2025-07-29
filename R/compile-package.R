


#' Compile all `quick()` functions in a package.
#'
#' This will compile all `quick()` functions in an R package, and
#' generate source files in the `src/` directory.
#'
#' Note, this function is automatically invoked during a `pkgload::load_all()` call.
#'
#' @param path Path to an R package
#'
#' @returns Called for its side effect.
#' @export
compile_package <- function(path = ".") {
  if (path != ".") {
    owd <- setwd(path)
    on.exit(setwd(owd), add = TRUE)
  }

  if (!dir.exists("R") || !file.exists("DESCRIPTION"))
    stop(path, " does not appear to be an R package.")

  pkgname <- read.dcf("DESCRIPTION", "Package")
  if (length(pkgname) != 1)
    stop(sprintf("path '%s' does not point to an R package", path))
  pkgname <- as.character(pkgname)

  # collect all `quick()` calls in the package
  collector$activate(paste0(pkgname, ":quick_funcs"))

  # TODO: need to unset various R_* env vars, or just
  # take a dep on callr
  system2(file.path(R.home("bin"), "R"),
          c("-q", "-e", shQuote("pkgload::load_all()")))
}


dump_collected <- function() {

  collected <- collector$get_collected()

  # try to resolve closure names for anonymous functions
  pkg_ns <- topenv(environment(collected[[1L]]$closure))
  pkg_funcs <- as.list.environment(pkg_ns, all.names = TRUE)
  tab <- hashtab("address", length(collected))
  for (i in seq_along(pkg_funcs)) {
    if (typeof(fn <- pkg_funcs[[i]]) == "closure")
      # if is quick closure ...
      sethash(tab, pkg_funcs[[i]], names(pkg_funcs)[i])
  }

  quick_funcs <- unlist(recursive = FALSE, lapply(collected, function(x) {
    if (!startsWith(x$name, "anonymous_quick_function_"))
      return(setNames(list(x$closure), x$name))
    true_name <- gethash(tab, x$quick_closure)
    if (is.null(true_name))
      return(setNames(list(x$closure), x$name))
    # update pkg_ns with true name
    quick_closure <- create_quick_closure(true_name, x$closure)
    pkg_ns[[true_name]] <- quick_closure
    remhash(tab, x$quick_closure)
    setNames(list(x$closure), true_name)
  }))


  pkgname <- basename(normalizePath("."))

  # check if we have a useDynLib line in NAMESPACE.
  if (!any(sapply(parse(file = "NAMESPACE"), function(e) {
    identical(e[[1]], quote(useDynLib)) && isTRUE(e$.registration)
  })))
    message("- Please add this roxygen directive somewhere in the Package R sources:\n  ",
            glue("#' @useDynLib {pkgname}, .registration = TRUE"), "\n",
            "- Then run `devtools::document()`\n")

  sources <- zip_lists(imap(quick_funcs, function(func, name) {
    fsub <- new_fortran_subroutine(name, func)
    cbridge <- make_c_bridge(fsub, headers = name == names(quick_funcs)[1])
    list(f90 = fsub, c = cbridge)
  })) |> lapply(\(x) x |> unlist() |> interleave("\n"))

  entries <- paste0(sprintf('  {"%1$s", (DL_FUNC) &%1$s, -1}',
                            paste0(names(quick_funcs), "_")),
                    collapse = ",\n")
  entries <- sprintf("static const R_ExternalMethodDef QuickrEntries[] = {\n%s\n};",
                     entries)

  append(sources$c) <- c("", entries, "")

  R_init_pkg <- paste0("R_init_", pkgname, "(")
  has_pkg_init_fn <- list.files("src", pattern = "\\.(c|cpp|h|hpp|c\\+\\+)$",
                                recursive = TRUE, all.files = TRUE,
                                full.names = TRUE) |>
    setdiff("src/quickr_entrypoints.c") |>
    lapply(function(f) {
      any(grepl(R_init_pkg, readLines(f, warn = FALSE), fixed = TRUE))
    }) |> unlist() |> any()

  append(sources$c) <- c("#include <R_ext/Rdynload.h>", "")

  init_fn <- if (has_pkg_init_fn) {
    glue("
      void R_init_{pkgname}_quick_functions(DllInfo *dll) {{
        R_registerRoutines(dll, NULL, NULL, NULL, QuickrEntries);
      }}")
  } else {
    init_pkgname <- gsub(".", "_", pkgname, fixed = TRUE)
    glue("
      void R_init_{init_pkgname}(DllInfo *dll) {{
        R_registerRoutines(dll, NULL, NULL, NULL, QuickrEntries);
        R_useDynamicSymbols(dll, FALSE);
      }}")
  }

  append(sources$c) <- init_fn

  sources <- lapply(sources, str_split_lines)

  src_files_written <- FALSE
  if (!file.exists("src")) dir.create("src")
  cbridges_filepath <- "src/quickr_entrypoints.c"
  if (!file.exists(cbridges_filepath) || !identical(sources$c, readLines(cbridges_filepath))) {
    unlink(sprintf("%s.o", tools::file_path_sans_ext(cbridges_filepath)))
    unlink(pkg_dll_path(pkgname)) # TODO: this might fail on windows - need a fallback.
    writeLines(sources$c, cbridges_filepath)
    cli::cli_inform(c(i = "Updated {.file {cbridges_filepath}}"))
    src_files_written <- TRUE
  }

  fsubs_filepath <- "src/quickr_sub_routines.f90"
  if (!file.exists(fsubs_filepath) || !identical(sources$f90, readLines(fsubs_filepath))) {
    unlink(sprintf("%s.o", tools::file_path_sans_ext(fsubs_filepath)))
    unlink(pkg_dll_path(pkgname)) # TODO: this might fail on windows - need a fallback.
    writeLines(sources$f90, fsubs_filepath)
    cli::cli_inform(c(i = "Updated {.file {fsubs_filepath}}"))
    src_files_written <- TRUE
  }

  if (src_files_written) {
    for (i in seq_along(sys.calls())) {
      if (identical(sys.function(i), pkgload::load_all)) {
        defer(pkgload::load_all(), sys.frame(i), after = TRUE)
        rlang::return_from(sys.frame(i), value = invisible())
        break
      }
    }
  }
  invisible()
}

pkg_dll_path <- function (pkgname) {
  file.path("src", paste0(pkgname, .Platform$dynlib.ext))
}


collector <- local({
  
  .collected <- NULL
  .scope_registry <- NULL
  .subfunction_registry <- list(
    internal = list(),
    module = list(),
    standalone = list()
  )  # Initialize with empty lists for each scope
  
  activate <- function(name = NULL) {
    .collected <<- list()
    .scope_registry <<- list(
      standalone = list(),
      internal = list(),
      module = list()
    )
    # Don't reset .subfunction_registry - keep it persistent
    attr(.collected, "name") <<- name
    cat("Collector activated with name:", name %||% "NULL", "\n")
  }
  
  is_active <- function() {
    is.list(.collected)
  }
  
  add <- function(...) {
    args <- list(...)
    .collected[[length(.collected)+1L]] <<- args
    
    # Track by scope
    scope <- args$scope %||% "standalone"
    func_name <- args$name
    .scope_registry[[scope]][[func_name]] <<- args
  }
  
  get_collected <- function(clear = TRUE) {
    if (clear)
      on.exit(.collected <<- NULL)
    .collected
  }
  
  get_by_scope <- function(scope = NULL) {
    if (is.null(scope)) {
      .scope_registry
    } else {
      .scope_registry[[scope]]
    }
  }
  
  # Improved subfunction registry methods
  register_subfunction <- function(name, closure, scope) {
    cat("Registering subfunction:", name, "with scope:", scope, "\n")
    
    # Ensure scope exists
    if (is.null(.subfunction_registry[[scope]])) {
      .subfunction_registry[[scope]] <<- list()
    }
    
    # Store the function info
    .subfunction_registry[[scope]][[name]] <<- list(
      scope = scope,
      closure = closure,
      name = name
    )
    
    cat("Successfully registered. Current registry for", scope, ":", 
        names(.subfunction_registry[[scope]]), "\n")
    
    invisible(TRUE)
  }
  
  get_registered_subfunctions <- function(scope = NULL) {
    if (is.null(scope)) {
      .subfunction_registry
    } else {
      result <- .subfunction_registry[[scope]] %||% list()
      cat("Getting registered subfunctions for scope", scope, ":", names(result), "\n")
      result
    }
  }
  
  clear_subfunctions <- function() {
    cat("Clearing all registered subfunctions\n")
    .subfunction_registry <<- list(
      internal = list(),
      module = list(),
      standalone = list()
    )
  }
  
  # New method to check if a function is registered
  is_subfunction_registered <- function(name, scope = "internal") {
    registered <- .subfunction_registry[[scope]] %||% list()
    result <- name %in% names(registered)
    cat("Checking if", name, "is registered in scope", scope, ":", result, "\n")
    result
  }
  
  # Debug method to show current state
  debug_registry <- function() {
    cat("=== COLLECTOR DEBUG ===\n")
    cat("Active:", is_active(), "\n")
    cat("Collected functions:", length(.collected %||% list()), "\n")
    cat("Subfunction registry:\n")
    for (scope_name in names(.subfunction_registry)) {
      funcs <- names(.subfunction_registry[[scope_name]] %||% list())
      cat("  ", scope_name, ":", if(length(funcs) > 0) paste(funcs, collapse = ", ") else "none", "\n")
    }
    cat("=====================\n")
  }
  
  environment()
})