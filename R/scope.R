new_ordered_env <- function(parent = emptyenv()) {
  env <- new.env(parent = parent)
  class(env) <- "quickr_ordered_env"
  env
}

#' @export
`[[<-.quickr_ordered_env` <- function(x, name, value) {
  attr(x, "ordered_names") <- unique(c(attr(x, "ordered_names", TRUE), name))
  assign(name, value, envir = x)
  x
  # NextMethod()
}

#' @export
`[[.quickr_ordered_env` <- function(x, name) {
  get0(name, x) # name can be a symbols too
}

#' @export
names.quickr_ordered_env <- function(x) {
  all_names <- ls(envir = x, sorted = FALSE)
  ordered_names <- attr(x, "ordered_names", TRUE)
  if (!setequal(all_names, ordered_names)) {
    warning("untracked name")
    stop("untracked name")
  }
  ordered_names
}

#' @export
as.list.quickr_ordered_env <- function(x, ...) {
  out <- as.list.environment(x, all.names = TRUE, ...)
  out[names.quickr_ordered_env(x)]
}

#' @export
print.quickr_ordered_env <- function(x, ...) {
  emit("env (class: ", str_flatten_commas(class(x)), ") with bindings:")
  str(as.list.quickr_ordered_env(x), no.list = TRUE)
}


# Enhanced assignment compatibility checking with STRICT type checking
check_assignment_compatible <- function(target, value, expr = NULL) {
  if (is.null(value)) {
    return()
  }
  
  stopifnot(exprs = {
    inherits(target, Variable)
    inherits(value, Variable)
  })
  
  # Check shape compatibility
  if (!passes_as_scalar(target) && !passes_as_scalar(value)) {
    if (target@rank != value@rank) {
      stop(sprintf(
        "Rank mismatch in assignment%s: target has rank %d, value has rank %d",
        if (!is.null(expr)) paste0(" '", deparse1(expr), "'") else "",
        target@rank, value@rank
      ))
    }
    
    # Check dimension compatibility if both have fixed dimensions
    for (i in seq_along(target@dims)) {
      td <- target@dims[[i]]
      vd <- value@dims[[i]]
      if (!is_scalar_na(td) && !is_scalar_na(vd) && !identical(td, vd)) {
        stop(sprintf(
          "Dimension mismatch in assignment%s: dimension %d differs",
          if (!is.null(expr)) paste0(" '", deparse1(expr), "'") else "",
          i
        ))
      }
    }
  }
  
  # STRICT type checking - no implicit type changes allowed
  if (target@mode != value@mode) {
    # Type hierarchy for determining promotion vs demotion
    type_hierarchy <- list(
      logical = 1,
      integer = 2, 
      double = 3,
      complex = 4
    )
    
    target_level <- type_hierarchy[[target@mode]]
    value_level <- type_hierarchy[[value@mode]]
    
    if (is.null(target_level) || is.null(value_level)) {
      stop(sprintf(
        "Unsupported type in assignment: %s to %s",
        value@mode, target@mode
      ))
    }
    
    # Trying to assign a "higher" type to a "lower" type variable
    if (value_level > target_level) {
      # Extract variable name for better error message
      var_name <- if (!is.null(expr)) deparse1(expr) else "variable"
      
      stop(sprintf(
        paste0(
          "Cannot assign %s value to %s variable '%s'.\n",
          "In quickr, variables have fixed types.\n",
          "Suggestions:\n",
          "  1. Declare '%s' as %s: declare(type(%s = %s(...)))\n",
          "  2. Use a different variable for the result\n",
          "  3. Use explicit casting if truncation is intended: as.%s(...)"
        ),
        value@mode, target@mode, var_name,
        var_name, value@mode, var_name, value@mode,
        target@mode
      ))
    }
    
    # Allow safe promotion (e.g., integer to double)
    if (value_level < target_level) {
      message(sprintf(
        "Note: %s value will be promoted to %s in assignment to '%s'",
        value@mode, target@mode, 
        if (!is.null(expr)) deparse1(expr) else "variable"
      ))
    }
  }
}

new_scope <- function(closure, parent = emptyenv()) {
  scope <- new_ordered_env(parent = parent)
  class(scope) <- unique(c("quickr_scope", class(scope)))
  attr(scope, "closure") <- closure

  attr(scope, "get_unique_var") <- local({
    i <- 0L
    function(...) {
      name <- paste0("tmp", i <<- i + 1L, "_")
      (scope[[name]] <- Variable(..., name = name))
    }
  })
  attr(scope, "assign") <- function(name, value) {
    stopifnot(inherits(value, Variable), is.symbol(name) || is_string(name))
    name <- as.character(name)
    if (exists(name, scope)) {
      check_assignment_compatible(get(name, scope), value)
    }
    value@name <- name
    assign(name, value, scope)
  }
  scope
}


#' @export
`@.quickr_scope` <- function(x, name) attr(x, name, exact = TRUE)

#' @export
`@<-.quickr_scope` <- function(x, name, value) `attr<-`(x, name, value = value)

#' @importFrom utils .AtNames findMatches
#' @export
.AtNames.quickr_scope <- function(x, pattern = "") {
  findMatches(pattern, names(attributes(x)))
}
