# ============================================================================
# registry.R - Internal Function Registry for QuickR
# ============================================================================
# 
# This file implements the registry system that allows quickr functions to call
# other quickr functions internally. The registry stores compiled internal 
# functions and their metadata to enable code generation with contains blocks.
#
# Design decisions:
# - Session-scoped storage (cleared when R session ends)
# - Package-aware namespacing to avoid conflicts
# - Minimal metadata - just what's needed for code generation
# ============================================================================

#' @import S7
NULL

# ----------------------------------------------------------------------------
# S7 Class Definition
# ----------------------------------------------------------------------------

#' Internal Function Registry Entry
#' 
#' Stores the essential information needed to generate Fortran code that calls
#' an internal quickr function from within another quickr function.
#' 
#' @field name Function name (character)
#' @field r_function Original R function (function)
#' @field fortran_subroutine Compiled FortranSubroutine object
#' @field scope Environment containing all variable declarations
#' @field argument_vars List of Variable objects for function arguments
#' @field return_var Variable object describing the return value
#' @field size_names Character vector of size parameter names (e.g., "x__len_")
InternalFunction := new_class(
  properties = list(
    name = prop_string(),
    r_function = class_function,
    fortran_subroutine = FortranSubroutine,
    scope = class_environment,
    argument_vars = class_list,      # List of Variable objects for arguments
    return_var = Variable           # Variable object for return value  
  )
)

# ----------------------------------------------------------------------------
# Global Registry Storage
# ----------------------------------------------------------------------------

# Session-scoped registry using package-aware keys
# Format: "package::function_name" -> InternalFunction object
.quickr_internal_registry <- new.env(parent = emptyenv())

# ----------------------------------------------------------------------------
# Registry Management Functions  
# ----------------------------------------------------------------------------

#' Register an Internal QuickR Function
#' 
#' Stores a compiled quickr function in the registry so it can be called
#' from within other quickr functions. The function is stored with its
#' type information and compiled Fortran code.
#' 
#' @param name Character string, name of the function
#' @param r_function The original R function (before compilation)
#' @param fsub The compiled FortranSubroutine object from r2f()
#' 
#' @details
#' The registry uses package-scoped keys to avoid naming conflicts.
#' Functions are stored as "package::name" where package is determined
#' by parent.pkg() or defaults to ".GlobalEnv" for interactive use.
#' 
#' @return InternalFunction object (invisibly)
#' @examples
#' \dontrun{
#' # This would typically be called internally by quick()
#' my_fun <- function(x) { declare(type(x = double(1))); x + 1 }
#' fsub <- r2f(my_fun)
#' register_internal_function("add_one", my_fun, fsub)
#' }
register_internal_function <- function(name, r_function, fsub) {
  stopifnot(
    "name must be a non-empty string" = is_string(name) && nzchar(name),
    "r_function must be a function" = is.function(r_function),
    "fsub must be a FortranSubroutine" = inherits(fsub, FortranSubroutine)
  )
  
  # Create package-scoped registry key to avoid naming conflicts
  pkg_name <- parent.pkg() %||% ".GlobalEnv"
  registry_key <- paste0(pkg_name, "::", name)
  
  # Extract argument variables from the compiled function's scope
  scope <- fsub@scope
  arg_names <- names(formals(r_function))
  
  argument_vars <- lapply(arg_names, function(arg_name) {
    var <- scope[[arg_name]]
    if (is.null(var)) {
      stop(sprintf("Argument '%s' not found in function scope. All arguments must be declared with declare(type(...))", arg_name))
    }
    var
  })
  names(argument_vars) <- arg_names
  
  # Extract return variable information
  return_var_name <- closure_return_var_name(r_function)
  return_var <- scope[[return_var_name]]
  if (is.null(return_var)) {
    stop(sprintf("Return variable '%s' not found in function scope", return_var_name))
  }
  
  # Create and store the internal function entry
  internal_func <- InternalFunction(
    name = name,
    r_function = r_function,
    fortran_subroutine = fsub,
    scope = scope,
    argument_vars = argument_vars,
    return_var = return_var
  )
  
  # Store in the global registry
  .quickr_internal_registry[[registry_key]] <- internal_func
  
  invisible(internal_func)
}

#' Retrieve an Internal Function from Registry
#' 
#' Looks up a registered internal function by name. Returns NULL if not found.
#' 
#' @param name Character string, name of the function to retrieve
#' @param pkg_name Character string, package name to search in. 
#'   If NULL, uses the current package context from parent.pkg()
#' 
#' @return InternalFunction object if found, NULL if not found
#' 
#' @examples
#' \dontrun{
#' # Look up a function in current package context
#' add_one_func <- get_internal_function("add_one")
#' 
#' # Look up a function in a specific package
#' other_func <- get_internal_function("helper", pkg_name = "mypackage")
#' }
get_internal_function <- function(name, pkg_name = NULL) {
  stopifnot("name must be a non-empty string" = is_string(name) && nzchar(name))
  
  # If pkg_name is explicitly provided, use only that
  if (!is.null(pkg_name)) {
    registry_key <- paste0(pkg_name, "::", name)
    return(.quickr_internal_registry[[registry_key]])
  }
  
  # Try multiple package contexts due to parent.pkg() inconsistency
  # 1. Try current package context from parent.pkg()
  current_pkg <- parent.pkg()
  if (!is.null(current_pkg)) {
    registry_key <- paste0(current_pkg, "::", name)
    result <- .quickr_internal_registry[[registry_key]]
    if (!is.null(result)) {
      return(result)
    }
  }
  
  # 2. Try .GlobalEnv context (for interactive use)
  registry_key <- paste0(".GlobalEnv", "::", name)
  result <- .quickr_internal_registry[[registry_key]]
  if (!is.null(result)) {
    return(result)
  }
  
  # 3. If still not found, search all registered functions with matching name
  # This is a fallback for complex package contexts
  all_keys <- ls(.quickr_internal_registry, all.names = TRUE)
  matching_keys <- all_keys[endsWith(all_keys, paste0("::", name))]
  
  if (length(matching_keys) == 1) {
    return(.quickr_internal_registry[[matching_keys[1]]])
  } else if (length(matching_keys) > 1) {
    # Multiple matches - prefer current package context if available
    current_pkg_key <- paste0(current_pkg %||% ".GlobalEnv", "::", name)
    if (current_pkg_key %in% matching_keys) {
      return(.quickr_internal_registry[[current_pkg_key]])
    }
    # Otherwise return the first match (could be improved with better logic)
    return(.quickr_internal_registry[[matching_keys[1]]])
  }
  
  # Not found
  NULL
}

#' Check if Function is Registered as Internal
#' 
#' Tests whether a function name corresponds to a registered internal function.
#' 
#' @param name Character string, function name to check
#' @param pkg_name Character string, package to search in (optional)
#' 
#' @return Logical, TRUE if function is registered, FALSE otherwise
#' 
#' @examples
#' \dontrun{
#' if (is_internal_function("add_one")) {
#'   message("add_one is available as internal function")
#' }
#' }
is_internal_function <- function(name, pkg_name = NULL) {
  !is.null(get_internal_function(name, pkg_name))
}

#' List All Internal Functions in Package Context
#' 
#' Returns the names of all internal functions registered in the specified
#' or current package context.
#' 
#' @param pkg_name Character string, package name. If NULL, uses current context.
#' 
#' @return Named character vector. Names are registry keys, values are function names.
#' 
#' @examples
#' \dontrun{
#' # List functions in current package
#' current_functions <- list_internal_functions()
#' 
#' # List functions in specific package  
#' pkg_functions <- list_internal_functions("mypackage")
#' }
list_internal_functions <- function(pkg_name = NULL) {
  pkg_name <- pkg_name %||% parent.pkg() %||% ".GlobalEnv"
  prefix <- paste0(pkg_name, "::")
  
  # Get all registry keys and filter by package prefix  
  # Note: Use all.names = TRUE to include keys starting with "." (like ".GlobalEnv::...")
  registry_keys <- ls(.quickr_internal_registry, all.names = TRUE)
  matching_keys <- registry_keys[startsWith(registry_keys, prefix)]
  
  # Extract function names (part after "::")
  function_names <- sub("^.*::", "", matching_keys)
  setNames(function_names, matching_keys)
}

#' Clear Internal Function Registry
#' 
#' Removes all registered internal functions. Primarily useful for testing
#' and development.
#' 
#' @param pkg_name Character string, package to clear. If NULL, clears all.
#' 
#' @return NULL (invisibly)
#' 
#' @examples
#' \dontrun{
#' # Clear all functions
#' clear_internal_registry()
#' 
#' # Clear functions from specific package
#' clear_internal_registry("mypackage")
#' }
clear_internal_registry <- function(pkg_name = NULL) {
  if (is.null(pkg_name)) {
    # Clear entire registry
    # Note: Use all.names = TRUE to include keys starting with "." (like ".GlobalEnv::...")
    rm(list = ls(.quickr_internal_registry, all.names = TRUE), envir = .quickr_internal_registry)
  } else {
    # Clear specific package
    prefix <- paste0(pkg_name, "::")
    # Note: Use all.names = TRUE to include keys starting with "." (like ".GlobalEnv::...")
    registry_keys <- ls(.quickr_internal_registry, all.names = TRUE)
    matching_keys <- registry_keys[startsWith(registry_keys, prefix)]
    if (length(matching_keys) > 0) {
      rm(list = matching_keys, envir = .quickr_internal_registry)
    }
  }
  invisible(NULL)
}

# ----------------------------------------------------------------------------
# Print Methods for Development/Debugging
# ----------------------------------------------------------------------------

#' @export
method(print, InternalFunction) <- function(x, ...) {
  cat("<InternalFunction>\n")
  cat("  Name:", x@name, "\n")
  cat("  Arguments:", length(x@argument_vars), "\n")
  cat("  Return type:", x@return_var@mode, "\n")
  cat("  Size parameters:", length(x@size_names), "\n")
  if (length(x@size_names) > 0) {
    cat("    ", paste(x@size_names, collapse = ", "), "\n")
  }
  invisible(x)
}

#' Print Registry Contents (for debugging)
#' 
#' @return NULL (invisibly) 
print_registry <- function() {
  # Note: Use all.names = TRUE to include keys starting with "." (like ".GlobalEnv::...")
  registry_keys <- ls(.quickr_internal_registry, all.names = TRUE)
  if (length(registry_keys) == 0) {
    cat("Internal function registry is empty\n")
    return(invisible(NULL))
  }
  
  cat("Internal Function Registry:\n")
  for (key in registry_keys) {
    func <- .quickr_internal_registry[[key]]
    cat("  ", key, " -> ", func@name, " (", 
        length(func@argument_vars), " args, returns ", 
        func@return_var@mode, ")\n", sep = "")
  }
  invisible(NULL)
}