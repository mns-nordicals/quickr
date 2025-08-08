

new_fortran_subroutine <- function(name, closure, parent = emptyenv()) {


  check_all_var_names_valid(closure)

  # translate body, and populate scope with variables
  body <- body(closure)

  # defuse calls like `-1` and `1+1i`. Not really necessary, but simplifies downstream a little.
  body <- defuse_numeric_literals(body)

  # TODO: try harder here to use one of the input vars as the output var
  body <- ensure_last_expr_sym(body)

  # update closure with sym return value
  base::body(closure) <- body
  # body <- rlang::zap_srcref(body)

  scope <- new_scope(closure, parent)

  # inject symbols for var sizes in declare calls, so like:
  #   declare(type(foo = integer(nr, NA)),
  #           type(bar = integer(nr, 3)))
  # become:
  #   declare(type(foo = integer(foo_dim_1_, foo_dim_2_)),
  #           type(bar = integer(foo_dim_1_, 3L)))
  body <- substitute_declared_sizes(body)

  # NEW: Find internal function calls BEFORE translating the body
  # This ensures we know what internal functions we need before generating the main body
  internal_calls <- find_internal_function_calls(body(closure))
  if (length(internal_calls) > 0) {
    cat("DEBUG: Found internal function calls in", name, ":", paste(internal_calls, collapse = ", "), "\n")
  }

  body <- r2f(drop_last(body), scope)

  # check all input vars were declared
  # TODO: this check might be too late, because r2f() might throw cryptic errors
  # when handling undeclared variables. Either throw better errors from r2f(), or
  # handle all declares first
  for(arg_name in names(formals(closure))) {
    if (is.null(var <- get0(arg_name, scope)))
      stop("arg not declared: ", arg_name)
  }

  # figure out the return variable.
  if (is.symbol(last_expr <- last(body(closure)))) {
    return_var <- get(last_expr, scope)
    return_var@is_return <- TRUE
    scope[[as.character(last_expr)]] <- return_var
  } else {
    # lots we can still do here, just not implemented yet.
    stop("last expression in the function must be a bare symbol")
  }

  cat("DEBUG: Variables in scope for", name, ":\n")
  cat("  Closure args:", names(formals(closure)), "\n")
  cat("  All scope vars:", names(scope), "\n")

  manifest <- r2f.scope(scope)
  fsub_arg_names <- attr(manifest, "signature", TRUE)

  used_iso_bindings <- unique(unlist(use.names = FALSE, list(
    lapply(scope, function(var) {
      list(
        switch(
          var@mode,
          double = "c_double",
          integer = "c_int",
          logical = if (var@name %in% fsub_arg_names)
            "c_int",
          complex = "c_double_complex",
          raw = "c_int8_t"
        ),
        lapply(var@dims, function(size) {
          syms <- all.vars(size)
          c(if (any(grepl("__len_$", syms))) "c_ptrdiff_t",
            if (any(grepl("__dim_[0-9]+_$", syms))) "c_int")
        })
      )
    }))))

  # check for literal kinds
  if (!"c_int" %in% used_iso_bindings) {
    if (grepl("\\b[0-9]+_c_int\\b", body))
      append(used_iso_bindings) <- "c_int"
  }
  if (!"c_double" %in% used_iso_bindings) {
    if (grepl("\\b[0-9]+\\.[0-9]+_c_double\\b", body))
      append(used_iso_bindings) <- "c_double"
  }
  used_iso_bindings <- sort(used_iso_bindings, method = "radix")

  # NEW: Generate contains section if we have internal function calls
  # NEW: Generate contains section if we have internal function calls
  contains_section <- ""
  if (length(internal_calls) > 0) {
    cat("DEBUG: Generating contains section for", name, "\n")
    
    # Generate internal FUNCTIONS (not procedures)
    internal_functions <- character()
    for (func_name in internal_calls) {
      internal_func <- get_internal_function(func_name)
      if (!is.null(internal_func)) {
        # Use the new function generator
        func_code <- generate_internal_function(func_name, internal_func, scope)
        internal_functions <- c(internal_functions, func_code)
      }
    }
    
    if (length(internal_functions) > 0) {
      contains_section <- paste0("\ncontains\n\n", paste(internal_functions, collapse = "\n\n"))
    }
  }

  # MODIFIED: Include contains section in the subroutine
  subroutine <- glue("
    subroutine {name}({str_flatten_commas(fsub_arg_names)}) bind(c)
      use iso_c_binding, only: {str_flatten_commas(used_iso_bindings)}
      implicit none

    {indent(manifest)}

    {indent(body)}{contains_section}
    end subroutine
    ")

  subroutine <- insert_fortran_line_continuations(subroutine)

  FortranSubroutine(
    subroutine,
    name = name,
    signature = fsub_arg_names,
    scope = scope,
    closure = closure
  )
}

insert_fortran_line_continuations <- function(code, preserve_attributes = TRUE) {
  attrs_in <- attributes(code)

  code <- as.character(code)
  lines <- str_split_lines(code)
  lines <- trimws(lines, "right")

  if (any(too_long <- nchar(lines) > 132)) {
    # remove leading indentation
    lines[too_long] <- trimws(lines[too_long], "left")

    # move trailing comment at the end
    lines[too_long] <- sub("^(.*)!(.*)$", "!\\2\n\\1", lines[too_long])
    lines <- str_split_lines(lines)

    # maximum 255 continuations are allowed
    for (i in 1:256) {
      if (!any(too_long <- nchar(lines) > 132))
        break
      lines[too_long] <- sub("^(.{1,130})\\s", "\\1 &\n", lines[too_long])
      lines <- str_split_lines(lines)
    }
    if (i > 255L)
      stop("Too long line encountered. Please split long expressions into a sequence of smaller expressions.")
  }

  code <- str_flatten_lines(lines)
  if (preserve_attributes)
    attributes(code) <- attrs_in
  code
}

## utility

# Step 2: Add Contains Block Generation to R/subroutine.R

# Add this helper function to find internal function calls in function body
find_internal_function_calls <- function(func_body) {
  # Find all calls to registered internal functions in a function body.
  # 
  # Args:
  #   func_body: The body() of an R function
  #   
  # Returns:
  #   Character vector of internal function names that are called
  internal_calls <- character()
  
  # Recursive helper to walk the expression tree
  walk_expr <- function(expr) {
    if (is.call(expr)) {
      # Check if this call is to an internal function
      callable <- expr[[1L]]
      if (is.symbol(callable)) {
        func_name <- as.character(callable)
        if (is_internal_function(func_name)) {
          internal_calls <<- c(internal_calls, func_name)
          cat("DEBUG: Found internal function call:", func_name, "\n")
        }
      }
      
      # Recursively check all parts of the call
      for (i in seq_along(expr)) {
        walk_expr(expr[[i]])
      }
    } else if (is.pairlist(expr) || is.list(expr)) {
      for (element in expr) {
        walk_expr(element)
      }
    }
  }
  
  walk_expr(func_body)
  return(unique(internal_calls))
}



#' Generate a Fortran function for an internal quickr function
#' 
#' @param func_name Name of the function
#' @param internal_func InternalFunction object from registry
#' @param parent_scope Parent scope for variable resolution
#' @return Character string of Fortran function code
# In generate_internal_function, we need to make sure ALL local variables are found
generate_internal_function <- function(func_name, internal_func, parent_scope) {
  cat("DEBUG: Generating Fortran FUNCTION for:", func_name, "\n")
  
  closure <- internal_func@r_function
  
  # Create scope for the internal function
  func_scope <- new_scope(closure, parent = parent_scope)
  
  # Process function body
  body <- body(closure)
  body <- defuse_numeric_literals(body)
  body <- ensure_last_expr_sym(body)
  body <- substitute_declared_sizes(body)
  
  # Translate the ENTIRE body to populate the scope with ALL variables
  translated_body <- r2f(body, func_scope)
  
  # Get return variable info
  return_var_name <- closure_return_var_name(closure)
  return_var <- func_scope[[return_var_name]]
  if (is.null(return_var)) {
    stop("Return variable '", return_var_name, "' not found in function scope")
  }
  
  # Generate the Fortran function code
  # Pass the FULL scope so we can extract all local variables
  generate_fortran_function_code(
    func_name,
    internal_func@argument_vars,
    return_var,
    translated_body,
    func_scope  # This contains ALL variables including locals
  )
}

# Update generate_fortran_function_code in subroutine.R

# Fix in generate_fortran_function_code - better local variable extraction

# In subroutine.R - update generate_fortran_function_code
# In subroutine.R - complete fix for generate_fortran_function_code
generate_fortran_function_code <- function(func_name, arg_vars, return_var, body, func_scope) {
  # Build parameter list (just the input arguments)
  param_list <- names(arg_vars)
  
  # Generate declarations
  declarations <- character()
  
  # Add parameter declarations
  for (arg_name in names(arg_vars)) {
    var <- arg_vars[[arg_name]]
    type_spec <- switch(var@mode,
      double = "real(c_double)",
      integer = "integer(c_int)",
      logical = "logical",
      complex = "complex(c_double_complex)",
      stop("Unsupported type: ", var@mode)
    )
    
    # Check if it's actually a scalar or array
    if (var@rank > 0 && !identical(var@dims, list(1L))) {
      dims_spec <- paste0("(", paste(rep(":", var@rank), collapse = ","), ")")
      declarations <- c(declarations,
        sprintf("%s, intent(in) :: %s%s", type_spec, arg_name, dims_spec))
    } else {
      declarations <- c(declarations,
        sprintf("%s, intent(in), value :: %s", type_spec, arg_name))
    }
  }
  
  # Generate result declaration - FIX: Use correct type from return_var
  result_name <- "result_"
  result_type <- switch(return_var@mode,
    double = "real(c_double)",
    integer = "integer(c_int)",
    logical = "logical",
    complex = "complex(c_double_complex)",
    stop("Unsupported return type: ", return_var@mode)
  )
  
  # Determine result dimensions
  if (return_var@rank > 0 && !identical(return_var@dims, list(1L))) {
    result_dims <- infer_result_dimensions(return_var, arg_vars, func_scope)
    result_decl <- sprintf("%s :: %s%s", result_type, result_name, result_dims)
  } else {
    result_decl <- sprintf("%s :: %s", result_type, result_name)
  }
  
  # FIX: Properly extract ALL local variables
  local_declarations <- character()
  
  cat("DEBUG: Variables in function scope:", names(func_scope), "\n")
  cat("DEBUG: Argument names:", names(arg_vars), "\n")
  cat("DEBUG: Return var name:", return_var@name, "\n")
  
  for (var_name in names(func_scope)) {
    # Skip arguments
    if (var_name %in% names(arg_vars)) {
      cat("DEBUG: Skipping argument:", var_name, "\n")
      next
    }
    
    # Skip return variable
    if (var_name == return_var@name) {
      cat("DEBUG: Skipping return var:", var_name, "\n")
      next
    }
    
    # Get the variable from the scope
    var <- func_scope[[var_name]]
    
    # Check if it's a Variable object
    if (!inherits(var, "Variable")) {
      cat("DEBUG: Not a Variable object:", var_name, "\n")
      next
    }
    
    cat("DEBUG: Processing local variable:", var_name, "mode:", var@mode, "\n")
    
    # Determine the type
    var_type <- switch(var@mode,
      double = "real(c_double)",
      integer = "integer(c_int)",
      logical = "logical",
      complex = "complex(c_double_complex)",
      NULL
    )
    
    if (!is.null(var_type)) {
      # Check if it's scalar or array
      if (!is.null(var@rank) && var@rank > 0 && !identical(var@dims, list(1L))) {
        # Array local variable
        local_dims <- infer_local_dimensions(var, func_scope)
        local_declarations <- c(local_declarations,
          sprintf("%s :: %s%s", var_type, var_name, local_dims))
      } else {
        # Scalar local variable
        local_declarations <- c(local_declarations,
          sprintf("%s :: %s", var_type, var_name))
      }
    }
  }
  
  # FIX: Process the body to replace return variable with result_
  body_str <- as.character(body)
  lines <- strsplit(body_str, "\n")[[1]]
  
  # Remove any standalone return variable name (like "out" at the end)
  # and replace with assignment to result_
  processed_lines <- character()
  for (i in seq_along(lines)) {
    line <- lines[i]
    
    # Check if it's the last line and just the return variable name
    if (i == length(lines) && grepl(paste0("^\\s*", return_var@name, "\\s*$"), line)) {
      # Skip this line - we don't need it in a function
      next
    }
    
    # Check if it's an assignment to the return variable
    if (grepl(paste0("^\\s*", return_var@name, "\\s*="), line)) {
      # Replace with assignment to result_
      line <- sub(paste0("^(\\s*)", return_var@name, "(\\s*=)"),
                  paste0("\\1", result_name, "\\2"),
                  line)
    }
    
    processed_lines <- c(processed_lines, line)
  }
  body_str <- paste(processed_lines, collapse = "\n")
  
  # Generate the complete function
  function_code <- character()
  function_code <- c(function_code,
    sprintf("function %s_internal(%s) result(%s)",
            func_name,
            str_flatten_commas(param_list),
            result_name))
  
  # Add declarations in order: parameters, result, locals
  if (length(declarations) > 0) {
    function_code <- c(function_code, paste0("  ", declarations))
  }
  
  function_code <- c(function_code, paste0("  ", result_decl))
  
  if (length(local_declarations) > 0) {
    function_code <- c(function_code, paste0("  ", local_declarations))
  }
  
  # Add empty line before body
  function_code <- c(function_code, "")
  
  # Add the function body
  body_lines <- strsplit(body_str, "\n")[[1]]
  function_code <- c(function_code, paste0("  ", body_lines))
  
  # Close the function
  function_code <- c(function_code,
    sprintf("end function %s_internal", func_name))
  
  paste(function_code, collapse = "\n")
}

#' Infer result dimensions for a function
# Update infer_result_dimensions to handle more cases
infer_result_dimensions <- function(return_var, arg_vars, func_scope) {
  if (return_var@rank == 0 || identical(return_var@dims, list(1L))) {
    return("")  # Scalar
  }
  
  # For arrays, try to match with input arrays
  for (arg_name in names(arg_vars)) {
    arg_var <- arg_vars[[arg_name]]
    if (arg_var@rank == return_var@rank && arg_var@rank > 0) {
      # Use size intrinsic for each dimension
      if (arg_var@rank == 1) {
        return(sprintf("(size(%s))", arg_name))
      } else {
        dims <- paste(sapply(1:arg_var@rank, function(i) {
          sprintf("size(%s,%d)", arg_name, i)
        }), collapse = ",")
        return(sprintf("(%s)", dims))
      }
    }
  }
  
  # If we can't infer, use allocatable
  return("(:)")
}

#' Infer dimensions for local variables
infer_local_dimensions <- function(var, func_scope) {
  if (var@rank == 0) return("")
  
  # Try to use the dimension information stored in the variable
  if (!is.null(var@dims)) {
    dims <- sapply(var@dims, function(d) {
      if (is.numeric(d)) as.character(d)
      else if (is.symbol(d)) as.character(d)
      else "size_unknown"
    })
    return(sprintf("(%s)", paste(dims, collapse = ",")))
  }
  
  # Default to deferred-shape for allocatable
  return(sprintf("(%s)", paste(rep(":", var@rank), collapse = ",")))
}
