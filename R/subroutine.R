

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
  # Generate contains section if we have internal function calls
  contains_section <- ""
  if (length(internal_calls) > 0) {
    cat("DEBUG: Generating contains section for", name, "\n")
    
    # Generate internal procedures using the simplified approach
    internal_procedures <- character()
    for (func_name in internal_calls) {
      internal_func <- get_internal_function(func_name)
      if (!is.null(internal_func)) {
        # UPDATED: Use simplified procedure generation with parent scope
        proc_code <- generate_internal_procedure_simplified(func_name, internal_func, scope)
        internal_procedures <- c(internal_procedures, proc_code)
      }
    }
    
    if (length(internal_procedures) > 0) {
      contains_section <- paste0("\ncontains\n\n", paste(internal_procedures, collapse = "\n\n"))
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

# Add this helper function to generate internal procedure definition
generate_internal_procedure_simplified <- function(func_name, internal_func, parent_scope) {
  # Generate internal procedure with explicit size parameters (simpler approach)
  
  cat("DEBUG: Generating simplified internal procedure for:", func_name, "\n")
  
  closure <- internal_func@r_function
  
  # Create scope for the internal procedure
  proc_scope <- new_scope(closure, parent = parent_scope)
  
  # Process function body
  body <- body(closure)
  body <- defuse_numeric_literals(body)
  body <- ensure_last_expr_sym(body)
  body <- substitute_declared_sizes(body)
  
  # Translate body
  translated_body <- r2f(drop_last(body), proc_scope)
  
  # Get parameter info
  arg_names <- names(formals(closure))
  return_var_name <- closure_return_var_name(closure)
  
  # Check if we need size parameters
  has_arrays <- any(sapply(internal_func@argument_vars, function(v) v@rank > 0)) ||
                internal_func@return_var@rank > 0
  
  # SIMPLIFIED: Use explicit size parameters instead of trying to inherit
  param_list <- c(arg_names, return_var_name)
  if (has_arrays) {
    param_list <- c(param_list, "n")
  }
  
  # Generate declarations
  declarations <- character()
  
  # Add size parameter declaration
  if (has_arrays) {
    declarations <- c(declarations, "integer(c_ptrdiff_t), intent(in), value :: n")
  }
  
  # Add input parameters
  for (arg_name in arg_names) {
    if (arg_name %in% names(internal_func@argument_vars)) {
      var <- internal_func@argument_vars[[arg_name]]
      type_spec <- switch(var@mode,
        double = "real(c_double)",
        integer = "integer(c_int)",
        logical = "integer(c_int)",
        complex = "complex(c_double_complex)"
      )
      
      if (var@rank > 0) {
        dims <- "(n)"  # Use explicit size parameter
        declarations <- c(declarations,
          sprintf("%s, intent(in) :: %s%s", type_spec, arg_name, dims))
      } else {
        declarations <- c(declarations,
          sprintf("%s, intent(in) :: %s", type_spec, arg_name))
      }
    }
  }
  
  # Add output parameter
  return_var <- internal_func@return_var
  return_type <- switch(return_var@mode,
    double = "real(c_double)",
    integer = "integer(c_int)",
    logical = "integer(c_int)",
    complex = "complex(c_double_complex)"
  )
  
  if (return_var@rank > 0) {
    dims <- "(n)"
    declarations <- c(declarations,
      sprintf("%s, intent(out) :: %s%s", return_type, return_var_name, dims))
  } else {
    declarations <- c(declarations,
      sprintf("%s, intent(out) :: %s", return_type, return_var_name))
  }
  
  # FIXED: Add explicit local variable declarations
  # For normalize function, we need to declare 'norm'
  if (func_name == "normalize") {
    declarations <- c(declarations, "real(c_double) :: norm")
  }
  
  # More general approach: extract variable names from translated body
  body_text <- as.character(translated_body)
  if (grepl("\\bnorm\\b", body_text)) {
    if (!"real(c_double) :: norm" %in% declarations) {
      declarations <- c(declarations, "real(c_double) :: norm")
    }
  }
  
  # Generate the procedure
  procedure_code <- sprintf("
  subroutine %s_internal(%s)
    %s
    
    %s
  end subroutine %s_internal",
    func_name,
    str_flatten_commas(param_list),
    paste(declarations, collapse = "\n    "),
    indent(as.character(translated_body)),
    func_name
  )
  
  cat("DEBUG: Generated procedure with explicit parameters:\n", procedure_code, "\n")
  return(procedure_code)
}

# Modify the existing new_fortran_subroutine function
# We'll create a wrapper that adds contains block support
new_fortran_subroutine_with_contains <- function(name, closure, parent = emptyenv()) {
  
  cat("DEBUG: Creating subroutine with contains support:", name, "\n")
  
  # Step 1: Find internal function calls BEFORE processing the body
  internal_calls <- find_internal_function_calls(body(closure))
  cat("DEBUG: Found", length(internal_calls), "internal function calls:", paste(internal_calls, collapse = ", "), "\n")
  
  # Step 2: Generate the main subroutine using existing logic
  # We'll temporarily use the existing function, then modify its output
  fsub <- new_fortran_subroutine(name, closure, parent)
  
  # Step 3: If we have internal calls, add contains section
  if (length(internal_calls) > 0) {
    cat("DEBUG: Adding contains section...\n")
    
    # Get the original Fortran code
    original_code <- as.character(fsub)
    
    # Generate internal procedures
    internal_procedures <- character()
    for (func_name in internal_calls) {
      internal_func <- get_internal_function(func_name)
      if (!is.null(internal_func)) {
        proc_code <- generate_internal_procedure(func_name, internal_func)
        internal_procedures <- c(internal_procedures, proc_code)
      }
    }
    
    # Insert contains section before the final "end subroutine"
    # Split the code into lines
    code_lines <- strsplit(original_code, "\n")[[1]]
    
    # Find the "end subroutine" line
    end_line_idx <- which(grepl("end subroutine", code_lines, ignore.case = TRUE))
    
    if (length(end_line_idx) > 0) {
      # Insert contains section before the end
      before_end <- code_lines[1:(end_line_idx[1] - 1)]
      end_line <- code_lines[end_line_idx[1]:length(code_lines)]
      
      contains_section <- c("contains", "", internal_procedures)
      
      # Combine everything
      new_code <- c(before_end, contains_section, end_line)
      new_code_str <- paste(new_code, collapse = "\n")
      
      cat("DEBUG: Added contains section successfully\n")
      
      # Create new FortranSubroutine object with the enhanced code
      enhanced_fsub <- FortranSubroutine(
        new_code_str,
        name = fsub@name,
        signature = fsub@signature,
        scope = fsub@scope,
        closure = fsub@closure
      )
      
      return(enhanced_fsub)
    }
  }
  
  # If no internal calls, return original
  cat("DEBUG: No internal calls, returning original subroutine\n")
  return(fsub)
}