generate_internal_procedure <- function(name, closure, parent_scope) {
  # Create a scope that inherits from parent
  scope <- new_scope(closure, parent = parent_scope)
  
  # Process the body
  body <- body(closure)
  body <- defuse_numeric_literals(body)
  body <- ensure_last_expr_sym(body)
  base::body(closure) <- body
  
  # Translate the body
  body_fortran <- r2f(drop_last(body), scope)
  
  # Get the return variable info
  return_var_name <- closure_return_var_name(closure)
  return_var <- scope[[return_var_name]]
  
  # Simple manifest generation - just declare what we need
  arg_names <- names(formals(closure))
  
  # Build declarations
  decls <- character()
  
  # Add argument declarations
  for (arg in arg_names) {
    var <- scope[[arg]]
    if (!is.null(var)) {
      # Check if this variable exists in parent scope as a scalar
      parent_var <- parent_scope[[arg]]
      if (!is.null(parent_var) && parent_var@rank == 0) {
        # It's a scalar in parent scope
        decls <- c(decls, glue("real(c_double), intent(in) :: {arg}"))
      } else if (!var@is_scalar) {  # ← Changed from var@rank > 0
        decls <- c(decls, glue("real(c_double), intent(in) :: {arg}(n)"))
      } else {
        decls <- c(decls, glue("real(c_double), intent(in) :: {arg}"))
      }
    }
  }
  
  # Add output declaration
  if (!return_var@is_scalar) {  # ← Changed from return_var@rank > 0
    decls <- c(decls, "real(c_double), intent(out) :: result(n)")
  } else {
    decls <- c(decls, "real(c_double), intent(out) :: result")
  }
  
  # Add size parameter
  decls <- c("integer(c_ptrdiff_t), intent(in), value :: n", "", decls)
  
  # Add local variables
  local_vars <- setdiff(names(scope), c(arg_names, return_var_name))
  if (length(local_vars) > 0) {
    decls <- c(decls, "")
    for (var_name in local_vars) {
      var <- scope[[var_name]]
      if (var@rank == 0) {
        decls <- c(decls, glue("real(c_double) :: {var_name}"))
      }
    }
  }
  
  # Replace last assignment to use 'result'
  body_lines <- str_split_lines(body_fortran)
  if (length(body_lines) > 0) {
    last_line <- body_lines[length(body_lines)]
    if (grepl(paste0("^", return_var_name, " = "), last_line)) {
      body_lines[length(body_lines)] <- sub(paste0("^", return_var_name), "result", last_line)
    }
  }
  body_fortran <- str_flatten_lines(body_lines)
  
  # Simple parameter list - arguments, result, size
  params <- c(arg_names, "result", "n")
  
  # Generate the internal procedure
  glue("
    subroutine {name}({str_flatten_commas(params)})
    {indent(str_flatten_lines(decls))}
    
    {indent(body_fortran)}
    end subroutine {name}
  ")
}

new_fortran_subroutine <- function(name, closure, parent = emptyenv(), available_subfunctions = NULL) {


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
  
  # Store available subfunctions in scope for r2f to access
  if (!is.null(available_subfunctions)) {
    scope@available_subfunctions <- available_subfunctions
  }
  # inject symbols for var sizes in declare calls, so like:
  #   declare(type(foo = integer(nr, NA)),
  #           type(bar = integer(nr, 3)))
  # become:
  #   declare(type(foo = integer(foo_dim_1_, foo_dim_2_)),
  #           type(bar = integer(foo_dim_1_, 3L)))
  body <- substitute_declared_sizes(body)
  body <- r2f(drop_last(body), scope)

  contains_section <- character()
  if (!is.null(available_subfunctions)) {
    # Process each subfunction
    for (func_name in names(available_subfunctions)) {
      func_info <- available_subfunctions[[func_name]]
      
      if (func_info$scope == "internal") {
        internal_proc <- generate_internal_procedure(
          paste0(func_name, "_internal"),
          func_info$closure,
          scope  # parent scope
        )
        contains_section <- c(contains_section, internal_proc)
      }
    }
  }

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

  subroutine <- if (length(contains_section) > 0) {
    glue("
      subroutine {name}({str_flatten_commas(fsub_arg_names)}) bind(c)
        use iso_c_binding, only: {str_flatten_commas(used_iso_bindings)}
        implicit none

      {indent(manifest)}

      {indent(body)}
      
      contains
      
      {indent(str_flatten_lines(contains_section))}
      
      end subroutine
      ")
  } else {
    # Keep the existing glue for subroutines without contains
    glue("
      subroutine {name}({str_flatten_commas(fsub_arg_names)}) bind(c)
        use iso_c_binding, only: {str_flatten_commas(used_iso_bindings)}
        implicit none

      {indent(manifest)}

      {indent(body)}
      end subroutine
      ")
  }

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

