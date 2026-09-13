# Value information used by extent inference. Keep source expressions
# (@r) separate: substituting an old assignment is unsound after mutation, and
# substituting an alias's RHS later loses R's copy-at-assignment semantics.

size_scope_body <- function(scope) {
  closure <- scope_closure(scope)
  if (is.function(closure)) body(closure) else NULL
}

size_written_names <- function(expr, super_only = FALSE, unique_names = TRUE) {
  if (!is.call(expr)) {
    return(character())
  }
  op <- expr[[1L]]
  writes <- character()
  if (is.symbol(op) && as.character(op) %in% c("<-", "=", "<<-", "for")) {
    if (!super_only || identical(op, quote(`<<-`))) {
      target <- expr[[2L]]
      while (is.call(target)) {
        target <- target[[2L]]
      }
      if (is.symbol(target)) writes <- as.character(target)
    }
  }
  writes <- c(
    writes,
    unlist(lapply(
      as.list(expr)[-1L],
      size_written_names,
      super_only = super_only,
      unique_names = unique_names
    ))
  )
  if (unique_names) unique(writes) else writes
}

size_entry_available <- function(expr, scope, allow_modified = FALSE) {
  if (is.null(expr) || anyNA(unlist(as.list(expr)))) {
    return(FALSE)
  }
  if ("quickr_runtime_size" %in% all.names(expr)) {
    return(FALSE)
  }
  syms <- all.vars(expr)
  all(vapply(
    syms,
    function(nm) {
      if (is_size_name(nm)) {
        # Only sizes supplied by inputs are available to the C bridge.
        root <- scope_root(scope)
        args <- names(formals(scope_closure(root)))
        return(any(vapply(
          args,
          function(arg) {
            var <- get0(arg, root)
            inherits(var, Variable) &&
              nm %in%
                vapply(
                  seq_len(var@rank),
                  function(axis) get_size_name(var, axis),
                  character(1L)
                )
          },
          logical(1L)
        )))
      }
      var <- get0(nm, scope)
      if (!inherits(var, Variable)) {
        var <- scope_var_by_fortran_name(scope, nm)
      }
      inherits(var, Variable) && var@is_arg && (allow_modified || !var@modified)
    },
    logical(1L)
  ))
}

# Mark a scalar whose current value is only available inside the body. This
# distinction must survive even when the source is an argument: the bridge
# sees its entry value, whereas Fortran sees the reassigned value.
runtime_size <- function(expr) call("quickr_runtime_size", expr)

size_source_expr <- function(expr) {
  if (!is.call(expr)) {
    return(expr)
  }
  if (is_call(expr, "quickr_runtime_size")) {
    return(size_source_expr(expr[[2L]]))
  }
  if (is_call(expr, "quickr_extent_element")) {
    return(call("[", expr[[2L]], expr[[3L]]))
  }
  as.call(lapply(as.list(expr), size_source_expr))
}

snapshot_size_dims <- function(dims, scope, hoist) {
  lapply(dims, function(dim) {
    if (is_scalar_na(dim) || size_entry_available(dim, scope)) {
      return(dim)
    }
    tmp <- scope_unique_var(scope, "integer", integer_kind = "c_ptrdiff_t")
    tmp@size_tracked <- TRUE
    tmp@r <- if (is_call(dim, "quickr_runtime_size")) dim[[2L]] else dim
    scope[[tmp@name]] <- tmp
    register_openmp_private(scope, tmp@name)
    hoist$emit(glue("{tmp@name} = {dims2f(list(dim), scope)}"))
    as.symbol(tmp@name)
  })
}

size_assignment_value <- function(rhs, scope) {
  if (is_number(rhs)) {
    return(list(rhs))
  }
  if (is_call(rhs, "c")) {
    args <- lapply(as.list(rhs)[-1L], size_assignment_value, scope = scope)
    if (any(lengths(args) != 1L)) {
      return(list())
    }
    return(list(as.call(c(list(quote(c)), lapply(args, `[[`, 1L)))))
  }
  if (
    is_call(rhs, ":") &&
      length(rhs) == 3L &&
      is_wholenumber(rhs[[2L]]) &&
      is_wholenumber(rhs[[3L]])
  ) {
    return(list(rhs))
  }
  if (is_call(rhs, "dim")) {
    dims <- tryCatch(r2dims(rhs, scope), error = function(e) NULL)
    if (
      !is.null(dims) &&
        all(vapply(dims, size_entry_available, logical(1L), scope = scope))
    ) {
      return(list(as.call(c(list(quote(c)), dims))))
    }
    return(list())
  }
  value <- tryCatch(suppressWarnings(r2size(rhs, scope)), error = function(e) {
    NULL
  })
  if (is.null(value) || !size_entry_available(value, scope)) {
    return(list())
  }
  # Expressions copied from an input remain valid only if that input cannot
  # change later. Constants, including snapshots of reassigned locals, need no
  # such restriction.
  writes <- size_written_names(size_scope_body(scope))
  if (any(all.vars(value) %in% writes)) {
    return(list())
  }
  list(value)
}

size_binding_is_dynamic <- function(r, var, scope) {
  # Captures are read when the closure runs, not when its procedure is compiled.
  captured <- scope_is_closure(scope) &&
    !exists(as.character(r), scope, inherits = FALSE) &&
    !var@is_arg
  if (captured && var@size_tracked && length(var@size_value)) {
    writes <- size_written_names(
      size_scope_body(scope_root(scope)),
      unique_names = FALSE
    )
    captured <- sum(writes == as.character(r)) > 1L
  }
  captured ||
    as.character(r) %in%
      size_written_names(
        size_scope_body(scope_root(scope)),
        super_only = TRUE
      )
}

size_forget <- function(scope, names) {
  for (nm in names) {
    var <- get0(nm, scope, inherits = FALSE)
    if (!inherits(var, Variable)) {
      next
    }
    var@size_tracked <- TRUE
    var@size_value <- list()
    scope[[nm]] <- var
  }
}

size_state <- function(scope) {
  lapply(scope_vars(scope), function(var) {
    list(tracked = var@size_tracked, value = var@size_value)
  })
}

size_restore <- function(scope, state) {
  for (nm in names(scope_vars(scope))) {
    var <- scope[[nm]]
    fact <- state[[nm]]
    var@size_tracked <- if (is.null(fact)) TRUE else fact$tracked
    var@size_value <- fact$value %||% list()
    scope[[nm]] <- var
  }
}

size_join <- function(scope, left, right) {
  state <- lapply(names(scope_vars(scope)), function(nm) {
    if (identical(left[[nm]], right[[nm]]) && !is.null(left[[nm]])) {
      left[[nm]]
    } else {
      list(tracked = TRUE, value = list())
    }
  })
  names(state) <- names(scope_vars(scope))
  size_restore(scope, state)
}

verify_output_sizes <- function(scope) {
  for (nm in unique(unname(scope_return_var_names(scope)))) {
    var <- scope[[nm]]
    if (is.null(var) || var@is_arg) {
      next
    }
    for (dim in var@dims) {
      if (size_entry_available(dim, scope, allow_modified = TRUE)) {
        next
      }
      source <- if (is.null(var@r)) nm else deparse1(var@r)
      if (is.symbol(dim)) {
        dim_var <- get0(as.character(dim), scope)
        if (inherits(dim_var, Variable) && !is.null(dim_var@r)) dim <- dim_var@r
      }
      stop(
        "Output size could not be verified before the function body runs: `",
        source,
        "` (output `",
        nm,
        "`, dimension `",
        deparse1(size_source_expr(dim)),
        "`). ",
        "Use a constant or a size derived from unchanged inputs.",
        call. = FALSE
      )
    }
  }
}
