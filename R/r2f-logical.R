# r2f-logical.R
# Handlers for logical and comparison operators: !, &, |, >=, >, <, <=, ==, !=
# plus the scalar short-circuit forms && and || (compile_andor below).

# --- Handlers ---

# ---- comparison operators ----

# R supports equality on complex values but refuses ordering. Refuse it
# here rather than handing gfortran an invalid comparison; shared by the
# four ordering handlers below (== and /= stay legal, as in R).
check_ordered_operands <- function(left, right) {
  if ("complex" %in% c(left@value@mode, right@value@mode)) {
    stop("invalid comparison with complex values", call. = FALSE)
  }
  invisible(TRUE)
}

r2f_handlers[[">="]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lower_elementwise_operands(args, scope, ..., hoist = hoist)
  check_ordered_operands(left, right)
  # R compares logicals as integers; Fortran has no logical comparison.
  .[left, right] <- promote_arith_pair(left, right, "comparison")
  .[left, right] <- maybe_reshape_vector_matrix(
    left,
    right,
    hoist,
    scope,
    scalarize_one_by_one = FALSE
  )
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} >= {right})"), var)
}

r2f_handlers[[">"]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lower_elementwise_operands(args, scope, ..., hoist = hoist)
  check_ordered_operands(left, right)
  # R compares logicals as integers; Fortran has no logical comparison.
  .[left, right] <- promote_arith_pair(left, right, "comparison")
  .[left, right] <- maybe_reshape_vector_matrix(
    left,
    right,
    hoist,
    scope,
    scalarize_one_by_one = FALSE
  )
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} > {right})"), var)
}

r2f_handlers[["<"]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lower_elementwise_operands(args, scope, ..., hoist = hoist)
  check_ordered_operands(left, right)
  # R compares logicals as integers; Fortran has no logical comparison.
  .[left, right] <- promote_arith_pair(left, right, "comparison")
  .[left, right] <- maybe_reshape_vector_matrix(
    left,
    right,
    hoist,
    scope,
    scalarize_one_by_one = FALSE
  )
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} < {right})"), var)
}

r2f_handlers[["<="]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lower_elementwise_operands(args, scope, ..., hoist = hoist)
  check_ordered_operands(left, right)
  # R compares logicals as integers; Fortran has no logical comparison.
  .[left, right] <- promote_arith_pair(left, right, "comparison")
  .[left, right] <- maybe_reshape_vector_matrix(
    left,
    right,
    hoist,
    scope,
    scalarize_one_by_one = FALSE
  )
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} <= {right})"), var)
}

r2f_handlers[["=="]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lower_elementwise_operands(args, scope, ..., hoist = hoist)
  # R compares logicals as integers; Fortran has no logical comparison.
  .[left, right] <- promote_arith_pair(left, right, "comparison")
  .[left, right] <- maybe_reshape_vector_matrix(
    left,
    right,
    hoist,
    scope,
    scalarize_one_by_one = FALSE
  )
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} == {right})"), var)
}

r2f_handlers[["!="]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lower_elementwise_operands(args, scope, ..., hoist = hoist)
  # R compares logicals as integers; Fortran has no logical comparison.
  .[left, right] <- promote_arith_pair(left, right, "comparison")
  .[left, right] <- maybe_reshape_vector_matrix(
    left,
    right,
    hoist,
    scope,
    scalarize_one_by_one = FALSE
  )
  var <- conform(left@value, right@value)
  var@mode <- "logical"
  Fortran(glue("({left} /= {right})"), var)
}

# ---- unary logical not ----

r2f_handlers[["!"]] <- function(args, scope, ...) {
  stopifnot(length(args) == 1L)
  x <- r2f(args[[1L]], scope, ...)
  if (x@value@mode != "logical") {
    stop("'!' expects a logical value; numeric coercions not yet supported")
  }
  x <- booleanize_logical_as_int(x)
  Fortran(glue("(.not. {x})"), Variable("logical", x@value@dims))
}

register_r2f_handler(
  "is.null",
  function(args, scope, ...) {
    stopifnot(length(args) == 1L)
    arg <- args[[1L]]
    if (!is.symbol(arg)) {
      stop("is.null() is only supported on symbols", call. = FALSE)
    }
    var <- get0(as.character(arg), scope)
    if (!inherits(var, Variable) || is.null(var@optional_dummy)) {
      stop(
        "is.null() is only supported for optional arguments with NULL defaults",
        call. = FALSE
      )
    }
    Fortran(glue("(.not. present({var@optional_dummy}))"), Variable("logical"))
  }
)


# ---- binary logical operators ----

# TODO: gfortran supports implicit casting that of logical to integer when
# assigning a logical to a variable declared integer, converting `.true.` to `1`,
# but this is not a standard language feature, and Intel's `ifort` uses `-1` for `.true`.
# We should explicitly use
#   `merge(1_c_int, 0_c_int, <lgl>)` to cast logical to int.
register_r2f_handler(
  c("&", "|"),
  function(args, scope, ..., hoist = NULL) {
    op <- last(list(...)$calls)
    args <- lower_elementwise_operands(args, scope, ..., hoist = hoist)
    args <- lapply(args, function(a) {
      if (a@value@mode != "logical") {
        stop("`", op, "` requires logical operands", call. = FALSE)
      }
      a
    })
    .[left, right] <- args
    left <- booleanize_logical_as_int(left)
    right <- booleanize_logical_as_int(right)
    .[left, right] <- maybe_reshape_vector_matrix(
      left,
      right,
      hoist,
      scope,
      scalarize_one_by_one = FALSE
    )

    operator <- switch(op, `&` = ".and.", `|` = ".or.")

    s <- glue("{left} {operator} {right}")
    val <- conform(left@value, right@value)
    val@mode <- "logical"
    Fortran(s, val)
  }
)

# ---- scalar short-circuit operators: && and || ----

# && and || are R's *scalar* control operators: operands must be length 1
# (R errors otherwise), and the right operand is evaluated only when the
# left side does not already decide the answer.
scalarize_andor_operand <- function(
  x,
  op,
  hoist,
  scope,
  defer_length_error = FALSE
) {
  if (is.null(x@value) || !identical(x@value@mode, "logical")) {
    stop("`", op, "` requires logical operands", call. = FALSE)
  }

  message <- paste0(
    "`",
    op,
    "` requires length-1 operands; use `",
    if (op == "&&") "&" else "|",
    "` for elementwise operations"
  )
  if (passes_as_scalar(x@value)) {
    return(booleanize_logical_as_int(x))
  }

  dims <- lapply(x@value@dims, r2size, scope = scope)
  length_known_bad <- any(vapply(
    dims,
    \(dim) is_wholenumber(dim) && !dim_is_one(dim),
    logical(1L)
  ))
  if (length_known_bad) {
    if (!defer_length_error) {
      stop(message, call. = FALSE)
    }
    emit_quickr_error_if(".true.", message, hoist, scope)
  }
  if (is.null(hoist)) {
    stop("internal error: `", op, "` requires hoist context", call. = FALSE)
  }

  if (isTRUE(x@logical_booleanized)) {
    tmp <- hoist$declare_tmp(mode = "logical", dims = x@value@dims)
    hoist$emit(glue("{tmp@name} = {x}"))
    x <- Fortran(tmp@name, tmp)
  } else {
    x <- hoist_unless_name(x, hoist)
  }

  if (
    !length_known_bad &&
      !all(vapply(dims, dim_is_one, logical(1L)))
  ) {
    emit_quickr_error_if(
      glue("size({x}, kind=c_ptrdiff_t) /= 1_c_ptrdiff_t"),
      message,
      hoist,
      scope
    )
  }
  idxs <- rep("1", x@value@rank)
  Fortran(
    glue("{x}({str_flatten_commas(idxs)})"),
    Variable("logical")
  )
}

# TRUE when evaluating `e` eagerly is indistinguishable from R's lazy
# right-operand evaluation: no side effects, no errors, no traps. A
# conservative whitelist -- names, literals, and compositions of pure
# non-trapping operations. Anything else (subscripts, %%/%/%, function
# calls, ...) gets the conditional lowering.
is_pure_scalar_condition <- function(e, scope) {
  if (is.symbol(e)) {
    var <- get0(as.character(e), scope)
    if (inherits(var, Variable) && !is.null(var@optional_dummy)) {
      return(FALSE)
    }
    return(
      !inherits(var, Variable) ||
        passes_as_scalar(var) ||
        var@rank > 0L && all(vapply(var@dims, dim_is_one, logical(1L)))
    )
  }
  if (is.atomic(e) && length(e) == 1L) {
    return(TRUE)
  }
  if (!is.call(e) || !is.symbol(e[[1L]])) {
    return(FALSE)
  }
  op <- as.character(e[[1L]])
  pure_ops <- c(
    "(",
    "!",
    "&&",
    "||",
    "&",
    "|",
    "<",
    "<=",
    ">",
    ">=",
    "==",
    "!=",
    "+",
    "-",
    "*",
    "abs"
  )
  if (!op %in% pure_ops) {
    return(FALSE)
  }
  if (inherits(scope[[op]], LocalClosure)) {
    return(FALSE)
  }
  all(vapply(
    as.list(e)[-1L],
    is_pure_scalar_condition,
    logical(1L),
    scope = scope
  ))
}

lazy_builtin_arities <- list(
  `(` = 1L,
  `!` = 1L,
  `&&` = 2L,
  `||` = 2L,
  `&` = 2L,
  `|` = 2L,
  `<` = 2L,
  `<=` = 2L,
  `>` = 2L,
  `>=` = 2L,
  `==` = 2L,
  `!=` = 2L,
  `+` = 1:2,
  `-` = 1:2,
  `*` = 2L,
  abs = 1L
)

lazy_builtin_arity_error <- function(e, scope, recursive = TRUE) {
  if (!is.call(e) || !is.symbol(e[[1L]])) {
    return(NULL)
  }

  op <- as.character(e[[1L]])
  allowed <- lazy_builtin_arities[[op]]
  is_builtin <- !inherits(scope[[op]], LocalClosure)
  if (
    is_builtin &&
      !is.null(allowed) &&
      !((length(e) - 1L) %in% allowed)
  ) {
    expected <- if (length(allowed) == 2L) {
      "one or two arguments"
    } else {
      paste0(
        "exactly ",
        if (allowed == 1L) "one" else "two",
        " argument",
        if (allowed == 1L) "" else "s"
      )
    }
    return(paste0("`", op, "` requires ", expected))
  }
  if (!recursive) {
    return(NULL)
  }

  args <- as.list(e)[-1L]
  if (is_builtin && op %in% c("&&", "||") && length(args) == 2L) {
    # A nested scalar short-circuit owns its right operand. Only its left
    # operand is unconditionally evaluated when the nested call is reached.
    args <- args[1L]
  }
  for (arg in args) {
    error <- lazy_builtin_arity_error(arg, scope, recursive = TRUE)
    if (!is.null(error)) {
      return(error)
    }
  }
  NULL
}

compile_andor <- function(
  args,
  scope,
  ...,
  hoist = NULL,
  defer_andor_length_error = FALSE
) {
  op <- last(list(...)$calls)
  stopifnot(length(args) == 2L, op %in% c("&&", "||"))

  # R always evaluates the left operand: its hoists stay unconditional.
  left <- r2f(
    args[[1L]],
    scope,
    ...,
    hoist = hoist,
    defer_andor_length_error = defer_andor_length_error
  )
  left <- scalarize_andor_operand(
    left,
    op,
    hoist,
    scope,
    defer_length_error = defer_andor_length_error
  )

  f <- if (op == "&&") ".and." else ".or."
  rhs_arity_error <- lazy_builtin_arity_error(args[[2L]], scope)

  if (
    is.null(rhs_arity_error) &&
      is_pure_scalar_condition(args[[2L]], scope)
  ) {
    # Fortran may evaluate both operands of .and./.or.; for a pure right
    # operand that is indistinguishable from short-circuiting, so keep
    # the compact infix form.
    right <- r2f(
      args[[2L]],
      scope,
      ...,
      hoist = hoist,
      defer_andor_length_error = defer_andor_length_error
    )
    right <- scalarize_andor_operand(
      right,
      op,
      hoist,
      scope,
      defer_length_error = defer_andor_length_error
    )
    return(Fortran(glue("{left} {f} {right}"), Variable("logical")))
  }

  # The right operand can error or have side effects; R reaches it only
  # when the left side does not decide. Compile it into its own hoist and
  # emit everything inside the conditional.
  if (is.null(hoist)) {
    stop("internal error: `", op, "` requires hoist context", call. = FALSE)
  }
  # The result must remain visible outside the nested right-operand block.
  # Declare it in the procedure scope so block-local temporaries cannot
  # shadow it.
  tmp <- scope_unique_var(scope, mode = "logical", dims = NULL)
  register_openmp_private(scope, tmp@name)
  sub <- new_hoist(scope)
  sub$defer_static_shape_error <- TRUE
  sub$defer_builtin_arity_error <- TRUE
  deferred_error <- NULL
  right <- tryCatch(
    r2f(
      args[[2L]],
      scope,
      ...,
      hoist = sub,
      defer_andor_length_error = TRUE
    ),
    quickr_deferred_branch_error = function(error) {
      deferred_error <<- conditionMessage(error)
      NULL
    }
  )
  if (is.null(deferred_error)) {
    right <- scalarize_andor_operand(
      right,
      op,
      sub,
      scope,
      defer_length_error = TRUE
    )
  } else {
    emit_quickr_error_if(".true.", deferred_error, sub, scope)
    right <- Fortran(".false.", Variable("logical"))
  }

  hoist$emit(glue("{tmp@name} = {left}"))
  cond <- if (op == "&&") tmp@name else glue(".not. {tmp@name}")
  hoist$emit(glue("if ({cond}) then"))
  hoist$emit(indent(sub$render(glue("{tmp@name} = {right}"))))
  hoist$emit("end if")
  Fortran(tmp@name, tmp)
}

register_r2f_handler(c("&&", "||"), compile_andor)
