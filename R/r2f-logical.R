# r2f-logical.R
# Handlers for comparison and logical operators, plus is.null().

# --- Handlers ---

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

lower_comparison_operands <- function(args, scope, op, ..., hoist = NULL) {
  .[left, right] <- lower_elementwise_operands(args, scope, ..., hoist = hoist)
  if (
    op %in%
      c("<", "<=", ">", ">=") &&
      "complex" %in% c(left@value@mode, right@value@mode)
  ) {
    stop("invalid comparison with complex values", call. = FALSE)
  }
  .[left, right] <- promote_arith_pair(left, right, "comparison")
  conform_elementwise_operands(
    left,
    right,
    hoist,
    scope,
    scalarize_one_by_one = FALSE
  )
}

r2f_handlers[["<"]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lower_comparison_operands(
    args,
    scope,
    "<",
    ...,
    hoist = hoist
  )
  value <- infer_result_variable(left@value, right@value)
  value@mode <- "logical"
  Fortran(glue("({left} < {right})"), value)
}

r2f_handlers[["<="]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lower_comparison_operands(
    args,
    scope,
    "<=",
    ...,
    hoist = hoist
  )
  value <- infer_result_variable(left@value, right@value)
  value@mode <- "logical"
  Fortran(glue("({left} <= {right})"), value)
}

r2f_handlers[[">"]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lower_comparison_operands(
    args,
    scope,
    ">",
    ...,
    hoist = hoist
  )
  value <- infer_result_variable(left@value, right@value)
  value@mode <- "logical"
  Fortran(glue("({left} > {right})"), value)
}

r2f_handlers[[">="]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lower_comparison_operands(
    args,
    scope,
    ">=",
    ...,
    hoist = hoist
  )
  value <- infer_result_variable(left@value, right@value)
  value@mode <- "logical"
  Fortran(glue("({left} >= {right})"), value)
}

r2f_handlers[["=="]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lower_comparison_operands(
    args,
    scope,
    "==",
    ...,
    hoist = hoist
  )
  value <- infer_result_variable(left@value, right@value)
  value@mode <- "logical"
  Fortran(glue("({left} == {right})"), value)
}

r2f_handlers[["!="]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lower_comparison_operands(
    args,
    scope,
    "!=",
    ...,
    hoist = hoist
  )
  value <- infer_result_variable(left@value, right@value)
  value@mode <- "logical"
  Fortran(glue("({left} /= {right})"), value)
}

lower_logical_operands <- function(args, scope, op, ..., hoist = NULL) {
  .[left, right] <- lower_elementwise_operands(args, scope, ..., hoist = hoist)
  for (operand in list(left, right)) {
    if (operand@value@mode != "logical") {
      stop("`", op, "` requires logical operands", call. = FALSE)
    }
  }
  left <- booleanize_logical_as_int(left)
  right <- booleanize_logical_as_int(right)
  .[left, right] <- conform_elementwise_operands(
    left,
    right,
    hoist,
    scope,
    scalarize_one_by_one = FALSE
  )
  list(left, right)
}

r2f_handlers[["&"]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lower_logical_operands(
    args,
    scope,
    "&",
    ...,
    hoist = hoist
  )
  value <- infer_result_variable(left@value, right@value)
  value@mode <- "logical"
  Fortran(glue("{left} .and. {right}"), value)
}

r2f_handlers[["|"]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- lower_logical_operands(
    args,
    scope,
    "|",
    ...,
    hoist = hoist
  )
  value <- infer_result_variable(left@value, right@value)
  value@mode <- "logical"
  Fortran(glue("{left} .or. {right}"), value)
}

# && and || are R's *scalar* control operators: operands must be length 1
# (R errors otherwise), and the right operand is evaluated only when the
# left side does not already decide the answer.
scalarize_andor_operand <- function(x, op, hoist, scope) {
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
  if (
    any(vapply(
      dims,
      \(dim) is_wholenumber(dim) && !dim_is_one(dim),
      logical(1L)
    ))
  ) {
    stop(
      message,
      call. = FALSE
    )
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

  if (!all(vapply(dims, dim_is_one, logical(1L)))) {
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
    "/",
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

lower_short_circuit_operator <- function(args, scope, op, ..., hoist = NULL) {
  stopifnot(length(args) == 2L, op %in% c("&&", "||"))

  left <- r2f(args[[1L]], scope, ..., hoist = hoist)
  left <- scalarize_andor_operand(left, op, hoist, scope)

  f <- if (op == "&&") ".and." else ".or."

  if (is_pure_scalar_condition(args[[2L]], scope)) {
    # Fortran may evaluate both operands of .and./.or.; for a pure right
    # operand that is indistinguishable from short-circuiting, so keep
    # the compact infix form.
    right <- r2f(args[[2L]], scope, ..., hoist = hoist)
    right <- scalarize_andor_operand(right, op, hoist, scope)
    return(Fortran(glue("{left} {f} {right}"), Variable("logical")))
  }

  if (is.null(hoist)) {
    stop("internal error: `", op, "` requires hoist context", call. = FALSE)
  }
  # The result must remain visible outside the nested right-operand block.
  # Declare it in the procedure scope so block-local temporaries cannot
  # shadow it.
  tmp <- scope_unique_var(scope, mode = "logical", dims = NULL)
  sub <- new_hoist(scope)
  right <- r2f(args[[2L]], scope, ..., hoist = sub)
  right <- scalarize_andor_operand(right, op, sub, scope)

  hoist$emit(glue("{tmp@name} = {left}"))
  condition <- if (op == "&&") tmp@name else glue(".not. {tmp@name}")
  hoist$emit(glue("if ({condition}) then"))
  hoist$emit(indent(sub$render(glue("{tmp@name} = {right}"))))
  hoist$emit("end if")
  Fortran(tmp@name, tmp)
}

r2f_handlers[["&&"]] <- function(args, scope, ..., hoist = NULL) {
  lower_short_circuit_operator(args, scope, "&&", ..., hoist = hoist)
}

r2f_handlers[["||"]] <- function(args, scope, ..., hoist = NULL) {
  lower_short_circuit_operator(args, scope, "||", ..., hoist = hoist)
}
