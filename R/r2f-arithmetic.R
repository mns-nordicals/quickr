# r2f-arithmetic.R
# Handlers for arithmetic operators: +, -, *, /, ^, %%, and %/%.

r2f_handlers[["+"]] <- function(args, scope, ..., hoist = NULL) {
  if (length(args) == 1L) {
    x <- r2f(args[[1L]], scope, ..., hoist = hoist)
    x <- cast_to_mode(x, arith_join_mode(x), "unary +")
    return(Fortran(glue("(+{x})"), Variable(x@value@mode, x@value@dims)))
  }

  .[left, right] <- compile_binop_operands(args, scope, ..., hoist = hoist)
  .[left, right] <- promote_arith_pair(left, right, "+")
  .[left, right] <- resolve_elementwise(
    left,
    right,
    hoist,
    scope,
    scalarize_one_by_one = TRUE
  )
  Fortran(glue("({left} + {right})"), conform(left@value, right@value))
}

r2f_handlers[["-"]] <- function(args, scope, ..., hoist = NULL) {
  if (length(args) == 1L) {
    x <- r2f(args[[1L]], scope, ..., hoist = hoist)
    x <- cast_to_mode(x, arith_join_mode(x), "unary -")
    return(Fortran(glue("(-{x})"), Variable(x@value@mode, x@value@dims)))
  }

  .[left, right] <- compile_binop_operands(args, scope, ..., hoist = hoist)
  .[left, right] <- promote_arith_pair(left, right, "-")
  .[left, right] <- resolve_elementwise(
    left,
    right,
    hoist,
    scope,
    scalarize_one_by_one = TRUE
  )
  Fortran(glue("({left} - {right})"), conform(left@value, right@value))
}

r2f_handlers[["*"]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- compile_binop_operands(args, scope, ..., hoist = hoist)
  .[left, right] <- promote_arith_pair(left, right, "*")
  .[left, right] <- resolve_elementwise(
    left,
    right,
    hoist,
    scope,
    scalarize_one_by_one = TRUE
  )
  Fortran(glue("({left} * {right})"), conform(left@value, right@value))
}

r2f_handlers[["/"]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- compile_binop_operands(args, scope, ..., hoist = hoist)
  left <- maybe_cast_double(left)
  right <- maybe_cast_double(right)
  .[left, right] <- resolve_elementwise(
    left,
    right,
    hoist,
    scope,
    scalarize_one_by_one = TRUE
  )
  Fortran(glue("({left} / {right})"), conform(left@value, right@value))
}

r2f_handlers[["^"]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- compile_binop_operands(args, scope, ..., hoist = hoist)

  # R's ^ always returns double. Keep integer exponents as integer so negative
  # bases with whole-number exponents use Fortran's real ** integer form.
  left <- maybe_cast_double(left)
  if (identical(right@value@mode, "logical")) {
    right <- cast_to_mode(right, "integer", "^")
  }
  .[left, right] <- resolve_elementwise(
    left,
    right,
    hoist,
    scope,
    scalarize_one_by_one = TRUE
  )

  mode <- reduce_promoted_mode(left, right)
  if (!identical(mode, "complex")) {
    mode <- "double"
  }
  Fortran(
    glue("({left} ** ({right}))"),
    conform(left@value, right@value, mode = mode)
  )
}

r2f_handlers[["%%"]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- compile_binop_operands(args, scope, ..., hoist = hoist)

  # modulo() requires same-typed operands and has no complex form.
  mode <- arith_join_mode(left, right)
  if (identical(mode, "complex")) {
    stop("unimplemented complex operation", call. = FALSE)
  }
  left <- cast_to_mode(left, mode, "%%")
  right <- cast_to_mode(right, mode, "%%")
  .[left, right] <- resolve_elementwise(
    left,
    right,
    hoist,
    scope,
    scalarize_one_by_one = TRUE
  )
  Fortran(glue("modulo({left}, {right})"), conform(left@value, right@value))
}

r2f_handlers[["%/%"]] <- function(args, scope, ..., hoist = NULL) {
  .[left, right] <- compile_binop_operands(args, scope, ..., hoist = hoist)
  .[left, right] <- promote_arith_pair(left, right, "%/%")
  .[left, right] <- resolve_elementwise(
    left,
    right,
    hoist,
    scope,
    scalarize_one_by_one = TRUE
  )
  out <- conform(left@value, right@value)

  expr <- switch(
    out@mode,
    integer = glue(
      "int(floor(real({left}, kind=c_double) / real({right}, kind=c_double)), kind=c_int)"
    ),
    double = {
      # Avoid integer overflow in floor() for large real quotients.
      q <- hoist_unless_name(Fortran(glue("({left} / {right})"), out), hoist)
      real_floor_expr(q)
    },
    stop("%/% only implemented for numeric types")
  )
  Fortran(expr, out)
}
