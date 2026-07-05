# r2f-operators.R
# Table-driven handlers for elementwise binary operators:
#   arithmetic  + - * / ^ %% %/%   (and unary + -)
#   comparison  < <= > >= == !=
#   logical     & && | ||
#
# Each operator is a row in `binop_table`; `compile_binop` answers the two
# framework questions with shared machinery -- result mode and operand
# casts via the layer-1 helpers (arith_join_mode / cast_to_mode /
# promote_arith_pair), operand shapes via resolve_elementwise() -- then
# renders the row's Fortran spelling. Mode rules are a closed set
# (`promote`, `double`, `compare`, `logical`), not arbitrary lambdas, so
# the table stays greppable and the contract stays enumerable.

# One row per operator. Fields:
#   f       -- the Fortran spelling (infix operator text, or function name
#              when `form` is "call")
#   mode    -- the layer-1 rule for operand casts, and thereby the result
#              mode (the join of the cast operands, reported by conform()):
#              "promote"  logical operands join as integer and are cast
#                         (R: TRUE + TRUE is 2L; Fortran has no logical
#                         arithmetic); int/double mixes need no cast --
#                         Fortran's own promotion matches R
#              "double"   operands cast to double (complex passes through)
#              "compare"  operands promoted as for arithmetic; result is
#                         logical
#              "logical"  operands must already be logical; result is
#                         logical
#   form    -- how to render: "infix" (parenthesized, the default), "call"
#              (`f(left, right)`), or "bare" (unparenthesized infix, for
#              `.and.`/`.or.`)
#   cast_operands -- for "promote": cast *both* operands to the join even
#              with no logical present (Fortran `modulo` requires
#              same-typed arguments)
#   special -- the two spellings that need more than `form`:
#              "int_exponent"  `^` casts its base to double but keeps an
#                              integer exponent, and always returns double
#              "floor_divide"  `%/%` lowers to floor(left / right) in the
#                              result mode's arithmetic
binop_table <- list(
  "+" = list(f = "+", mode = "promote"),
  "-" = list(f = "-", mode = "promote"),
  "*" = list(f = "*", mode = "promote"),
  "/" = list(f = "/", mode = "double"),
  "^" = list(f = "**", mode = "double", special = "int_exponent"),
  "%%" = list(
    f = "modulo",
    mode = "promote",
    cast_operands = TRUE,
    form = "call"
  ),
  "%/%" = list(f = NULL, mode = "promote", special = "floor_divide"),
  "<" = list(f = "<", mode = "compare"),
  "<=" = list(f = "<=", mode = "compare"),
  ">" = list(f = ">", mode = "compare"),
  ">=" = list(f = ">=", mode = "compare"),
  "==" = list(f = "==", mode = "compare"),
  "!=" = list(f = "/=", mode = "compare"),
  "&" = list(f = ".and.", mode = "logical", form = "bare"),
  # TODO: && and || are compiled like & and | (elementwise, no
  # short-circuit); the scalar forms probably need more type checking.
  "&&" = list(f = ".and.", mode = "logical", form = "bare"),
  "|" = list(f = ".or.", mode = "logical", form = "bare"),
  "||" = list(f = ".or.", mode = "logical", form = "bare")
)

# Layer 1 for one table row: cast operands per the row's mode rule.
cast_binop_operands <- function(spec, op, left, right) {
  switch(
    spec$mode,
    promote = if (isTRUE(spec$cast_operands)) {
      # `modulo` requires same-typed arguments, so cast both operands to
      # the join (logical joins as integer: R's TRUE %% TRUE is 0L).
      mode <- arith_join_mode(left, right)
      list(
        left = cast_to_mode(left, mode, op),
        right = cast_to_mode(right, mode, op)
      )
    } else {
      promote_arith_pair(left, right, op)
    },
    double = if (identical(spec$special, "int_exponent")) {
      # R's ^ always returns double (R_pow), so cast the base. Keep an
      # integer exponent as integer: Fortran `real ** int` is exact and,
      # unlike `real ** real`, defined for negative bases -- matching R,
      # which special-cases whole-number exponents.
      if (identical(right@value@mode, "logical")) {
        right <- cast_to_mode(right, "integer", op)
      }
      list(left = maybe_cast_double(left), right = right)
    } else {
      list(left = maybe_cast_double(left), right = maybe_cast_double(right))
    },
    # R compares logicals as integers; Fortran has no logical comparison.
    compare = promote_arith_pair(left, right, "comparison"),
    logical = {
      for (operand in list(left, right)) {
        if (operand@value@mode != "logical") {
          stop("must be logical")
        }
      }
      list(
        left = booleanize_logical_as_int(left),
        right = booleanize_logical_as_int(right)
      )
    }
  )
}

# Render one table row over cast, shape-resolved operands. `var` is the
# conformed result Variable (floor_divide branches on its mode and hoists
# a quotient temporary with its dims).
render_binop <- function(spec, left, right, var, hoist) {
  if (identical(spec$special, "int_exponent")) {
    # Parenthesizing the exponent avoids non-standard `** -1_c_int`.
    return(glue("({left} ** ({right}))"))
  }
  if (identical(spec$special, "floor_divide")) {
    return(switch(
      var@mode,
      integer = glue(
        "int(floor(real({left}, kind=c_double) / real({right}, kind=c_double)), kind=c_int)"
      ),
      double = {
        # The quotient is spliced three times by real_floor_expr(), so
        # hoist it to evaluate once.
        q <- hoist_unless_name(
          Fortran(glue("({left} / {right})"), var),
          hoist
        )
        real_floor_expr(q)
      },
      stop("%/% only implemented for numeric types")
    ))
  }
  switch(
    spec$form %||% "infix",
    infix = glue("({left} {spec$f} {right})"),
    call = glue("{spec$f}({left}, {right})"),
    bare = glue("{left} {spec$f} {right}")
  )
}

compile_binop <- function(args, scope, ..., hoist = NULL) {
  op <- last(list(...)$calls)
  spec <- binop_table[[op]]

  if (length(args) == 1L) {
    # Unary + and -. R: +TRUE is 1L, -TRUE is -1L.
    x <- r2f(args[[1L]], scope, ..., hoist = hoist)
    x <- cast_to_mode(x, arith_join_mode(x), paste("unary", op))
    return(Fortran(glue("({op}{x})"), Variable(x@value@mode, x@value@dims)))
  }

  .[left, right] <- lapply(args, r2f, scope, ..., hoist = hoist)

  .[left, right] <- cast_binop_operands(spec, op, left, right)

  # Comparisons and & | follow R's split over length-1 arrays: arithmetic
  # recycles a 1x1 matrix against a longer vector, strict ops error.
  .[left, right] <- resolve_elementwise(
    left,
    right,
    hoist,
    scope,
    scalarize_one_by_one = spec$mode %in% c("promote", "double")
  )

  var <- if (identical(spec$special, "int_exponent")) {
    mode <- reduce_promoted_mode(left, right)
    if (!identical(mode, "complex")) {
      mode <- "double"
    }
    conform(left@value, right@value, mode = mode)
  } else {
    conform(left@value, right@value)
  }

  fortran <- render_binop(spec, left, right, var, hoist)

  if (spec$mode %in% c("compare", "logical")) {
    var@mode <- "logical"
  }
  Fortran(fortran, var)
}

register_r2f_handler(names(binop_table), compile_binop)
