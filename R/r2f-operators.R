# r2f-operators.R
# Table-driven handlers for elementwise binary operators:
#   arithmetic  + - * / ^ %% %/%   (and unary + -)
#   comparison  < <= > >= == !=
#   logical     & |
# plus the scalar short-circuit forms && and || (compile_andor below).
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
  "|" = list(f = ".or.", mode = "logical", form = "bare")
)

# Layer 1 for one table row: cast operands per the row's mode rule.
cast_binop_operands <- function(spec, op, left, right) {
  switch(
    spec$mode,
    promote = if (isTRUE(spec$cast_operands)) {
      # `modulo` requires same-typed arguments, so cast both operands to
      # the join (logical joins as integer: R's TRUE %% TRUE is 0L).
      mode <- arith_join_mode(left, right)
      if (identical(mode, "complex")) {
        # Fortran modulo() has no complex form; R refuses too.
        stop("unimplemented complex operation", call. = FALSE)
      }
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
    compare = {
      # R supports equality on complex values but refuses ordering; refuse
      # cleanly here instead of handing gfortran an invalid comparison.
      if (
        op %in%
          c("<", "<=", ">", ">=") &&
          "complex" %in% c(left@value@mode, right@value@mode)
      ) {
        stop("invalid comparison with complex values", call. = FALSE)
      }
      promote_arith_pair(left, right, "comparison")
    },
    logical = {
      for (operand in list(left, right)) {
        if (operand@value@mode != "logical") {
          stop("`", op, "` requires logical operands", call. = FALSE)
        }
      }
      list(
        left = booleanize_logical_as_int(left),
        right = booleanize_logical_as_int(right)
      )
    }
  )
}

# Match `matrix(<scalar>, nrow, ncol)`: a matrix() call
# matrix_call_args() accepts (data/nrow/ncol present, no
# byrow/dimnames) whose data is a length-1 literal or a declared
# scalar. Returns the matched arguments or NULL. Used by
# compile_binop_operands() to lower the fill to a native scalar
# broadcast instead of the O(nrow * ncol) temporary the matrix()
# handler would otherwise materialize; anything it declines falls back
# to the matrix() handler, which raises the real diagnostics.
matrix_scalar_fill_args <- function(e, scope) {
  if (!is.call(e) || !identical(e[[1L]], quote(matrix))) {
    return(NULL)
  }
  mc <- tryCatch(match.call(matrix, e), error = function(...) NULL)
  if (is.null(mc)) {
    return(NULL)
  }
  margs <- tryCatch(
    matrix_call_args(as.list(mc)[-1L]),
    error = function(...) NULL
  )
  if (is.null(margs)) {
    return(NULL)
  }
  data <- margs$data
  data_is_scalar <- (is.atomic(data) && length(data) == 1L && !is.na(data)) ||
    (is.symbol(data) &&
      {
        var <- get0(as.character(data), scope)
        inherits(var, Variable) && passes_as_scalar(var)
      })
  if (!data_is_scalar) {
    return(NULL)
  }
  margs
}

# Compile the two operands of an elementwise binary op. The one special
# case: `matrix(scalar, m, n)` against a genuine rank-2 array broadcasts
# natively -- compile just the scalar and enforce the claimed dims against
# the other operand (compile error when statically wrong, runtime guard
# when symbolic, spelled from the dim expressions since the fill has no
# array to size()). Everything else compiles as written.
compile_binop_operands <- function(args, scope, ..., hoist = NULL) {
  fills <- lapply(args, matrix_scalar_fill_args, scope = scope)
  fill_idx <- which(!map_lgl(fills, is.null))

  if (length(fill_idx) == 1L && !is.null(hoist)) {
    j <- fill_idx
    other <- r2f(args[[3L - j]], scope, ..., hoist = hoist)
    fill_dims <- r2dims(list(fills[[j]]$nrow, fills[[j]]$ncol), scope)
    fill_dims_f <- map_chr(fill_dims, \(d) dims2f(list(d), scope))
    broadcastable <- inherits(other, Fortran) &&
      !is.null(other@value) &&
      other@value@rank == 2L &&
      !passes_as_scalar(other@value) &&
      !any(map_lgl(fill_dims, is_scalar_na)) &&
      all(nzchar(fill_dims_f)) &&
      !any(grepl(":", fill_dims_f, fixed = TRUE))
    if (broadcastable) {
      other_dims <- matrix_dims(other)
      for (axis in 1:2) {
        # The fill has no array to size(), so its side of a runtime guard
        # is spelled from the claimed dim expression via `left_f`.
        guard_conformable_dims(
          fill_dims[[axis]],
          if (axis == 1L) other_dims$rows else other_dims$cols,
          elementwise_matrix_msg,
          hoist,
          scope,
          left = NULL,
          right = other,
          right_axis = axis,
          left_f = glue("({fill_dims_f[[axis]]})")
        )
      }
      fill <- r2f(fills[[j]]$data, scope, ..., hoist = hoist)
      out <- list(fill, other)
      return(if (j == 1L) out else rev(out))
    }
    fallback <- r2f(args[[j]], scope, ..., hoist = hoist)
    out <- list(fallback, other)
    return(if (j == 1L) out else rev(out))
  }

  lapply(args, r2f, scope, ..., hoist = hoist)
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

  .[left, right] <- compile_binop_operands(args, scope, ..., hoist = hoist)

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

# --- Scalar short-circuit operators: && and || ---

# && and || are R's *scalar* control operators: operands must be length 1
# (R errors otherwise), and the right operand is evaluated only when the
# left side does not already decide the answer.
check_andor_operand <- function(x, op) {
  if (is.null(x@value) || !identical(x@value@mode, "logical")) {
    stop("`", op, "` requires logical operands", call. = FALSE)
  }
  if (!passes_as_scalar(x@value)) {
    stop(
      "`",
      op,
      "` requires length-1 operands; use `",
      if (op == "&&") "&" else "|",
      "` for elementwise operations",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# TRUE when evaluating `e` eagerly is indistinguishable from R's lazy
# right-operand evaluation: no side effects, no errors, no traps. A
# conservative whitelist -- names, literals, and compositions of pure
# non-trapping operations. Anything else (subscripts, %%/%/%, function
# calls, ...) gets the conditional lowering.
is_pure_scalar_condition <- function(e) {
  if (is.symbol(e) || (is.atomic(e) && length(e) == 1L)) {
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
  all(vapply(as.list(e)[-1L], is_pure_scalar_condition, logical(1L)))
}

compile_andor <- function(args, scope, ..., hoist = NULL) {
  op <- last(list(...)$calls)
  stopifnot(length(args) == 2L, op %in% c("&&", "||"))

  # R always evaluates the left operand: its hoists stay unconditional.
  left <- r2f(args[[1L]], scope, ..., hoist = hoist)
  check_andor_operand(left, op)
  left <- booleanize_logical_as_int(left)

  f <- if (op == "&&") ".and." else ".or."

  if (is_pure_scalar_condition(args[[2L]])) {
    # Fortran may evaluate both operands of .and./.or.; for a pure right
    # operand that is indistinguishable from short-circuiting, so keep
    # the compact infix form.
    right <- r2f(args[[2L]], scope, ..., hoist = hoist)
    check_andor_operand(right, op)
    right <- booleanize_logical_as_int(right)
    return(Fortran(glue("{left} {f} {right}"), Variable("logical")))
  }

  # The right operand can error or have side effects; R reaches it only
  # when the left side does not decide. Compile it into its own hoist and
  # emit everything inside the conditional.
  if (is.null(hoist)) {
    stop("internal error: `", op, "` requires hoist context", call. = FALSE)
  }
  sub <- new_hoist(scope)
  right <- r2f(args[[2L]], scope, ..., hoist = sub)
  check_andor_operand(right, op)
  right <- booleanize_logical_as_int(right)

  tmp <- hoist$declare_tmp(mode = "logical", dims = NULL)
  hoist$emit(glue("{tmp@name} = {left}"))
  cond <- if (op == "&&") tmp@name else glue(".not. {tmp@name}")
  hoist$emit(glue("if ({cond}) then"))
  hoist$emit(indent(sub$render(glue("{tmp@name} = {right}"))))
  hoist$emit("end if")
  Fortran(tmp@name, tmp)
}

register_r2f_handler(c("&&", "||"), compile_andor)
