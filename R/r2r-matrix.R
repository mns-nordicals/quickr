# Matrix-specific r2f handlers and helpers

# Return the R symbol name if operand is a bare symbol; otherwise NULL.
symbol_name_or_null <- function(x) {
  stopifnot(inherits(x, Fortran))
  rx <- x@r
  if (is.symbol(rx)) as.character(rx) else NULL
}

# TODO(blas): If/when we add complex or mixed-precision BLAS support,
# consider reintroducing dedicated emitter helpers for readability and
# centralized alpha/beta handling and dtype dispatch.
#
# Example stubs (intentionally commented out for now):
# emit_gemm <- function(opA, opB, A, B, C, m_expr, n_expr, k_expr,
#                       lda_expr, ldb_expr, ldc_expr, scope, hoist,
#                       alpha = "1.0_c_double", beta = "0.0_c_double") {
#   hoist(glue(
#     "call dgemm('{opA}','{opB}', {m_expr}, {n_expr}, {k_expr}, {alpha}, {A}, {lda_expr}, {B}, {ldb_expr}, {beta}, {C}, {ldc_expr})"
#   ))
# }
#
# emit_gemv <- function(transA, A, x, y, m_expr, n_expr, lda_expr, scope, hoist,
#                       alpha = "1.0_c_double", beta = "0.0_c_double") {
#   hoist(glue(
#     "call dgemv('{transA}', {m_expr}, {n_expr}, {alpha}, {A}, {lda_expr}, {x}, 1, {beta}, {y}, 1)"
#   ))
# }

# ---- helpers to reduce repetition ----

# Whether it's safe and useful to write into dest (no aliasing with inputs)
can_use_output <- function(dest, left, right) {
  if (is.null(dest)) return(FALSE)
  output_name <- dest@name
  !identical(output_name, as.character(left)) && !identical(output_name, as.character(right))
}

# Centralized GEMM emission with optional destination
# gemm: centralized BLAS GEMM emission.
# - 'hoist' is required and provided by r2f(); handlers thread it through so
#   helpers can pre-emit temporary assignments and BLAS calls.
gemm <- function(
  opA, opB,
  left, right,
  m_expr, n_expr, k_expr,
  lda_expr, ldb_expr, ldc_expr,
  scope, hoist,
  dest = NULL
) {
  if (!is.function(hoist)) stop("internal: hoist must be a function")
  A_name <- symbol_name_or_null(left)
  if (is.null(A_name)) {
    tmp <- scope@get_unique_var(left@value@mode %||% "double")
    tmp@dims <- left@value@dims
    scope[[tmp@name]] <- tmp
    hoist(glue("{tmp@name} = {left}"))
    A_name <- tmp@name
  }
  B_name <- symbol_name_or_null(right)
  if (is.null(B_name)) {
    tmp <- scope@get_unique_var(right@value@mode %||% "double")
    tmp@dims <- right@value@dims
    scope[[tmp@name]] <- tmp
    hoist(glue("{tmp@name} = {right}"))
    B_name <- tmp@name
  }

  if (can_use_output(dest, left, right)) {
    hoist(glue(
      "call dgemm('{opA}','{opB}', {m_expr}, {n_expr}, {k_expr}, 1.0_c_double, {A_name}, {lda_expr}, {B_name}, {ldb_expr}, 0.0_c_double, {dest@name}, {ldc_expr})"
    ))
    out <- Fortran(dest@name, dest)
    attr(out, "writes_to_dest") <- TRUE
    return(out)
  }

  output_var <- scope@get_unique_var("double")
  output_var@dims <- list(m_expr, n_expr)
  scope[[output_var@name]] <- output_var
  hoist(glue(
    "call dgemm('{opA}','{opB}', {m_expr}, {n_expr}, {k_expr}, 1.0_c_double, {A_name}, {lda_expr}, {B_name}, {ldb_expr}, 0.0_c_double, {output_var@name}, {ldc_expr})"
  ))
  Fortran(output_var@name, output_var)
}

# Centralized GEMV emission with optional destination
# gemv: centralized BLAS GEMV emission.
# - 'hoist' is required and provided by r2f(); handlers thread it through so
#   helpers can pre-emit temporary assignments and BLAS calls.
gemv <- function(
  transA,
  A, x,
  m_expr, n_expr,
  lda_expr,
  out_dims,
  scope, hoist,
  dest = NULL
) {
  if (!is.function(hoist)) stop("internal: hoist must be a function")
  A_name <- symbol_name_or_null(A)
  if (is.null(A_name)) {
    tmp <- scope@get_unique_var(A@value@mode %||% "double")
    tmp@dims <- A@value@dims
    scope[[tmp@name]] <- tmp
    hoist(glue("{tmp@name} = {A}"))
    A_name <- tmp@name
  }
  x_name <- symbol_name_or_null(x)
  if (is.null(x_name)) {
    tmp <- scope@get_unique_var(x@value@mode %||% "double")
    tmp@dims <- x@value@dims
    scope[[tmp@name]] <- tmp
    hoist(glue("{tmp@name} = {x}"))
    x_name <- tmp@name
  }

  if (can_use_output(dest, A, x)) {
    # Assign output to output destination
    hoist(glue(
      "call dgemv('{transA}', {m_expr}, {n_expr}, 1.0_c_double, {A_name}, {lda_expr}, {x_name}, 1, 0.0_c_double, {dest@name}, 1)"
    ))
    out <- Fortran(dest@name, dest)
    attr(out, "writes_to_dest") <- TRUE
    return(out)
  }
  # Else assign to a temporary variable
  output_var <- scope@get_unique_var("double")
  output_var@dims <- out_dims
  scope[[output_var@name]] <- output_var
  hoist(glue(
    "call dgemv('{transA}', {m_expr}, {n_expr}, 1.0_c_double, {A_name}, {lda_expr}, {x_name}, 1, 0.0_c_double, {output_var@name}, 1)"
  ))
  Fortran(output_var@name, output_var)
}

# ---- matrix operation handlers ----

# %*% handler with optional destination hint
r2f_handlers[["%*%"]] <- function(args, scope, ..., hoist = NULL, dest = NULL) {
  stopifnot(length(args) == 2L)
  left  <- r2f(args[[1L]], scope, ..., hoist = hoist)
  right <- r2f(args[[2L]], scope, ..., hoist = hoist)

  # Promote to double for BLAS
  left  <- maybe_cast_double(left)
  right <- maybe_cast_double(right)

  left_rank <- left@value@rank
  right_rank <- right@value@rank
  if (left_rank > 2 || right_rank > 2) {
    stop("%*% only supports vectors/matrices (rank <= 2)")
  }

  # Compute effective shapes
  m_expr <- if (left_rank == 2) left@value@dims[[1]] else 1L
  k_expr <- if (left_rank == 2) left@value@dims[[2]] else left@value@dims[[1]]
  n_expr <- if (right_rank == 2) right@value@dims[[2]] else 1L

  # Leading dimensions
  lda_expr <- if (left_rank == 2) left@value@dims[[1]] else 1L
  ldb_expr <- right@value@dims[[1]]
  ldc_expr <- m_expr

  # Matrix-Vector: use GEMV
  if (left_rank == 2 && right_rank == 1) {
    return(gemv(
      transA = "N",
      A = left, x = right,
      m_expr = left@value@dims[[1]], n_expr = left@value@dims[[2]],
      lda_expr = left@value@dims[[1]],
      out_dims = list(m_expr, 1L),
      scope = scope, hoist = hoist, dest = dest
    ))
  }
  # Vector-Matrix: use GEMV with transpose
  if (left_rank == 1 && right_rank == 2) {
    return(gemv(
      transA = "T",
      A = right, x = left,
      m_expr = right@value@dims[[1]], n_expr = right@value@dims[[2]],
      lda_expr = right@value@dims[[1]],
      out_dims = list(1L, n_expr),
      scope = scope, hoist = hoist, dest = dest
    ))
  }

  # Matrix-Matrix
  gemm(
    opA = "N", opB = "N",
    left = left, right = right,
    m_expr = m_expr, n_expr = n_expr, k_expr = k_expr,
    lda_expr = lda_expr, ldb_expr = ldb_expr, ldc_expr = ldc_expr,
    scope = scope, hoist = hoist, dest = dest
  )
}


# t(x) handler: transpose 2D; 1D becomes a 1 x n row matrix
r2f_handlers[["t"]] <- function(args, scope, ..., hoist = NULL) {
  stopifnot(length(args) == 1L)
  x <- r2f(args[[1L]], scope, ..., hoist = hoist)
  x <- maybe_cast_double(x)
  if (x@value@rank == 2) {
    val <- Variable("double", list(x@value@dims[[2]], x@value@dims[[1]]))
    return(Fortran(glue("transpose({x})"), val))
  } else if (x@value@rank == 1) {
    len <- x@value@dims[[1]]
    val <- Variable("double", list(1L, len))
    return(Fortran(glue("reshape({x}, [1, int({len})])"), val))
  } else if (x@value@rank == 0) {
    return(x)
  } else {
    stop("t() only supports rank 0-2 inputs")
  }
}


r2f_handlers[["crossprod"]] <- function(args, scope, ..., hoist = NULL, dest = NULL) {
  x_arg <- args[[1L]]
  y_arg <- if (length(args) >= 2L) args[[2L]] else args$y

  x <- r2f(x_arg, scope, ..., hoist = hoist)
  x <- maybe_cast_double(x)
  y <- if (is.null(y_arg)) x else maybe_cast_double(r2f(y_arg, scope, ..., hoist = hoist))

  m_expr <- if (x@value@rank == 2) x@value@dims[[2]] else 1L
  n_expr <- if (y@value@rank == 2) y@value@dims[[2]] else 1L
  k_expr <- x@value@dims[[1]]

  # Fortran column-major: LDA/LDB are rows of original arrays
  lda_expr <- x@value@dims[[1]]
  ldb_expr <- y@value@dims[[1]]
  ldc_expr <- m_expr

  gemm(
    opA = "T", opB = "N",
    left = x, right = y,
    m_expr = m_expr, n_expr = n_expr, k_expr = k_expr,
    lda_expr = lda_expr, ldb_expr = ldb_expr, ldc_expr = ldc_expr,
    scope = scope, hoist = hoist, dest = dest
  )
}

r2f_handlers[["tcrossprod"]] <- function(args, scope, ..., hoist = NULL, dest = NULL) {
  x_arg <- args[[1L]]
  y_arg <- if (length(args) >= 2L) args[[2L]] else args$y

  x <- r2f(x_arg, scope, ..., hoist = hoist)
  x <- maybe_cast_double(x)
  y <- if (is.null(y_arg)) x else maybe_cast_double(r2f(y_arg, scope, ..., hoist = hoist))

  m_expr <- x@value@dims[[1]]
  n_expr <- y@value@dims[[1]]
  k_expr <- if (x@value@rank == 2) x@value@dims[[2]] else 1L

  lda_expr <- x@value@dims[[1]]
  # LDB is rows of original y regardless of transpose
  ldb_expr <- y@value@dims[[1]]
  ldc_expr <- m_expr

  gemm(
    opA = "N", opB = "T",
    left = x, right = y,
    m_expr = m_expr, n_expr = n_expr, k_expr = k_expr,
    lda_expr = lda_expr, ldb_expr = ldb_expr, ldc_expr = ldc_expr,
    scope = scope, hoist = hoist, dest = dest
  )
}
