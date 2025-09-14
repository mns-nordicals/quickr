# Matrix-specific r2f handlers and helpers

size_code <- function(e) {
  # Render a scalar size expression suitable for Fortran actual args
  if (is.null(e)) return("")
  switch(typeof(e),
    integer = as.character(as.integer(e)),
    double  = as.character(as.integer(e)), # sizes are integral
    symbol  = as.character(e),
    language = deparse1(e),
    { as.character(e) }
  )
}

# Emit dgemm call with op flags. Expects double-precision inputs.
emit_dgemm <- function(
  opA, opB,
  A, B, C,
  m_expr, n_expr, k_expr,
  lda_expr, ldb_expr, ldc_expr,
  scope, hoist
) {
  m  <- size_code(m_expr)
  n  <- size_code(n_expr)
  k  <- size_code(k_expr)
  lda <- size_code(lda_expr)
  ldb <- size_code(ldb_expr)
  ldc <- size_code(ldc_expr)

  call <- glue(
    "call dgemm('{opA}','{opB}', {m}, {n}, {k}, 1.0_c_double, {A}, {lda}, {B}, {ldb}, 0.0_c_double, {C}, {ldc})"
  )
  hoist(call)
}


# Emit dgemv call. Expects double-precision inputs.
emit_dgemv <- function(
  transA,
  A, x, y,
  m_expr, n_expr,
  lda_expr,
  scope, hoist
) {
  m  <- size_code(m_expr)
  n  <- size_code(n_expr)
  lda <- size_code(lda_expr)
  call <- glue(
    "call dgemv('{transA}', {m}, {n}, 1.0_c_double, {A}, {lda}, {x}, 1, 0.0_c_double, {y}, 1)"
  )
  hoist(call)
}


# %*% handler with optional destination hint
r2f_handlers[["%*%"]] <- function(args, scope, ..., hoist = NULL, dest = NULL) {
  stopifnot(length(args) == 2L)

  left  <- r2f(args[[1L]], scope, ..., hoist = hoist)
  right <- r2f(args[[2L]], scope, ..., hoist = hoist)

  # Promote to double for BLAS
  left  <- maybe_cast_double(left)
  right <- maybe_cast_double(right)

  lr <- left@value@rank
  rr <- right@value@rank
  if (lr > 2 || rr > 2) {
    stop("%*% only supports vectors/matrices (rank <= 2)")
  }

  # Compute effective shapes
  m_expr <- if (lr == 2) left@value@dims[[1]] else 1L
  k_expr <- if (lr == 2) left@value@dims[[2]] else left@value@dims[[1]]
  n_expr <- if (rr == 2) right@value@dims[[2]] else 1L

  # Leading dimensions
  lda_expr <- if (lr == 2) left@value@dims[[1]] else 1L
  ldb_expr <- if (rr == 2) right@value@dims[[1]] else right@value@dims[[1]]
  ldc_expr <- m_expr

  # Alias guard: LHS must not alias inputs if we write into it.
  use_dest <- FALSE
  if (!is.null(dest)) {
    cname <- dest@name
    use_dest <- !identical(cname, as.character(left)) && !identical(cname, as.character(right))
  }
  # Matrix-Vector: use GEMV
  if (lr == 2 && rr == 1) {
    if (use_dest) {
      emit_dgemv(
        transA = "N",
        A = as.character(left), x = as.character(right), y = dest@name,
        m_expr = left@value@dims[[1]], n_expr = left@value@dims[[2]],
        lda_expr = left@value@dims[[1]],
        scope = scope, hoist = hoist
      )
      out <- Fortran(dest@name, dest)
      attr(out, "writes_to_dest") <- TRUE
      return(out)
    } else {
      yvar <- scope@get_unique_var("double")
      yvar@dims <- list(m_expr, 1L)
      assign(yvar@name, yvar, scope)
      emit_dgemv(
        transA = "N",
        A = as.character(left), x = as.character(right), y = yvar@name,
        m_expr = left@value@dims[[1]], n_expr = left@value@dims[[2]],
        lda_expr = left@value@dims[[1]],
        scope = scope, hoist = hoist
      )
      return(Fortran(yvar@name, yvar))
    }
  }
  # Vector-Matrix: use GEMV with transpose
  if (lr == 1 && rr == 2) {
    if (use_dest) {
      emit_dgemv(
        transA = "T",
        A = as.character(right), x = as.character(left), y = dest@name,
        m_expr = right@value@dims[[1]], n_expr = right@value@dims[[2]],
        lda_expr = right@value@dims[[1]],
        scope = scope, hoist = hoist
      )
      out <- Fortran(dest@name, dest)
      attr(out, "writes_to_dest") <- TRUE
      return(out)
    } else {
      yvar <- scope@get_unique_var("double")
      yvar@dims <- list(1L, n_expr)
      assign(yvar@name, yvar, scope)
      emit_dgemv(
        transA = "T",
        A = as.character(right), x = as.character(left), y = yvar@name,
        m_expr = right@value@dims[[1]], n_expr = right@value@dims[[2]],
        lda_expr = right@value@dims[[1]],
        scope = scope, hoist = hoist
      )
      return(Fortran(yvar@name, yvar))
    }
  }
  if (use_dest) {
    emit_dgemm(
      opA = "N", opB = "N",
      A = as.character(left), B = as.character(right), C = dest@name,
      m_expr = m_expr, n_expr = n_expr, k_expr = k_expr,
      lda_expr = lda_expr, ldb_expr = ldb_expr, ldc_expr = ldc_expr,
      scope = scope, hoist = hoist
    )
    out <- Fortran(dest@name, dest)
    attr(out, "writes_to_dest") <- TRUE
    return(out)
  }

  # No dest or aliased → write to temp and return it
  Cvar <- scope@get_unique_var("double")
  Cvar@dims <- list(m_expr, n_expr)
  assign(Cvar@name, Cvar, scope)

  emit_dgemm(
    opA = "N", opB = "N",
    A = as.character(left), B = as.character(right), C = Cvar@name,
    m_expr = m_expr, n_expr = n_expr, k_expr = k_expr,
    lda_expr = lda_expr, ldb_expr = ldb_expr, ldc_expr = ldc_expr,
    scope = scope, hoist = hoist
  )
  return(Fortran(Cvar@name, Cvar))
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
    len_f <- size_code(len)
    return(Fortran(glue("reshape({x}, [1, int({len_f})])"), val))
  } else if (x@value@rank == 0) {
    return(x)
  } else {
    stop("t() only supports rank 0–2 inputs")
  }
}


# crossprod(x, y = NULL) = t(x) %*% y (or %*% x when y missing)
r2f_handlers[["crossprod"]] <- function(args, scope, ..., hoist = NULL, dest = NULL) {
  x_arg <- args[[1L]]
  y_arg <- if (length(args) >= 2L) args[[2L]] else args$y

  x <- r2f(x_arg, scope, ..., hoist = hoist)
  x <- maybe_cast_double(x)
  y <- if (is.null(y_arg)) x else maybe_cast_double(r2f(y_arg, scope, ..., hoist = hoist))

  m_expr <- if (x@value@rank == 2) x@value@dims[[2]] else 1L
  n_expr <- if (y@value@rank == 2) y@value@dims[[2]] else 1L
  k_expr <- if (x@value@rank == 2) x@value@dims[[1]] else x@value@dims[[1]]

  # Fortran column-major: LDA/LDB are rows of original arrays
  lda_expr <- if (x@value@rank == 2) x@value@dims[[1]] else x@value@dims[[1]]
  ldb_expr <- if (y@value@rank == 2) y@value@dims[[1]] else y@value@dims[[1]]
  ldc_expr <- m_expr

  use_dest <- FALSE
  if (!is.null(dest)) {
    cname <- dest@name
    use_dest <- !identical(cname, as.character(x)) && !identical(cname, as.character(y))
  }
  if (use_dest) {
    emit_dgemm(
      opA = "T", opB = "N",
      A = as.character(x), B = as.character(y), C = dest@name,
      m_expr = m_expr, n_expr = n_expr, k_expr = k_expr,
      lda_expr = lda_expr, ldb_expr = ldb_expr, ldc_expr = ldc_expr,
      scope = scope, hoist = hoist
    )
    out <- Fortran(dest@name, dest)
    attr(out, "writes_to_dest") <- TRUE
    return(out)
  }

  Cvar <- scope@get_unique_var("double")
  Cvar@dims <- list(m_expr, n_expr)
  assign(Cvar@name, Cvar, scope)

  emit_dgemm(
    opA = "T", opB = "N",
    A = as.character(x), B = as.character(y), C = Cvar@name,
    m_expr = m_expr, n_expr = n_expr, k_expr = k_expr,
    lda_expr = lda_expr, ldb_expr = ldb_expr, ldc_expr = ldc_expr,
    scope = scope, hoist = hoist
  )
  return(Fortran(Cvar@name, Cvar))
}


# tcrossprod(x, y = NULL) = x %*% t(y) (or %*% t(x) when y missing)
r2f_handlers[["tcrossprod"]] <- function(args, scope, ..., hoist = NULL, dest = NULL) {
  x_arg <- args[[1L]]
  y_arg <- if (length(args) >= 2L) args[[2L]] else args$y

  x <- r2f(x_arg, scope, ..., hoist = hoist)
  x <- maybe_cast_double(x)
  y <- if (is.null(y_arg)) x else maybe_cast_double(r2f(y_arg, scope, ..., hoist = hoist))

  m_expr <- if (x@value@rank == 2) x@value@dims[[1]] else x@value@dims[[1]]
  n_expr <- if (y@value@rank == 2) y@value@dims[[1]] else y@value@dims[[1]]
  k_expr <- if (x@value@rank == 2) x@value@dims[[2]] else 1L

  lda_expr <- if (x@value@rank == 2) x@value@dims[[1]] else x@value@dims[[1]]
  # LDB is rows of original y regardless of transpose
  ldb_expr <- if (y@value@rank == 2) y@value@dims[[1]] else y@value@dims[[1]]
  ldc_expr <- m_expr

  use_dest <- FALSE
  if (!is.null(dest)) {
    cname <- dest@name
    use_dest <- !identical(cname, as.character(x)) && !identical(cname, as.character(y))
  }
  if (use_dest) {
    emit_dgemm(
      opA = "N", opB = "T",
      A = as.character(x), B = as.character(y), C = dest@name,
      m_expr = m_expr, n_expr = n_expr, k_expr = k_expr,
      lda_expr = lda_expr, ldb_expr = ldb_expr, ldc_expr = ldc_expr,
      scope = scope, hoist = hoist
    )
    out <- Fortran(dest@name, dest)
    attr(out, "writes_to_dest") <- TRUE
    return(out)
  }

  Cvar <- scope@get_unique_var("double")
  Cvar@dims <- list(m_expr, n_expr)
  assign(Cvar@name, Cvar, scope)

  emit_dgemm(
    opA = "N", opB = "T",
    A = as.character(x), B = as.character(y), C = Cvar@name,
    m_expr = m_expr, n_expr = n_expr, k_expr = k_expr,
    lda_expr = lda_expr, ldb_expr = ldb_expr, ldc_expr = ldc_expr,
    scope = scope, hoist = hoist
  )
  return(Fortran(Cvar@name, Cvar))
}
