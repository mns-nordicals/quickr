# Matrix-specific r2f handlers and helpers

# Helper: returns a default Fortran INTEGER expression for a size node
# Use default INTEGER for BLAS M,N,K,LDA,LDB,LDC to match library expectations.
as_fortran_blas_int <- function(size_expr, scope) {
  se <- dims2f(list(size_expr), scope)
  se <- if (length(se)) se[[1]] else se
  glue("int({se})")
}

# Helper: detect if a Fortran value originated from a bare symbol (simple var)
is_bare_symbol_fortran <- function(f) {
  r <- attr(f, "r", TRUE)
  is.symbol(r)
}

# Emit dgemm call with op flags. Expects double-precision inputs.
emit_dgemm <- function(
  opA, opB,
  A, B, C,
  m_expr, n_expr, k_expr,
  lda_expr, ldb_expr, ldc_expr,
  scope, hoist
) {
  m  <- as_fortran_blas_int(m_expr, scope)
  n  <- as_fortran_blas_int(n_expr, scope)
  k  <- as_fortran_blas_int(k_expr, scope)
  lda <- as_fortran_blas_int(lda_expr, scope)
  ldb <- as_fortran_blas_int(ldb_expr, scope)
  ldc <- as_fortran_blas_int(ldc_expr, scope)

  call <- glue(
    "call dgemm('{opA}','{opB}', {m}, {n}, {k}, 1_c_double, {A}, {lda}, {B}, {ldb}, 0_c_double, {C}, {ldc})"
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

  # Compute effective matrix shapes
  m_expr <- if (lr == 2) left@value@dims[[1]] else 1L
  k_expr <- if (lr == 2) left@value@dims[[2]] else left@value@dims[[1]]
  n_expr <- if (rr == 2) right@value@dims[[2]] else 1L

  # Leading dimensions
  lda_expr <- if (lr == 2) left@value@dims[[1]] else 1L
  ldb_expr <- if (rr == 2) right@value@dims[[1]] else right@value@dims[[1]]
  ldc_expr <- m_expr

  # Prefer BLAS only when both are simple variables (no array sections/exprs)
  use_blas <- isTRUE(getOption("quickr.use_blas", TRUE)) && nzchar(Sys.getenv("QUICKR_DISABLE_BLAS", unset = "")) == FALSE
  can_blas <- use_blas && is_bare_symbol_fortran(left) && is_bare_symbol_fortran(right)

  # Alias guard: LHS must not alias inputs if we write into it.
  use_dest <- FALSE
  if (!is.null(dest) && can_blas) {
    cname <- dest@name
    use_dest <- !identical(cname, as.character(left)) && !identical(cname, as.character(right))
  }

  if (can_blas) {
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

  # Fallback to intrinsic matmul for complex expressions
  Fortran(glue("matmul({left}, {right})"), Variable("double", list(m_expr, n_expr)))
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
    len_f <- as_fortran_blas_int(len, scope)
    return(Fortran(glue("reshape({x}, [int(1,kind=c_int), {len_f}])"), val))
  } else if (x@value@rank == 0) {
    return(x)
  } else {
    stop("t() only supports rank 0–2 inputs")
  }
}


# crossprod(x, y = NULL) = t(x) %*% y (or %*% x when y missing)
r2f_handlers[["crossprod"]] <- function(args, scope, ..., hoist = NULL, dest = NULL) {
  x_arg <- args[[1L]]
  y_arg <- args[[2L]] %||% args$y %||% quote(expr = )

  x <- r2f(x_arg, scope, ..., hoist = hoist)
  x <- maybe_cast_double(x)
  has_y <- !is_missing(y_arg)
  y <- if (has_y) maybe_cast_double(r2f(y_arg, scope, ..., hoist = hoist)) else x

  m_expr <- if (x@value@rank == 2) x@value@dims[[2]] else 1L
  n_expr <- if (y@value@rank == 2) y@value@dims[[2]] else 1L
  k_expr <- if (x@value@rank == 2) x@value@dims[[1]] else x@value@dims[[1]]

  lda_expr <- if (x@value@rank == 2) x@value@dims[[2]] else x@value@dims[[1]]
  ldb_expr <- if (y@value@rank == 2) y@value@dims[[1]] else y@value@dims[[1]]
  ldc_expr <- m_expr

  use_blas <- isTRUE(getOption("quickr.use_blas", TRUE)) && nzchar(Sys.getenv("QUICKR_DISABLE_BLAS", unset = "")) == FALSE
  can_blas <- use_blas && is_bare_symbol_fortran(x) && is_bare_symbol_fortran(y)
  use_dest <- FALSE
  if (!is.null(dest) && can_blas) {
    cname <- dest@name
    use_dest <- !identical(cname, as.character(x)) && !identical(cname, as.character(y))
  }

  if (can_blas) {
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

  Fortran(glue("matmul(transpose({x}), {y})"), Variable("double", list(m_expr, n_expr)))
}


# tcrossprod(x, y = NULL) = x %*% t(y) (or %*% t(x) when y missing)
r2f_handlers[["tcrossprod"]] <- function(args, scope, ..., hoist = NULL, dest = NULL) {
  x_arg <- args[[1L]]
  y_arg <- args[[2L]] %||% args$y %||% quote(expr = )

  x <- r2f(x_arg, scope, ..., hoist = hoist)
  x <- maybe_cast_double(x)
  has_y <- !is_missing(y_arg)
  y <- if (has_y) maybe_cast_double(r2f(y_arg, scope, ..., hoist = hoist)) else x

  m_expr <- if (x@value@rank == 2) x@value@dims[[1]] else x@value@dims[[1]]
  n_expr <- if (y@value@rank == 2) y@value@dims[[1]] else y@value@dims[[1]]
  k_expr <- if (x@value@rank == 2) x@value@dims[[2]] else 1L

  lda_expr <- if (x@value@rank == 2) x@value@dims[[1]] else x@value@dims[[1]]
  ldb_expr <- if (y@value@rank == 2) y@value@dims[[2]] else y@value@dims[[1]]
  ldc_expr <- m_expr

  use_blas <- isTRUE(getOption("quickr.use_blas", TRUE)) && nzchar(Sys.getenv("QUICKR_DISABLE_BLAS", unset = "")) == FALSE
  can_blas <- use_blas && is_bare_symbol_fortran(x) && is_bare_symbol_fortran(y)
  use_dest <- FALSE
  if (!is.null(dest) && can_blas) {
    cname <- dest@name
    use_dest <- !identical(cname, as.character(x)) && !identical(cname, as.character(y))
  }

  if (can_blas) {
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

  Fortran(glue("matmul({x}, transpose({y}))"), Variable("double", list(m_expr, n_expr)))
}
