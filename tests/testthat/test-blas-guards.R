# Runtime conformability guards in BLAS/LAPACK lowerings: dims that cannot
# be verified at compile time get a size() check before the BLAS call
# (never a compile-time warning, never an unchecked call).

skip_on_cran()

test_that("matrix-vector %*% guards an unknown vector length", {
  fn <- function(m, x) {
    declare(type(m = double(3, 3)), type(x = double(NA)))
    m %*% x
  }
  qfn <- expect_no_warning(quick(fn))
  expect_equal(qfn(diag(3), as.double(1:3)), diag(3) %*% 1:3)
  # was: dgemv read past the end of x, returning garbage
  expect_error(
    qfn(diag(3), as.double(1:2)),
    "non-conformable arguments in %*%",
    fixed = TRUE
  )
})

test_that("vector-matrix %*% guards an unknown vector length", {
  fn <- function(x, m) {
    declare(type(x = double(NA)), type(m = double(3, 3)))
    t(x) %*% m
  }
  qfn <- expect_no_warning(quick(fn))
  expect_equal(qfn(as.double(1:3), diag(3)), t(as.double(1:3)) %*% diag(3))
  expect_error(
    qfn(as.double(1:5), diag(3)),
    "non-conformable arguments in %*%",
    fixed = TRUE
  )
})

test_that("triangular solve guards squareness and RHS length", {
  fn <- function(l, x) {
    declare(type(l = double(n, k)), type(x = double(NA)))
    forwardsolve(l, x)
  }
  qfn <- expect_no_warning(quick(fn))
  l <- matrix(c(1, 2, 0, 3), 2, 2)
  expect_equal(qfn(l, c(1, 5)), forwardsolve(l, c(1, 5)))
  expect_error(
    qfn(l, c(1, 5, 9)),
    "non-conformable arguments in triangular solve"
  )
  expect_error(
    qfn(matrix(as.double(1:6), 2, 3), c(1, 5)),
    "triangular solve requires a square matrix"
  )
})

test_that("vector %*% vector guards unknown lengths as whole sizes", {
  fn <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(NA)))
    x %*% y
  }
  # was: the fallthrough guard hardcoded rank-2 axes, emitting size(x, 2)
  # on a rank-1 array -- a gfortran error that made a conformable
  # unknown-length dot product fail to compile at all
  qfn <- expect_no_warning(quick(fn))
  expect_equal(qfn(c(1, 2, 3), c(4, 5, 6)), c(1, 2, 3) %*% c(4, 5, 6))
  expect_error(
    qfn(c(1, 2), c(4, 5, 6)),
    "non-conformable arguments in %*%",
    fixed = TRUE
  )
})

test_that("solve() guards an unknown RHS length", {
  fn <- function(a, b) {
    declare(type(a = double(2, 2)), type(b = double(NA)))
    solve(a, b)
  }
  qfn <- expect_no_warning(quick(fn))
  expect_equal(qfn(diag(2), c(1, 2)), c(1, 2))
  expect_error(qfn(diag(2), c(1, 2, 3)), "non-conformable arguments in solve")
})


test_that("solve(a) and chol() guard squareness", {
  inv <- function(a) {
    declare(type(a = double(n, k)))
    solve(a)
  }
  qinv <- expect_no_warning(quick(inv))
  expect_equal(qinv(diag(2)), diag(2))
  expect_error(
    qinv(matrix(as.double(1:6), 2, 3)),
    "solve requires a square matrix"
  )

  chol_fn <- function(a) {
    declare(type(a = double(n, k)))
    chol(a)
  }
  qchol <- expect_no_warning(quick(chol_fn))
  expect_equal(qchol(diag(2)), diag(2))
  expect_error(
    qchol(matrix(as.double(1:6), 2, 3)),
    "chol requires a square matrix"
  )
})

test_that("matrix-matrix %*% returns zeros for a known empty contraction", {
  fn <- function(a, b) {
    declare(type(a = double(2, 0)), type(b = double(0, 3)))
    a %*% b
  }
  expect_quick_identical(
    fn,
    list(matrix(double(), 2, 0), matrix(double(), 0, 3))
  )
})

test_that("matrix-vector %*% returns zeros for a known empty contraction", {
  fn <- function(a, x) {
    declare(type(a = double(2, 0)), type(x = double(0)))
    a %*% x
  }
  expect_quick_identical(fn, list(matrix(double(), 2, 0), double()))
})

test_that("%*% returns zeros for a symbolic empty contracted dimension", {
  fn <- function(a, b, k) {
    declare(
      type(a = double(2, k)),
      type(b = double(k, 3)),
      type(k = integer(1))
    )
    a %*% b
  }
  expect_quick_identical(
    fn,
    list(matrix(double(), 2, 0), matrix(double(), 0, 3), 0L)
  )
})

test_that("symmetric products return zeros for known empty contractions", {
  cross_vec <- function(x) {
    declare(type(x = double(0)))
    crossprod(x)
  }
  cross_mat <- function(x) {
    declare(type(x = double(0, 2)))
    crossprod(x)
  }
  tcross_mat <- function(x) {
    declare(type(x = double(2, 0)))
    tcrossprod(x)
  }
  cross_symbolic <- function(x, n) {
    declare(type(x = double(n, 2)), type(n = integer(1)))
    crossprod(x)
  }
  tcross_symbolic <- function(x, n) {
    declare(type(x = double(2, n)), type(n = integer(1)))
    tcrossprod(x)
  }

  expect_quick_identical(cross_vec, list(double()))
  expect_quick_identical(cross_mat, list(matrix(double(), 0, 2)))
  expect_quick_identical(tcross_mat, list(matrix(double(), 2, 0)))
  expect_quick_identical(cross_symbolic, list(matrix(double(), 0, 2), 0L))
  expect_quick_identical(tcross_symbolic, list(matrix(double(), 2, 0), 0L))
})

test_that("matrix BLAS rejects known zero-sized outputs", {
  matrix_matrix <- function(a, b) {
    declare(type(a = double(0, 2)), type(b = double(2, 3)))
    a %*% b
  }
  matrix_vector <- function(a, x) {
    declare(type(a = double(0, 2)), type(x = double(2)))
    a %*% x
  }
  tcross_vec <- function(x) {
    declare(type(x = double(0)))
    tcrossprod(x)
  }
  cross_mat <- function(x) {
    declare(type(x = double(0, 0)))
    crossprod(x)
  }

  expect_error(quick(matrix_matrix), "zero-sized outputs are not supported")
  expect_error(quick(matrix_vector), "zero-sized outputs are not supported")
  expect_error(quick(tcross_vec), "zero-sized outputs are not supported")
  expect_error(quick(cross_mat), "zero-sized outputs are not supported")
})

test_that("NA dims are never treated as equal", {
  fn <- function(a, b) {
    declare(type(a = double(NA, NA)), type(b = double(NA, NA)))
    a %*% b
  }
  qfn <- expect_no_warning(quick(fn))
  m <- matrix(as.double(1:4), 2, 2)
  expect_equal(qfn(m, m), m %*% m)
  # was: identical(NA, NA) blessed the pair with no check at all
  expect_error(
    qfn(m, matrix(as.double(1:6), 3, 2)),
    "non-conformable arguments in %*%",
    fixed = TRUE
  )
})
