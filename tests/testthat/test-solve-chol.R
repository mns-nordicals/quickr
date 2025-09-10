test_that("solve(A, b) via dgesv", {
  fn <- function(A, b) {
    declare(type(A = double(n, n)))
    declare(type(b = double(n)))
    x <- solve(A, b)
    x
  }
  qfn <- quick(fn)

  set.seed(123)
  A <- crossprod(matrix(rnorm(9), 3, 3))
  b <- rnorm(3)
  expect_equal(qfn(A, b), as.numeric(solve(A, b)))
})

test_that("chol(A) upper triangular matches base::chol", {
  fn <- function(A) {
    declare(type(A = double(n, n)))
    U <- chol(A)
    U
  }
  qfn <- quick(fn)

  set.seed(42)
  X <- matrix(rnorm(16), 4, 4)
  A <- crossprod(X) + diag(4) * 1e-6
  expect_equal(qfn(A), chol(A))
})
