test_that("svd(A) returns singular values (values-only)", {
  fn <- function(A) {
    declare(type(A = double(m, n)))
    svd(A)
  }
  qf <- quick(fn)

  set.seed(11)
  # Tall matrix
  A1 <- matrix(rnorm(8 * 3), 8, 3)
  expect_equal(qf(A1), svd(A1)$d, tolerance = 1e-10)

  # Wide matrix
  A2 <- matrix(rnorm(3 * 8), 3, 8)
  expect_equal(qf(A2), svd(A2)$d, tolerance = 1e-10)
})

test_that("eigen(A) returns eigenvalues for symmetric matrices (values-only)", {
  fn <- function(A) {
    declare(type(A = double(n, n)))
    eigen(A)
  }
  qf <- quick(fn)

  set.seed(12)
  X <- matrix(rnorm(5 * 5), 5, 5)
  A <- crossprod(X)  # symmetric PD
  expect_equal(sort(qf(A)), sort(eigen(A)$values), tolerance = 1e-10)
})

