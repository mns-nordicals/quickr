test_that("tcrossprod() single vector returns outer product", {
  fn <- function(x) {
    declare(type(x = double(m)))
    tcrossprod(x)
  }
  qf <- quick(fn)
  set.seed(2)
  x <- runif(5)
  expect_equal(qf(x), tcrossprod(x))
})

test_that("tcrossprod(x, y) for vectors returns outer product", {
  fn <- function(x, y) {
    declare(type(x = double(m)))
    declare(type(y = double(n)))
    tcrossprod(x, y)
  }
  qf <- quick(fn)
  set.seed(3)
  x <- runif(4)
  y <- runif(6)
  expect_equal(qf(x, y), tcrossprod(x, y))
})

test_that("crossprod(x, y) for vectors returns scalar dot product", {
  fn <- function(x, y) {
    declare(type(x = double(n)))
    declare(type(y = double(n)))
    crossprod(x, y)
  }
  qf <- quick(fn)
  set.seed(4)
  x <- runif(7)
  y <- runif(7)
  expect_equal(as.numeric(qf(x, y)), as.numeric(crossprod(x, y)))
})

test_that("%*% dimension mismatch errors with friendly message", {
  fn <- function(A, B) {
    declare(type(A = double(m, k)))
    declare(type(B = double(k, n)))
    A %*% B
  }
  qf <- quick(fn)
  A <- matrix(runif(6), 2, 3)
  B <- matrix(runif(4), 2, 2) # B dim(1) != A dim(2)
  expect_error(qf(A, B), regexp = "must equal", fixed = FALSE)
})

test_that("crossprod dimension mismatch errors", {
  fn <- function(A, B) {
    declare(type(A = double(m, k)))
    declare(type(B = double(m, n)))
    crossprod(A, B)
  }
  qf <- quick(fn)
  A <- matrix(runif(6), 2, 3)
  B <- matrix(runif(10), 5, 2) # B dim(1) != A dim(1)
  expect_error(qf(A, B), regexp = "must equal", fixed = FALSE)
})

test_that("tcrossprod dimension mismatch errors", {
  fn <- function(A, B) {
    declare(type(A = double(m, k)))
    declare(type(B = double(n, k)))
    tcrossprod(A, B)
  }
  qf <- quick(fn)
  A <- matrix(runif(6), 2, 3)
  B <- matrix(runif(8), 4, 2) # B dim(2) != A dim(2)
  expect_error(qf(A, B), regexp = "must equal", fixed = FALSE)
})
