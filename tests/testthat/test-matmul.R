test_that("matrix multiply with BLAS", {
  fn <- function(A, B) {
    declare(type(A = double(m, k)))
    declare(type(B = double(k, n)))
    C <- A %*% B
    C
  }

  qfn <- quick(fn)

  A <- matrix(as.double(1:6), 2L, 3L)
  B <- matrix(as.double(1:6), 3L, 2L)
  expect_equal(qfn(A, B), A %*% B)
})

test_that("matrix-vector and vector-matrix multiply", {
  fn1 <- function(A, x) {
    declare(type(A = double(m, n)))
    declare(type(x = double(n)))
    y <- A %*% x
    y
  }
  fn2 <- function(x, A) {
    declare(type(x = double(m)))
    declare(type(A = double(m, n)))
    y <- x %*% A
    y
  }

  qfn1 <- quick(fn1)
  qfn2 <- quick(fn2)

  set.seed(1)
  A <- matrix(runif(6), 2L, 3L)
  x <- runif(3)
  expect_equal(qfn1(A, x), drop(A %*% x))

  x2 <- runif(2)
  expect_equal(qfn2(x2, A), drop(x2 %*% A))
})

test_that("transpose t(x)", {
  fn <- function(A) {
    declare(type(A = double(m, n)))
    tA <- t(A)
    tA
  }
  qfn <- quick(fn)
  A <- matrix(as.double(1:6), 2L, 3L)
  expect_equal(qfn(A), t(A))
})

