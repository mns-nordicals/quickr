test_that("colSums/rowSums work for double matrices", {
  fn1 <- function(A) {
    declare(type(A = double(m, n)))
    colSums(A)
  }
  fn2 <- function(A) {
    declare(type(A = double(m, n)))
    rowSums(A)
  }
  qf1 <- quick(fn1)
  qf2 <- quick(fn2)

  set.seed(123)
  A <- matrix(rnorm(5 * 7), 5, 7)
  expect_equal(qf1(A), colSums(A))
  expect_equal(qf2(A), rowSums(A))
})

test_that("colMeans/rowMeans work for double matrices", {
  fn1 <- function(A) {
    declare(type(A = double(m, n)))
    colMeans(A)
  }
  fn2 <- function(A) {
    declare(type(A = double(m, n)))
    rowMeans(A)
  }
  qf1 <- quick(fn1)
  qf2 <- quick(fn2)

  set.seed(42)
  A <- matrix(rnorm(6 * 4), 6, 4)
  expect_equal(qf1(A), colMeans(A))
  expect_equal(qf2(A), rowMeans(A))
})

