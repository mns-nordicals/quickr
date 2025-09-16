test_that("%*% parity with base R (mat-mat)", {
  fn_mm <- function(x, y) {
    declare(type(x = double(m, k)), type(y = double(k, n)))
    x %*% y
  }
  q_mm <- quick(fn_mm)

  set.seed(42)
  m <- 5L; k <- 7L; n <- 3L
  X <- matrix(runif(m * k), m, k)
  Y <- matrix(runif(k * n), k, n)

  expect_equal(q_mm(X, Y), X %*% Y, tolerance = 1e-12)
})

test_that("%*% handles scalar operands like base R", {
  fn_ss <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(NA)))
    x %*% y
  }

  expect_quick_equal(
    fn_ss,
    list(2, 5),
    list(pi, 0.5)
  )
})

test_that("%*% parity with base R (mat-vec, vec-mat)", {
  fn_mv <- function(x, y) {
    declare(type(x = double(m, k)), type(y = double(k)))
    x %*% y
  }
  fn_vm <- function(x, y) {
    declare(type(x = double(m)), type(y = double(m, n)))
    x %*% y
  }

  q_mv <- quick(fn_mv)
  q_vm <- quick(fn_vm)

  set.seed(142)
  m <- 5L; k <- 7L; n <- 3L
  X <- matrix(runif(m * k), m, k)
  Y <- matrix(runif(m * n), m, n)
  v <- runif(k)
  u <- runif(m)

  expect_equal(q_mv(X, v), X %*% v, tolerance = 1e-12)
  expect_equal(q_vm(u, Y), u %*% Y, tolerance = 1e-12)
})


test_that("t() parity with base R (matrix, vector)", {
  fn_tm <- function(x) {
    declare(type(x = double(m, n)))
    t(x)
  }
  fn_tv <- function(x) {
    declare(type(x = double(n)))
    t(x)
  }

  q_tm <- quick(fn_tm)
  q_tv <- quick(fn_tv)

  set.seed(43)
  m <- 4L; n <- 6L
  X <- matrix(runif(m * n), m, n)
  v <- runif(n)

  expect_equal(q_tm(X), t(X), tolerance = 1e-12)
  expect_equal(q_tv(v), t(v), tolerance = 1e-12)
})


test_that("crossprod() parity with base R (x, x) and (x, y)", {
  fn_cx <- function(x) {
    declare(type(x = double(m, n)))
    crossprod(x)
  }
  fn_cxy <- function(x, y) {
    declare(type(x = double(m, k)), type(y = double(m, n)))
    crossprod(x, y)
  }

  q_cx <- quick(fn_cx)
  q_cxy <- quick(fn_cxy)

  set.seed(44)
  m <- 8L; k <- 5L; n <- 3L
  X <- matrix(runif(m * k), m, k)
  Y <- matrix(runif(m * n), m, n)

  expect_equal(q_cx(X), crossprod(X), tolerance = 1e-12)
  expect_equal(q_cxy(X, Y), crossprod(X, Y), tolerance = 1e-12)
})

test_that("crossprod() handles scalar operands like base R", {
  fn_x <- function(x) {
    declare(type(x = double(NA)))
    crossprod(x)
  }
  fn_xy <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(NA)))
    crossprod(x, y)
  }

  expect_quick_equal(fn_x, list(4))
  expect_quick_equal(fn_xy, list(3, 7))
})


test_that("tcrossprod() parity with base R (x, x) and (x, y)", {
  fn_tx <- function(x) {
    declare(type(x = double(m, n)))
    tcrossprod(x)
  }
  fn_txy <- function(x, y) {
    declare(type(x = double(m, k)), type(y = double(n, k)))
    tcrossprod(x, y)
  }

  q_tx <- quick(fn_tx)
  q_txy <- quick(fn_txy)

  set.seed(45)
  m <- 6L; k <- 4L; n <- 7L
  X <- matrix(runif(m * k), m, k)
  Y <- matrix(runif(n * k), n, k)

  expect_equal(q_tx(X), tcrossprod(X), tolerance = 1e-12)
  expect_equal(q_txy(X, Y), tcrossprod(X, Y), tolerance = 1e-12)
})

test_that("tcrossprod() handles scalar operands like base R", {
  fn_x <- function(x) {
    declare(type(x = double(NA)))
    tcrossprod(x)
  }
  fn_xy <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(NA)))
    tcrossprod(x, y)
  }

  expect_quick_equal(fn_x, list(11))
  expect_quick_equal(fn_xy, list(5, -2))
})
