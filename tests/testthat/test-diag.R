test_that("diag extracts matrix diagonal", {
  fn <- function(A) {
    declare(type(A = double(m, n)))
    d <- diag(A)
    d
  }
  qfn <- quick(fn)

  set.seed(1)
  A <- matrix(runif(6), 2L, 3L)
  expect_equal(qfn(A), diag(A))

  B <- matrix(runif(16), 4L, 4L)
  expect_equal(qfn(B), diag(B))
})

test_that("diag(k) identity matrix and diag(v) diagonal matrix", {
  fn_eye <- function(k) {
    declare(type(k = integer(1)))
    I <- diag(k)
    I
  }
  q_eye <- quick(fn_eye)

  expect_equal(q_eye(1L), diag(1))
  expect_equal(q_eye(3L), diag(3))

  fn_diagv <- function(v) {
    declare(type(v = double(n)))
    D <- diag(v)
    D
  }
  q_diagv <- quick(fn_diagv)

  set.seed(2)
  v <- runif(4)
  expect_equal(q_diagv(v), diag(v))
})
