test_that("qr(A) returns upper-triangular R (thin)", {
  fn <- function(A) {
    declare(type(A = double(m, n)))
    qr(A)
  }
  qf <- quick(fn)

  set.seed(1)
  # Tall matrix: m > n
  A1 <- matrix(rnorm(7 * 3), 7, 3)
  R1 <- qf(A1)
  base_qr1 <- qr(A1)
  base_R1 <- qr.R(base_qr1)
  expect_equal(R1, base_R1)

  # Wide matrix: m < n (R is upper trapezoidal: k x n)
  A2 <- matrix(rnorm(3 * 7), 3, 7)
  R2 <- qf(A2)
  base_qr2 <- qr(A2)
  base_R2 <- qr.R(base_qr2)
  expect_equal(R2, base_R2)
})

