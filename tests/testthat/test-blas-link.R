test_that("BLAS linkage and numerics (gemm/gemv)", {
  # Keep vendor BLAS single-threaded in tests for stability
  withr::local_envvar(c(
    OPENBLAS_NUM_THREADS = "1",
    MKL_NUM_THREADS = "1",
    OMP_NUM_THREADS = "1"
  ))

  # Matrix %*% matrix via GEMM
  fn_mm <- function(A, B) {
    declare(type(A = double(m, k)))
    declare(type(B = double(k, n)))
    C <- A %*% B
    C
  }

  q_mm <- quick(fn_mm)

  set.seed(123)
  A <- matrix(runif(12), 3L, 4L)
  B <- matrix(runif(20), 4L, 5L)
  expect_equal(q_mm(A, B), A %*% B, tolerance = 1e-12)

  # Matrix %*% vector via GEMV
  fn_mv <- function(A, x) {
    declare(type(A = double(m, n)))
    declare(type(x = double(n)))
    y <- A %*% x
    y
  }
  q_mv <- quick(fn_mv)

  A2 <- matrix(runif(6), 2L, 3L)
  x2 <- runif(3)
  expect_equal(q_mv(A2, x2), drop(A2 %*% x2), tolerance = 1e-12)
})

