test_that("dest-aware %*% assigns directly", {
  fn <- function(x, y) {
    declare(
      type(x = double(m, k)),
      type(y = double(k, n)),
      type(z = double(m, n))
    )
    z <- x %*% y
    z
  }
  fs <- r2f(fn)
  fs_txt <- as.character(fs)
  # uses dgemm and writes into z (no temp), no matmul fallback
  expect_true(grepl("call dgemm\\(", fs_txt, perl = TRUE))
  expect_true(grepl("[, ]z[, )]", fs_txt, perl = TRUE))
  expect_false(grepl("tmp[0-9]+_", fs_txt))
  expect_false(grepl("matmul\\(", fs_txt))
})


test_that("chained x %*% x %*% x computes correctly and uses two dgemm calls", {
  fn <- function(x) {
    declare(type(x = double(n, n)))
    out <- x %*% x %*% x
    out
  }
  # numeric parity
  qfn <- quick(fn)
  set.seed(1)
  n <- 4L
  x <- matrix(runif(n * n), n, n)
  expect_equal(qfn(x), x %*% x %*% x, tolerance = 1e-12)

  # codegen: exactly two dgemm calls, no matmul fallback
  fs <- r2f(fn)
  fs_txt <- as.character(fs)
  n_dgemm <- length(gregexpr("call dgemm\\(", fs_txt, perl = TRUE)[[1]])
  # gregexpr returns -1 when no matches
  if (n_dgemm == -1) n_dgemm <- 0
  expect_identical(n_dgemm, 2L)
  expect_false(grepl("matmul\\(", fs_txt, fixed = FALSE))
})
