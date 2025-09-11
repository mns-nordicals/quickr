test_that("chol2inv matches base and solve(X, I)", {
  fn <- function(A) {
    declare(type(A = double(n, n)))
    U <- chol(A)
    invA <- chol2inv(U)
    invA
  }
  qfn <- quick(fn)

  set.seed(123)
  X <- matrix(rnorm(16), 4, 4)
  A <- crossprod(X) + diag(4) * 1e-6

  invA_quick <- qfn(A)
  invA_base <- chol2inv(chol(A))
  expect_equal(invA_quick, invA_base, tolerance = 1e-8)

  invA_solve <- solve(A, diag(4))
  expect_equal(invA_quick, invA_solve, tolerance = 1e-8)
})

