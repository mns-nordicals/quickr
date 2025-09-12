test_that("linear regression example compiles", {
  my_lm <- function(X, y) {
    declare(type(X = double(n, k)), type(y = double(n)))
    n <- nrow(X)
    k <- ncol(X)

    XtX <- crossprod(X)
    Xty <- crossprod(X, y)
    coef <- solve(XtX, Xty)

    res <- y - X %*% coef
    s2 <- crossprod(res) / (n - k)

    U <- chol(XtX)
    XtX_inv <- chol2inv(U)

    std_err <- sqrt(diag(XtX_inv) * s2)
    df <- n - k

    list(
      coefficients = coef,
      stderr       = std_err,
      df.residual  = df
    )
  }

  # Compiles eagerly when running in tests for this package
  expect_error(quick(my_lm), NA)
})

