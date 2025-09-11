# OLS Linear Regression (quickr example)

This example shows how to implement an OLS linear regression with quickr using the matrix features wired to BLAS/LAPACK.

It computes
- coefficients via normal equations: coef = solve(t(X) %*% X, t(X) %*% y)
- residual variance: s2 = crossprod(res) / (n - k)
- standard errors: sqrt(diag((t(X) %*% X)^-1) * s2)

For the standard errors, we avoid forming a full inverse or using a pseudo-inverse by leveraging the Cholesky factor of XtX and performing two triangular solves per coefficient to obtain the diagonal of (XtX)^-1. Alternatively, you can now use `chol2inv(chol(XtX))` directly.

## quickr implementation

```r
qlm <- quick(function(X, y) {
  declare(type(X = double(n, k)), type(y = double(n)))
  n <- nrow(X)
  k <- ncol(X)

  XtX <- crossprod(X)
  Xty <- crossprod(X, y)
  coef <- solve(XtX, Xty)

  res <- y - X %*% coef
  s2 <- crossprod(res) / (n - k)

  # Standard errors via SPD inverse of XtX
  U <- chol(XtX)
  XtX_inv <- chol2inv(U)           # preferred
  # Alternative (simpler, slower): XtX_inv <- solve(XtX, diag(k))

  std_err <- sqrt(diag(XtX_inv) * s2)
  df <- n - k

  list(
    coefficients = coef,
    stderr       = std_err,
    df.residual  = df
  )
})
```

## Usage

```r
set.seed(1)
X <- cbind(1, matrix(rnorm(5 * 2), 5, 2))  # include intercept column if desired
y <- rnorm(5)

fit <- qlm(X, y)
str(fit)
# $ coefficients: num [1:3] ...
# $ stderr      : num [1:3] ...
# $ df.residual : int 2
```

## Notes
- This uses normal equations; for best numerical stability on ill-conditioned X, a QR-based least squares (dgels) solve is preferable — planned next.
- Standard errors can also be computed via the SPD inverse:
  - `XtX_inv <- chol2inv(chol(crossprod(X)))`
  - `std_err <- sqrt(diag(XtX_inv) * s2)`
- `diag(k)` creates a k×k identity; `diag(v)` creates a diagonal matrix; `diag(M)` extracts the diagonal vector.

### chol2inv vs solve(XtX, I)

- Speed: Cholesky factorization/inversion (dpotrf + dpotri) for SPD matrices costs ~1/2 the flops of LU-based solve, and uses triangular structure — generally faster.
- Stability: For SPD matrices like `XtX`, Cholesky-based methods are numerically more stable than LU for the same problem.
- Preconditions: `chol2inv` requires `XtX` to be positive definite (full-rank X). If `chol()` fails, the model is rank-deficient; both paths will struggle (a pseudoinverse would be needed).
- Simplicity: `solve(XtX, diag(k))` is conceptually simple and uses the existing `solve(A, b)` path, but it materializes the full inverse and does more work.
