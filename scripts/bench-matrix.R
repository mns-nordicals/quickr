#!/usr/bin/env Rscript

# Benchmark matrix-heavy functions: base R vs quickr-compiled

quiet_attach <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Please install '%s' (install.packages('%s'))", pkg, pkg))
  }
}

quiet_attach("devtools")
quiet_attach("bench")

devtools::load_all(quiet = TRUE)

# Pure R reference implementation
f_r <- function(A, B, x) {
  # y2 = A %*% (B %*% x) + 0.1 * (A %*% x)
  y1 <- B %*% x
  y2 <- A %*% y1
  y3 <- A %*% x
  y2 + 0.1 * y3
}

# quickr implementation (matrix multiply chain)
f_q <- quick(function(A, B, x) {
  declare(
    type(A = double(m, k)),
    type(B = double(k, m)),
    type(x = double(m))
  )
  y1 <- B %*% x
  y2 <- A %*% y1
  y3 <- A %*% x
  y2 + 0.1 * y3
})

# Pure R solve (SPD)
g_r <- function(S, y) {
  solve(S, y)
}

# quickr solve (SPD)
g_q <- quick(function(S, y) {
  declare(
    type(S = double(n, n)),
    type(y = double(n))
  )
  solve(S, y)
})

run_one <- function(m = 512L, k = 512L, seed = 42L) {
  set.seed(seed)
  A <- matrix(rnorm(m * k), m, k)
  B <- matrix(rnorm(k * m), k, m)
  x <- rnorm(m)

  cat(sprintf("\n--- Matmul chain: m=%d, k=%d ---\n", m, k))
  res1 <- bench::mark(
    R = f_r(A, B, x),
    quickr = f_q(A, B, x),
    iterations = 1000,
    check = FALSE
  )
  print(res1)

  # SPD solve benchmark (size = m)
  set.seed(seed)
  X <- matrix(rnorm(m * m), m, m)
  S <- crossprod(X) + diag(m) * 1e-6
  y <- rnorm(m)

  cat(sprintf("\n--- Solve SPD: n=%d ---\n", m))
  res2 <- bench::mark(
    R = g_r(S, y),
    quickr = g_q(S, y),
    iterations = 100,
    check = FALSE
  )
  print(res2)

  invisible(list(matmul = res1, solve = res2))
}

# Run with defaults if invoked directly
if (identical(environment(), globalenv())) {
  run_one(512L, 512L)
}

