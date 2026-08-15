# Unit tests for size expression lowering/evaluation

skip_on_cran()

test_that("constant arithmetic in declared dims is evaluated", {
  fn <- function(x) {
    declare(type(x = double(2L + 3L)))
    sum(x)
  }
  qfn <- quick(fn)

  expect_error(
    qfn(as.double(1:4)),
    "length(x) must be 5, not 4",
    fixed = TRUE
  )
  expect_identical(qfn(as.double(1:5)), 15)

  fn_pow <- function(x) {
    declare(type(x = double(2L^3L)))
    sum(x)
  }
  qfn_pow <- quick(fn_pow)

  expect_error(
    qfn_pow(as.double(1:7)),
    "length(x) must be 8, not 7",
    fixed = TRUE
  )
  expect_identical(qfn_pow(as.double(1:8)), 36)
})

test_that("dynamic exponentiation works in declared dims", {
  fn <- function(n) {
    declare(type(n = integer(1)))
    out <- double(n^2L)
    for (i in seq_len(length(out))) {
      out[i] <- as.double(i)
    }
    out
  }

  expect_quick_identical(fn, list(3L), list(4L))
})

test_that("size division keeps double precision until the final cast", {
  fn <- function(x, y) {
    declare(type(x = double(1)), type(y = double(1)))
    double(as.integer(x / y))
  }

  code <- r2f(fn)
  expect_match(
    as.character(code),
    "real(x, kind=c_double) / real(y, kind=c_double)",
    fixed = TRUE
  )
  expect_match(as.character(code), "kind=c_ptrdiff_t", fixed = TRUE)
  expect_match(code@c_bridge, "Rf_asReal(x)", fixed = TRUE)
  expect_match(code@c_bridge, "Rf_asReal(y)", fixed = TRUE)
})

test_that("size integer division evaluates numeric operands before casting", {
  fn <- function(x, y) {
    declare(type(x = double(1)), type(y = double(1)))
    double(x %/% y)
  }

  code <- r2f(fn)
  expect_match(as.character(code), "floor(", fixed = TRUE)
  expect_match(as.character(code), "real(x, kind=c_double)", fixed = TRUE)
  expect_match(as.character(code), "real(y, kind=c_double)", fixed = TRUE)
  expect_match(code@c_bridge, "floor(", fixed = TRUE)
  expect_match(code@c_bridge, "Rf_asReal(x)", fixed = TRUE)
  expect_match(code@c_bridge, "Rf_asReal(y)", fixed = TRUE)
})

test_that("size modulo uses the divisor's sign", {
  fn <- function(x, y) {
    declare(type(x = integer(1)), type(y = integer(1)))
    double((x %% y) + 1L)
  }

  code <- r2f(fn)
  expect_match(as.character(code), "modulo(", fixed = TRUE)
  expect_match(code@c_bridge, "floor(", fixed = TRUE)
})

test_that("size powers use the double domain before casting", {
  fn <- function(x, y) {
    declare(type(x = integer(1)), type(y = integer(1)))
    double(as.integer((x^-1L) * y))
  }

  code <- r2f(fn)
  expect_match(as.character(code), "real(x, kind=c_double)", fixed = TRUE)
  expect_match(as.character(code), "kind=c_ptrdiff_t", fixed = TRUE)
  expect_match(code@c_bridge, "R_pow", fixed = TRUE)
})

test_that("dim/length/nrow/ncol are supported in allocation sizes", {
  vec <- function(x) {
    declare(type(x = double(NA)))
    out <- double(length(x) + 1L)
    length(out)
  }
  expect_quick_identical(vec, list(as.double(1:3)), list(as.double(1)))

  mat <- function(x) {
    declare(type(x = double(NA, NA)))
    out <- double(length(x) + 1L)
    length(out)
  }
  expect_quick_identical(mat, list(matrix(as.double(1:12), nrow = 3, ncol = 4)))

  mat_dims <- function(x) {
    declare(type(x = double(NA, NA)))
    out1 <- double(dim(x)[2L] + 1L)
    out2 <- double(nrow(x) + ncol(x))
    c(length(out1), length(out2))
  }
  expect_quick_identical(
    mat_dims,
    list(matrix(as.double(1:12), nrow = 3, ncol = 4)),
    list(matrix(as.double(1:6), nrow = 2, ncol = 3))
  )
})

test_that("dim(x)[axis] errors when axis exceeds rank", {
  bad <- function(x) {
    declare(type(x = double(NA, NA)))
    double(dim(x)[3L])
  }

  expect_error(quick(bad), "insufficient rank", fixed = TRUE)
})
