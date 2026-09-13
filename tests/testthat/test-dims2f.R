# Tests for dynamic dimension expressions evaluated in Fortran environment

skip_on_cran()

test_that("arithmetic expressions in dimensions compile", {
  fn <- function(n) {
    declare(type(n = integer(1)))
    x <- double(n * 2L)
    y <- double(n - 1L)
    length(x) + length(y)
  }
  expect_translation_snapshots(fn)

  expect_identical(fn(4L), 11L)
  expect_identical(fn(7L), 20L)
  expect_quick_identical(fn, 4L, 7L)
})


test_that("integer division and modulus in dimensions compile", {
  fn <- function(n) {
    declare(type(n = integer(1)))
    out <- double(n %/% 2L + n %% 2L)
    length(out)
  }
  expect_translation_snapshots(fn)

  expect_identical(fn(5L), 3L)
  expect_identical(fn(8L), 4L)
  expect_quick_identical(fn, 5L, 8L)
})


test_that("matrix dimension expressions compile", {
  fn <- function(n) {
    declare(type(n = integer(1)))
    out <- matrix(1, n + 1L, n %/% 2L + 1L)
    dim(out)
  }
  expect_translation_snapshots(fn)

  expect_identical(fn(3L), c(4L, 2L))
  expect_identical(fn(6L), c(7L, 4L))
  expect_quick_identical(fn, 3L, 6L)
})

test_that("constructor extents use reassigned values and alias snapshots", {
  arithmetic <- function() {
    n <- 2L
    n <- n + 1L
    matrix(1, n + 1L, 1L)
  }
  before <- function() {
    n <- 2L
    m <- n
    n <- 3L
    matrix(1, m, 1L)
  }
  after <- function() {
    n <- 2L
    n <- 3L
    m <- n
    n <- 4L
    matrix(1, m, 1L)
  }
  vector <- function() {
    n <- 2L
    n <- 3L
    numeric(n)
  }
  arr <- function() {
    n <- 2L
    n <- 3L
    array(1, c(n, 2L))
  }
  for (fn in list(arithmetic, before, after, vector, arr)) {
    expect_quick_identical(fn, list())
  }
})

test_that("branch joins retain only agreed extent values", {
  same <- function(flag) {
    declare(type(flag = logical(1)))
    n <- 2L
    if (flag) {
      n <- 3L
    } else {
      n <- 3L
    }
    matrix(1, n + 1L, 1L)
  }
  different <- function(flag) {
    declare(type(flag = logical(1)))
    n <- 2L
    if (flag) {
      n <- 3L
    }
    matrix(1, n + 1L, 1L)
  }
  local <- function(flag) {
    declare(type(flag = logical(1)))
    n <- 2L
    if (flag) {
      n <- 3L
    }
    x <- matrix(1, n + 1L, 1L)
    n <- 10L
    sum(x) + nrow(x)
  }
  expect_quick_identical(same, list(FALSE), list(TRUE))
  expect_error(quick(different), "Output size could not be verified.*matrix")
  expect_quick_identical(local, list(FALSE), list(TRUE))
})

test_that("loop extents read each iteration's current value", {
  fn <- function(k) {
    declare(type(k = integer(1)))
    n <- 2L
    total <- 0
    for (i in seq_len(k)) {
      n <- n + 1L
      total <- total + sum(matrix(1, n, 1L))
    }
    total
  }
  output <- function(k) {
    declare(type(k = integer(1)))
    n <- 2L
    for (i in seq_len(k)) {
      n <- n + 1L
    }
    numeric(n)
  }
  expect_quick_identical(fn, list(0L), list(1L), list(3L))
  expect_error(quick(output), "Output size could not be verified")
})

test_that("subassignment and closure mutations invalidate extent values", {
  subset <- function() {
    n <- 2L
    n[1L] <- 3L
    sum(matrix(1, n, 1L))
  }
  closure <- function() {
    n <- 2L
    bump <- function() {
      n <<- n + 1L
      0L
    }
    bump()
    sum(matrix(1, n, 1L))
  }
  expect_quick_identical(subset, list())
  expect_quick_identical(closure, list())
})

test_that("unchanged input arithmetic and arrays created before mutation work", {
  input <- function(n) {
    declare(type(n = integer(1)))
    matrix(1, n + 1L, 1L)
  }
  symbolic <- function(n) {
    declare(type(n = integer(1)))
    m <- n
    m <- m + 1L
    matrix(1, m, 1L)
  }
  before <- function() {
    n <- 2L
    x <- matrix(1, n, 1L)
    n <- 3L
    x
  }
  same <- function() {
    n <- 2L
    n <- 2L
    matrix(1, n, 1L)
  }
  expect_quick_identical(input, list(2L), list(3L))
  expect_quick_identical(symbolic, list(2L), list(3L))
  expect_quick_identical(before, list())
  expect_quick_identical(same, list())
})

test_that("extent queries preserve stored shapes after input mutation", {
  vector <- function(n) {
    declare(type(n = integer(1)))
    x <- numeric(n)
    n <- n + 1L
    y <- numeric(length(x))
    length(y)
  }
  matrix <- function(n) {
    declare(type(n = integer(1)))
    x <- array(1, c(n, 2L))
    n <- 10L
    y <- array(1, dim(x))
    z <- numeric(nrow(x) + ncol(x))
    sum(y) + length(z)
  }
  copied <- function(n) {
    declare(type(n = integer(1)))
    m <- n
    n <- n + 1L
    sum(array(1, c(m, 1L)))
  }
  fill <- function(n) {
    declare(type(n = integer(1)))
    n <- n + 1L
    sum(integer(n))
  }
  for (fn in list(vector, matrix, copied, fill)) {
    expect_quick_identical(fn, list(2L), list(4L))
  }
})

test_that("while and repeat invalidate values carried between iterations", {
  wh <- function(k) {
    declare(type(k = integer(1)))
    n <- 2L
    total <- 0
    while (n < k) {
      total <- total + sum(matrix(1, n, 1L))
      n <- n + 1L
    }
    total
  }
  rep <- function(k) {
    declare(type(k = integer(1)))
    n <- 2L
    total <- 0
    repeat {
      total <- total + sum(matrix(1, n, 1L))
      n <- n + 1L
      if (n >= k) break
    }
    total
  }
  expect_quick_identical(wh, list(2L), list(5L))
  expect_quick_identical(rep, list(2L), list(5L))
})

test_that("local arrays constructed in loops retain their first shape", {
  fn <- function(k) {
    declare(type(k = integer(1)))
    n <- 2L
    total <- 0
    for (i in seq_len(k)) {
      n <- n + 1L
      x <- matrix(1, n, 1L)
      total <- total + sum(x)
    }
    total
  }
  expect_quick_identical(fn, list(0L), list(1L))
  expect_error(quick(fn)(2L), "assignment must preserve its shape")
})

test_that("closure extents preserve constants and observe changing captures", {
  constant <- function() {
    n <- 2L
    make <- function() matrix(1, n, 1L)
    make()
  }
  changing <- function() {
    n <- 2L
    measure <- function() sum(matrix(1, n, 1L))
    first <- measure()
    n <- 3L
    first + measure()
  }
  expect_quick_identical(constant, list())
  expect_quick_identical(changing, list())
})

test_that("reassigned argument extents cannot use the input's entry value for outputs", {
  matrix_output <- function(n) {
    declare(type(n = integer(1)))
    n <- n + 1L
    matrix(1, n, 1L)
  }
  sequence_output <- function(n) {
    declare(type(n = integer(1)))
    n <- n + 1L
    seq_len(n)
  }
  diagonal_output <- function(n) {
    declare(type(n = integer(1)))
    n <- n + 1L
    diag(n)
  }
  double_diagonal_output <- function(n) {
    declare(type(n = double(1)))
    n <- n + 1
    diag(n)
  }
  for (fn in list(
    matrix_output,
    sequence_output,
    diagonal_output,
    double_diagonal_output
  )) {
    expect_error(quick(fn), "Output size could not be verified")
  }
  local <- function(n) {
    declare(type(n = integer(1)))
    n <- n + 1L
    sum(seq_len(n)) + sum(diag(n))
  }
  subset <- function() {
    n <- 2L
    n[1L] <- 3L
    sum(diag(n))
  }
  expect_quick_identical(local, list(2L), list(4L))
  expect_quick_identical(subset, list())
})

test_that("array dimension vectors retain assignment snapshots", {
  copied <- function() {
    n <- 2L
    dims <- c(n, 2L)
    n <- 3L
    array(1, dims)
  }
  reassigned <- function() {
    dims <- c(2L, 2L)
    dims <- c(3L, 2L)
    array(1, dims)
  }
  queried <- function(x) {
    declare(type(x = double(n, m)))
    dims <- dim(x)
    array(1, dims)
  }
  mutated <- function(flag) {
    declare(type(flag = logical(1)))
    dims <- c(2L, 2L)
    if (flag) {
      dims[1L] <- 3L
    }
    sum(array(1, dims))
  }
  expect_quick_identical(copied, list())
  expect_quick_identical(reassigned, list())
  expect_quick_identical(queried, list(matrix(1, 2L, 3L)))
  expect_quick_identical(mutated, list(FALSE), list(TRUE))
})
