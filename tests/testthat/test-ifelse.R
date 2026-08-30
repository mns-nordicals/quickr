# Unit test for ifelse translation

test_that("ifelse", {
  fn <- function(a, b) {
    declare(
      type(a = integer(n)),
      type(b = integer(n))
    )

    out <- ifelse(a < b, 1, -1)
    out
  }
  expect_quick_identical(fn, list(-10:10, integer(21)))

  # double version of above
  fn <- function(a, b) {
    declare(
      type(a = double(n)),
      type(b = double(n))
    )

    out <- ifelse(a < b, 1, -1)
    out
  }
  expect_quick_equal(fn, list(seq(-5, 5, length.out = 20), double(20)))
})

test_that("ifelse promotes branches and shapes like test", {
  fn <- function(c, a) {
    declare(type(c = logical(n)), type(a = double(n)))
    ifelse(c, 1L, a)
  }
  expect_translation_snapshots(fn)
  expect_quick_equal(fn, list(c(TRUE, FALSE, TRUE), c(2, 4, 6)))

  # logical branches join as logical
  fn2 <- function(c, a) {
    declare(type(c = logical(n)), type(a = logical(n)))
    ifelse(c, FALSE, a)
  }
  expect_quick_equal(fn2, list(c(TRUE, FALSE, TRUE), c(TRUE, TRUE, FALSE)))
})

test_that("ifelse keeps pure known-shape branches inline", {
  fn <- function(test, a, b, no) {
    declare(
      type(test = logical(3)),
      type(a = double(3)),
      type(b = double(3)),
      type(no = double(3))
    )
    ifelse(test, a + b, no)
  }

  fsub <- r2f(fn)
  expect_false(grepl("block", as.character(fsub), fixed = TRUE))
  expect_quick_identical(
    fn,
    list(
      test = c(TRUE, FALSE, TRUE),
      a = c(1, 2, 3),
      b = c(4, 5, 6),
      no = c(10, 20, 30)
    )
  )
})

test_that("ifelse with scalar test and array branch errors cleanly", {
  fn <- function(c, a) {
    declare(type(c = logical(1)), type(a = double(n)))
    ifelse(c, a, 0)
  }
  expect_error(quick(fn), "shape of `test`")
})

test_that("ifelse with statically mismatched branch lengths is a compile error", {
  fn <- function(c, a) {
    declare(type(c = logical(3)), type(a = double(2)))
    ifelse(c, a, 0)
  }
  expect_error(quick(fn), "R-style recycling is not supported")
})

test_that("ifelse with a branch of different rank than test is a compile error", {
  # merge() requires conformable arguments: a matrix branch under a vector
  # `test` is R recycling, not broadcasting.
  fn <- function(c, m) {
    declare(type(c = logical(3)), type(m = double(3, 3)))
    ifelse(c, m, 0)
  }
  expect_error(quick(fn), "R-style recycling is not supported")

  # the mirror image, and in `no` position: a vector branch under a matrix test
  fn2 <- function(c, a) {
    declare(type(c = logical(2, 2)), type(a = double(4)))
    ifelse(c, 0, a)
  }
  expect_error(quick(fn2), "R-style recycling is not supported")
})

test_that("ifelse guards unknown branch lengths at runtime", {
  fn <- function(c, a, b) {
    declare(type(c = logical(NA)), type(a = double(NA)), type(b = double(NA)))
    ifelse(c, a, b)
  }
  # locks the size guards: a bare merge() with runtime-mismatched
  # assumed-shape vectors read past the shorter branch (returned garbage
  # like 4.65e-310 where R recycles)
  expect_translation_snapshots(fn)
  qfn <- quick(fn)

  cc <- c(TRUE, FALSE, TRUE)
  a <- c(10, 20, 30)
  b <- c(1, 2, 3)
  expect_identical(qfn(cc, a, b), ifelse(cc, a, b))

  expect_error(qfn(cc, c(10, 20), b), "match the shape of `test`")
  expect_error(qfn(cc, a, c(1, 2, 3, 4)), "match the shape of `test`")
})

test_that("ifelse evaluates earlier branches before later shape errors", {
  fn <- function(x) {
    declare(type(x = double(n)))
    ifelse(c(TRUE, FALSE, TRUE), runif(3), runif(3) + x)
  }
  qfn <- quick(fn)

  set.seed(914)
  expect_error(qfn(c(1, 2)), "elementwise vector operations")
  actual_seed <- .Random.seed

  set.seed(914)
  runif(3)
  runif(3)
  expect_identical(actual_seed, .Random.seed)
})

test_that("ifelse does not evaluate unselected branches", {
  fn <- function(test) {
    declare(type(test = logical(3)))
    ifelse(test, runif(3), runif(3))
  }
  qfn <- quick(fn)

  for (test in list(rep(TRUE, 3), rep(FALSE, 3))) {
    set.seed(613)
    expected <- fn(test)
    expected_seed <- .Random.seed

    set.seed(613)
    actual <- qfn(test)
    actual_seed <- .Random.seed

    expect_equal(actual, expected)
    expect_identical(actual_seed, expected_seed)
  }
})

test_that("ifelse defers malformed calls in unselected branches", {
  skipped <- function() {
    ifelse(FALSE, abs(), 1)
  }
  reached <- function() {
    ifelse(TRUE, abs(), 1)
  }

  expect_quick_identical(skipped, list())
  qfn <- quick(reached)
  expect_error(qfn(), "abs")

  reached_with_effects <- function() {
    ifelse(TRUE, runif(1) + abs(), 1)
  }
  qfn <- quick(reached_with_effects)
  set.seed(818)
  expect_error(qfn(), "abs")
  actual_seed <- .Random.seed

  set.seed(818)
  runif(1)
  expect_identical(actual_seed, .Random.seed)
})

test_that("ifelse defers shape errors in unselected branches", {
  fn <- function(test) {
    declare(type(test = logical(3)))
    ifelse(test, logical(2) & logical(3), logical(3))
  }

  expect_quick_identical(fn, list(rep(FALSE, 3)))
  qfn <- quick(fn)
  expect_error(
    qfn(c(TRUE, FALSE, FALSE)),
    "elementwise vector operations",
    fixed = TRUE
  )
})

test_that("ifelse defers mode errors in unselected branches", {
  skipped <- function() {
    ifelse(FALSE, 1L & 1L, FALSE)
  }
  reached <- function() {
    ifelse(TRUE, 1L & 1L, FALSE)
  }

  expect_quick_identical(skipped, list())
  expect_error(quick(reached)(), "requires logical operands")
})

test_that("ifelse allocates impure branch temporaries only when selected", {
  fn <- function(test) {
    declare(type(test = logical(NA)))
    ifelse(test, runif(length(test)), 0)
  }

  code <- as.character(r2f(fn))
  branch <- regexpr("if (any(", code, fixed = TRUE)
  allocation <- regexpr("allocate(btmp", code, fixed = TRUE)
  expect_gt(branch, 0)
  expect_gt(allocation, branch)

  qfn <- quick(fn)
  for (test in list(rep(FALSE, 32), rep(TRUE, 32))) {
    set.seed(729)
    expected <- fn(test)
    expected_seed <- .Random.seed

    set.seed(729)
    actual <- qfn(test)
    actual_seed <- .Random.seed

    expect_equal(actual, expected)
    expect_identical(actual_seed, expected_seed)
  }
})
