skip_on_cran()

test_that("assignment is not skipped when RHS does not write to dest", {
  fn <- function(x) {
    declare(type(x = double(1)))
    x <- 1
    x
  }
  qfn <- quick(fn)

  args <- list(x = 2)
  expect_equal(do.call(fn, args), do.call(qfn, args))
})

test_that("whole-variable assignments reject static shape changes", {
  cases <- list(
    function() {
      x <- integer(2L)
      x <- integer(3L)
      x
    },
    function() {
      x <- integer(2L)
      x <- 0L
      x
    },
    function() {
      x <- matrix(0L, 2L, 3L)
      x <- matrix(0L, 3L, 2L)
      x
    },
    function() {
      x <- integer(6L)
      x <- matrix(0L, 2L, 3L)
      x
    }
  )
  for (fn in cases) {
    expect_error(quick(fn), "assignment must preserve its shape")
  }
})

test_that("whole-variable assignments check dynamic extents", {
  fill <- function(n, m) {
    declare(type(n = integer(1)), type(m = integer(1)))
    x <- integer(n)
    x <- integer(m)
    x
  }
  copy <- function(x, y) {
    declare(type(x = integer(n)), type(y = integer(m)))
    x <- y
    x
  }
  unknown <- function(x, y) {
    declare(type(x = integer(NA)), type(y = integer(NA)))
    x <- y
    x
  }
  qfill <- quick(fill)
  expect_error(qfill(2L, 3L), "assignment must preserve its shape")
  expect_error(qfill(3L, 2L), "assignment must preserve its shape")
  expect_quick_identical(fill, list(2L, 2L), list(0L, 0L))
  for (fn in list(copy, unknown)) {
    qfn <- quick(fn)
    expect_error(qfn(1:2, 1:3), "assignment must preserve its shape")
    expect_error(qfn(1:3, 1:2), "assignment must preserve its shape")
    expect_quick_identical(fn, list(1:2, 3:4), list(integer(), integer()))
  }
})

test_that("matrix and array fills check every assignment extent", {
  mat <- function(x, rows, cols) {
    declare(
      type(x = integer(m, n)),
      type(rows = integer(1)),
      type(cols = integer(1))
    )
    x <- matrix(0L, rows, cols)
    x
  }
  arr <- function(x, rows, cols) {
    declare(
      type(x = integer(m, n)),
      type(rows = integer(1)),
      type(cols = integer(1))
    )
    x <- array(0L, c(rows, cols))
    x
  }
  for (fn in list(mat, arr)) {
    qfn <- quick(fn)
    x <- matrix(1L, 2L, 3L)
    expect_error(qfn(x, 3L, 2L), "assignment must preserve its shape")
    expect_error(qfn(x, 2L, 4L), "assignment must preserve its shape")
    expect_quick_identical(fn, list(x, 2L, 3L))
  }
})

test_that("whole superassignment checks shape and slice fills stay supported", {
  fn <- function(x, n) {
    declare(type(x = integer(NA)), type(n = integer(1)))
    replace_x <- function() {
      x <<- integer(n)
      0L
    }
    replace_x()
    x
  }
  qfn <- quick(fn)
  expect_error(qfn(1:2, 3L), "assignment must preserve its shape")
  expect_quick_identical(fn, list(1:2, 2L))

  fill <- function(x) {
    declare(type(x = integer(n)))
    x[] <- 0L
    x
  }
  expect_quick_identical(fill, list(1:3))
})

test_that("assignment guards bridge scalar and dynamic length-one storage", {
  scalar_target <- function(x, y) {
    declare(type(x = integer(1)), type(y = integer(n)))
    x <- y
    x
  }
  vector_target <- function(x, y) {
    declare(type(x = integer(n)), type(y = integer(1)))
    x <- y
    x
  }
  expect_quick_identical(scalar_target, list(1L, 2L))
  expect_quick_identical(vector_target, list(1L, 2L))
  expect_error(
    quick(scalar_target)(1L, 1:2),
    "assignment must preserve its shape"
  )
  expect_error(
    quick(vector_target)(1:2, 1L),
    "assignment must preserve its shape"
  )
})

test_that("assignment checks stored extents after a symbolic size changes", {
  fn <- function(n) {
    declare(type(n = integer(1)))
    x <- integer(n)
    n <- n + 1L
    x <- integer(n)
    x
  }
  expect_error(quick(fn)(2L), "assignment must preserve its shape")
})

test_that("deferred locals acquire their shape once", {
  copy <- function(x, y) {
    declare(type(x = integer(n)), type(y = integer(m)), type(a = integer(NA)))
    a <- x
    a <- y
    sum(a)
  }
  fill <- function(n, m) {
    declare(type(n = integer(1)), type(m = integer(1)), type(a = integer(NA)))
    a <- integer(n)
    a <- integer(m)
    sum(a)
  }
  expect_quick_identical(copy, list(1:2, 3:4))
  expect_quick_identical(fill, list(2L, 2L))
  expect_error(quick(copy)(1:2, 1:3), "assignment must preserve its shape")
  expect_error(quick(fill)(2L, 3L), "assignment must preserve its shape")
})

test_that("assignment guards accept matching dynamic slices", {
  fn <- function(x, y) {
    declare(type(x = integer(n)), type(y = integer(m, k)))
    x <- y[, 1L]
    x
  }
  y <- matrix(1:6, 3L, 2L)
  expect_quick_identical(fn, list(integer(3L), y))
  expect_error(quick(fn)(integer(2L), y), "assignment must preserve its shape")
})
