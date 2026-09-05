test_that("conditional and loop-only bindings cannot escape uninitialized", {
  bodies <- list(
    quote({
      if (flag) {
        out <- 1L
      }
      out
    }),
    quote({
      while (flag) {
        out <- 1L
        flag <- FALSE
      }
      out
    }),
    quote({
      for (i in seq_len(n)) {
        out <- 1L
      }
      out
    }),
    quote({
      for (i in seq_len(n)) {
        n <- n
      }
      i
    }),
    quote({
      if (flag) {
        out <- 1L
      }
      copy <- out
      copy
    }),
    quote({
      if (flag) {
        out <- 1L
      }
      list(flag, out)
    }),
    quote({
      repeat {
        if (flag) {
          break
        }
        out <- 1L
        break
      }
      out
    })
  )
  for (expr in bodies) {
    fn <- function(flag, n) {}
    body(fn) <- as.call(c(
      list(
        quote(`{`),
        quote(declare(type(flag = logical(1)), type(n = integer(1))))
      ),
      as.list(expr)[-1L]
    ))
    expect_error(quick(fn), "may be uninitialized")
  }
})

test_that("initialization on every path preserves compiled results", {
  fn <- function(flag) {
    declare(type(flag = logical(1)))
    if (flag) {
      out <- 1L
    } else {
      out <- 2L
    }
    out
  }
  expect_quick_identical(fn, TRUE, FALSE)

  fn <- function(flag, n) {
    declare(type(flag = logical(1)), type(n = integer(1)))
    out <- 0L
    if (flag) {
      out <- 1L
    }
    for (i in seq_len(n)) {
      out <- out + 1L
    }
    out
  }
  expect_quick_identical(fn, list(FALSE, 0L), list(TRUE, 0L), list(FALSE, 3L))
})

test_that("local closure returns require initialization", {
  fn <- function(flag) {
    declare(type(flag = logical(1)))
    inner <- function() {
      if (flag) {
        out <- 1L
      }
      out
    }
    inner()
  }
  expect_error(quick(fn), "may be uninitialized")
})

test_that("closure captures require initialization where the closure is used", {
  fn <- function(flag) {
    declare(type(flag = logical(1)))
    if (flag) {
      x <- 1L
    }
    inner <- function() x
    inner()
  }
  expect_error(quick(fn), "may be uninitialized")

  fn <- function(flag, n) {
    declare(type(flag = logical(1)), type(n = integer(1)))
    out <- double(n)
    if (flag) {
      x <- 1
    }
    out <- sapply(seq_along(out), function(i) x)
    out
  }
  expect_error(quick(fn), "may be uninitialized")

  fn <- function(flag, n) {
    declare(type(flag = logical(1)), type(n = integer(1)))
    out <- double(n)
    if (flag) {
      x <- 1
    }
    f <- function(i) x
    out <- sapply(seq_along(out), f)
    out
  }
  expect_error(quick(fn), "may be uninitialized")
})

test_that("initialized captures keep compiling and returning R's result", {
  fn <- function(flag) {
    declare(type(flag = logical(1)))
    if (flag) {
      x <- 1L
    } else {
      x <- 2L
    }
    inner <- function() x
    inner()
  }
  expect_quick_identical(fn, TRUE, FALSE)

  fn <- function(flag, n) {
    declare(type(flag = logical(1)), type(n = integer(1)))
    out <- double(n)
    x <- 0
    if (flag) {
      x <- 1
    }
    out <- sapply(seq_along(out), function(i) x + as.double(i))
    out
  }
  expect_quick_identical(fn, list(TRUE, 3L), list(FALSE, 2L))
})
