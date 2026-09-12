test_that("parallel regions preserve definitely assigned body locals", {
  withr::local_envvar(c(OMP_NUM_THREADS = "2", OMP_THREAD_LIMIT = "2"))
  template <- function() {
    values <- c(4L, 2L)
    declare(parallel())
    for (i in 1:2) {
      scratch <- i
    }
    scratch
  }
  for (iterable in list(quote(1:2), quote(values), quote(rev(values)))) {
    for (rhs in list(quote(i), quote(c(i, i + 1L)), quote(as.double(i)))) {
      fn <- template
      body(fn)[[4L]][[3L]] <- iterable
      body(fn)[[4L]][[4L]] <- call("<-", quote(scratch), rhs)
      expect_quick_identical(fn, list())
    }
  }

  # Specialized assignment lowering must preserve its result as well.
  specialized <- function() {
    f <- function(i) i + 1L
    declare(parallel())
    for (i in 1:2) {
      scratch <- f(i)
    }
    scratch
  }
  expect_quick_identical(specialized, list())
})

test_that("body locals propagate through serial and parallel nested loops", {
  fn <- function() {
    declare(parallel())
    for (i in 1:2) {
      for (j in 1:3) {
        scratch <- i + j
      }
    }
    scratch
  }
  expect_quick_identical(fn, list())
  body(fn)[[3L]][[4L]] <- quote({
    declare(parallel())
    for (j in 1:3) {
      scratch <- i + j
    }
  })
  expect_quick_identical(fn, list())
})

test_that("private body locals cannot escape a skipped assignment", {
  fn <- function() {
    declare(parallel())
    for (i in 1:2) {
      if (i == 1L) scratch <- i
    }
    scratch
  }
  expect_identical(fn(), 1L)
  expect_error(quick(fn), "local variable `scratch` may be uninitialized")
  body(fn)[[3L]][[4L]] <- quote({
    if (i == 2L) {
      next
    }
    scratch <- i
  })
  expect_error(quick(fn), "local variable `scratch` may be uninitialized")
  body(fn)[[3L]][[4L]] <- quote({
    if (i == 1L) scratch <- 3L else scratch <- 4L
  })
  expect_quick_identical(fn, list())
})

test_that("private locals do not privatize preexisting shared outputs", {
  fn <- function(out) {
    declare(type(out = integer(2)))
    declare(parallel())
    for (i in 1:2) {
      scratch <- i + 1L
      out[i] <- scratch
    }
    out
  }
  expect_quick_identical(fn, integer(2))
  # A local array initialized before the region is also shared.
  formals(fn) <- NULL
  body(fn)[[2L]] <- quote(out <- integer(2))
  expect_quick_identical(fn, list())
})
