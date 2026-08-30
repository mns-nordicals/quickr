test_that("local closure calls reject effectful argument promises", {
  forward <- function() {
    pair <- function(first, second) {
      c(first, second)
    }
    pair(runif(1), runif(1) + 1)
  }
  reverse <- function() {
    pair <- function(first, second) {
      c(second, first)
    }
    pair(runif(1), runif(1) + 1)
  }

  message <- "local closure calls only support pure argument expressions"
  expect_error(quick(forward), message, fixed = TRUE)
  expect_error(quick(reverse), message, fixed = TRUE)
})

test_that("local closure purity respects shadowed operators", {
  fn <- function() {
    state <- 0L
    abs <- function() {
      state <<- state + 1L
      1L
    }
    ignore <- function(x) {
      0L
    }
    ignore(abs())
  }

  expect_error(
    quick(fn),
    "local closure calls only support pure argument expressions",
    fixed = TRUE
  )
})

test_that("local closure calls reject guarded argument promises", {
  fn <- function(a, b) {
    declare(type(a = double(n)), type(b = double(m)))
    ignore <- function(x) {
      1L
    }
    ignore(a + b)
  }

  expect_error(
    quick(fn),
    "local closure calls only support pure argument expressions",
    fixed = TRUE
  )
})

test_that("cbind/rbind evaluate effectful arguments from left to right", {
  cbind_fn <- function() {
    cbind(runif(2), runif(2) + 1)
  }
  rbind_fn <- function() {
    rbind(runif(2), runif(2) + 1)
  }

  set.seed(914)
  cbind_expected <- cbind_fn()
  set.seed(914)
  expect_identical(quick(cbind_fn)(), cbind_expected)

  set.seed(915)
  rbind_expected <- rbind_fn()
  set.seed(915)
  expect_identical(quick(rbind_fn)(), rbind_expected)
})
