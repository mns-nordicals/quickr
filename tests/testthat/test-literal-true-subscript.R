test_that("literal TRUE rejects empty vector reads and writes", {
  read <- function(x) {
    declare(type(x = double(NA)))
    x[TRUE]
  }
  write <- function(x) {
    declare(type(x = double(NA)))
    x[(TRUE)] <- 7
    x
  }
  for (fn in list(read, write)) {
    qfn := quick(fn)
    expect_error(qfn(numeric()), "logical mask selects beyond empty")
    expect_identical(qfn(c(2, 3)), fn(c(2, 3)))
    expect_identical(qfn(2), fn(2))

    body(fn)[[2L]] <- quote(declare(type(x = double(0))))
    expect_error(quick(fn), "logical mask selects beyond empty")
    body(fn)[[2L]] <- quote(declare(type(x = double(2))))
    expect_quick_identical(fn, list(c(2, 3)))
  }
})

test_that("literal TRUE validates its selected matrix axis", {
  read <- function(x) {
    declare(type(x = double(NA, NA)))
    x[TRUE, , drop = FALSE]
  }
  write <- function(x) {
    declare(type(x = double(NA, NA)))
    x[TRUE, ] <- 7
    x
  }
  for (fn in list(read, write)) {
    qfn := quick(fn)
    expect_error(
      qfn(matrix(numeric(), 0, 2)),
      "logical mask selects beyond empty"
    )
    for (x in list(matrix(numeric(), 2, 0), matrix(1:4 + 0, 2, 2))) {
      expect_identical(qfn(x), fn(x))
    }
  }
  body(read)[[3L]] <- quote(x[, TRUE, drop = FALSE])
  body(write)[[3L]] <- quote(x[, TRUE] <- 7)
  for (fn in list(read, write)) {
    qfn := quick(fn)
    expect_error(
      qfn(matrix(numeric(), 2, 0)),
      "logical mask selects beyond empty"
    )
    expect_identical(qfn(matrix(numeric(), 0, 2)), fn(matrix(numeric(), 0, 2)))
  }
})

test_that("reductions retain literal TRUE extent guards", {
  template <- function(x) {
    declare(type(x = double(NA)))
    sum(x[TRUE])
  }
  for (op in c("sum", "prod", "min", "max", "any", "all")) {
    fn <- template
    body(fn)[[3L]][[1L]] <- as.name(op)
    logical <- op %in% c("any", "all")
    if (logical) {
      body(fn)[[2L]] <- quote(declare(type(x = logical(NA))))
    }
    qfn := quick(fn)
    expect_error(
      qfn(if (logical) logical() else numeric()),
      "logical mask selects beyond empty"
    )
    x <- if (logical) c(TRUE, FALSE) else c(2, 3)
    expect_identical(qfn(x), fn(x))
  }
})

test_that("literal TRUE guards also apply to superassignment", {
  fn <- function(x) {
    declare(type(x = double(NA)))
    update <- function() {
      x[TRUE] <<- 7
      1L
    }
    ignored <- update()
    x
  }
  qfn := quick(fn)
  expect_error(qfn(numeric()), "logical mask selects beyond empty")
  expect_identical(qfn(c(2, 3)), fn(c(2, 3)))
})
