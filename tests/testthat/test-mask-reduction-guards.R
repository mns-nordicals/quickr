test_that("all masked reductions validate dynamic and static extents", {
  template <- function(x, mask) {
    declare(type(x = double(n)), type(mask = logical(m)))
    REDUCE(x[mask])
  }
  for (op in c("sum", "prod", "min", "max", "any", "all")) {
    fn <- template
    body(fn)[[3L]][[1L]] <- as.name(op)
    logical <- op %in% c("any", "all")
    if (logical) {
      body(fn)[[2L]] <- quote(declare(
        type(x = logical(n)),
        type(mask = logical(m))
      ))
    }
    x <- if (logical) c(TRUE, FALSE, TRUE, FALSE) else c(1, 2, 4, 8)
    qfn <- quick(fn)
    for (mask in list(c(TRUE, FALSE, TRUE, FALSE), rep(TRUE, 4L))) {
      expect_identical(qfn(x, mask), fn(x, mask))
    }
    if (op %in% c("min", "max")) {
      expect_error(qfn(x[FALSE], logical(0)), "empty inputs")
    } else {
      expect_identical(qfn(x[FALSE], logical(0)), fn(x[FALSE], logical(0)))
    }
    for (mask in list(logical(0), c(TRUE, FALSE), rep(TRUE, 6L))) {
      expect_error(qfn(x, mask), "logical mask extents", info = op)
    }
    body(fn)[[2L]] <- if (logical) {
      quote(declare(type(x = logical(4)), type(mask = logical(2))))
    } else {
      quote(declare(type(x = double(4)), type(mask = logical(2))))
    }
    expect_error(quick(fn), "logical mask extents", info = op)
  }
})

test_that("masked reductions check every matrix axis", {
  fn <- function(x, mask) {
    declare(type(x = double(n, k)), type(mask = logical(m, j)))
    sum(x[mask])
  }
  x <- matrix(as.double(1:6), 2L)
  qfn <- quick(fn)
  mask <- x > 2
  expect_identical(qfn(x, mask), fn(x, mask))
  for (dims in list(c(3L, 2L), c(2L, 4L), c(1L, 3L))) {
    expect_error(
      qfn(x, matrix(TRUE, dims[1L], dims[2L])),
      "logical mask extents"
    )
  }
})

test_that("scalar masks broadcast and scalar data obeys empty identities", {
  template <- function(x, mask) {
    declare(type(x = double(n)), type(mask = logical(1)))
    REDUCE(x[mask])
  }
  for (op in c("sum", "prod", "min", "max", "any", "all")) {
    for (scalar in c(FALSE, TRUE)) {
      fn <- template
      body(fn)[[3L]][[1L]] <- as.name(op)
      logical <- op %in% c("any", "all")
      decl <- if (logical) quote(logical(n)) else quote(double(n))
      if (scalar) {
        decl[[2L]] <- 1L
      }
      body(fn)[[2L]][[2L]][[2L]] <- decl
      x <- if (logical) c(TRUE, FALSE, TRUE) else c(2, 4, 8)
      if (scalar) {
        x <- x[1L]
      }
      qfn <- quick(fn)
      expect_identical(qfn(x, TRUE), fn(x, TRUE))
      if (op %in% c("min", "max")) {
        expect_error(qfn(x, FALSE), "empty inputs")
        if (!scalar) {
          expect_error(qfn(x[FALSE], TRUE), "logical mask selects beyond empty")
        }
      } else {
        expect_identical(qfn(x, FALSE), fn(x, FALSE))
        if (!scalar) {
          expect_error(qfn(x[FALSE], TRUE), "logical mask selects beyond empty")
          expect_identical(qfn(x[FALSE], FALSE), fn(x[FALSE], FALSE))
        }
      }
    }
  }
})

test_that("mask hoisting does not cross operations or nested subsets", {
  template <- function(x, mask) {
    declare(type(x = double(n)), type(mask = logical(n)))
    RESULT
  }
  for (expr in list(
    quote(sum(rev(x[mask]))),
    quote(sum(length(x[mask]))),
    quote(sum(x[mask][c(TRUE, FALSE)])),
    quote(sum(x[mask] + 1)),
    quote(sum(x[mask] + x[!mask])),
    quote(sum(x[(c(TRUE))])),
    quote(any(rev(x[mask] > 3))),
    quote(all(rev(x[mask] > 0)))
  )) {
    fn <- template
    body(fn)[[3L]] <- expr
    expect_quick_identical(fn, list(c(1, 2, 4, 8), c(TRUE, FALSE, TRUE, FALSE)))
  }
})

test_that("scalar logical data accepts a matching dynamic mask", {
  fn <- function(x, mask) {
    declare(type(x = logical(1)), type(mask = logical(m)))
    all(x[mask])
  }
  qfn <- quick(fn)
  for (x in c(TRUE, FALSE)) {
    for (mask in c(TRUE, FALSE)) {
      expect_identical(qfn(x, mask), fn(x, mask))
    }
  }
  expect_error(qfn(TRUE, c(TRUE, FALSE)), "logical mask extents")
})

test_that("length-one masks broadcast in PACK and axis reads and writes", {
  vector <- function(x, mask) {
    declare(type(x = double(n)), type(mask = logical(1)))
    length(x[mask])
  }
  rows <- function(x, mask) {
    declare(type(x = double(n, k)), type(mask = logical(1)))
    sum(x[mask, , drop = FALSE])
  }
  write <- function(x, mask) {
    declare(type(x = double(n, k)), type(mask = logical(1)))
    x[, mask] <- 0
    x
  }
  for (mask in c(TRUE, FALSE)) {
    expect_quick_identical(vector, list(c(1, 2, 4), mask))
    x <- matrix(as.double(1:6), 2L)
    expect_quick_identical(rows, list(x, mask))
    expect_quick_identical(write, list(x, mask))
  }
})

test_that("mask guards preserve operand effects", {
  fn <- function(mask) {
    declare(type(mask = logical(m)))
    sum(runif(4L)[mask])
  }
  qfn <- quick(fn)
  withr::local_seed(937)
  runif(4L)
  expected_seed <- .Random.seed
  set.seed(937)
  expect_error(qfn(c(TRUE, FALSE)), "logical mask extents")
  expect_identical(.Random.seed, expected_seed)
})
