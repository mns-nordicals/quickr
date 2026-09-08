test_that("runtime return sizes allow preceding observable effects", {
  printed <- function(n) {
    declare(type(n = integer(1)))
    marker <- 123L
    print(marker)
    matrix(1, n, n)
  }
  draws <- function(n) {
    declare(type(n = integer(1)))
    noise <- runif(1L)
    matrix(1, n, n)
  }
  nested <- function(n) {
    declare(type(n = integer(1)))
    make <- function() {
      marker <- 123L
      print(marker)
      out <- matrix(1, n, n)
      out
    }
    make()
  }
  operand <- function(n) {
    declare(type(n = integer(1)))
    matrix(runif(1L), n, n)
  }
  listed <- function(n, m) {
    declare(type(n = integer(1)), type(m = integer(1)))
    first <- runif(n)
    second <- numeric(m)
    list(first, second)
  }
  conditional <- function(n, flag) {
    declare(type(n = integer(1)), type(flag = logical(1)))
    if (flag) {
      noise <- runif(1L)
    }
    matrix(1, n, n)
  }
  withr::local_seed(281)
  cases <- list(
    list(fn = printed, args = list(2L), prints = TRUE),
    list(fn = draws, args = list(2L), prints = FALSE),
    list(fn = nested, args = list(2L), prints = TRUE),
    list(fn = operand, args = list(2L), prints = FALSE),
    list(fn = listed, args = list(2L, 3L), prints = FALSE),
    list(fn = conditional, args = list(2L, TRUE), prints = FALSE),
    list(fn = conditional, args = list(2L, FALSE), prints = FALSE)
  )
  for (case in cases) {
    qfn <- quick(case$fn)
    set.seed(281)
    expected_output <- capture.output(expected <- do.call(case$fn, case$args))
    expected_seed <- .Random.seed
    set.seed(281)
    output <- capture.output(actual <- do.call(qfn, case$args))
    expect_identical(actual, expected)
    expect_identical(.Random.seed, expected_seed)
    if (case$prints) {
      expect_match(paste(expected_output, collapse = "\n"), "123")
      expect_match(paste(output, collapse = "\n"), "123")
    } else {
      expect_length(output, 0L)
    }

    # All return buffers are validated before entering the body, so a bad
    # return extent can prevent an earlier print or random draw.
    invalid_args <- case$args
    invalid_args[[if (identical(case$fn, listed)) 2L else 1L]] <- -1L
    seed <- .Random.seed
    output <- capture.output(expect_error(
      do.call(qfn, invalid_args),
      "return dimensions must be non-negative"
    ))
    expect_length(output, 0L)
    expect_identical(.Random.seed, seed)
  }
})

test_that("return allocation supports sizes established before effects", {
  initialized <- function(n) {
    declare(type(n = integer(1)))
    out <- matrix(1, n, n)
    marker <- 123L
    print(marker)
    out
  }
  qinitialized <- quick(initialized)
  output <- capture.output(expect_identical(
    qinitialized(2L),
    matrix(1, 2L, 2L)
  ))
  expect_match(paste(output, collapse = "\n"), "123")
  output <- capture.output(expect_error(
    qinitialized(-1L),
    "return dimensions must be non-negative"
  ))
  expect_length(output, 0L)

  draws <- function(n) {
    declare(type(n = integer(1)))
    runif(n)
  }
  qrandom <- quick(draws)
  withr::local_seed(281)
  expected <- draws(3L)
  expected_seed <- .Random.seed
  set.seed(281)
  expect_identical(qrandom(3L), expected)
  expect_identical(.Random.seed, expected_seed)
  expect_error(qrandom(-1L), "return dimensions must be non-negative")
  expect_identical(.Random.seed, expected_seed)

  same_shape <- function(a) {
    declare(type(a = double(n, m)))
    marker <- 123L
    print(marker)
    a + 1
  }
  qsame <- quick(same_shape)
  a <- matrix(as.double(1:6), 2L, 3L)
  output <- capture.output(expect_identical(qsame(a), a + 1))
  expect_match(paste(output, collapse = "\n"), "123")
})
