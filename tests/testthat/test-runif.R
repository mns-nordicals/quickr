test_that("runif generates random numbers", {
  ## test simple runif
  fn <- function(n) {
    declare(type(n = integer(1)))
    runif(n)
  }

  expect_translation_snapshots(fn)
  qrunif <- quick(fn)

  expect_identical(
    set_seed_and_call(runif, 5L),
    set_seed_and_call(qrunif, 5L)
  )

  expect_identical(
    set_seed_and_call(runif, 1L),
    set_seed_and_call(qrunif, 1L)
  )

  # scalar runif in fortran local
  fn <- function(x) {
    declare(type(x = double(NA)))
    x * runif(1)
  }
  expect_translation_snapshots(fn)
  qfn <- quick(fn)

  x <- runif(5)
  expect_identical(
    set_seed_and_call(fn, x),
    set_seed_and_call(qfn, x)
  )

  # 1d runif array in fortran local
  fn <- function(x) {
    declare(type(x = double(NA)))
    x * runif(length(x))
  }
  expect_translation_snapshots(fn)
  qfn <- quick(fn)

  expect_identical(
    set_seed_and_call(fn, x),
    set_seed_and_call(qfn, x)
  )
})

test_that("runif rejects non-scalar sample counts", {
  expect_error(
    quick(function() {
      sum(runif(c(5L, 3L)))
    }),
    "runif() requires a scalar sample count",
    fixed = TRUE
  )
  expect_error(
    quick(function(n) {
      declare(type(n = integer(NA)))
      runif(n)
    }),
    "runif() requires a scalar sample count",
    fixed = TRUE
  )
  expect_error(
    quick(function(n) {
      declare(type(n = double(2)))
      sum(runif(n))
    }),
    "runif() requires a scalar sample count",
    fixed = TRUE
  )

  fn <- function() {
    sum(runif(c(5L)))
  }
  qfn <- quick(fn)
  expect_identical(set_seed_and_call(qfn), set_seed_and_call(fn))
  set.seed(42)
  expected <- fn()
  expected_seed <- .Random.seed
  set.seed(42)
  expect_identical(qfn(), expected)
  expect_identical(.Random.seed, expected_seed)
})

test_that("runif rejects non-scalar bounds", {
  expect_error(
    quick(function(n, b) {
      declare(type(n = integer(1)), type(b = double(n)))
      sum(runif(n, max = b))
    }),
    "runif() requires a scalar `max` bound",
    fixed = TRUE
  )
  expect_error(
    quick(function(n, b) {
      declare(type(n = integer(1)), type(b = double(n)))
      sum(runif(n, min = b, max = 10))
    }),
    "runif() requires a scalar `min` bound",
    fixed = TRUE
  )
  expect_error(
    quick(function(n) {
      declare(type(n = integer(1)))
      sum(runif(n, max = c(1, 2)))
    }),
    "runif() requires a scalar `max` bound",
    fixed = TRUE
  )

  # A scalar bound still draws exactly `n` values and leaves R's RNG state
  # where R leaves it.
  fn <- function(n, a, b) {
    declare(type(n = integer(1)), type(a = double(1)), type(b = double(1)))
    runif(n, a, b)
  }
  qfn <- quick(fn)
  set.seed(42)
  expected <- fn(4L, 1, 3)
  expected_seed <- .Random.seed
  set.seed(42)
  expect_identical(qfn(4L, 1, 3), expected)
  expect_identical(.Random.seed, expected_seed)
})

test_that("runif with min/max", {
  fn <- function(n, a, b) {
    declare(
      type(n = integer(1)),
      type(a = double(1)),
      type(b = double(1))
    )
    runif(n, a, b)
  }

  expect_translation_snapshots(fn)
  qfn <- quick(fn)

  expect_identical(
    set_seed_and_call(fn, 10L, 3, 11),
    set_seed_and_call(qfn, 10L, 3, 11)
  )

  expect_identical(
    set_seed_and_call(fn, 1L, 3, 11),
    set_seed_and_call(qfn, 1L, 3, 11)
  )

  fn <- function(n, b) {
    declare(
      type(n = integer(1)),
      type(b = double(1))
    )
    runif(n, max = b)
  }

  expect_translation_snapshots(fn)
  qfn <- quick(fn)

  expect_identical(
    set_seed_and_call(fn, 10L, 20),
    set_seed_and_call(qfn, 10L, 20)
  )

  fn <- function(b) {
    declare(
      type(b = double(1))
    )
    runif(1, max = b)
  }

  expect_translation_snapshots(fn)
  qfn <- quick(fn)

  expect_identical(
    set_seed_and_call(fn, 20),
    set_seed_and_call(qfn, 20)
  )

  fn <- function(b) {
    declare(
      type(b = double(1))
    )
    runif(10, max = b)
  }

  expect_translation_snapshots(fn)
  qfn <- quick(fn)

  expect_identical(
    set_seed_and_call(fn, 20),
    set_seed_and_call(qfn, 20)
  )
})

test_that("impure runif() bounds are evaluated exactly once", {
  # `min` is spliced twice into the emitted expression, and the implied-do
  # for array results would re-evaluate spliced bounds per element; R
  # evaluates bounds once per call.
  fn <- function() {
    out <- runif(2L, runif(1L), 10)
    out
  }
  expect_translation_snapshots(fn)
  qfn <- quick(fn)

  expect_identical(
    set_seed_and_call(fn),
    set_seed_and_call(qfn)
  )

  set.seed(1)
  qfn()
  q_next <- runif(1L)
  set.seed(1)
  fn()
  r_next <- runif(1L)
  expect_identical(q_next, r_next)
})

test_that("runif evaluates bounds before rejecting a dynamic count", {
  fn <- function(n) {
    declare(type(n = integer(1)))
    sum(runif(n, runif(1L), runif(1L)))
  }
  qfn <- quick(fn)

  set.seed(1)
  expect_error(fn(-1L), "invalid arguments")
  expected_seed <- .Random.seed

  set.seed(1)
  expect_error(qfn(-1L), "sample count must be non-negative")
  expect_identical(.Random.seed, expected_seed)

  fn <- function(n, x) {
    declare(type(n = integer(1)), type(x = double(1)))
    bump <- function() {
      n <<- -1L
      x <<- x + 10
      20
    }
    runif(n, x, bump())
  }
  qfn <- quick(fn)
  expect_equal(set_seed_and_call(fn, 1L, 1), set_seed_and_call(qfn, 1L, 1))
})

test_that("serial local closures share R's RNG state", {
  direct <- function() {
    draw <- function() runif(1L)
    draw()
  }
  assigned <- function() {
    draw <- function() runif(3L)
    out <- draw()
    out
  }
  nested <- function() {
    draw <- function() runif(1L)
    twice <- function() {
      first <- draw()
      second <- draw()
      c(first, second)
    }
    twice()
  }
  statement <- function() {
    draw <- function() {
      noise <- runif(1L)
      NULL
    }
    draw()
    1L
  }
  default <- function() {
    draw <- function(x = NULL) {
      if (is.null(x)) {
        x <- runif(1L)
      }
      x
    }
    draw()
  }
  supplied <- function() {
    draw <- function(x = runif(1L)) x
    draw(2)
  }
  mapped <- function() {
    sapply(seq_len(3L), function(i) runif(1L))
  }
  nested_map <- function() {
    draw <- function() runif(1L)
    sapply(seq_len(3L), function(i) draw())
  }

  withr::local_seed(735)
  for (fn in list(
    direct,
    assigned,
    nested,
    statement,
    default,
    supplied,
    mapped,
    nested_map
  )) {
    qfn <- quick(fn)
    set.seed(735)
    expected <- list(fn(), fn())
    expected_seed <- .Random.seed
    expected_next <- runif(3L)
    set.seed(735)
    expect_identical(list(qfn(), qfn()), expected)
    expect_identical(.Random.seed, expected_seed)
    expect_identical(runif(3L), expected_next)
  }
})

test_that("conditional closure draws consume RNG only on executed paths", {
  fn <- function(flag) {
    declare(type(flag = logical(1)))
    draw <- function() runif(1L)
    if (flag) {
      out <- draw()
    } else {
      out <- 0
    }
    out
  }
  qfn <- quick(fn)
  withr::local_seed(735)
  for (flag in c(FALSE, TRUE)) {
    set.seed(735)
    expected <- fn(flag)
    expected_seed <- .Random.seed
    set.seed(735)
    expect_identical(qfn(flag), expected)
    expect_identical(.Random.seed, expected_seed)
  }
})

test_that("closure RNG state is published before a runtime error", {
  fn <- function(a) {
    declare(type(a = double(n, m)))
    draw <- function() runif(1L)
    compute <- function(x) {
      noise <- draw()
      sum(solve(x))
    }
    compute(a)
  }
  qfn <- quick(fn)
  a <- matrix(1, 2L, 1L)
  withr::local_seed(735)
  expect_error(fn(a), "square")
  expected_seed <- .Random.seed
  expected_next <- runif(3L)
  set.seed(735)
  expect_error(qfn(a), "solve requires a square matrix")
  expect_identical(.Random.seed, expected_seed)
  expect_identical(runif(3L), expected_next)
})

test_that("parallel loops reject RNG calls hidden in local closures", {
  mapped <- function() {
    draw <- function() runif(1L)
    declare(parallel())
    out <- sapply(seq_len(3L), function(i) draw())
    out
  }
  looped <- function() {
    draw <- function() runif(1L)
    outer <- function() draw()
    out <- numeric(3L)
    declare(parallel())
    for (i in seq_len(3L)) {
      out[i] <- outer()
    }
    out
  }
  for (fn in list(mapped, looped)) {
    expect_error(
      quick(fn),
      "runif() is not supported inside parallel loops",
      fixed = TRUE
    )
  }
})
