test_that("declare(parallel()) and declare(omp()) parallelize loops", {
  skip_if_no_openmp()

  parallel_for <- function(x, n) {
    declare(type(x = double(n)), type(n = integer(1)), type(out = double(n)))
    out <- double(n)
    declare(parallel())
    for (i in seq_len(n)) {
      out[i] <- x[i] + 1
    }
    out
  }

  parallel_sapply <- function(x) {
    declare(type(x = double(NA)))
    out <- double(length(x))
    declare(omp())
    out <- sapply(seq_along(out), function(i) x[i] * 2)
    out
  }

  set.seed(1)
  x <- runif(5)
  expect_quick_identical(parallel_for, list(x, 5L))
  expect_quick_identical(parallel_sapply, list(x))
})

test_that("parallel loops execute fill constructors with private indices", {
  skip_if_no_openmp()

  fn <- function(n) {
    declare(type(n = integer(1)), type(out = integer(n)))
    out <- integer(n)
    declare(parallel())
    for (i in seq_len(n)) {
      out[i] <- sum(c(integer(n), array(integer(n), c(n)), i))
    }
    out
  }

  lines <- strsplit(as.character(r2f(fn)), "\n", fixed = TRUE)[[1L]]
  directive <- grep("!$omp parallel do", lines, value = TRUE, fixed = TRUE)
  expect_match(directive, ",")
  expect_quick_identical(fn, list(256L))
})

test_that("parallel quick avoids unsupported R CMD config probes", {
  skip_if_no_openmp()
  withr::local_options(quickr.fortran_compiler = "gfortran")

  probe <- quickr_r_cmd_config_probe
  names <- character()
  local_mocked_bindings(
    quickr_r_cmd_config_probe = function(name, ...) {
      names <<- c(names, name)
      probe(name, ...)
    },
    .package = "quickr"
  )

  fn <- function(x) {
    declare(
      type(x = double(NA)),
      type(out = double(length(x)))
    )
    out <- double(length(x))
    declare(parallel())
    for (i in seq_along(x)) {
      out[i] <- x[i] + 1
    }
    out
  }

  expect_quick_identical(fn, list(c(1, 2, 3)))
  unsupported <- c("SHLIB_OPENMP_FFLAGS", "SHLIB_OPENMP_CFLAGS")
  expect_length(intersect(names, unsupported), 0L)
})

test_that("parallel sapply supports axpy patterns", {
  skip_if_no_openmp()

  axpy <- function(x, y, a) {
    declare(
      type(x = double(n)),
      type(y = double(n)),
      type(a = double(1)),
      type(out = double(n))
    )
    n <- length(x)
    out <- double(n)
    declare(parallel())
    out <- sapply(seq_along(out), function(i) a * x[i] + y[i])
    out
  }

  axpy_no_out <- function(x, y, a) {
    declare({
      type(x = double(n))
      type(y = double(n))
      type(a = double(1))
      type(out = double(n))
    })
    declare(parallel())
    sapply(seq_along(out), function(i) a * x[i] + y[i])
  }

  set.seed(123)
  x <- runif(8)
  y <- runif(8)
  a <- 0.25

  expect_quick_identical(axpy, list(x, y, a))

  qaxpy_no_out := quick(axpy_no_out)
  expect_identical(qaxpy_no_out(x, y, a), a * x + y)
})

test_that("parallel sapply rejects writes to enclosing bindings", {
  direct <- function() {
    state <- 0L
    declare(parallel())
    out <- sapply(seq_len(1000L), function(i) {
      state <<- state + 1L
      i
    })
    state
  }
  subset <- function() {
    state <- integer(2L)
    declare(omp())
    out <- sapply(seq_len(1000L), function(i) {
      state[1L] <<- state[1L] + 1L
      i
    })
    state
  }
  named <- function() {
    state <- 0L
    bump <- function(i) {
      state <<- state + 1L
      i
    }
    out <- integer(1000L)
    declare(parallel())
    out <- sapply(seq_along(out), bump)
    state
  }
  indirect <- function() {
    state <- 0L
    bump <- function(i) {
      state <<- state + 1L
      i
    }
    declare(parallel())
    out <- sapply(seq_len(1000L), function(i) bump(i))
    state
  }
  nested <- function() {
    state <- 0L
    declare(parallel())
    out <- sapply(seq_len(1000L), function(i) {
      bump <- function(j) {
        state <<- state + 1L
        j
      }
      bump(i)
    })
    state
  }

  for (fn in list(direct, subset, named, indirect, nested)) {
    expect_error(
      quick(fn),
      "parallel sapply() callbacks must not modify enclosing bindings: state",
      fixed = TRUE
    )

    # Removing the parallel declaration preserves sequential host writes.
    serial <- fn
    body(serial) <- as.call(Filter(
      function(expr) {
        !identical(expr, quote(declare(parallel()))) &&
          !identical(expr, quote(declare(omp())))
      },
      as.list(body(serial))
    ))
    expect_quick_identical(serial, list())
  }
})

test_that("parallel sapply allows read-only captures and local writes", {
  fn <- function(x) {
    declare(type(x = integer(NA)))
    offset <- 2L
    declare(parallel())
    out <- sapply(seq_along(x), function(i) {
      value <- x[i] + offset
      value <- value + 1L
      local <- integer(2L)
      local[1L] <- value
      sum(local)
    })
    out
  }

  expect_quick_identical(fn, list(seq_len(1000L)))
})

test_that("parallel sapply supports seq_len(nrow(x))", {
  skip_if_no_openmp()

  row_sums <- function(x) {
    declare(type(x = double(NA, NA)))
    declare(parallel())
    sapply(seq_len(nrow(x)), function(r) sum(x[r, ]))
  }

  set.seed(42)
  x <- matrix(runif(12), nrow = 3)
  expect_quick_identical(row_sums, list(x))
})

test_that("parallel for-loops support value iteration", {
  skip_if_no_openmp()

  value_iter <- function(x) {
    declare(
      type(x = double(NA)),
      type(idx = integer(length(x))),
      type(out = double(length(x)))
    )
    idx <- seq_along(x)
    out <- double(length(x))
    declare(parallel())
    for (i in idx) {
      out[i] <- x[i] * 2
    }
    out
  }

  set.seed(11)
  x <- runif(6)
  expect_quick_identical(value_iter, list(x))
})

test_that("parallel declarations require supported targets", {
  skip_if_no_openmp()

  wrong_target <- function(x) {
    declare(type(x = double(1)))
    declare(parallel())
    x <- x + 1
    x
  }

  dangling_parallel <- function(x) {
    declare(type(x = double(1)))
    declare(parallel())
    x
  }

  expect_error(
    quick(wrong_target),
    regexp = "parallel\\(\\)/omp\\(\\) must be followed by a for-loop or sapply\\(\\)"
  )
  expect_error(
    quick(dangling_parallel),
    regexp = "parallel\\(\\)/omp\\(\\) must be followed by a for-loop or sapply\\(\\)"
  )
})

test_that("parallel declarations do not cross control flow boundaries", {
  skip_if_no_openmp()

  if_decl <- function(x) {
    declare(type(x = double(1)))
    if (x > 0) {
      declare(parallel())
    }
    for (i in seq_len(1L)) {
      x <- x + i
    }
    x
  }

  for_body_decl <- function(x) {
    declare(type(x = double(1)))
    for (i in seq_len(1L)) {
      declare(parallel())
    }
    for (j in seq_len(1L)) {
      x <- x + j
    }
    x
  }

  expect_error(
    quick(if_decl),
    regexp = "parallel\\(\\)/omp\\(\\) must be followed by a for-loop or sapply\\(\\)"
  )
  expect_error(
    quick(for_body_decl),
    regexp = "parallel\\(\\)/omp\\(\\) must be followed by a for-loop or sapply\\(\\)"
  )
})

test_that("openmp functions that use BLAS load and run", {
  skip_if_no_openmp()

  blas_parallel <- function(x, n) {
    declare(
      type(x = double(n, n)),
      type(n = integer(1)),
      type(out = double(n, n))
    )
    declare(parallel())
    for (i in seq_len(1L)) {
      out <- x %*% x
    }
    out
  }

  set.seed(123)
  x <- matrix(runif(16), nrow = 4)
  expect_quick_equal(blas_parallel, list(x, 4L))
})
