# Unit tests for `for (... in <iterable>)` lowering

skip_on_cran()

test_that("for() supports parenthesized iterables", {
  sum_i <- function(n) {
    declare(type(n = integer(1)))
    s <- 0L
    for (i in (1:n)) {
      s <- s + i
    }
    s
  }

  sum_seq_len <- function(n) {
    declare(type(n = integer(1)))
    s <- 0L
    for (i in (seq_len(n))) {
      s <- s + i
    }
    s
  }

  sum_seq_along <- function(x) {
    declare(type(x = double(NA)))
    s <- 0L
    for (i in (seq_along(x))) {
      s <- s + i
    }
    s
  }

  expect_quick_identical(sum_i, -1L, 0L, 1L, 5L)
  expect_quick_identical(sum_seq_len, 0L, 1L, 5L)
  expect_quick_identical(sum_seq_along, numeric(), c(1, 2, 3))
})

test_that("seq_along() works for scalars", {
  fn <- function(x) {
    declare(type(x = double(1)))
    s <- 0L
    for (i in seq_along(x)) {
      s <- s + i
    }
    s
  }

  expect_quick_identical(fn, 0, 1, -3)
})

test_that("for() supports scalar indexing in singleton iterables", {
  seq_len_scalar <- function(x) {
    declare(type(x = double(1)))
    for (i in seq_len(1L)) {
      x[i] <- x[i] + 1.5
    }
    x
  }

  colon_scalar <- function(x) {
    declare(type(x = double(1)))
    for (i in 1:1) {
      x[i] <- x[i] + 2.0
    }
    x
  }

  expect_quick_identical(seq_len_scalar, 0, 1, -2.5)
  expect_quick_identical(colon_scalar, 0, 1, -2.5)
})

test_that("for() supports seq() direction and step", {
  digits_from_seq <- function(a, b) {
    declare(type(a = integer(1)), type(b = integer(1)))
    out <- 0L
    for (i in seq(a, b)) {
      out <- out * 10L + i
    }
    out
  }

  sum_by_2 <- function(n) {
    declare(type(n = integer(1)))
    s <- 0L
    for (i in seq(1L, n, by = 2L)) {
      s <- s + i
    }
    s
  }

  expect_quick_identical(digits_from_seq, list(3L, 1L), list(1L, 3L))
  expect_quick_identical(sum_by_2, 1L, 5L, 6L)
})

test_that("for() iterable errors are clear", {
  unsupported_iterable <- function() {
    s <- 0L
    for (i in rev(list(1L, 2L))) {
      s <- s + i
    }
    s
  }

  non_integer_seq <- function() {
    s <- 0L
    for (i in seq(1, 2, by = 0.5)) {
      s <- s + i
    }
    s
  }

  non_integer_seq_len <- function() {
    s <- 0L
    for (i in seq_len(2.5)) {
      s <- s + i
    }
    s
  }

  literal_iterable <- function() {
    s <- 0L
    for (i in 1L) {
      s <- s + i
    }
    s
  }

  expect_error(
    quick(unsupported_iterable),
    regexp = "unsupported iterable in for\\(\\): rev\\(list\\("
  )
  expect_error(
    quick(non_integer_seq),
    regexp = "non-integer seq\\(\\)'s not implemented yet"
  )
  expect_error(
    quick(non_integer_seq_len),
    regexp = "seq_len\\(\\) expects an integer scalar"
  )
  expect_error(
    quick(literal_iterable),
    regexp = "unsupported iterable in for\\(\\): 1"
  )
})

test_that("for() supports rev() on index iterables", {
  rev_colon <- function(n) {
    declare(type(n = integer(1)))
    out <- 0L
    for (i in rev(1:n)) {
      out <- out * 10L + i
    }
    out
  }

  rev_seq <- function() {
    out <- 0L
    for (i in rev(seq(1L, 6L, by = 2L))) {
      out <- out * 10L + i
    }
    out
  }

  rev_seq_len <- function(n) {
    declare(type(n = integer(1)))
    out <- 0L
    for (i in rev(seq_len(n))) {
      out <- out * 10L + i
    }
    out
  }

  rev_seq_along <- function(x) {
    declare(type(x = double(NA)))
    out <- 0L
    for (i in rev(seq_along(x))) {
      out <- out * 10L + i
    }
    out
  }

  expect_quick_identical(rev_colon, 0L, 1L, 3L, 5L)
  expect_quick_identical(rev_seq, list())
  expect_quick_identical(rev_seq_len, 0L, 1L, 5L)
  expect_quick_identical(rev_seq_along, numeric(), c(1, 2, 3))
})

test_that("for() rev(seq()) validates scalar bounds and step", {
  non_scalar_bounds <- function(x) {
    declare(type(x = integer(NA)))
    s <- 0L
    for (i in rev(seq(1L, x))) {
      s <- s + i
    }
    s
  }

  non_scalar_step <- function(x) {
    declare(type(x = integer(NA)))
    s <- 0L
    for (i in rev(seq(1L, 5L, by = x))) {
      s <- s + i
    }
    s
  }

  expect_error(
    quick(non_scalar_bounds),
    regexp = "seq\\(\\) iterable bounds must be scalars"
  )
  expect_error(
    quick(non_scalar_step),
    regexp = "seq\\(\\) iterable step must be a scalar"
  )
})

test_that("for() supports iterating over a symbol (value iteration)", {
  sum_values <- function(x) {
    declare(type(x = double(NA)))
    s <- 0
    for (v in x) {
      s <- s + v
    }
    s
  }

  count_true <- function(x) {
    declare(type(x = logical(NA)))
    n <- 0L
    for (v in x) {
      if (v) n <- n + 1L
    }
    n
  }

  linearize_matrix <- function(m) {
    declare(type(m = integer(NA, NA)))
    out <- integer(length(m))
    j <- 1L
    for (v in m) {
      out[j] <- v
      j <- j + 1L
    }
    out
  }

  iterable_hoisted <- function(x) {
    declare(type(x = integer(NA)))
    y <- x
    out <- 0L
    for (v in y) {
      out <- out * 10L + v
      y[length(y)] <- 9L
    }
    out
  }

  expect_quick_identical(sum_values, numeric(), c(1, 2, 3))
  expect_quick_identical(
    count_true,
    logical(),
    c(TRUE, FALSE, TRUE),
    c(FALSE, FALSE)
  )
  expect_quick_identical(linearize_matrix, matrix(1:6, nrow = 2))
  expect_quick_identical(iterable_hoisted, 1:3, c(1L, 2L, 3L, 4L))
})

test_that("for() supports rev() for value iteration", {
  rev_values <- function(x) {
    declare(type(x = integer(NA)))
    out <- 0L
    for (v in rev(x)) {
      out <- out * 10L + v
    }
    out
  }

  rev_matrix_values <- function(m) {
    declare(type(m = integer(NA, NA)))
    out <- 0L
    for (v in rev(m)) {
      out <- out * 10L + v
    }
    out
  }

  expect_quick_identical(rev_values, 1:3, c(1L, 2L, 3L, 4L))
  expect_quick_identical(rev_matrix_values, matrix(1:6, nrow = 2))
})

test_that("unbraced for() bodies keep hoisted setup inside index loops", {
  rolling_sum <- function(x, weights) {
    declare(type(x = double(NA)), type(weights = double(NA)))
    out <- double(length(x) - length(weights) + 1L)
    n <- length(weights)
    # fmt: skip
    for (i in seq_along(out))
      out[i] <- sum(x[i:(i + n - 1L)] * weights)
    out
  }

  seq_lengths <- function(k) {
    declare(type(k = integer(1)))
    out <- 0L
    # fmt: skip
    for (i in 1:3)
      out <- out + length(seq(i, i + 3L, by = k))
    out
  }

  expect_quick_identical(
    rolling_sum,
    list(as.double(1:8), c(0.25, 0.75))
  )
  expect_quick_identical(seq_lengths, 1L, 2L)

  qseq_lengths := quick(seq_lengths)
  expect_error(
    qseq_lengths(0L),
    "invalid '(to - from)/by'",
    fixed = TRUE
  )
  expect_error(
    qseq_lengths(-1L),
    "wrong sign in 'by' argument",
    fixed = TRUE
  )
})

test_that("unbraced for() bodies keep hoisted setup inside value loops", {
  sum_floors <- function(starts) {
    declare(type(starts = integer(NA)))
    out <- 0
    # fmt: skip
    for (i in starts)
      out <- out + floor(i + 0.5)
    out
  }

  expect_quick_identical(
    sum_floors,
    c(1L, 3L, 5L)
  )
})


test_that("loop bindings refuse changes of mode in either direction", {
  template <- function(x) {
    declare(type(x = double(2)))
    i <- 0L
    for (i in x) {}
    i
  }
  modes <- list(logical = FALSE, integer = 0L, double = 0, complex = 0i)
  for (initial in names(modes)) {
    for (element in setdiff(names(modes), initial)) {
      fn <- template
      body(fn)[[2L]][[2L]][[2L]] <- call(element, 2L)
      body(fn)[[3L]][[3L]] <- modes[[initial]]
      expect_error(quick(fn), "for-loop binding `i` cannot change type")
      body(fn)[[4L]][[3L]] <- quote(rev(x))
      expect_error(quick(fn), "for-loop binding `i` cannot change type")
    }
  }
  body(template)[[3L]][[3L]] <- 0
  body(template)[[4L]][[3L]] <- quote(seq_len(2L))
  expect_error(quick(template), "for-loop binding `i` cannot change type")
})

test_that("same-mode value loops preserve the final value and type", {
  template <- function(x) {
    declare(type(x = double(2)))
    i <- 0
    for (i in x) {}
    i
  }
  modes <- list(logical = FALSE, integer = 0L, double = 0, complex = 0i)
  inputs <- list(
    logical = c(TRUE, FALSE),
    integer = 1:2,
    double = c(1.5, 2.5),
    complex = c(1i, 2i)
  )
  for (mode in names(modes)) {
    fn <- template
    body(fn)[[2L]][[2L]][[2L]] <- call(mode, 2L)
    body(fn)[[3L]][[3L]] <- modes[[mode]]
    expect_quick_identical(fn, list(inputs[[mode]]))
    body(fn)[[4L]][[3L]] <- quote(rev(x))
    expect_quick_identical(fn, list(inputs[[mode]]))
  }
})

test_that("index loop bindings retain R's final value", {
  template <- function() {
    i <- 9L
    for (i in seq_len(3L)) {}
    i
  }
  for (iterable in list(
    quote(seq_len(3L)),
    quote(3:1),
    quote(seq(1L, 6L, by = 2L)),
    quote(rev(seq_len(3L)))
  )) {
    fn <- template
    body(fn)[[3L]][[3L]] <- iterable
    expect_quick_identical(fn, list())
    body(fn)[[3L]][[4L]] <- quote({
      if (i == 2L) break
    })
    expect_quick_identical(fn, list())
    body(fn)[[3L]][[4L]] <- quote({
      if (i == 2L) next
    })
    expect_quick_identical(fn, list())
  }
})

test_that("index loop bindings can be assigned and reused by nested loops", {
  fn <- function() {
    out <- 0L
    for (i in 1:3) {
      i <- 10L
      out <- out + i
    }
    c(out, i)
  }
  expect_quick_identical(fn, list())
  body(fn)[[3L]][[4L]] <- quote({
    for (i in 1:2) {
      out <- out + i
    }
    out <- out + i
  })
  expect_quick_identical(fn, list())
})

test_that("index loops copy modified arguments and use their old bounds", {
  fn <- function(i) {
    declare(type(i = integer(1)))
    for (i in seq_len(i)) {}
    1L
  }
  input <- 3L
  qfn <- quick(fn)
  expect_identical(qfn(input), fn(input))
  expect_identical(input, 3L)
})

test_that("reads after possibly empty loops cannot retain an old scalar", {
  template <- function(x) {
    declare(type(x = integer(NA)))
    i <- 9L
    for (i in x) {}
    i
  }
  for (iterable in list(
    quote(x),
    quote(rev(x)),
    quote(seq_along(x)),
    quote(seq_len(0L))
  )) {
    fn <- template
    body(fn)[[4L]][[3L]] <- iterable
    expect_null(fn(integer()))
    expect_error(quick(fn), "may be uninitialized")
  }
  body(template)[[5L]] <- 1L
  expect_quick_identical(template, integer(), 1:2)
})

test_that("parallel loops retain the last iteration's binding", {
  skip_if_no_openmp()
  fn <- function() {
    i <- 0L
    declare(parallel())
    for (i in 1:3) {
      i <- i * 2L
    }
    i
  }
  expect_quick_identical(fn, list())
  values <- function(x) {
    declare(type(x = double(3)))
    i <- 0
    declare(parallel())
    for (i in rev(x)) {
      i <- i + 1
    }
    i
  }
  expect_quick_identical(values, list(c(1.5, 2.5, 3.5)))
})

test_that("seq with an explicit double step cannot silently bind integers", {
  fn <- function() {
    i <- 0L
    for (i in seq(1, 3, by = 1)) {}
    i
  }
  expect_error(quick(fn), "non-integer seq")
})


test_that("empty inner loops invalidate bindings across enclosing loops", {
  template <- function() {
    i <- 9L
    LOOP
    i
  }
  for (loop in list(
    quote(
      for (i in 1:2) {
        for (i in seq_len(0L)) {}
      }
    ),
    quote(
      for (j in 1:2) {
        for (i in seq_len(0L)) {}
        break
      }
    ),
    quote(
      while (i > 0L) {
        for (i in seq_len(0L)) {}
        break
      }
    ),
    quote(
      repeat {
        for (i in seq_len(0L)) {}
        break
      }
    )
  )) {
    fn <- template
    body(fn)[[3L]] <- loop
    expect_null(fn())
    expect_error(quick(fn), "may be uninitialized")
  }
})


test_that("serial loops inside parallel loops keep iteration storage private", {
  skip_if_no_openmp()
  fn <- function(out) {
    declare(type(out = integer(NA)))
    declare(parallel())
    for (i in seq_along(out)) {
      values <- c(i, i + 1L)
      for (j in values) {
        out[i] <- out[i] + j
      }
    }
    out
  }
  expect_quick_identical(fn, integer(10000L))
  body(fn)[[4L]][[4L]][[3L]][[3L]] <- quote(i:(i + 1L))
  expect_quick_identical(fn, integer(10000L))
})


test_that("both loop paths refuse existing array bindings", {
  fn <- function(i) {
    declare(type(i = integer(2)))
    for (i in seq_len(2L)) {}
    1L
  }
  expect_error(quick(fn), "for-loop variable must be scalar")
  body(fn)[[3L]][[3L]] <- quote(i)
  expect_error(quick(fn), "for-loop variable must be scalar")
})
