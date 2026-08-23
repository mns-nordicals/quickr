# Unit tests for logical operations

skip_on_cran()

test_that("between", {
  between <- function(x, left, right) {
    declare({
      type(x = double(n))
      type(left = double(1))
      type(right = double(1))
    })
    out <- x >= left & x <= right
    out
  }

  expect_translation_snapshots(between)
  expect_quick_identical(between, list(x = runif(100), left = .4, right = .6))
})

test_that("logical ops", {
  test_args <- list(
    list(1, 2),
    list(2, 1),
    list(-2, 2),
    list(-2, -2),
    list(3, 3),
    list(4, 1),
    list(1, 4)
  )

  fn <- function(a, b) {
    declare(
      type(a = double(1)),
      type(b = double(1))
    )

    delta <- a - b
    if (delta < 0) {
      delta <- (-1) * delta
    }

    a_gt_b <- a > b
    b_gt_a <- b > a
    delta_lt_3 <- delta <= 3

    out <- (a_gt_b || b_gt_a) && delta_lt_3
    out
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, !!!test_args)

  # simpler version of above
  fn <- function(a, b) {
    declare({
      type(a = double(1))
      type(b = double(1))
    })

    delta <- abs(a - b)
    out <- (a != b) & (delta <= 3)
    out
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, !!!test_args)

  # even simpler version
  fn <- function(a, b) {
    declare(type(a = double(1)), type(b = double(1)))
    out <- (a != b) && abs(a - b) <= 3
    out
  }
  expect_translation_snapshots(fn)
  expect_quick_identical(fn, !!!test_args)

  # vectorized version
  fn <- function(a, b) {
    declare(type(a = double(n)), type(b = double(n)))
    out <- (a != b) & abs(a - b) <= 3
    out
  }
  expect_translation_snapshots(fn)
  .[a, b] <- .mapply(c, test_args, NULL)
  expect_quick_identical(fn, list(a, b))
})

test_that("parentheses preserve logical precedence", {
  fn_a <- function(x, y) {
    declare(type(x = integer(1)), type(y = integer(1)))
    cond <- (x > 8L || x <= 0L) && (y > 8L || y <= 0L)
    cond
  }

  fn_b <- function(x, y) {
    declare(type(x = integer(1)), type(y = integer(1)))
    cond_x <- x > 8L || x <= 0L
    cond_y <- y > 8L || y <= 0L
    cond_x && cond_y
  }

  cases <- list(
    list(9L, 1L),
    list(9L, 9L),
    list(0L, 9L),
    list(1L, 0L),
    list(5L, 5L)
  )

  expect_translation_snapshots(fn_a)
  expect_translation_snapshots(fn_b)
  expect_quick_identical(fn_a, !!!cases)
  expect_quick_identical(fn_b, !!!cases)
})

test_that("&& and || require length-1 operands", {
  vector_matrix_and <- function(x, y) {
    declare(type(x = logical(2)), type(y = logical(2, 2)))
    x && y
  }
  expect_error(quick(vector_matrix_and), "requires length-1 operands")

  matrix_vector_or <- function(x, y) {
    declare(type(x = logical(2, 2)), type(y = logical(2)))
    x || y
  }
  expect_error(quick(matrix_vector_or), "requires length-1 operands")
})

test_that("&& and || guard unknown operand lengths at runtime", {
  and_fn <- function(x, y) {
    declare(type(x = logical(NA)), type(y = logical(1)))
    x && y
  }
  qand <- quick(and_fn)
  expect_identical(qand(TRUE, FALSE), FALSE)
  expect_error(qand(c(TRUE, FALSE), TRUE), "requires length-1 operands")

  or_fn <- function(x, y) {
    declare(type(x = logical(1)), type(y = logical(NA)))
    x || y
  }
  qor <- quick(or_fn)
  expect_identical(qor(FALSE, TRUE), TRUE)
  expect_error(qor(FALSE, c(FALSE, TRUE)), "requires length-1 operands")
})

test_that("&& and || accept one-element matrices", {
  matrix_and <- function(x, y) {
    declare(type(x = logical(1, 1)), type(y = logical(1, 1)))
    x && y
  }
  expect_quick_identical(
    matrix_and,
    list(matrix(TRUE, 1, 1), matrix(FALSE, 1, 1))
  )

  matrix_or <- function(x, y) {
    declare(type(x = logical(1, 1)), type(y = logical(1, 1)))
    x || y
  }
  expect_quick_identical(
    matrix_or,
    list(matrix(FALSE, 1, 1), matrix(TRUE, 1, 1))
  )
})
