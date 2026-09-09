test_that("cat prints newline-terminated literal strings", {
  template <- function() {
    cat("text\n")
    1L
  }
  for (label in c(
    "text\n",
    "\n",
    "first\nsecond\n",
    'quotes: " and \\ path\n',
    "\u00e6\u00f8\u00e5\n"
  )) {
    fn <- template
    body(fn)[[2L]][[2L]] <- label
    qfn <- quick(fn)
    expect_identical(
      capture.output(actual <- qfn()),
      capture.output(expected <- fn())
    )
    expect_identical(actual, expected)
  }
  for (expr in list(
    quote(cat("text")),
    quote(cat("a", "b\n")),
    quote(cat("text\n", file = "out.txt"))
  )) {
    fn <- template
    body(fn)[[2L]] <- expr
    expect_error(quick(fn), "cat\\(\\)")
  }
})

test_that("print accepts numeric expressions and evaluates them once", {
  fn <- function(x) {
    declare(type(x = double(n)))
    print(1 + 1)
    print(x + 1)
    print(x[1L] > 0)
    print(x > 0)
    print(c(1L))
    print((c(FALSE)))
    print((x > 0))
    x
  }
  qfn <- quick(fn)
  output <- capture.output(result <- qfn(c(-2, 4)))
  expect_identical(result, c(-2, 4))
  expect_identical(
    output,
    c("[1] 2", "[1] -1  5", "[1] 0", "[1] 0 1", "[1] 1", "[1] 0", "[1] 0 1")
  )

  draws <- function() {
    print(runif(3L))
    runif(1L)
  }
  qdraws <- quick(draws)
  withr::local_seed(921)
  capture.output(expected <- draws())
  seed <- .Random.seed
  set.seed(921)
  capture.output(actual <- qdraws())
  expect_identical(actual, expected)
  expect_identical(.Random.seed, seed)

  mutation <- function() {
    x <- 1L
    bump <- function() {
      x <<- x + 1L
      x
    }
    print(bump())
    x
  }
  qmutation <- quick(mutation)
  expect_identical(capture.output(result <- qmutation()), "[1] 2")
  expect_identical(result, 2L)
})

test_that("print rejects unsupported options and types", {
  template <- function() {
    print(1L)
    1L
  }
  for (expr in list(
    quote(print(digits = 1L)),
    quote(print(1L, digits = 1L)),
    quote(print(1i))
  )) {
    fn <- template
    body(fn)[[2L]] <- expr
    expect_error(quick(fn), "print\\(\\)")
  }
})
