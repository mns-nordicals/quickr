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
test_that("later subscript effects cannot change the evaluated base", {
  fn <- function(x) {
    declare(type(x = double(2)))
    first <- function() {
      x[] <<- x + 1
      1L
    }
    x[first()]
  }

  expect_quick_identical(fn, list(c(3, 4)))

  rep_index <- function(v) {
    declare(type(v = integer(2)))
    i <- 1L
    count <- function() {
      i <<- 2L
      2L
    }
    sum(v[rep.int(i, count())])
  }
  expect_quick_identical(rep_index, list(10:11))
})
test_that("later matrix effects cannot change the evaluated left operand", {
  fn <- function(x) {
    declare(type(x = double(1, 1)))
    rhs <- function() {
      x <<- x + 1
      matrix(2, 1, 1)
    }
    x %*% rhs()
  }

  expect_quick_identical(fn, list(matrix(3, 1, 1)))
})


test_that("nested closure writes preserve earlier operands across consumers", {
  template <- function(x) {
    declare(type(x = integer(2)))
    f <- function() {
      g <- function() {
        x[2L] <<- x[2L] + 1L
        2L
      }
      g()
    }
    RESULT
  }
  for (expr in list(
    quote(x + f()),
    quote(c(x, f())),
    quote(sum(x, f())),
    quote(x[f()]),
    quote(cbind(x + 0L, c(f(), 0L)))
  )) {
    fn <- template
    body(fn)[[4L]] <- expr
    expect_quick_identical(fn, list(c(3L, 4L)))
  }

  scalar <- function(x) {
    declare(type(x = integer(1)))
    f <- function() {
      g <- function() {
        x <<- x + 1L
        2L
      }
      g()
    }
    x + f()
  }
  expect_quick_identical(scalar, 3L)
})

test_that("mutation summaries follow callbacks and shadowed nested names", {
  template <- function() {
    x <- 3L
    f <- function() {
      g <- function(i) {
        x <<- x + 1L
        2L
      }
      out <- sapply(1:2, g)
      sum(out)
    }
    x + f()
  }
  expect_quick_identical(template, list())
  body(template)[[3L]] <- quote(
    f <- function() {
      out <- sapply(1:2, function(i) {
        x <<- x + 1L
        2L
      })
      sum(out)
    }
  )
  expect_quick_identical(template, list())

  shadow <- function() {
    x <- 3L
    f <- function() {
      f <- function() {
        g <- function() {
          x <<- x + 1L
          2L
        }
        g()
      }
      (f)()
    }
    x + f()
  }
  expect_quick_identical(shadow, list())
})

test_that("nested mutation summaries also protect eager closure arguments", {
  fn <- function() {
    x <- 3L
    f <- function(a) {
      g <- function() {
        h <- function() {
          x <<- x + 1L
          2L
        }
        h()
      }
      unused <- g()
      a
    }
    f(x)
  }
  expect_error(quick(fn), "arguments cannot depend on bindings modified")
})

test_that("sequence bounds preserve reads before later host writes", {
  fn <- function() {
    x <- 1L
    end <- function() {
      x <<- 2L
      3L
    }
    out <- 0L
    for (i in x:end()) {
      out <- out * 10L + i
    }
    out
  }
  expect_quick_identical(fn, list())
  body(fn)[[5L]][[3L]] <- quote(seq(x, end(), by = 1L))
  expect_quick_identical(fn, list())
})
