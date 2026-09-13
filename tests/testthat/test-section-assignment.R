test_that("dynamic section replacements conform or broadcast a single value", {
  local <- function(x, y) {
    declare(type(x = double(n)), type(y = double(m)))
    x[] <- y
    x
  }
  host <- function(x, y) {
    declare(type(x = double(n)), type(y = double(m)))
    replace <- function(i) {
      x[] <<- y
      NULL
    }
    replace(1L)
    x
  }
  for (fn in list(local, host)) {
    expect_quick_identical(
      fn,
      list(rep(-1, 4), c(10, 20, 30, 40)),
      list(rep(-1, 4), 10),
      list(numeric(), numeric()),
      list(numeric(), c(10, 20)),
      list(numeric(), 10)
    )
    qfn <- quick(fn)
    for (y in list(c(10, 20), numeric(), as.double(1:5))) {
      x <- rep(-1, 4)
      expect_error(qfn(x, y), "section replacement")
      expect_identical(x, rep(-1, 4))
    }
  }
})

test_that("known section mismatches fail at compilation", {
  local <- function(x, y) {
    declare(type(x = double(4)), type(y = double(2)))
    x[] <- y
    x
  }
  host <- function(x, y) {
    declare(type(x = double(4)), type(y = double(2)))
    replace <- function(i) {
      x[] <<- y
      NULL
    }
    replace(1L)
    x
  }
  for (fn in list(local, host)) {
    expect_error(quick(fn), "section replacement")
  }
})

test_that("section guards preserve integer, logical, and complex values", {
  inputs <- list(
    integer = list(x = rep(-1L, 4), y = c(2L, 3L)),
    logical = list(x = rep(FALSE, 4), y = c(TRUE, FALSE)),
    complex = list(x = rep(-1 + 0i, 4), y = c(2 + 3i, 4 + 5i))
  )
  for (mode in names(inputs)) {
    decl_x <- call(mode, quote(n))
    decl_y <- call(mode, quote(m))
    fn <- eval(bquote(function(x, y) {
      declare(type(x = .(decl_x)), type(y = .(decl_y)))
      x[] <- y
      x
    }))
    input <- inputs[[mode]]
    expect_quick_identical(
      fn,
      list(input$x, rep(input$y, 2)),
      list(input$x, input$y[1])
    )
    expect_error(quick(fn)(input$x, input$y), "section replacement")
  }
})

test_that("scalar destinations and length-one sections accept dynamic singletons", {
  for (target in expression(x[2L], x[2:2], x[])) {
    fn <- eval(bquote(function(x, y) {
      declare(type(x = double(4)), type(y = double(NA)))
      .(target) <- y
      x
    }))
    expect_quick_identical(fn, list(rep(-1, 4), 10))
    expect_error(quick(fn)(rep(-1, 4), c(10, 20)), "section replacement")
  }
  scalar <- function(x, y) {
    declare(type(x = double(1)), type(y = double(NA)))
    x[] <- y
    x
  }
  expect_quick_identical(scalar, list(1, 10))
  expect_error(quick(scalar)(1, numeric()), "section replacement")
})

test_that("replacement guards use the selected matrix extents", {
  local <- function(x, y, k) {
    declare(
      type(x = double(NA, NA)),
      type(y = double(NA, NA)),
      type(k = integer(1))
    )
    x[seq_len(k), ] <- y
    x
  }
  host <- function(x, y, k) {
    declare(
      type(x = double(NA, NA)),
      type(y = double(NA, NA)),
      type(k = integer(1))
    )
    replace <- function(i) {
      x[seq_len(k), ] <<- y
      NULL
    }
    replace(1L)
    x
  }
  x <- matrix(-1, 4, 3)
  for (fn in list(local, host)) {
    expect_quick_identical(
      fn,
      list(x, matrix(as.double(1:6), 2, 3), 2L),
      list(x, matrix(10, 1, 1), 2L),
      list(x, matrix(10, 1, 1), 0L)
    )
    qfn <- quick(fn)
    expect_error(qfn(x, matrix(10, 1, 3), 2L), "section replacement")
    expect_error(qfn(x, matrix(10, 3, 2), 2L), "section replacement")
    expect_error(qfn(x, matrix(10, 2, 2), 2L), "section replacement")
  }
})

test_that("logical selections guard their selected element count", {
  local <- function(x, mask, y) {
    declare(type(x = double(n)), type(mask = logical(n)), type(y = double(m)))
    x[mask] <- y
    x
  }
  host <- function(x, mask, y) {
    declare(type(x = double(n)), type(mask = logical(n)), type(y = double(m)))
    replace <- function(i) {
      x[mask] <<- y
      NULL
    }
    replace(1L)
    x
  }
  for (fn in list(local, host)) {
    expect_quick_identical(
      fn,
      list(rep(-1, 4), c(TRUE, FALSE, TRUE, FALSE), c(10, 20)),
      list(rep(-1, 4), c(TRUE, FALSE, TRUE, FALSE), 10),
      list(rep(-1, 4), rep(FALSE, 4), numeric()),
      list(rep(-1, 4), rep(FALSE, 4), c(10, 20))
    )
    expect_error(
      quick(fn)(rep(-1, 4), c(TRUE, TRUE, TRUE, FALSE), c(10, 20)),
      "section replacement"
    )
  }
})

test_that("fill replacements retain their R length", {
  fn <- function(x, m) {
    declare(type(x = double(n)), type(m = integer(1)))
    x[] <- double(m)
    x
  }
  expect_quick_identical(fn, list(rep(-1, 4), 4L), list(rep(-1, 4), 1L))
  qfn <- quick(fn)
  expect_error(qfn(rep(-1, 4), 2L), "section replacement")
  expect_error(qfn(rep(-1, 4), 0L), "section replacement")
})

test_that("guarded assignments initialize closure shadows before querying them", {
  fn <- function(x, y) {
    declare(type(x = double(NA)), type(y = double(NA)))
    replace <- function(i) {
      x[] <- y
      x
    }
    out <- replace(1L)
    list(x = x, out = out)
  }
  expect_quick_identical(
    fn,
    list(rep(-1, 4), 10),
    list(rep(-1, 4), as.double(1:4))
  )
  expect_error(quick(fn)(rep(-1, 4), c(10, 20)), "section replacement")
})

test_that("replacement and index effects run once in R order", {
  fn <- function(x) {
    declare(type(x = double(4)))
    trace <- 0L
    replacement <- function(i) {
      trace <<- trace * 10L + 1L
      c(10, 20)
    }
    indices <- function(i) {
      trace <<- trace * 10L + 2L
      c(1L, 2L)
    }
    x[indices(1L)] <- replacement(1L)
    list(x = x, trace = trace)
  }
  expect_quick_identical(fn, list(rep(-1, 4)))

  snapshot <- function(x) {
    declare(type(x = double(4)))
    out <- double(4)
    indices <- function(i) {
      x[] <<- x + 1
      1:4
    }
    out[indices(1L)] <- x
    list(x = x, out = out)
  }
  expect_quick_identical(snapshot, list(as.double(1:4)))
})
