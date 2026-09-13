test_that("rank-one array returns preserve dimensions for every storage mode", {
  for (data in list(3L, 3, TRUE, 3 + 2i)) {
    for (n in c(0L, 1L, 3L)) {
      fn <- eval(bquote(function() {
        array(.(data), dim = .(n))
      }))
      expect_quick_identical(fn, list())
    }
    fn <- eval(bquote(function(n) {
      declare(type(n = integer(1)))
      array(.(data), dim = n)
    }))
    expect_quick_identical(fn, list(0L), list(1L), list(3L))
  }
})

test_that("rank-one array metadata follows copies, temporaries, and operations", {
  expressions <- expression(
    a,
    b,
    a[],
    a + 1,
    1 + a,
    a + x,
    x + a,
    a / 2,
    -a,
    +a,
    abs(-a),
    floor(a / 2),
    ceiling(a / 2),
    sqrt(a),
    trunc(a / 2),
    a > 1,
    1 < a,
    !(a > 1),
    ifelse(a > 1, 4, 5),
    rev(a),
    drop(a),
    a[1:2],
    a[1],
    a[1:1],
    a[1, drop = FALSE],
    c(a),
    as.double(a),
    as.integer(a),
    array(as.integer(a), 2L) + 0.5,
    dim(a),
    nrow(a),
    length(a)
  )
  for (expr in expressions) {
    fn <- eval(bquote(function(x) {
      declare(type(x = double(2)))
      a <- array(x, 2L)
      b <- a
      .(expr)
    }))
    expect_quick_identical(fn, list(c(1, 3)))
  }
})

test_that("singleton array attributes follow R scalar operations", {
  for (expr in expression(
    sum(a),
    min(a),
    max(a),
    prod(a),
    any(a > 1),
    all(a > 1),
    a,
    a[],
    a + 1,
    1 + a,
    a > 1,
    ifelse(a > 1, 4, 5),
    rev(a),
    drop(a),
    a[1],
    a[1, drop = FALSE],
    c(a),
    as.double(a),
    as.integer(a),
    dim(a),
    nrow(a),
    length(a)
  )) {
    fn <- eval(bquote(function() {
      a <- array(3, 1L)
      .(expr)
    }))
    expect_quick_identical(fn, list())
  }
})

test_that("local closures preserve array arguments and results", {
  fn <- function(x) {
    declare(type(x = double(2)))
    make <- function(z) {
      array(z, 2L)
    }
    copy <- function(z) {
      z + 1
    }
    a <- make(x)
    b <- copy(a)
    c <- copy(a) + a
    list(a = a, b = b, c = c)
  }
  expect_quick_identical(fn, list(c(1, 2)))

  shadow <- function(x) {
    declare(type(x = double(2)))
    a <- array(x, 2L)
    change <- function(i) {
      a[i] <- 5
      a
    }
    b <- change(1L)
    list(a = a, b = b)
  }
  expect_quick_identical(shadow, list(c(1, 2)))
})

test_that("rank-one array extents and attribute changes are guarded", {
  large_extent <- function(x) {
    declare(type(x = double(NA)))
    array(1, length(x) * length(x))
  }
  expect_error(quick(large_extent)(double(50000L)), "supported range")
  fn <- function(n) {
    declare(type(n = double(1)))
    array(1, n)
  }
  expect_error(quick(fn)(as.double(.Machine$integer.max) + 1), "dimensions")

  vector_to_array <- function(x) {
    declare(type(x = double(2)))
    x <- array(x, 2L)
    x
  }
  array_to_vector <- function() {
    x <- array(1, 2L)
    x <- c(2, 3)
    x
  }
  expect_error(quick(vector_to_array), "preserve its shape")
  expect_error(quick(array_to_vector), "preserve its shape")

  incompatible <- function() {
    array(1, 2L) + matrix(1, 2L, 1L)
  }
  expect_error(quick(incompatible), "matching dimensions")

  fn <- function(n) {
    declare(type(n = integer(1)))
    a <- array(1, n)
    rev(a)
  }
  expect_quick_identical(fn, list(2L), list(3L))
  expect_error(quick(fn)(1L), "runtime length is 1")
})

test_that("array arithmetic handles runtime singleton vectors and array extents", {
  for (expr in expression(a + x, x + a, a == x, a + array(x, n))) {
    fn <- eval(bquote(function(x, n) {
      declare(type(n = integer(1)), type(x = double(n)))
      a <- array(2, 1L)
      .(expr)
    }))
    expect_quick_identical(fn, list(3, 1L))
    expect_error(quick(fn)(c(3, 4), 2L), "length-one|matching dimensions")
  }
})

test_that("rank-one array sections preserve dimensions when drop is false", {
  fn <- function(x, k) {
    declare(type(x = double(3)), type(k = integer(1)))
    a <- array(x, 3L)
    a[seq_len(k), drop = FALSE]
  }
  expect_quick_identical(fn, list(c(1, 2, 3), 1L), list(c(1, 2, 3), 2L))

  fn <- function(x) {
    declare(type(x = double(2)))
    a <- array(x, 2L)
    a[1:2] <- c(3, 4)
    a
  }
  expect_quick_identical(fn, list(c(1, 2)))
})

test_that("specialized bindings respect array attributes", {
  loop <- function() {
    a <- array(0L, 1L)
    for (a in 1:2) {
      NULL
    }
    a
  }
  expect_error(quick(loop), "for-loop variable must be scalar")

  replaced <- function() {
    a <- array(0L, 2L)
    a <- sapply(1:2, function(i) {
      i
    })
    a
  }
  expect_error(quick(replaced), "cannot replace an array binding")

  # sapply() simplifies its component arrays; their attributes need not
  # match the shape of the Fortran output slice supplied to the closure.
  simplified <- function(n) {
    declare(type(n = integer(1)), type(out = integer(2, n)))
    out <- sapply(seq_len(n), function(i) {
      array(i, 2L)
    })
    out
  }
  expect_quick_identical(simplified, list(2L), list(3L))

  scalar <- function(n) {
    declare(type(n = integer(1)), type(out = integer(n)))
    out <- sapply(seq_len(n), function(i) {
      array(i, 1L)
    })
    out
  }
  expect_quick_identical(scalar, list(2L), list(3L))
})
