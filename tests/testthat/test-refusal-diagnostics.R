test_that("character declarations are refused with a clean message", {
  fn <- function(x) {
    declare(type(x = character(1)))
    x
  }
  expect_error(
    quick(fn),
    "character values are not supported by quickr"
  )
})
test_that("unsupported complex operations are refused with R's messages", {
  complex_lt <- function(x, y) {
    declare(type(x = complex(1)), type(y = complex(1)))
    x < y
  }
  expect_error(quick(complex_lt), "invalid comparison with complex values")
  complex_eq <- function(x, y) {
    declare(type(x = complex(1)), type(y = complex(1)))
    x == y
  }
  expect_quick_identical(complex_eq, list(1i, 1i))
  expect_quick_identical(complex_eq, list(1i, 2i))
  complex_mod <- function(x, y) {
    declare(type(x = complex(1)), type(y = complex(1)))
    x %% y
  }
  expect_error(quick(complex_mod), "unimplemented complex operation")
})
test_that("as.double refuses unsupported complex coercion", {
  fn <- function(x) {
    declare(type(x = complex(n)))
    as.double(x)
  }
  expect_error(quick(fn), "does not support complex")
})
test_that("lazy branches defer unsupported as.double coercion", {
  skipped <- function(x) {
    declare(type(x = complex(1)))
    ifelse(FALSE, as.double(x), 0)
  }
  reached <- function(x) {
    declare(type(x = complex(1)))
    ifelse(TRUE, as.double(x), 0)
  }
  expect_quick_identical(skipped, list(1 + 1i))
  expect_error(quick(reached)(1 + 1i), "does not support complex")
})
test_that("arithmetic refuses raw operands", {
  for (op in c("+", "-", "*", "/", "^", "%%", "%/%")) {
    fn <- eval(bquote(function(x, y) {
      declare(type(x = raw(1)), type(y = raw(1)))
      .(as.call(list(as.name(op), quote(x), quote(y))))
    }))
    expect_error(quick(fn), "does not support raw operands", fixed = TRUE)
  }
  unary <- function(x) {
    declare(type(x = raw(1)))
    -x
  }
  expect_error(quick(unary), "does not support raw operands", fixed = TRUE)
})
test_that("division operators refuse zero divisors", {
  for (op in c("%%", "%/%")) {
    literal <- eval(bquote(function(x) {
      declare(type(x = integer(1)))
      .(as.call(list(as.name(op), quote(x), 0L)))
    }))
    expect_error(
      quick(literal),
      "does not support zero divisors",
      fixed = TRUE
    )
    dynamic <- eval(bquote(function(x, y) {
      declare(type(x = integer(1)), type(y = integer(1)))
      .(as.call(list(as.name(op), quote(x), quote(y))))
    }))
    q_dynamic <- quick(dynamic)
    expect_identical(q_dynamic(5L, 2L), do.call(op, list(5L, 2L)))
    expect_error(
      q_dynamic(1L, 0L),
      "does not support zero divisors",
      fixed = TRUE
    )
  }
})
test_that("complex operands are refused in linear algebra", {
  complex_matmul <- function(x, y) {
    declare(type(x = complex(2)), type(y = complex(2)))
    x %*% y
  }
  expect_error(
    quick(complex_matmul),
    "%*% does not support complex operands",
    fixed = TRUE
  )
  complex_mixed <- function(x, y) {
    declare(type(x = complex(2, 2)), type(y = double(2, 2)))
    x %*% y
  }
  expect_error(quick(complex_mixed), "does not support complex operands")
  complex_solve <- function(x) {
    declare(type(x = complex(2, 2)))
    solve(x)
  }
  expect_error(quick(complex_solve), "does not support complex operands")
  complex_crossprod <- function(x) {
    declare(type(x = complex(2, 2)))
    crossprod(x)
  }
  expect_error(quick(complex_crossprod), "does not support complex operands")
  complex_t <- function(x) {
    declare(type(x = complex(2, 2)))
    t(x)
  }
  expect_quick_identical(
    complex_t,
    list(matrix(c(1 + 1i, 2 + 0i, 3 - 1i, 4 + 2i), 2, 2))
  )
})
test_that("as.integer refuses values outside R's integer range", {
  fn <- function(x) {
    declare(type(x = double(1)))
    as.integer(x)
  }
  qfn <- quick(fn)
  expect_identical(qfn(42.9), 42L)
  expect_error(qfn(Inf), "representable as an R integer")
  expect_error(qfn(1e100), "representable as an R integer")
})
test_that("as.integer(logical matrix) drops dimensions", {
  fn <- function(x) {
    declare(type(x = logical(2, 2)))
    as.integer(x)
  }
  x <- matrix(c(TRUE, FALSE, TRUE, FALSE), nrow = 2L)
  expect_quick_identical(fn, list(x))
})
test_that("runif refuses negative runtime sample counts", {
  static <- function() {
    sum(runif(-1L))
  }
  fn <- function(n) {
    declare(type(n = double(1)))
    sum(runif(n))
  }
  expect_error(quick(static), "sample count must be non-negative")
  qfn <- quick(fn)
  expect_type(qfn(2), "double")
  for (n in c(-1, Inf, NaN, 2147483648)) {
    expect_error(qfn(n), "sample count must be non-negative")
  }
})
test_that("seq_len refuses negative bounds", {
  static <- function() {
    out <- 0L
    for (i in seq_len(-1L)) {
      out <- out + i
    }
    out
  }
  dynamic <- function(n) {
    declare(type(n = integer(1)))
    out <- 0L
    for (i in seq_len(n)) {
      out <- out + i
    }
    out
  }
  value <- function(n) {
    declare(type(n = integer(1)))
    sum(seq_len(n))
  }
  message <- "seq_len() bound must be non-negative"
  expect_error(quick(static), message, fixed = TRUE)
  qdynamic <- quick(dynamic)
  expect_identical(qdynamic(3L), dynamic(3L))
  expect_error(qdynamic(-1L), message, fixed = TRUE)
  qvalue <- quick(value)
  expect_identical(qvalue(3L), value(3L))
  expect_error(qvalue(-1L), message, fixed = TRUE)
})
test_that("logical PACK masks require matching extents", {
  fn <- function(x, pred) {
    declare(type(x = double(n)), type(pred = logical(m)))
    length(x[pred])
  }
  qfn <- quick(fn)
  expect_identical(qfn(as.double(1:4), c(TRUE, FALSE, TRUE, FALSE)), 2L)
  expect_error(qfn(as.double(1:4), c(TRUE, FALSE)), "logical mask extent")
})
test_that("parallel loops refuse RNG calls", {
  parallel_for <- function(n, out) {
    declare(type(n = integer(1)), type(out = double(n)))
    declare(parallel())
    for (i in seq_len(n)) {
      out[i] <- runif(1L)
    }
    out
  }
  parallel_sapply <- function(n) {
    declare(type(n = integer(1)))
    declare(parallel())
    sapply(seq_len(n), function(i) runif(1L))
  }
  message <- "runif() is not supported inside parallel loops"
  expect_error(quick(parallel_for), message, fixed = TRUE)
  expect_error(quick(parallel_sapply), message, fixed = TRUE)
})
test_that("rep.int refuses negative repetition counts in subscripts", {
  static <- function(x) {
    declare(type(x = double(2)))
    x[rep.int(1L, -1L)]
  }
  dynamic <- function(x, n) {
    declare(type(x = double(2)), type(n = double(1)))
    sum(x[rep.int(1L, n)])
  }
  message <- "invalid 'times' value"
  expect_error(quick(static), message, fixed = TRUE)
  qdynamic <- quick(dynamic)
  expect_identical(qdynamic(c(1, 2), 2), 2)
  for (times in c(-1, Inf, NaN, 2147483648)) {
    expect_error(qdynamic(c(1, 2), times), message, fixed = TRUE)
  }
})

test_that("logical axis masks require matching extents", {
  rows <- function(x, pred) {
    declare(type(x = double(n, k)), type(pred = logical(m)))
    sum(x[pred, , drop = FALSE])
  }
  cols <- function(x, pred) {
    declare(type(x = double(n, k)), type(pred = logical(m)))
    sum(x[, pred, drop = FALSE])
  }
  write_rows <- function(x, pred) {
    declare(type(x = double(n, k)), type(pred = logical(m)))
    out <- x
    out[pred, ] <- 0
    sum(out)
  }
  write_cols <- function(x, pred) {
    declare(type(x = double(n, k)), type(pred = logical(m)))
    out <- x
    out[, pred] <- 0
    sum(out)
  }
  x <- matrix(as.double(1:12), 4L, 3L)
  for (fn in list(rows, cols)) {
    qfn <- quick(fn)
    extent <- if (identical(fn, rows)) nrow(x) else ncol(x)
    pred <- rep(c(TRUE, FALSE), length.out = extent)
    expect_identical(qfn(x, pred), fn(x, pred))
    expect_identical(qfn(x, rep(FALSE, extent)), 0)
    for (size in c(0L, 1L, extent - 1L, extent + 1L)) {
      expect_error(
        qfn(x, rep(TRUE, size)),
        "logical mask extents must match indexed axis"
      )
    }
  }
  for (fn in list(write_rows, write_cols)) {
    qfn <- quick(fn)
    extent <- if (identical(fn, write_rows)) nrow(x) else ncol(x)
    pred <- rep(c(TRUE, FALSE), length.out = extent)
    expect_identical(qfn(x, pred), fn(x, pred))
    expect_identical(qfn(x, rep(FALSE, extent)), sum(x))
    expect_error(
      qfn(x, rep(TRUE, extent - 1L)),
      "logical mask extents must match indexed axis"
    )
    expect_error(
      qfn(x, rep(TRUE, extent + 1L)),
      "logical mask extents must match indexed axis"
    )
  }
  expect_identical(quick(rows)(matrix(numeric(), 0L, 3L), logical()), 0)
  expect_identical(quick(cols)(matrix(numeric(), 4L, 0L), logical()), 0)
  fixed <- function(x, pred) {
    declare(type(x = double(4, 3)), type(pred = logical(2)))
    sum(x[pred, ])
  }
  expect_error(quick(fixed), "logical mask extents must match indexed axis")
  singleton <- function(x, pred) {
    declare(type(x = double(1, 3)), type(pred = logical(1)))
    sum(x[pred, , drop = FALSE])
  }
  qsingleton <- quick(singleton)
  expect_identical(qsingleton(x[1L, , drop = FALSE], TRUE), sum(x[1L, ]))
  expect_identical(qsingleton(x[1L, , drop = FALSE], FALSE), 0)
})

test_that("unproven return sizes cannot skip preceding observable effects", {
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
  for (fn in list(printed, draws, nested, operand, listed, conditional)) {
    expect_error(
      quick(fn),
      "cannot validate return dimensions before RNG or output effects"
    )
  }
})

test_that("return sizes established before effects remain supported", {
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
