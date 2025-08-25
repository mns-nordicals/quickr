test_that("multiple return values", {
  slow_multi <- function(x) {
    declare(type(x = double(NA)))
    y <- x + 1
    z <- x + 2
    list(res = y, inc = z)
  }
  quick_multi <- quick(name = "slow_multi", slow_multi)
  x <- as.double(1:3)
  expect_equal(quick_multi(x), list(res = x + 1, inc = x + 2))
})
