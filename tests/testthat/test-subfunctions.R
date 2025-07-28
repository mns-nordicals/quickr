test_that("quick() accepts scope parameter", {
  # Test default scope
  f1 <- function(x) {
    declare(type(x = double(1)))
    x + 1
  }
  
  # Should not error with default scope
  expect_silent(quick(f1, name = "test_default"))
  
  # Test explicit standalone
  expect_silent(quick(f1, name = "test_standalone", scope = "standalone"))
  
  # Test internal scope
  expect_silent(quick(f1, name = "test_internal", scope = "internal"))
  
  # Test module scope  
  expect_silent(quick(f1, name = "test_module", scope = "module"))
  
  # Test invalid scope
  expect_error(
    quick(f1, name = "test_invalid", scope = "invalid"),
    "scope must be one of"
  )
})

test_that("scope parameter is passed to collector", {
  # Skip if not in development context
  skip_if_not(exists("collector", where = asNamespace("quickr")))
  
  # Activate collector to simulate package context
  quickr:::collector$activate("test_package")
  
  # Define test functions with different scopes
  f1 <- function(x) {
    declare(type(x = double(1)))
    x + 1
  }
  
  quick(f1, name = "standalone_fn", scope = "standalone")
  quick(f1, name = "internal_fn", scope = "internal") 
  quick(f1, name = "module_fn", scope = "module")
  
  # Get collected functions
  collected <- quickr:::collector$get_collected()
  
  # Verify we collected 3 functions
  expect_length(collected, 3)
  
  # Verify scopes were recorded
  expect_equal(collected[[1]]$scope, "standalone")
  expect_equal(collected[[2]]$scope, "internal")
  expect_equal(collected[[3]]$scope, "module")
})

test_that("collector tracks functions by scope", {
  skip_if_not(exists("collector", where = asNamespace("quickr")))
  
  # Activate collector
  quickr:::collector$activate("test_package")
  
  # Add functions with different scopes
  normalize <- function(x) {
    declare(type(x = double(n)))
    x / sqrt(sum(x * x))
  }
  
  dot_product <- function(a, b) {
    declare(type(a = double(n)), type(b = double(n)))
    sum(a * b)
  }
  
  main_func <- function(x, y) {
    declare(type(x = double(n)), type(y = double(n)))
    x + y
  }
  
  quick(normalize, name = "normalize", scope = "internal")
  quick(dot_product, name = "dot_product", scope = "module")
  quick(main_func, name = "main_func", scope = "standalone")
  
  # Check scope registry
  by_scope <- quickr:::collector$get_by_scope()
  
  expect_length(by_scope$internal, 1)
  expect_length(by_scope$module, 1)
  expect_length(by_scope$standalone, 1)
  
  expect_equal(names(by_scope$internal), "normalize")
  expect_equal(names(by_scope$module), "dot_product")
  expect_equal(names(by_scope$standalone), "main_func")
  
  # Clean up
  quickr:::collector$get_collected()
})