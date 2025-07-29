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
  
  # Test module scope (not implemented yet) - should be skipped
  skip("Module scope not implemented yet")
  # expect_error(
  #   quick(f1, name = "test_module", scope = "module"),
  #   "Module scope is not implemented yet"
  # )
  
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
  # Skip module scope test
  # quick(f1, name = "module_fn", scope = "module")
  
  # Get collected functions
  collected <- quickr:::collector$get_collected()
  
  # Verify we collected 2 functions (not 3)
  expect_length(collected, 2)
  
  # Verify scopes were recorded
  expect_equal(collected[[1]]$scope, "standalone")
  expect_equal(collected[[2]]$scope, "internal")
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
  
  # Skip module scope for now
  # dot_product <- function(a, b) {
  #   declare(type(a = double(n)), type(b = double(n)))
  #   sum(a * b)
  # }
  
  main_func <- function(x, y) {
    declare(type(x = double(n)), type(y = double(n)))
    x + y
  }
  
  quick(normalize, name = "normalize", scope = "internal")
  # quick(dot_product, name = "dot_product", scope = "module")
  quick(main_func, name = "main_func", scope = "standalone")
  
  # Check scope registry
  by_scope <- quickr:::collector$get_by_scope()
  
  expect_length(by_scope$internal, 1)
  # expect_length(by_scope$module, 1)
  expect_length(by_scope$standalone, 1)
  
  expect_equal(names(by_scope$internal), "normalize")
  # expect_equal(names(by_scope$module), "dot_product")
  expect_equal(names(by_scope$standalone), "main_func")
  
  # Clean up
  quickr:::collector$get_collected()
})

test_that("scope can store available subfunctions", {
  # Create a test closure
  test_fn <- function(x) x + 1
  
  # Create a scope
  scope <- quickr:::new_scope(test_fn)
  
  # Initially should be NULL
  expect_null(scope@available_subfunctions)
  
  # Should be able to set subfunctions
  scope@available_subfunctions <- list(
    helper1 = list(scope = "internal", name = "helper1"),
    helper2 = list(scope = "module", name = "helper2")
  )
  
  expect_length(scope@available_subfunctions, 2)
  expect_equal(names(scope@available_subfunctions), c("helper1", "helper2"))
})

test_that("scope can store available subfunctions", {
  # Create a test closure
  test_fn <- function(x) x + 1
  
  # Create a scope
  scope <- quickr:::new_scope(test_fn)
  
  # Initially should be NULL
  expect_null(scope@available_subfunctions)
  
  # Should be able to set subfunctions
  scope@available_subfunctions <- list(
    helper1 = list(scope = "internal", name = "helper1"),
    helper2 = list(scope = "module", name = "helper2")
  )
  
  expect_length(scope@available_subfunctions, 2)
  expect_equal(names(scope@available_subfunctions), c("helper1", "helper2"))
})

test_that("generate Fortran with internal procedure", {
  # Simple helper function
  normalize <- function(x) {
    declare(type(x = double(n)))
    norm <- sqrt(sum(x * x))
    x / norm
  }
  
  # Main function that uses helper
  main_func <- function(a) {
    declare(type(a = double(n)))
    a_norm <- normalize(a)
    a_norm
  }
  
  # Set up available subfunctions
  available_subs <- list(
    normalize = list(scope = "internal", closure = normalize)
  )
  
  # Generate Fortran
  fsub <- quickr:::new_fortran_subroutine(
    "main_func", 
    main_func,
    available_subfunctions = available_subs
  )
  
  fortran_code <- as.character(fsub)
  
  # Should contain a contains section
  expect_match(fortran_code, "contains", ignore.case = TRUE)
  expect_match(fortran_code, "subroutine normalize_internal", ignore.case = TRUE)
})

test_that("internal procedure generates correct Fortran", {
  # Simple helper function
  normalize <- function(x) {
    declare(type(x = double(n)))
    norm <- sqrt(sum(x * x))
    result <- x / norm
    result
  }
  
  # Main function that uses helper
  main_func <- function(a) {
    declare(type(a = double(n)))
    a_norm <- normalize(a)
    a_norm
  }
  
  # Set up available subfunctions
  available_subs <- list(
    normalize = list(scope = "internal", closure = normalize)
  )
  
  # Generate Fortran
  fsub <- quickr:::new_fortran_subroutine(
    "main_func", 
    main_func,
    available_subfunctions = available_subs
  )
  
  fortran_code <- as.character(fsub)
  
  # Print for inspection
  # cat("\nGenerated Fortran:\n")
  # cat(fortran_code)
  # cat("\n")
  
  # Check structure
  expect_match(fortran_code, "contains", ignore.case = TRUE)
  expect_match(fortran_code, "subroutine normalize_internal", ignore.case = TRUE)
  expect_match(fortran_code, "norm = sqrt.*sum.*x.*x", ignore.case = TRUE)
})

test_that("complex example with multiple internal procedures", {
  # Helper functions
  dot_product_helper <- function(a, b) {
    declare(type(a = double(n)))
    declare(type(b = double(n)))
    result <- sum(a * b)
    result
  }
  
  scale_vector <- function(x, factor) {
    declare(type(x = double(n)))
    declare(type(factor = double(1)))
    result <- x * factor
    result
  }
  
  # Main function using multiple helpers
  compute_projection <- function(u, v) {
    declare(type(u = double(n)))
    declare(type(v = double(n)))
    
    # Compute dot products
    uv <- dot_product_helper(u, v)
    vv <- dot_product_helper(v, v)
    
    # Compute projection scalar
    scalar <- uv / vv
    
    # Scale the vector
    projection <- scale_vector(v, scalar)
    projection
  }
  
  # Set up subfunctions
  available_subs <- list(
    dot_product_helper = list(scope = "internal", closure = dot_product_helper),
    scale_vector = list(scope = "internal", closure = scale_vector)
  )
  
  # Generate Fortran
  fsub <- quickr:::new_fortran_subroutine(
    "compute_projection",
    compute_projection,
    available_subfunctions = available_subs
  )
  
  fortran_code <- as.character(fsub)
  
  # Print for inspection
  # cat("\nGenerated Fortran (complex example):\n")
  # cat(fortran_code)
  # cat("\n")
  
  # Verify structure
  expect_match(fortran_code, "contains", ignore.case = TRUE)
  expect_match(fortran_code, "subroutine dot_product_helper_internal", ignore.case = TRUE)
  expect_match(fortran_code, "subroutine scale_vector_internal", ignore.case = TRUE)
  expect_match(fortran_code, "call dot_product_helper_internal", ignore.case = TRUE)
  expect_match(fortran_code, "call scale_vector_internal", ignore.case = TRUE)
})


test_that("basic internal procedure functionality works", {
  # Clear any previous registrations
  quickr:::collector$clear_subfunctions()
  
  # Register a simple internal function
  normalize <- quick(
    function(x) {
      declare(type(x = double(n)))
      norm <- sqrt(sum(x * x))
      result <- x / norm
      result
    }, 
    name = "normalize",
    scope = "internal"
  )
  
  # Verify it returns a placeholder with correct attributes
  expect_true(attr(normalize, "quickr_internal"))
  expect_equal(attr(normalize, "name"), "normalize")
  
  # Verify it gives the right error when called directly
  expect_error(normalize(c(1, 2, 3)), "cannot be called directly")
  
  # Use it in a main function
  use_normalize <- quick(
    function(data) {
      declare(type(data = double(n)))
      normalized <- normalize(data)
      normalized
    },
    name = "use_normalize"
  )
  
  # Test execution
  test_data <- c(3.0, 4.0)
  result <- use_normalize(test_data)
  expect_equal(result, c(0.6, 0.8))
  expect_equal(sum(result^2), 1.0, tolerance = 1e-10)
})

test_that("multiple internal procedures work together", {
  quickr:::collector$clear_subfunctions()
  
  # Register multiple internal functions
  dot_product <- quick(
    function(a, b) {
      declare(type(a = double(n)))
      declare(type(b = double(n)))
      result <- sum(a * b)
      result
    },
    name = "dot_product",
    scope = "internal"
  )
  
  scale_vector <- quick(
    function(x, factor) {
      declare(type(x = double(n)))
      declare(type(factor = double(1)))
      result <- x * factor
      result
    },
    name = "scale_vector", 
    scope = "internal"
  )
  
  # Use both in a main function
  project_vector <- quick(
    function(u, v) {
      declare(type(u = double(n)))
      declare(type(v = double(n)))
      
      # Project u onto v: (u·v/v·v) * v
      uv <- dot_product(u, v)
      vv <- dot_product(v, v)
      scalar <- uv / vv
      projection <- scale_vector(v, scalar)
      projection
    },
    name = "project_vector"
  )
  
  # Test
  u <- c(3.0, 4.0)
  v <- c(1.0, 0.0)
  result <- project_vector(u, v)
  expect_equal(result, c(3.0, 0.0))  # Projection onto x-axis
})

test_that("internal procedure registry persists across calls", {
  quickr:::collector$clear_subfunctions()
  
  # Register a function
  helper1 <- quick(
    function(x) {
      declare(type(x = double(n)))
      x * 2
    },
    name = "helper1",
    scope = "internal"
  )
  
  # Verify it's registered
  registered <- quickr:::collector$get_registered_subfunctions("internal")
  expect_true("helper1" %in% names(registered))
  
  # Register another function
  helper2 <- quick(
    function(x) {
      declare(type(x = double(n)))
      x + 1
    },
    name = "helper2",
    scope = "internal"
  )
  
  # Both should be registered
  registered <- quickr:::collector$get_registered_subfunctions("internal")
  expect_true("helper1" %in% names(registered))
  expect_true("helper2" %in% names(registered))
  
  # Use both in a function
  use_both <- quick(
    function(data) {
      declare(type(data = double(n)))
      temp <- helper1(data)  # data * 2
      result <- helper2(temp) # temp + 1
      result
    },
    name = "use_both"
  )
  
  # Test: (3 * 2) + 1 = 7
  expect_equal(use_both(3.0), 7.0)
})

test_that("error messages are informative", {
  quickr:::collector$clear_subfunctions()
  
  internal_fn <- quick(
    function(x) {
      declare(type(x = double(n)))
      x + 1
    },
    name = "internal_fn",
    scope = "internal"
  )
  
  # Check error message when calling directly
  expect_error(
    internal_fn(c(1, 2, 3)),
    "Internal function 'internal_fn' cannot be called directly"
  )
  
  # Check error when using undefined function
  expect_error(
    quick(function(x) {
      declare(type(x = double(n)))
      undefined_function(x)
    }),
    "Unsupported function: undefined_function"
  )
})

test_that("generated Fortran code is correct for internal procedures", {
  quickr:::collector$clear_subfunctions()
  
  # Register internal function
  helper <- quick(
    function(x) {
      declare(type(x = double(n)))
      x * x
    },
    name = "square",
    scope = "internal"
  )
  
  # Create main function
  main_fn <- function(data) {
    declare(type(data = double(n)))
    squared <- square(data)
    squared
  }
  
  # Generate Fortran
  fsub <- quickr:::new_fortran_subroutine(
    "test_main",
    main_fn,
    available_subfunctions = quickr:::collector$get_registered_subfunctions("internal")
  )
  
  fortran_code <- as.character(fsub)
  
  # Verify structure
  expect_match(fortran_code, "contains", ignore.case = TRUE)
  expect_match(fortran_code, "subroutine square_internal", ignore.case = TRUE)
  expect_match(fortran_code, "call square_internal\\(data, squared, data__len_\\)")
})