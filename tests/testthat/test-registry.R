# Tests for quickr internal function registry system

test_that("basic registration and retrieval works", {
  # Clear registry to start fresh
  clear_internal_registry()
  
  # Create a simple test function
  test_fun <- function(x) {
    declare(type(x = double(1)))
    out <- x + 1
    out
  }
  
  # Compile to FortranSubroutine
  attr(test_fun, "name") <- "test_add"
  fsub <- r2f(test_fun)
  
  # Register the function
  expect_silent(register_internal_function("test_add", test_fun, fsub))
  
  # Test retrieval
  retrieved <- get_internal_function("test_add")
  expect_false(is.null(retrieved))
  expect_s7_class(retrieved, InternalFunction)
  expect_equal(retrieved@name, "test_add")
  expect_equal(length(retrieved@argument_vars), 1)
  expect_equal(names(retrieved@argument_vars), "x")
  expect_equal(retrieved@return_var@mode, "double")
})

test_that("is_internal_function works correctly", {
  # Clear registry to start fresh  
  clear_internal_registry()
  
  # Function should not be registered initially
  expect_false(is_internal_function("test_add"))
  expect_false(is_internal_function("nonexistent"))
  
  # Register a function
  test_fun <- function(x) {
    declare(type(x = double(1)))
    out <- x + 1
    out
  }
  attr(test_fun, "name") <- "test_add"
  fsub <- r2f(test_fun)
  register_internal_function("test_add", test_fun, fsub)
  
  # Now it should be found
  expect_true(is_internal_function("test_add"))
  expect_false(is_internal_function("nonexistent"))
})

test_that("registry listing functions work", {
  # Clear and register multiple functions
  clear_internal_registry()
  
  # Register first function
  fun1 <- function(x) {
    declare(type(x = double(1)))
    out <- x + 1
    out
  }
  attr(fun1, "name") <- "add_one"
  fsub1 <- r2f(fun1)
  register_internal_function("add_one", fun1, fsub1)
  
  # Register second function  
  fun2 <- function(x) {
    declare(type(x = integer(1)))
    out <- x * 2L
    out
  }
  attr(fun2, "name") <- "mult_two"  
  fsub2 <- r2f(fun2)
  register_internal_function("mult_two", fun2, fsub2)
  
  # Test listing
  functions_list <- list_internal_functions()
  expect_length(functions_list, 2)
  expect_true("add_one" %in% functions_list)
  expect_true("mult_two" %in% functions_list)
  
  # Test print_registry (just check it doesn't error)
  expect_output(print_registry(), "Internal Function Registry")
})

test_that("registry clearing works", {
  # Register a function
  test_fun <- function(x) {
    declare(type(x = double(1)))
    out <- x + 1
    out
  }
  attr(test_fun, "name") <- "test_clear"
  fsub <- r2f(test_fun)
  register_internal_function("test_clear", test_fun, fsub)
  
  # Verify it's registered
  expect_true(is_internal_function("test_clear"))
  
  # Clear registry
  clear_internal_registry()
  
  # Verify it's gone
  expect_false(is_internal_function("test_clear"))
  expect_length(list_internal_functions(), 0)
})

test_that("input validation works", {
  # Test register_internal_function validation
  test_fun <- function(x) {
    declare(type(x = double(1)))
    out <- x + 1
    out
  }
  attr(test_fun, "name") <- "test_validation"
  fsub <- r2f(test_fun)
  
  # Invalid name
  expect_error(register_internal_function("", test_fun, fsub), 
               "name must be a non-empty string")
  expect_error(register_internal_function(NULL, test_fun, fsub), 
               "name must be a non-empty string")
  expect_error(register_internal_function(123, test_fun, fsub), 
               "name must be a non-empty string")
  
  # Invalid r_function
  expect_error(register_internal_function("test", "not_a_function", fsub), 
               "r_function must be a function")
  expect_error(register_internal_function("test", NULL, fsub), 
               "r_function must be a function")
  
  # Invalid fsub
  expect_error(register_internal_function("test", test_fun, "not_fsub"), 
               "fsub must be a FortranSubroutine")
  expect_error(register_internal_function("test", test_fun, NULL), 
               "fsub must be a FortranSubroutine")
  
  # Test get_internal_function validation
  expect_error(get_internal_function(""), "name must be a non-empty string")
  expect_error(get_internal_function(NULL), "name must be a non-empty string")
  expect_error(get_internal_function(123), "name must be a non-empty string")
})

test_that("quick() with register_as parameter works", {
  # Clear registry
  clear_internal_registry()
  
  # Test basic registration via quick() - must specify name explicitly
  add_one <- quick(function(x) {
    declare(type(x = double(1)))
    out <- x + 1
    out
  }, name = "add_one", register_as = "internal")
  
  # Verify it got registered
  expect_true(is_internal_function("add_one"))
  
  # Verify the returned function still works
  expect_equal(add_one(5), 6)
  
  # Test invalid register_as value
  expect_error(
    quick(function(x) x + 1, register_as = "invalid"),
    "register_as must be 'internal' or NULL"
  )
  
  # Test that name is required when register_as = "internal"
  expect_error(
    quick(function(x) {
      declare(type(x = double(1)))
      out <- x + 1
      out
    }, register_as = "internal"),
    "'name' parameter is required when register_as = 'internal'"
  )
})

test_that("registry handles complex function types", {
  clear_internal_registry()
  
  # Test with matrix function
  matrix_fun <- function(x) {
    declare(type(x = double(2, 3)))
    out <- x * 2.0
    out
  }
  attr(matrix_fun, "name") <- "matrix_double"
  fsub <- r2f(matrix_fun)
  register_internal_function("matrix_double", matrix_fun, fsub)
  
  retrieved <- get_internal_function("matrix_double")
  expect_equal(retrieved@name, "matrix_double")
  expect_equal(retrieved@argument_vars$x@rank, 2)
  expect_equal(retrieved@return_var@mode, "double")
  
  # Test with vector function  
  vector_fun <- function(x) {
    declare(type(x = integer(NA)))
    out <- x + 1L
    out
  }
  attr(vector_fun, "name") <- "vector_add"
  fsub <- r2f(vector_fun)
  register_internal_function("vector_add", vector_fun, fsub)
  
  retrieved <- get_internal_function("vector_add") 
  expect_equal(retrieved@name, "vector_add")
  expect_equal(retrieved@argument_vars$x@rank, 1)
  expect_equal(retrieved@return_var@mode, "integer")
})

test_that("registry handles package scoping edge cases", {
  clear_internal_registry()
  
  # Register function and see what package context it actually uses
  test_fun <- function(x) {
    declare(type(x = double(1)))
    out <- x + 1
    out
  }
  attr(test_fun, "name") <- "explicit_pkg"
  fsub <- r2f(test_fun)
  register_internal_function("explicit_pkg", test_fun, fsub)
  
  # Check what's actually in the registry after registration
  all_keys <- ls(.quickr_internal_registry, all.names = TRUE)
  registered_key <- all_keys[endsWith(all_keys, "::explicit_pkg")]
  expect_length(registered_key, 1)  # Should find exactly one match
  
  # Extract the actual package name that was used
  actual_pkg <- sub("::explicit_pkg$", "", registered_key)
  
  # The fallback logic in get_internal_function should find it without explicit package
  retrieved_implicit <- get_internal_function("explicit_pkg")
  expect_false(is.null(retrieved_implicit))
  expect_equal(retrieved_implicit@name, "explicit_pkg")
  
  # Test that explicit package lookup works with the actual package that was used
  retrieved_explicit <- get_internal_function("explicit_pkg", actual_pkg)
  expect_false(is.null(retrieved_explicit))
  expect_equal(retrieved_explicit@name, retrieved_implicit@name)
})

test_that("print methods work correctly", {
  clear_internal_registry()
  
  # Test empty registry
  expect_output(print_registry(), "Internal function registry is empty")
  
  # Register a function and test non-empty output
  test_fun <- function(x) {
    declare(type(x = double(1)))
    out <- x + 1
    out
  }
  attr(test_fun, "name") <- "print_test"
  fsub <- r2f(test_fun)
  register_internal_function("print_test", test_fun, fsub)
  
  # Test print output contains expected information
  expect_output(print_registry(), "Internal Function Registry")
  expect_output(print_registry(), "print_test")
  expect_output(print_registry(), "1 args")
  expect_output(print_registry(), "returns double")
  
  # Test InternalFunction print method
  retrieved <- get_internal_function("print_test")
  expect_output(print(retrieved), "<InternalFunction>")
  expect_output(print(retrieved), "Name: print_test")
  expect_output(print(retrieved), "Arguments: 1")
  expect_output(print(retrieved), "Return type: double")
})