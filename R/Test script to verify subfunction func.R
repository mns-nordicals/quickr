# Test script to verify subfunction functionality
# Run this after applying the fixes
devtools::load_all()
# Clear any existing state
# Test script to verify subfunction functionality
# Run this after applying the fixes

# Clear any existing state
quickr:::collector$clear_subfunctions()

# Check environment - should be empty for interactive mode
cat("DEVTOOLS_LOAD env var:", Sys.getenv("DEVTOOLS_LOAD"), "\n")
cat("In package development mode:", nzchar(Sys.getenv("DEVTOOLS_LOAD")), "\n\n")

cat("=== Testing Subfunction Functionality ===\n")

# 1. Register an internal function
cat("\n1. Registering internal function 'normalize'...\n")
normalize <- quick(function(x) {
  declare(type(x = double(n)))
  norm <- sqrt(sum(x * x))
  result <- x / norm
  result
}, name = "normalize", scope = "internal")

# Check attributes
cat("normalize attributes:\n")
cat("  quickr_internal:", attr(normalize, "quickr_internal"), "\n")
cat("  name:", attr(normalize, "name"), "\n")

# Show collector state
cat("\nCollector state after registering internal function:\n")
quickr:::collector$debug_registry()

# 2. Try to call internal function directly (should error)
cat("\n2. Testing direct call (should error)...\n")
try({
  result <- normalize(c(1, 2, 3))
  cat("ERROR: Direct call should have failed\n")
}, silent = FALSE)

# 3. Create a main function that uses the internal function
cat("\n3. Creating main function that uses internal function...\n")
compute <- quick(function(data) {
  declare(type(data = double(n)))
  normalized <- normalize(data)
  normalized
}, name = "compute")

cat("compute function class:", class(compute), "\n")
cat("compute function typeof:", typeof(compute), "\n")

# Show collector state after creating standalone function
cat("\nCollector state after creating standalone function:\n")
quickr:::collector$debug_registry()

# 4. Test the main function
cat("\n4. Testing main function...\n")
test_data <- c(3.0, 4.0)
cat("Input data:", test_data, "\n")

try({
  result <- compute(test_data) 
  cat("Result:", result, "\n")
  cat("Expected: [0.6, 0.8]\n")
  cat("Sum of squares:", sum(result^2), "(should be ~1.0)\n")
  cat("SUCCESS: Internal function call worked!\n")
}, silent = FALSE)

cat("\n=== Test Complete ===\n")