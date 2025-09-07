# Extending quickr: Handlers, Runtime Helpers, and Libraries

This document outlines a practical pattern for extending quickr with new functions that aren’t Fortran intrinsics (e.g., `sort`, `order`, `rank`, rolling ops, etc.). It summarizes the current architecture, proposes an extensibility strategy, documents the runtime helper mechanism that was added, and discusses options for using the Fortran stdlib and Fortran modules.

## Overview

- quickr translates R AST to Fortran code using a registry of handlers (`r2f_handlers`) that each map an R call to generated Fortran.
- Intrinsics (e.g., `sum`, `max`, `min`) translate to compact expressions; most heavy lifting is delegated to the compiler/runtime.
- For non-intrinsic features (e.g., `sort`), we can: 1) emit Fortran statements via `hoist()`, and 2) call small internal helper procedures that we inject into the generated subroutine.
- The injected helpers live in a `contains` block by default (no extra files, no linking), but the same code can later be moved into a shared module to reduce duplication when building a package.

## Relevant Files

- `R/r2f.R`: Core translation and handler registry (`r2f_handlers`).
- `R/subroutine.R`: Builds the Fortran subroutine and now optionally appends a `contains` block with runtime helpers.
- `R/runtime.R`: Small runtime registry for internal Fortran procedures and helper functions for requesting them.
- `R/c-wrapper.R`: Generates the C bridge; unchanged by the runtime mechanism.

## Handler Pattern (How to add a new function)

1) Register a handler
- Add a function to `r2f_handlers[["your_call_name"]]`.
- Inside, translate arguments via `r2f()` and validate types/dimensions.
- Compute the output Variable (`Variable(mode, dims)`) for the expression you’ll return.

2) Emit Fortran
- For simple expressions, return `Fortran("expr", Variable(...))`.
- For multi-statement needs, use `hoist()` to emit statements (e.g., allocations, conditionals) and return a temporary variable name as a `Fortran()` expression.

3) Use runtime helpers when needed
- If you need a non-intrinsic algorithm (e.g., sorting), request a helper via `quickr_require_runtime(scope, id)` where `id` is a key into the runtime registry.
- Reference the helper by name in the emitted Fortran expression/statements.

4) Tests
- Add translation snapshots with `expect_translation_snapshots()` and behavioral tests with `expect_quick_equal()`/`expect_quick_identical()`.

## Runtime Helpers

Runtime helpers are small Fortran internal procedures injected into the subroutine’s `contains` section when requested by handlers.

- Declared in `R/runtime.R` as a registry: `quickr_runtime_sources()` maps an id → Fortran source text.
- Request from a handler with `quickr_require_runtime(scope, id)` (no-ops if already requested).
- `new_fortran_subroutine()` collects requested ids and appends a `contains` block to the subroutine with those internal procedures.
- Pros: No separate files or linking, works in `quick()` and tests; straightforward portability.
- Cons: Duplicates helper code in each subroutine that needs it; slightly larger generated code.

### Current helpers

- `sort_d`, `sort_d_desc`: insertion sort for `real(c_double)` ascending/descending.
- `sort_i`, `sort_i_desc`: insertion sort for `integer(c_int)` ascending/descending.

These are deliberately tiny and easy to audit. They can be replaced with faster implementations later without changing handler contracts.

## Example Handler: `sort(x, decreasing=FALSE)`

Implemented in `R/r2f.R`:
- Supports 1‑D `double` and `integer` vectors.
- Accepts `decreasing=` omitted, a logical scalar, or a length‑1 logical vector.
- For constant `decreasing`, calls a single helper; for dynamic `decreasing`, hoists an `if`/`else` to choose ascending/descending helper and assigns into a temp.
- Returns a fresh vector; does not modify inputs.

Limitations and notes:
- Only 1‑D vectors; non‑numeric types not yet supported.
- NA/NaN/Inf ordering isn’t aligned with base R’s full semantics.
- Insertion sort is O(n²) — fine for small/medium vectors and tests; can be upgraded.

## Modules vs `contains`

Two ways to provide reusable Fortran code:

- `contains` (current default)
  - Injects internal procedures directly in each generated subroutine.
  - Zero extra files; no linking or build changes.
  - Ideal for `quick()` and local testing; guarantees no symbol clashes.
  - Downsides: duplicates helper code across subroutines.

- Fortran modules (recommended for package builds)
  - Centralize helpers into a module, e.g., `module quickr_runtime; contains ... end module`.
  - Subroutines `use quickr_runtime, only: ...` instead of inlining.
  - Pros: Smaller output, one source of truth, easier upgrades.
  - Requires compile orchestration: emit one `quickr_runtime.f90` and pass it along to the SHLIB build for `compile()` and `compile_package()`.

Pragmatic path:
- Keep `contains` for ad‑hoc `quick()` calls.
- When compiling a package (`compile_package()`), aggregate required helpers across all quick functions into a single `src/quickr_runtime.f90` module and `use` it. This can be implemented without changing handler code.

## Using Fortran stdlib

The Fortran stdlib (https://github.com/fortran-lang/stdlib) offers well-tested procedures (sorting, stats, strings, containers, etc.) under a permissive license.

Options to integrate:
- Vendor subset: Copy selected stdlib modules (and any dependencies) into `src/` and `use` them. Pros: no external dependency at build time; stable reproducible builds. Cons: maintenance overhead and occasional sync with upstream.
- External dependency: Build/link against a preinstalled stdlib. Pros: smaller repo. Cons: increases user build friction; not universally available by default.

Recommendation:
- Start by vendoring a minimal subset (e.g., sorting and stats) into a `quickr_runtime` module. This keeps builds simple and portable while improving performance and correctness vs hand-rolled helpers.
- Keep the current `contains` fallback for `quick()` and testing.
- Later add a build flag to switch between vendored or system stdlib.

Notes:
- stdlib is Fortran and used from Fortran; no C bridge needed. We only need to compile the module alongside generated subroutines.
- Ensure feature parity and predictable ordering semantics (esp. NA/NaN) when replacing helpers.

## Gotchas / Design Calls

- Scalars vs length‑1 vectors: quickr treats certain length‑1 vectors as scalars for convenience (e.g., `logical(1)`), but handlers should check both forms.
- Hoisting: Use `hoist()` only when necessary — it produces statements above the current expression; ensure returned `Fortran()` references a temp that exists.
- Kinds: Use `iso_c_binding` kinds (`c_double`, `c_int`, etc.) consistently in helper code to match the codegen.
- Snapshots: Be careful about whitespace changes. `contains` injection was implemented to not perturb existing snapshots.

## Roadmap Ideas

- Sorting family: `order()`, `rank()`, `argsort` returns; stable/partial sorts; key selectors.
- Rolling ops: rolling mean/sum/min/max; windowed operations.
- Reductions: `any`, `all`, `mean`, `var`, robust reductions with masks.
- Signal/FFT: optionally integrate fftw or a stdlib equivalent (behind an opt‑in build flag).
- Random: more RNGs, distributions (with R RNG bridging via `GetRNGstate` already supported).
- Package build aggregation: emit a shared `quickr_runtime.f90` module during `compile_package()`.

## API Reference (runtime)

- `quickr_runtime_sources(id = NULL)` → character
  - When `id` is `NULL`, returns a named list of available helpers.
  - When `id` is a known key, returns the Fortran source for that helper.
- `quickr_require_runtime(scope, id)` → invisible(scope)
  - Mark a helper as required for the current translation scope.
- `quickr_get_runtime_deps(scope)` → character
  - Retrieve unique helper ids requested for the current scope.

## Runnable Example

Minimal `sort()` examples you can run locally:

Sort doubles (ascending):

```r
library(devtools)
load_all()
fn <- quick(function(x) {
  declare(type(x = double(NA)))
  sort(x)
})
fn(c(3, 1, 2))  # 1 2 3
```

Sort integers with dynamic `decreasing`:

```r
library(devtools)
load_all()
fn2 <- quick(function(x, d) {
  declare({
    type(x = integer(NA))
    type(d = logical(1))
  })
  sort(x, decreasing = d)
})
fn2(as.integer(c(4, 2, 3, 1)), TRUE)   # 4 3 2 1
fn2(as.integer(c(4, 2, 3, 1)), FALSE)  # 1 2 3 4
```

Inspect generated Fortran/C for a simple function and see the `contains` block only when required (e.g., a function that uses `sort()`):

```r
library(devtools)
load_all()
fsub <- r2f(function(x) {
  declare(type(x = double(NA)))
  sort(x)
})
cat(fsub)          # shows subroutine with a contains block holding sort helpers
cat(make_c_bridge(fsub))  # C wrapper
```

---

If you want, I can prototype the `quickr_runtime` module generation for `compile_package()` and sketch how to vendor a minimal subset of Fortran stdlib (sorting) to replace the current insertion sorts.
