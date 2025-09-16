# Matrix Support Progress Log

This document tracks the ongoing work to implement BLAS/LAPACK-backed matrix operations in quickr, following the design in `matrix-proposal.md`.

## 2025-09-14

- Moved matrix handlers into `R/r2r-matrix.R` and implemented BLAS-backed lowering with destination awareness.

- Operators implemented and behavior:
  - `%*%` (Matrix×Matrix): Emits `dgemm('N','N', M,N,K, ...)` with correct leading dimensions (`LDA/LDB/LDC` = rows of original arrays). Destination-aware: when the LHS exists and does not alias inputs, we write directly into it; otherwise we allocate a temp.
  - `%*%` (Matrix×Vector / Vector×Matrix): Emits `dgemv('N', ...)` and `dgemv('T', ...)` respectively. Shapes match base R (m×1 and 1×n). Destination-aware as above.
  - `crossprod(x[, y])`: Emits `dgemm('T','N', ...)` with correct LDAs/LDBs.
  - `tcrossprod(x[, y])`: Emits `dgemm('N','T', ...)` with correct LDAs/LDBs.
  - `t(x)`: Uses `transpose(x)` for matrices. For vectors, reshapes to 1×n via `reshape(x, [1, int(len)])` to satisfy INTEGER kind in the constructor.

- Safety and calling conventions:
  - BLAS is always used for these ops. Sizes are passed as-is (no casts); `alpha/beta` use `1.0_c_double` and `0.0_c_double`.
  - We only pass simple identifiers (e.g., `x`, `y`, `tmp1_`) into BLAS to ensure contiguity. Non‑identifier expressions are first materialized into a temp, then passed to BLAS. This also ensures multi‑op expressions like `x %*% x %*% x` become multiple BLAS calls with temps, not intrinsic `matmul`.
  - LDA/LDB selection follows column‑major rules: always rows of the original arrays, regardless of transpose flags.

- Tests added (parity vs base R):
  - `%*%` matrix×matrix, matrix×vector, vector×matrix.
  - `t()` for matrix and vector.
  - `crossprod(x)`, `crossprod(x,y)`, `tcrossprod(x)`, `tcrossprod(x,y)`.
  - Also a codegen sanity check that direct assignment `z <- x %*% y` uses a `dgemm` writing into `z` without extra temps.

- Current status: All parity tests pass for dense doubles with BLAS enabled.

- Notes:
  - The `is_fortran_identifier()` helper conservatively detects when an operand is a simple, contiguous variable name; this is used to decide when we can safely call BLAS directly.
  - The `<-` handler threads a `dest` hint into children so a single BLAS op can write directly into the LHS when safe.

- Next steps:
  - Add accumulation support (`alpha/beta`) for patterns like `Y <- A %*% B + C` to avoid an extra add.
  - Recognize explicit transpose operands to set BLAS op flags instead of materializing `transpose()` when safe.
  - Add LAPACK-backed `solve`, `chol`, and `chol2inv` with minimal copying.
  - Extend tests to cover error messages for dimension mismatches and aliasing scenarios.

## Known limitations

- No full alias analysis yet; we conservatively write into the LHS only when it is known not to alias inputs.
- Non‑identifier expressions/slices are materialized to temps before BLAS for safety (by design).
- Vector transpose uses `reshape` to 1×n; revisit once we add view semantics.
