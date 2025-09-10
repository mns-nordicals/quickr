# Matrix Support Investigation

This document tracks decisions and findings for adding matrix/linear algebra support to quickr (R→Fortran transpiler).

## Summary

- We can and should link to the exact BLAS/LAPACK that R uses via `$(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)` in `src/Makevars*`.
- Start with dense matrix operations from base R: `%*%`, `crossprod`, `tcrossprod`, `t`, `solve`, `chol`, `qr`, `svd`, `eigen`, `determinant`, plus `colSums/rowSums` and `colMeans/rowMeans`.
- The Matrix package is not “base R”; it is a Recommended package with dense and sparse classes. For v1, scope dense operations only; defer sparse (CHOLMOD, SuiteSparse) to later.
- Fortran stdlib provides reference BLAS/LAPACK in pure Fortran (optional external LAPACK hook). It does not automatically link to vendor BLAS like OpenBLAS/MKL; performance will generally trail optimized BLAS.

---

## BLAS/LAPACK: How to Link in an R Package

- Use R’s build macros so quickr-generated Fortran code links against the same BLAS/LAPACK that the user’s R is built with.
- In `src/Makevars` (and platform variants), append in this order:

```make
# src/Makevars (Unix/macOS)
PKG_LIBS += $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)
```

```make
# src/Makevars.win or src/Makevars.ucrt (Windows, Rtools)
PKG_LIBS += $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)
```

- Notes and caveats (from “Writing R Extensions”):
  - If `$(BLAS_LIBS)` is empty, R’s executable provides the double and double-complex BLAS; linking is still correct and portable.
  - `$(LAPACK_LIBS)` may point to `libRlapack` (reference) or an external LAPACK, or be empty if the external BLAS includes LAPACK.
  - Keep the order: `LAPACK_LIBS` then `BLAS_LIBS` then `FLIBS`.
  - Do not hardcode `-lopenblas`, `-lmkl_rt`, etc.; rely on macros so CRAN binaries and user custom BLAS are honored.

### Calling from Fortran

- quickr emits Fortran; call BLAS/LAPACK by their Fortran names directly (e.g., `dgemm`, `dgesv`, `dpotrf`) — no underscore juggling or `F77_CALL` needed.
- R and Fortran agree on column-major layout; R’s numeric is double, matching BLAS “D” routines. Pass dimensions as default 32-bit `integer`.

Example (matrix multiply C = A %*% B):

```fortran
! A is (m x k), B is (k x n), C is (m x n)
call dgemm('N','N', m, n, k, 1.0d0, A, m, B, k, 0.0d0, C, m)
```

---

## What Matrix Operations to Support (v1 → vN)

Prioritize coverage of base R dense operations with clean BLAS/LAPACK mappings. All are column-major and double precision.

### Phase 1: Core dense ops

- `%*%` (matrix multiply): BLAS `dgemm` (and `dgemv` for vector cases)
- `crossprod(x, y)` and `tcrossprod(x, y)`: map to `dgemm` with transposes; consider `dsyrk` where applicable
- `t(x)` (transpose): Fortran intrinsic `transpose(x)`; BLAS has no dedicated transpose routine
- Elementwise `+ - * /` and comparisons: loop kernels (non-BLAS)

### Phase 2: Solves & decompositions

- `solve(A, b)` general dense: LAPACK `dgesv` (or `dgetrf` + `dgetrs`)
- `solve(A)` (inverse) if needed: discourage; if required, `dgetrf` + `dgetri`
- `chol(A)`: `dpotrf` (and `dpotrs` for solves)
- `qr(A)`: `dgeqrf` (thin QR) returning upper-triangular `R` (k×n, k=min(m,n)); `Q` via `dorgqr` to follow.
- `svd(A)`: initial support returns values-only (`d`), via `dgesvd` with `JOBU=JOBVT='N'`.
- `eigen(A)`: initial support returns values-only for symmetric matrices, via `dsyev` with `JOBZ='N'`.
- `determinant(A)`: via LU `dgetrf` (sign and log-abs det)

### Phase 3: Utilities & reductions

- `colSums/rowSums`, `colMeans/rowMeans`: BLAS `dgemv` with a ones vector or loop
- `diag`, `diag<-`, diagonal extraction/formation: loops
- `sweep`, `scale`: loops + BLAS axpy/scal where applicable (`dscal`, `daxpy`)
- Triangular/symmetric helpers: `dtrsm`, `dtrsv`, `dsymm`, `dsyrk`

### Out of scope (initially)

- Sparse matrices and operations (Matrix/CHOLMOD, SuiteSparse)
- Complex types (R base uses double real; add later as needed)

---

## Is Matrix “base R”? What to support from Matrix

- Matrix is a “Recommended” package, not part of the base namespace. It ships with most R binaries but is separate from base.
- It provides S4 dense (dgeMatrix, etc.) and sparse (dgCMatrix, etc.) classes, using BLAS/LAPACK for dense and SuiteSparse/CHOLMOD/ARPACK for sparse.
- Proposal: v1 targets base R’s dense semantics on `matrix`/`array`. Treat Matrix support as a later layer once dense is stable. Sparse requires different data structures and external deps.

---

## Fortran stdlib (fortran-lang/stdlib) and Linear Algebra

- stdlib includes BLAS and LAPACK modules implemented in pure Fortran (reference-style) with optional switches to call external LAPACK.
  - BLAS: e.g., `stdlib_blas` with `stdlib_dgemm`, etc. (pure Fortran reference kernels)
  - LAPACK: `stdlib_linalg_lapack` can be built with `STDLIB_EXTERNAL_LAPACK*` to use external LAPACK; otherwise supplies its own module procedures.
- Implications for quickr:
  - Pros: zero external dependency, easy vendoring for niche ops, nice generic-kinds coverage.
  - Cons: performance below OpenBLAS/MKL/Accelerate; duplicates what R already brings; adds maintenance surface.
- Recommendation: default to R’s BLAS/LAPACK via macros. Consider stdlib as an optional fallback or for niche algorithms not exposed by standard BLAS/LAPACK.

---

## Proposed Design for quickr

- Codegen strategy:
  - Introduce `r2f` handlers for matrix primitives (`%*%`, `crossprod`, `tcrossprod`, `t`, `solve`, `chol`, `qr`, `svd`, `eigen`, `determinant`).
  - Emit direct calls to BLAS/LAPACK Fortran routines with correct `lda/ldb/ldc` and transpose flags.
  - Add a tiny shim module (optional) to wrap common patterns (e.g., `q_dgemm_nn`, `q_dgesv`) for safer argument order and consistent error codes.

- Build/link:
  - Add `src/Makevars*` with `PKG_LIBS += $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)`.
  - Keep code double-precision only initially (matches R’s `numeric`).

- Semantics & checks:
  - Verify conformable dimensions at compile-time when possible; otherwise generate runtime checks that error like base R.
  - Preserve column-major layout; avoid unnecessary copies.

---

## Open Questions / Decisions

- Do we introduce a backend switch (`R_BLAS` default, optional `STDLIB`) for environments without external BLAS? (R nearly always provides BLAS; a switch may not be necessary.)
- Which decomp algorithms to expose by name vs via high-level wrappers (e.g., `solve.chol` when SPD is known)?
- Error propagation: return codes vs R-condition raising from the C wrapper — we should standardize this before implementing solvers.

---

## References

- Writing R Extensions: BLAS/LAPACK libraries, macros `BLAS_LIBS`, `LAPACK_LIBS`, `FLIBS` (R 4.5+)
- R headers with BLAS/LAPACK decls for C (`R_ext/BLAS.h`, `R_ext/Lapack.h`) — Fortran calls use routine names directly
- Fortran-lang stdlib API: `stdlib_blas`, `stdlib_linalg_lapack` modules

---

## Next Steps

- DONE: Add `src/Makevars*` stubs with the correct macros.
- DONE: Implement `%*%`, `crossprod`, `tcrossprod`, `t` handlers in `r2f` using BLAS and Fortran intrinsics.
- DONE: Implement `solve(A, b)` via LAPACK `dgesv` (copies inputs, returns `x`).
- DONE: Implement `chol(A)` via LAPACK `dpotrf('U', ...)` returning upper-triangular factor; zero out strict lower triangle to match base `chol` semantics.
- DONE: Implement `qr(A)` via LAPACK `dgeqrf` returning thin `R`; add tests.
- DONE: Implement `svd(A)` values-only and `eigen(A)` values-only for symmetric matrices; add tests.
- NEXT: Add `svd` full U/V and `eigen` vectors; then general (non-symmetric) eigen (`dgeev`).
- NEXT: Expand tests (conformability, NaN propagation; NA unsupported; error messages).

---

## Status (current)

- Linking:
  - Package builds: `src/Makevars` and `src/Makevars.win` now append `$(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)`.
  - `quick()` compilation: `R/quick.R` augments `R CMD SHLIB` with the same link flags via `R CMD config` to ensure parity with the running R.

- Implemented ops (Phase 1):
  - `%*%`:
    - matrix×matrix → `dgemm('N','N', m, n, k, alpha=1, A, lda=m, B, ldb=k, beta=0, C, ldc=m)`.
    - matrix×vector → `dgemv('N', m, n, alpha=1, A, lda=m, x, incx=1, beta=0, y, incy=1)`.
    - vector×matrix → `dgemv('T', m, n, alpha=1, A, lda=m, x, incx=1, beta=0, y, incy=1)`.
    - vector×vector → `dot_product(x, y)` (Fortran intrinsic). R returns a scalar; if strict 1×1 matrix is desired, we can wrap in the C bridge later.
  - `crossprod(x, y)`:
    - Single-arg: matrix → `dgemm('T','N', k, k, m, ...)`, vector → `dot_product(x, x)`.
    - Two-arg: maps to `dgemm('T','N', ...)`, `dgemv('T', ...)`, or `dot_product` by rank.
  - `tcrossprod(x, y)`:
    - Single-arg: matrix → `dgemm('N','T', m, m, k, ...)`, vector → `dot_product(x, x)`.
    - Two-arg: maps to `dgemm('N','T', ...)`, `dgemv('N', ...)`, or `dot_product` by rank.
- `t(x)`: Fortran `transpose(x)` with dims reversed.
- `solve(A, b)`: LAPACK `dgesv` (general dense linear solve); returns `x` with b’s shape.
- `chol(A)`: LAPACK `dpotrf('U', ...)`; emits upper-triangular factor with lower triangle zeroed for parity with base R.
- `colSums/rowSums`: Fortran `sum(X, dim=1|2)` for rank-2 arrays.
- `colMeans/rowMeans`: `sum(X, dim=1|2) / m|n` (computed from input dims).
- `qr(A)`: LAPACK `dgeqrf`; returns thin `R` matching `qr.R(qr(A))`.
- `svd(A)`: LAPACK `dgesvd` with `JOBU=JOBVT='N'`; returns singular values (`d`) only.
- `eigen(A)`: LAPACK `dsyev('N','U', ...)`; returns eigenvalues only for symmetric input.

- Tests:
  - `tests/testthat/test-matmul.R`: matrix×matrix, matrix×vector, vector×matrix `%*%`, and `t(x)`.
  - `tests/testthat/test-solve-chol.R`: `solve(A, b)` and `chol(A)` compare to base R.
  - `tests/testthat/test-colrow-sums-means.R`: `colSums/rowSums` and `colMeans/rowMeans` parity.
  - `tests/testthat/test-qr.R`: thin-`R` from `qr(A)` parity with base.
  - `tests/testthat/test-cross-tcrossprod.R`: `tcrossprod` vector outer-product semantics and `crossprod` vector dot-product.
  - `tests/testthat/test-svd-eigen.R`: `svd(A)` returns `svd(A)$d`; `eigen(A)` returns `eigen(A)$values` for symmetric.
  - Full suite passes locally; add more edge-case tests (conformability, error codes) next.

- Notes/decisions:
  - For vector dot products we currently use `dot_product` intrinsic to avoid declaring an explicit `ddot` interface under `implicit none`. We can switch to BLAS `ddot` later by emitting a function declaration/interface in the generated unit if needed.
  - We allocate outputs in Fortran/C bridge based on shape inference; inputs are treated read-only (copy-on-modify in the bridge when required).

---

## Fixes in This Branch

- Corrected `tcrossprod()` semantics for vector inputs:
  - Single vector: now returns the outer product matrix `x %*% t(x)` (previously returned a scalar dot product by mistake).
  - Two vectors: now returns the outer product matrix `x %*% t(y)` (previously returned a scalar).
- Added tests in `tests/testthat/test-cross-tcrossprod.R` to cover these cases.

- Added values-only support for `svd(A)` and `eigen(A)` and tests. Future work: return U/V matrices and eigenvectors.

---

## Temporary Error Strategy (LAPACK info)

- Constraint: quickr does not yet support building rich R error strings at runtime from Fortran.
- Approach for now: do not raise R errors on LAPACK failures. Instead, print a short diagnostic and continue returning the (possibly partial) result like base LAPACK would leave it.
- Implementation:
  - `solve(A, b)` via `dgesv`: after the call, if `info != 0` we print
    - `info < 0`: `"dgesv: illegal arg; INFO="` then the integer `info`.
    - `info > 0`: `"dgesv: singular; INFO="` then the integer `info`.
  - `chol(A)` via `dpotrf`: after the call, if `info != 0` we print
    - `info < 0`: `"dpotrf: illegal arg; INFO="` then `info`.
    - `info > 0`: `"dpotrf: not PD; INFO="` then `info`.
- Printing uses R’s Fortran printers available in the R runtime: `labelpr` (for a literal message) and `intpr1` (for a scalar integer). This avoids needing string objects in quickr while still surfacing failures for users.
- Future: once string support lands, translate common LAPACK `info` conditions into R conditions (errors) with parity to base R wording.

---

## Return Lists (Limitation)

- quickr only supports `list(...)` to pack final return values. Lists are not a general data structure inside quickr-generated code.
- Allowed patterns (final return only):
  - Direct: `list(a = a, b = b)` as the last expression in the function body.
  - Assigned just before return: `out <- list(a = a, b = b); out` (the transpiler rewrites this so the last expression is the list call).
- Constraints for return lists:
  - Elements must be symbols (no nested lists or expressions). Names, when provided, must be syntactic (same rules as base R identifiers).
  - Duplicate symbols are allowed and will be returned twice.
  - Using `list(...)` anywhere else (e.g., mid-function for general use, or accessing elements like `out$y`) is not supported and errors.
- Implication: multi-result algorithms (e.g., full SVD returning `U`, `d`, `V`) must pack results into the final return list; intermediate list manipulation is not supported.

---

## Feasibility & Difficulty

- Linking: straightforward and portable with R’s `$(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)` macros, valid on Unix/macOS/Windows and aligned with the user’s BLAS choice (Rblas, OpenBLAS, MKL, Accelerate). For Fortran callers, BLAS/LAPACK calls are direct (`call dgemm(...)`), no string-length glue needed.
- Translation: dense ops map cleanly: `%*%`→`dgemm`/`dgemv`, `crossprod`/`tcrossprod`→`dgemm`/`dsyrk`, `solve`→`dgesv`, `chol`→`dpotrf`, `qr`→`dgeqrf`+`dorgqr`, `svd`→`dgesdd`, `eigen`→`dsyevr`/`dgeev`. Elementwise ops remain Fortran loops.
- Main work is ensuring R-compatible edge semantics (vector cases in `%*%`, conformability errors, attribute handling). These are contained and testable.

## quickr Semantics: NA and Copy-on-Modify

- NA policy: quickr does not support NA in inputs. We assume arrays passed to Fortran are NA-free. We therefore do not add NA scans in BLAS/LAPACK paths; any NaN produced numerically will propagate per IEEE rules (acceptable). Tests should avoid NA inputs and, where relevant, assert NaN propagation only.
- Copy-on-modify: the C bridge duplicates inputs that are marked as modified and passes others by reference. Codegen must accurately mark variables/temporaries as `modified` to preserve R’s copy-on-modify semantics. See duplication in `R/c-wrapper.R:172` (within `closure_arg_c_defs`, the `if (var@modified)` branch) and pointer extraction that follows.

## Performance: What Do We Gain?

- Single ops: parity with base R (same BLAS). Quickr shines by:
  - Fusing operations (e.g., `C <- A %*% B + C` → one `dgemm` with `beta=1`).
  - Avoiding R interpreter overhead in loops and repeated allocations.
  - Keeping temporaries in Fortran and reducing copies; rely on BLAS for threading.

## Translation Details and Edge Cases

- `%*%` cases: matrix×matrix→`dgemm`; matrix×vector→`dgemv` (`trans='N'`); vector×matrix→`dgemv` (`trans='T'`); vector×vector→`dot_product`.
- `crossprod(x, y)` = `t(x) %*% y` → `dgemm('T','N', ...)`; if `y` missing, prefer `dsyrk`. `tcrossprod` → `dgemm('N','T', ...)`.
- `t(x)`: Fortran intrinsic `transpose(x)` (BLAS has no “transpose” op).
- LAPACK workspace: use `lwork=-1` queries where applicable; for now, do not raise R errors from LAPACK `info` codes. Instead, emit console messages from Fortran using R's Fortran printers (`labelpr`, `intpr1`).
- Shapes/lda: column-major; compute `lda/ldb/ldc` from op dims; runtime checks when static inference insufficient.
- Attributes: v1 can drop names; align incrementally.

## Effort Estimate (v1)

- Linking/scaffolding: 0.5–1 day
- Phase 1 handlers: 1–2 days + tests
- Phase 2 (`solve`, `chol`), then `qr/svd/eigen/determinant`: 3–6 days + tests

## Fusion Patterns (early wins)

- `C <- A %*% B + C` → `dgemm(..., beta=1)`
- `y <- a * (A %*% x) + b * y` → `dgemv` with `alpha=a`, `beta=b`
- `crossprod(X)` → `dsyrk`

## Risks & Mitigations

- Error text parity → copy base wording; snapshot tests.
 - NaN propagation (NA unsupported) → rely on IEEE; add tests; avoid reordering that hides NaNs.
- Aliasing vs R copy-on-modify → treat inputs read-only; copy if also used as output.
