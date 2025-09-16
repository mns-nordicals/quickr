# Matrix Support Re-Implementation Proposal (R → Fortran)

This document proposes a clean, principled redesign for matrix and linear‑algebra support in quickr. It is written as if we are implementing `%*%` and friends for the first time, but with the benefit of hindsight from early prototypes.

## Goals

- Correctness first: strict R semantics for dense double matrices, dimension checks, and copy‑on‑modify for inputs.
- Minimal temporaries: write results directly into the final destination whenever safe; avoid redundant allocations and copies.
- Clear lowering model: express multi‑operation expressions as sequences of safe, well‑ordered BLAS/LAPACK calls and simple Fortran statements.
- Performance via BLAS/LAPACK: exploit transpose flags, alpha/beta accumulation, and specialized routines when applicable. !! NOTE !! This is posponed. We don't do this now.
- Extensible: a small internal IR that allows localized fusions and future additions (QR/SVD/eigen, complex types, sparse backends).

## Scope (v1)

- Data type: base R numerics (double). Promote integer/logical inputs to double when entering BLAS/LAPACK.
- Memory layout: column‑major dense arrays; no views/strides in external calls (copy if needed).
- Operations:
  - `%*%`, `crossprod`, `tcrossprod`, `t`
  - `solve(A, b)` (values only), `chol`, `chol2inv`
  - Reductions: `colSums/rowSums`, `colMeans/rowMeans`, `diag` extract/form (basic)
- Out of scope (initially): sparse matrices, complex dtype, banded/symmetric storage APIs beyond simple use, full QR/SVD/Eigen vectors/matrices.

## Design Pillars

### 1) Classify routine effects (mutation model)

- Non‑destructive (BLAS): `dgemm`, `dgemv`, `ddot` do not modify input operands; they only overwrite their destinations (`C`, `y`).
- Destructive (LAPACK): many LAPACK drivers overwrite inputs (e.g., `dgesv`: LU in `A`, solution into `B`; `dpotrf`: Cholesky in `A`; `dpotri`: inverse overwrites factor).
- Codegen rule: never mutate user inputs implicitly. Make an explicit work copy for destructive calls only when needed by later uses.

### 2) Destination‑aware lowering (avoid unnecessary temps)

- BLAS wrappers accept optional destination and alpha/beta:
  - GEMM: `gemm(opA, opB, A, B, dest, alpha=1, beta=0)`
  - GEMV: `gemv(opA, A, x, dest, alpha=1, beta=0)`
- Assignment patterns:
  - `LHS <- A %*% B` → emit `call dgemm(..., dest=LHS, beta=0)` (no intermediate array), provided `LHS` does not alias `A` or `B`.
  - `y <- X %*% b` → `gemv(..., dest=y)`.
  - `XtX <- crossprod(X)` → `gemm('T','N', X, X, dest=XtX)`; optionally upgrade to `syrk` later.
- Accumulation patterns:
  - `Y <- A %*% B + C` → `Y = C; gemm(..., dest=Y, beta=1)`.
  - `res <- y - X %*% b` → `res = y; gemv('N', X, b, dest=res, alpha=-1, beta=1)`.
  - !! NOTE !! This is posponed. We don't do this now.
- Aliasing guardrails: if `LHS` equals any BLAS input symbol (e.g., `A <- A %*% B`), fall back to a temporary and assign.

### 3) Expression normalization for multi‑op expressions

- Lower complex expressions into a sequence of statements; BLAS/LAPACK calls cannot appear “inline” in arithmetic expressions.
- Strategy:
  - Build a tiny IR (nodes: GEMM, GEMV, DOT, LAPACK op, Elementwise, Constant, Var, Assign).
  - Top‑down pass threads an optional `dest` hint to children; children that support destinations consume it and return a symbol; others return a temp.
  - Left‑to‑right evaluation order to match R’s semantics. No reassociation unless proven safe.
- Examples:
  - `x %*% x + 1` → `t0 = gemm(x, x)`; `t0 = t0 + 1` (or `dest=y; gemm(..., y); y = y + 1`).
  - `x %*% x %*% x` → `t0 = gemm(x, x)`; `t1 = gemm(t0, x)`; result is `t1`. Heuristics to choose an order are optional later; default to R’s grouping.

### 4) Minimal‑copy destructive calls (LAPACK)

- `solve(A, b)`:
  - If the result binds to `coef`, generate: `coef = b; Awrk = A; call dgesv(Awrk, coef, ipiv, info)`.
  - Preserve `A` for later uses by copying once (into `Awrk`).
- `chol(A)` and `chol2inv(U)`:
  - `U = A; call dpotrf('U', U, info)` then zero lower triangle (optional for readability).
  - `U` in place for `chol2inv`: `call dpotri('U', U, info)`; symmetrize (`U(i,j)=U(j,i)` for `i>j)`. Bind `XtX_inv` to `U` rather than copying.
- Error handling: always capture `info` and gate printing/diagnostics behind a package option (e.g., `quickr.debug_lapack`). Do not raise runtime errors until string/condition support is in place.

### 5) Hoisting and operand preparation

- BLAS/LAPACK args must be named, contiguous arrays. When an operand is not a simple symbol of correct type:
  - Cast once to double and hoist into a temp (e.g., `real(x, kind=c_double)` → `tmp = ...`).
  - Hoist slices/subsets (non‑contiguous) into contiguous temps before passing to BLAS/LAPACK.
- Only hoist when needed:
  - Operand isn’t a simple symbol; or
  - Callee is destructive; or
  - Operand will be reused multiple times; or
  - Aliasing or flags cannot express the operation.

### 6) Compile‑time shape checks + safe LDx

- Dims from declared types propagate through nodes; perform compile‑time compat checks when possible.
- Compute leading dimensions from pre‑transpose shapes: `lda = rows(op(A))` (`lda = m` when `op(A)='N'`).
- When dims are symbolic and cannot be proven, optionally emit a small runtime guard (friendly error) before calling the routine.

### 7) Multiple return values

- Strategy: keep Fortran subroutines with multiple intent(out) locals; at the R interface, only pack lists at the final function return.
- Handlers for multi‑result routines (e.g., SVD, eigen) allocate local arrays and return them via `list(U=..., d=..., V=...)` when the R function’s last expression is `list(...)`.
- No general in‑function list manipulation. Derived types are not required; simple locals + final packing is sufficient and robust.

## Subroutine Mappings (baseline)

- `%*%` (matrix×matrix): `dgemm('N','N', ...)` (vector cases via `dgemv('N'/'T')`, vector×vector via `dot_product`).
- `crossprod(X)` (t(X)X): start with `dgemm('T','N', X, X, ...)`; consider `dsyrk` later to exploit symmetry.
- `tcrossprod(X)` (XX^T): `dgemm('N','T', X, X, ...)`.
- `t(X)`: `transpose(X)` intrinsic; prefer transpose flags in BLAS to avoid materializing (`transpose()` used only when needed as a value).
- `solve(A, b)`: `dgesv` (or `dgetrf` + `dgetrs`), destructive; plan minimal copies as above.
- `chol(A)`: `dpotrf('U', ...)`; `chol2inv(U)`: `dpotri('U', ...)` + symmetrize.
- `colSums/rowSums`, `colMeans/rowMeans`: loops or `sum(x, dim=)`; optionally gemv with a ones vector.
- `diag(x)`: extract with a loop; diagonal form via zeros + set diagonal; `diag(k)` identity.
- Elementwise operations like `+`, `-`, `*` and `/` is already taken care of. 

## Handler API and “target” handshake

- Assignment handler can provide a “destination hint” to exactly one child call expression that can produce a single result. Only routines supporting destination (BLAS) may consume it.
- Safer variant of the “target()” idea:
  - The parent constructs a dest symbol and an opaque token; passes both down.
  - A child may call `claim_dest(token)` exactly once to consume it and is then responsible for writing into that dest.
  - If the child does not claim the dest, the parent emits a regular assignment after receiving the child’s result.
  - Prevent misuse by scoping tokens to the immediate child only (no grandchild can claim it).
- For LAPACK (destructive), do not use destination hints on inputs; instead, copy to a work buffer and, when the destination is a RHS (e.g., `coef` from `b`), allow “destination as RHS” patterns (`dest = b; call dgesv(Awrk, dest, ...)`).

## What to do with multi‑op expressions

- Always legal: split into hoisted statements. No need to error.
- Preferred: exploit `alpha/beta` to fuse simple patterns into one BLAS call plus optional elementwise ops.
- Chained products: sequence of GEMMs/GEMVs with temps; no reordering in v1.

## Copy‑on‑modify and aliasing policy

- Inputs (formal args) are considered immutable by default. Mark an input as modified only if codegen explicitly writes into it.
- If LHS aliases any BLAS input, do not write in place; allocate a temp.
- For destructive LAPACK ops, copy only operands that are used later.

## NA/NaN and numerics

- NA unsupported in inputs; numeric NaN/Inf propagate per IEEE from BLAS/LAPACK. Do not add NA scans.
- Deterministic tests: avoid relying on exact LAPACK pivoting choices unless constrained; test against base R numerics with tolerances.

## Testing strategy

- Snapshot tests of generated Fortran for key idioms: `%*%`, crossprod/tcrossprod, `solve`, `chol`/`chol2inv`, accumulation patterns.
- Numeric parity vs base R on randomized shapes; include dim mismatch errors and lapack `info` print gating.
- Specific tests for “no extra temp” cases: `LHS <- crossprod(X)`, `res <- y - X %*% b`, `Y <- A %*% B + C`.

## Implementation Plan (incremental)

1) Add destination‑aware BLAS helpers with alias checks (`gemm/gemv` + `alpha/beta`).
2) Thread destination hints from `<-` to a single child BLAS call when RHS is just one call; otherwise skip.
3) Implement `%*%`, `crossprod`, `tcrossprod`, `t` using the new helpers.
4) Add minimal‑copy LAPACK handlers: `solve`, `chol`, `chol2inv` with in‑place on work buffers.
5) Introduce a tiny IR with a pass that hoists calls and applies simple accumulations; keep it small and local.
6) Add runtime guards for non‑conformable dims when compile‑time proof unavailable (friendly R errors).
7) Expand tests and docs; later consider `syrk`, `trsm/trsv`, and values‑only `svd/eigen`.

## Risks and mitigations

- Incorrect alias handling → conservative checks: if in doubt, allocate a temp.
- Over‑fusing expressions → keep fusions to alpha/beta accumulation only in v1.
- LAPACK diagnostics → gated prints now; later wire proper R errors with parity to base R text.
- Slices/subsets → always hoist to contiguous temps before BLAS/LAPACK.

## Summary

This plan makes BLAS/LAPACK calls destination‑aware, reduces temporaries by construction (not as a brittle post‑pass), and cleanly lowers multi‑op expressions into ordered statements. It preserves R semantics, prepares for future fusions, and keeps the implementation modular and testable.

