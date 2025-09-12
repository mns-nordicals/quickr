**Matrix Ops Review**

- **Scope reviewed:** `R/r2f.R:~650–~1500` and `docs/matrix-support.md`.
- **Summary:** The mapping of base R matrix ops to BLAS/LAPACK is broadly correct and well‑structured. The implementation favors explicitness and safety, which explains the number of temporaries and handler length. There are reasonable opportunities to simplify, centralize, and reduce repeated boilerplate without changing semantics.

**What’s Working**
- **BLAS/LAPACK mapping:** `%*%`, `crossprod`, `tcrossprod` → `dgemm`/`dgemv`; `solve` → `dgesv`; `chol` → `dpotrf`; `qr` → `dgeqrf` (thin R); `svd` (values‑only) → `dgesvd`; `t` via `transpose()`; reductions via `sum(..., dim=)`. These are the right choices for dense double.
- **Shape inference:** Output dims computed from inputs consistently; rank combinations handled thoughtfully (vector/matrix cases).
- **Input immutability:** Copies before LAPACK calls to avoid mutating user inputs (`A_`, `F`, etc.). Good call.
- **Type promotion:** `maybe_cast_double()` ensures BLAS sees doubles; logical/integer promotion looks correct.

**Complexity Hotspots**
- **Temporaries everywhere:** Frequent `scope@get_unique_var()` and `ensure_array_var()` calls lead to many hoisted assignments (e.g., `tmp_1 = ...`). Correct, but noisy.
- **Handler verbosity:** Each handler repeats similar patterns: derive `m/n/k`, allocate outputs, compute `lda/ldb/ldc`, hoist call, optional runtime INFO check.
- **LAPACK `info` handling:** Inlined conditional printing per routine adds boilerplate and mixes concerns (numerics vs. diagnostics).

**On LAPACK `info` (keep it, change usage)**
- **Necessary to pass:** LAPACK routines require an integer variable for `INFO` (intent out). We cannot replace that with static checks. Even with perfect shape checks, `INFO` conveys runtime statuses we cannot predict statically (e.g., singular matrix, not PD, convergence failures, illegal argument index computed at runtime).
- **Recommendation:** Keep allocating `info`, but centralize how we react:
  - Add a single helper that emits the `if (info /= 0)` block, optionally gated by a debug flag (e.g., package option or compile‑time setting). 
  - Default to “silent” (capture and ignore) until we wire proper R‑level error propagation in the bridge; enable printing only when `quickr.debug_lapack = TRUE`.
  - Later, route `INFO` to a standardized R error/warning path in the C/Fortran bridge.

**On `Variable` checks vs. `info`**
- **Yes for shapes:** Use `Variable@dims` and rank to do compile‑time shape checks where sizes are known (e.g., mismatch in `m × k` by `k × n`). When dims are symbolic, optionally emit lightweight runtime guards (compare sizes and raise a clean R error) before the BLAS/LAPACK call.
- **No for algorithmic status:** `INFO` still needed for singularity / non‑PD / non‑convergence — these cannot be inferred from shapes alone.

**On `ensure_array_var()` and temporaries**
- **Why it exists:** BLAS expects arrays (by reference). After `maybe_cast_double()` or when the expression isn’t a simple symbol (e.g., `real(x, kind=c_double)`, slices, or computed expressions), relying on implicit compiler‑generated temporaries is fragile for external procedures and obscures leading dimensions. Explicit hoisting to a named variable is safer and predictable.
- **Where it’s likely unnecessary:** When the operand is already a simple variable of the correct type and contiguous layout (your `is_simple_var_ref()` fast path). You already short‑circuit these — good.
- **Possible reduction:** Integrate casting + ensuring into one helper (see “Refactor helpers”) so call sites don’t need both `maybe_cast_double()` and `ensure_array_var()`; return the final variable name and value meta in one step.

**On frequent `scope@get_unique_var("double")`**
- **Why so many here:** BLAS/LAPACK require explicit outputs, work arrays (e.g., `WORK`, `TAU`, `IPIV`), loop indices, and temporary copies to preserve inputs. Matrix ops touch all of those; scalar/elementwise handlers don’t.
- **How to make it feel lighter:** Provide small alloc helpers (e.g., `new_d(n[,m])`, `new_i(n)`), and BLAS wrappers that internally allocate common scratch arrays. This reduces visual noise in handlers.

**Handler Length and “R’s Dynamic Nature”**
- The bulk comes from covering rank combinations and bookkeeping (dims, leading dimensions, temporaries, info). R’s dynamic types add some branching but the main verbosity is mechanical BLAS/LAPACK plumbing.
- A small, typed “matrix IR” (vocabulary) would help: a handful of internal builders like `gemm(NN, A, B)`, `gemv(T, A, x)`, `dot(x, y)`, `potrf(U, A)`, `gesv(A, B)`, etc., which return `(fortran_expr, Variable, hoists)` would let each handler collapse to intent and shape logic while the builders handle plumbing.

**Concrete Simplifications**
- **Refactor helpers (high‑impact, low risk):**
  - `as_double_var(x, scope, hoist)`: combine cast + ensure. If `x` is double symbol → return as‑is. If logical/int or non‑symbol → hoist a double temporary. Returns `(name, Variable)`.
  - `blas_gemm(scope, hoist, A, B, transA='N', transB='N')`: computes `m/n/k`, allocates `C`, sets `lda/ldb/ldc`, emits `dgemm`, returns `(C_name, C_var)`.
  - `blas_gemv(scope, hoist, A, x, trans='N')`: analogous for matrix‑vector.
  - `lapack_check(scope, hoist, info, tag)`: emits the standardized `if (info /= 0)` block only when debug is enabled.
  - `mk_double(scope, dims)`, `mk_int(scope, dims)`: thin wrappers around `get_unique_var()` + `assign()` to cut boilerplate.

- **Centralize dims/lda derivation:** Encapsulate the repeated `m <- dims[[1]]`, `n <- dims[[2]]`, `lda <- m`, `ldb <- ...`, `ldc <- m` patterns inside the BLAS helpers.

- **Runtime conformability asserts (optional, off by default):** When dims aren’t fully known at compile time, you can emit small guards like `if (nA /= mB) call quickr_stop('non-conformable args')` gated by a package option. This complements, but does not replace, LAPACK `INFO`.

- **Unify `info` printing:** Replace per‑handler glue blocks with `lapack_check(...)`. Default to silent; add an option to warn/print for debugging consistency.

- **Use specialized BLAS where natural (later):** `dsyrk` for `crossprod()`/`tcrossprod()` where the result is symmetric; `dtrsm/dtrsv` for triangular solves, etc. Not required for correctness; a future optimization/cleanup.

**Correctness/Performance Notes**
- **WORK sizes:** `svd()` computes a good `lwork`. `qr()` uses a minimal `work <- n`; consider the standard workspace query (`lwork = -1` then allocate optimal) for consistency/perf, though current code is fine functionally.
- **`dot_product` vs `ddot`:** Using the intrinsic is fine and keeps code simple. Switching to `ddot` would require an interface declaration; only worth it if you standardize BLAS function binding elsewhere.
- **Transpose usage:** You already prefer `dgemm('T',...)`/`dgemv('T',...)` over explicit `transpose(...)` where possible — keep that; it avoids extra temporaries and is the BLAS‑idiomatic way.

**Answers to Your Questions**
- **Is `info` necessary?** Yes to pass and capture (LAPACK requires it). It is not only about size constraints; it reports runtime status (illegal arg, singularity, not PD, etc.). You can stop printing it everywhere and centralize/silence it by default.
- **Can `Variable` checks replace `info`?** They can catch static or guardable shape mismatches, but not algorithmic failures. Use both: `Variable` for compile‑time/runtime shape checks; `info` for LAPACK status.
- **Is `ensure_array_var()` necessary?** Often yes, especially after casts or with non‑symbol expressions, to guarantee a named, contiguous array with known type and to compute leading dimensions reliably. You already skip it when the operand is a simple symbol. You can reduce call‑site noise by merging it with casting into a single helper.
- **Why so many `get_unique_var("double")` calls?** Matrix/LAPACK work needs explicit outputs, copies, work arrays, loop indices, and temporaries; unlike scalar ops, you can’t rely on implicit temps. Factor via helpers to reduce visual repetition.
- **Are handlers long due to R’s dynamic nature?** Partly, but mostly due to BLAS/LAPACK plumbing. Introducing a small internal matrix vocabulary/IR will shorten handlers substantially without changing external behavior.

**Proposed Next Steps**
- Add the helper layer (casting+ensure, BLAS wrappers, `lapack_check`, simple alloc helpers) and refactor `%*%`, `crossprod`, `tcrossprod`, `solve`, `chol`, `qr` to use it.
- Gate LAPACK `info` diagnostics behind an option (default silent) and standardize the message format.
- Optionally emit runtime conformability asserts when dims are symbolic, controlled by an option.
- Consider a follow‑up pass to adopt `dsyrk` and friends where symmetry is guaranteed.

Overall, the current design is sound. The main win now is reducing boilerplate by centralizing common patterns and tightening the boundary between numerical calls and diagnostics. This will make the code easier to read and maintain without sacrificing safety.

