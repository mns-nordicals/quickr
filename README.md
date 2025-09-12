# Master Matrix Operations: R ↔ BLAS/LAPACK

This document maps common matrix operations in **R** to their closest **Fortran BLAS/LAPACK** routines.
Mathematical notation uses LaTeX. For routine behavior and argument details, see the references.

## Basic / Elementwise

| Operation | Description | R (base or common) | BLAS/LAPACK (Fortran) | Notes / Recommendations |
|---|---|---|---|---|
| Matrix addition / subtraction | Elementwise combine: $C_{ij}=A_{ij}\pm B_{ij}$. | A + B; A - B | No direct BLAS/LAPACK (use vectorized loops) | Memory-bound; keep data contiguous and avoid unnecessary copies. |
| Hadamard (elementwise) product / scaling | Entrywise product $(A\circ B)_{ij}=A_{ij}B_{ij}$; scalar scaling $\alpha A$. | A * B; a * A; A / a | SCAL (vector scaling); no matrix–matrix elementwise in BLAS | Don’t confuse with matrix product `%*%`/GEMM. |
| Transpose / conjugate transpose | Reorder to $A^T$ or $A^H$. | t(A) | Use transpose flags in BLAS (`TRANS='T'` or `'C'`) within GEMM/GEMV; no standalone transpose | Prefer op(A) flags over materializing a transpose. |

## Products (basic)

| Operation | Description | R (base or common) | BLAS/LAPACK (Fortran) | Notes / Recommendations |
|---|---|---|---|---|
| Dot (inner) product | For vectors: $x^Ty$; projection and orthogonalization building block. | crossprod(x, y); sum(x * y) | DDOT / SDOT; ZDOTC for complex | For norms, prefer BLAS 2-norm routines. |
| Matrix–vector product | Compute $y := A x$ or $y := A^T x$. | A %*% x | DGEMV / SGEMV; SYMV/HEMV for symmetric/Hermitian | Batch multiple RHS as a matrix to promote GEMM usage when possible. |
| Matrix–matrix product | Compute $C := A B$. | A %*% B | DGEMM / SGEMM / ZGEMM | Central Level-3 BLAS; use transpose flags rather than forming transposes. |

## Products (specialized)

| Operation | Description | R (base or common) | BLAS/LAPACK (Fortran) | Notes / Recommendations |
|---|---|---|---|---|
| Crossproduct / Gram $X^TX$ | Form $G := X^TX$ efficiently using symmetry. | crossprod(X) | SYRK / HERK | More efficient than GEMM for symmetric outputs. |
| tcrossprod $XX^T$ | Form $G := XX^T$ efficiently. | tcrossprod(X) | SYRK (with transposed operands) or GEMM | Useful for covariance-like matrices. |
| Outer product / rank-1 update | Update $A := A + \alpha x y^T$. | outer(x, y) (build); in-place update via loops | GER / GERU / GERC | Use GER for in-place updates without forming full outer. |
| Kronecker product | Block matrix $A\otimes B$. | kronecker(A, B) or %x% | No direct BLAS; implement via tiled GEMM | Beware memory growth: $(mn)\times(pq)$. |

## Properties / Diagnostics

| Operation | Description | R (base or common) | BLAS/LAPACK (Fortran) | Notes / Recommendations |
|---|---|---|---|---|
| Trace | Sum of diagonal: $\operatorname{tr}(A)=\sum_i A_{ii}$. | sum(diag(A)) | No direct BLAS/LAPACK | Use a simple loop over the diagonal; stable and $O(n)$. |
| Matrix norms | Size measures: $\|A\|_F$, $\|A\|_1$, $\|A\|_\infty$, spectral $\|A\|_2$. | norm(A, type='F'|'1'|'I'|'2') | DLANGE / DLANSY / ZLANGE etc. | 2-norm requires SVD/eigenpower; Fro/1/Inf are cheap. |
| Determinant / log-det | Product of eigenvalues; $\log\det(A)$ for stability. | determinant(A, logarithm=TRUE); det(A) | Via LU: DGETRF (+ sign); SPD via Cholesky (DPOTRF) | Prefer log-det to avoid overflow/underflow. |
| Rank / null space / condition number | Rank (dimension of image); null space basis; condition number $\kappa$. | qr(A)$rank; svd(A); kappa(A) | DGELSY (RRQR), DGELSD/DGESVD (SVD), GECON/POCON | Use SVD for robust rank/null space; QRCP faster when adequate. |

## Linear systems

| Operation | Description | R (base or common) | BLAS/LAPACK (Fortran) | Notes / Recommendations |
|---|---|---|---|---|
| General solve $Ax=b$ | Solve for $x$ when $A$ is square/nonsingular. | solve(A, b) | DGESV (GETRF + GETRS) | If multiple RHS, pass as matrix B to reuse factorization. |
| SPD solve via Cholesky | If $A$ SPD, use $A=LL^T$ or $R^TR$. | chol(A); backsolve/forwardsolve | DPOSV / DPOTRF + DPOTRS | More stable & faster than LU for SPD. |
| Triangular solves | Solve $Lx=b$ or $Ux=b$. | forwardsolve(L,b); backsolve(U,b) | DTRSV (vector RHS), DTRSM (matrix RHS) | Core kernel inside many algorithms. |
| Banded systems | Exploit band structure to reduce cost. | Matrix package (banded types) | DGBSV (general band), DPBSV (SPD band), etc. | Store in banded format to get speedups. |

## Factorizations

| Operation | Description | R (base or common) | BLAS/LAPACK (Fortran) | Notes / Recommendations |
|---|---|---|---|---|
| LU factorization | Permutation-LU: $PA=LU$. | Matrix::lu(A) (package) / base uses LAPACK | DGETRF (LU), DGETRS (solve), DGETRI (inverse) | Pivoted by default; reuse LU for repeated solves. |
| QR (with/without pivoting) | $A=QR$; with pivoting for rank-revealing. | qr(A); qr.Q(); qr.R() | DGEQRF (QR), DORGQR/ORMQR; GEQP3 (QR with pivoting) | QRCP helps detect rank deficiency; stable LS. |
| Cholesky | $A=R^TR$ (or $LL^T$) for SPD. | chol(A) | DPOTRF (factor), DPOTRS (solve), DPOTRI (inverse) | Use pivoted Cholesky for near-semi-definite. |
| Eigen (symmetric / general) | Eigen decomposition of $A$; symmetric faster and real. | eigen(A, symmetric=TRUE/FALSE) | DSYEV/DSYEVD/DSYEVR (sym); DGEEV (general) | SYEVD/SYEVR are more robust/efficient than classic SYEV. |
| SVD | $A=U\Sigma V^T$. | svd(A) / La.svd(A) | DGESVD (classic) / DGESDD (divide-and-conquer) | GESDD often faster on large problems. |
| Real Schur | Real Schur: $A=Q T Q^T$ with (quasi-)triangular $T$. | Schur() (via packages like pracma) | DGEES / DGEESX (Schur), TREXC (reorder) | Foundation for stable functions of matrices. |

## Least squares

| Operation | Description | R (base or common) | BLAS/LAPACK (Fortran) | Notes / Recommendations |
|---|---|---|---|---|
| Overdetermined LS (min ||Ax-b||_2) | Solve $\min_x \|Ax-b\|_2$. | lm.fit(X,y); qr.solve(X,y); solve(qr(X), y) | DGELS (driver QR), DGELSY (RRQR), DGELSD (SVD) | GELSD most robust (SVD); GELSY faster; DGELS assumes full rank. |
| Rank-deficient LS / pseudoinverse | Handle rank deficiency; Moore–Penrose. | MASS::ginv; svd() | DGELSD/DGESVD | Use tolerance on singular values for numerical rank. |

## Matrix functions

| Operation | Description | R (base or common) | BLAS/LAPACK (Fortran) | Notes / Recommendations |
|---|---|---|---|---|
| Exponential / logarithm / square root | Matrix functions $\exp(A)$, $\log(A)$, $A^{1/2}$. | expm::expm, expm::logm, expm::sqrtm | Not in BLAS/LAPACK; implemented via Schur/SVD/eigen; SLICOT/EXPOKIT | Scaling-and-squaring w/ Padé (expm); conditioning critical. |

## References
- [BLAS](https://www.netlib.org/blas/)
- [LAPACK](https://www.netlib.org/lapack/)
- [LAPACK Users' Guide](https://netlib.org/lapack/lug/)
- [Intel oneMKL BLAS/LAPACK refs](https://www.intel.com/content/www/us/en/docs/onemkl/developer-reference-c/)
- [OpenBLAS](https://www.openblas.net/)
- [R Matrix package](https://cran.r-project.org/package=Matrix)
- [R expm package](https://cran.r-project.org/package=expm)
- [SLICOT](https://github.com/SLICOT)
- [EXPOKIT](https://www.maths.uq.edu.au/expokit/)