# Runtime helpers for non-intrinsic functionality

# Map of runtime unit identifiers to Fortran internal procedures.
# These are designed to be included in a subroutine's CONTAINS block.
quickr_runtime_sources <- local({
  # insertion sort (stable) for real(c_double)
  qkr_sort_d <- glue::trim('
    pure function qkr_sort_d(x) result(y)
      use iso_c_binding, only: c_double
      real(c_double), intent(in) :: x(:)
      real(c_double), allocatable :: y(:)
      integer :: n, i, j
      real(c_double) :: key
      n = size(x)
      allocate(y(n))
      y = x
      do i = 2, n
        key = y(i)
        j = i - 1
        do while (j >= 1 .and. y(j) > key)
          y(j+1) = y(j)
          j = j - 1
        end do
        y(j+1) = key
      end do
    end function qkr_sort_d
  ')

  qkr_sort_d_desc <- glue::trim('
    pure function qkr_sort_d_desc(x) result(y)
      use iso_c_binding, only: c_double
      real(c_double), intent(in) :: x(:)
      real(c_double), allocatable :: y(:)
      integer :: n, i, j
      real(c_double) :: key
      n = size(x)
      allocate(y(n))
      y = x
      do i = 2, n
        key = y(i)
        j = i - 1
        do while (j >= 1 .and. y(j) < key)
          y(j+1) = y(j)
          j = j - 1
        end do
        y(j+1) = key
      end do
    end function qkr_sort_d_desc
  ')

  # insertion sort for integer(c_int)
  qkr_sort_i <- glue::trim('
    pure function qkr_sort_i(x) result(y)
      use iso_c_binding, only: c_int
      integer(c_int), intent(in) :: x(:)
      integer(c_int), allocatable :: y(:)
      integer :: n, i, j, key
      n = size(x)
      allocate(y(n))
      y = x
      do i = 2, n
        key = y(i)
        j = i - 1
        do while (j >= 1 .and. y(j) > key)
          y(j+1) = y(j)
          j = j - 1
        end do
        y(j+1) = key
      end do
    end function qkr_sort_i
  ')

  qkr_sort_i_desc <- glue::trim('
    pure function qkr_sort_i_desc(x) result(y)
      use iso_c_binding, only: c_int
      integer(c_int), intent(in) :: x(:)
      integer(c_int), allocatable :: y(:)
      integer :: n, i, j, key
      n = size(x)
      allocate(y(n))
      y = x
      do i = 2, n
        key = y(i)
        j = i - 1
        do while (j >= 1 .and. y(j) < key)
          y(j+1) = y(j)
          j = j - 1
        end do
        y(j+1) = key
      end do
    end function qkr_sort_i_desc
  ')

  sources <- list(
    sort_d = qkr_sort_d,
    sort_d_desc = qkr_sort_d_desc,
    sort_i = qkr_sort_i,
    sort_i_desc = qkr_sort_i_desc
  )

  function(id = NULL) {
    if (is.null(id)) return(sources)
    sources[[id]] %||% stop("Unknown runtime unit: ", id)
  }
})


# Attach and query runtime dependencies on a scope
quickr_require_runtime <- function(scope, id) {
  req <- attr(scope, "runtime_deps", TRUE)
  if (is.null(req)) req <- character()
  if (!id %in% req) {
    attr(scope, "runtime_deps") <- c(req, id)
  }
  invisible(scope)
}

quickr_get_runtime_deps <- function(scope) {
  unique(attr(scope, "runtime_deps", TRUE))
}

# Record that a given subroutine will use procedures from the quickr_runtime module.
quickr_require_runtime_module_symbol <- function(scope, symbol) {
  syms <- attr(scope, "runtime_module_symbols", TRUE)
  if (is.null(syms)) syms <- character()
  attr(scope, "runtime_module_symbols") <- unique(c(syms, symbol))
  invisible(scope)
}

quickr_get_runtime_module_symbols <- function(scope) {
  unique(attr(scope, "runtime_module_symbols", TRUE))
}

# Map helper ids to their public procedure names by parsing the source
quickr_runtime_symbol_for_id <- function(id) {
  src <- quickr_runtime_sources(id)
  # Find first "function <name>(" occurrence (case-insensitive)
  m <- regexec("(?i)function\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*\\(", src, perl = TRUE)
  regmatches(src, m)[[1]][2] %||% stop("Could not infer symbol name for id: ", id)
}

quickr_runtime_symbols_for_ids <- function(ids) {
  unique(vapply(ids, quickr_runtime_symbol_for_id, ""))
}

# Build a Fortran module source string for the given helper ids.
quickr_build_runtime_module <- function(ids, module_name = "quickr_runtime") {
  ids <- unique(ids)
  if (!length(ids)) return("")
  body <- str_flatten_lines(lapply(ids, quickr_runtime_sources))
  glue::as_glue(str_flatten_lines(
    paste0("module ", module_name),
    "  implicit none",
    "contains",
    body,
    paste0("end module ", module_name)
  ))
}
