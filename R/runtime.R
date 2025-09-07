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

