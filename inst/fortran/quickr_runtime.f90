module quickr_runtime
  use iso_c_binding, only: c_double, c_int
  implicit none
contains

  pure function qkr_sort_d(x) result(y)
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

  pure function qkr_sort_d_desc(x) result(y)
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

  pure function qkr_sort_i(x) result(y)
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

  pure function qkr_sort_i_desc(x) result(y)
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

end module quickr_runtime

