# deferred-shape locals (self-sized dims) snapshot

    Code
      r2f(f)
    Output
      subroutine f(x, out_, x__dim_1_, x__dim_2_, quickr_err_msg) bind(c)
        use iso_c_binding, only: c_char, c_double, c_int, c_null_char, c_ptrdiff_t
        implicit none
      
        ! manifest start
        ! sizes
        integer(c_int), intent(in), value :: x__dim_1_
        integer(c_int), intent(in), value :: x__dim_2_
      
        ! error
        character(kind=c_char), intent(inout) :: quickr_err_msg(256)
      
        ! args
        real(c_double), intent(in) :: x(x__dim_1_, x__dim_2_)
        real(c_double), intent(out) :: out_
      
        ! locals
        real(c_double), allocatable :: a(:, :)
        ! manifest end
      
      
        if (allocated(a)) then
        if (size(a, 1, kind=c_ptrdiff_t) /= size(x, 1, kind=c_ptrdiff_t)) then
          call quickr_set_error_msg("cannot reassign `a`: assignment must preserve its shape")
          return
        end if
        end if
        if (allocated(a)) then
        if (size(a, 2, kind=c_ptrdiff_t) /= size(x, 2, kind=c_ptrdiff_t)) then
          call quickr_set_error_msg("cannot reassign `a`: assignment must preserve its shape")
          return
        end if
        end if
        a = x
        out_ = 1.0_c_double
      
        contains
          subroutine quickr_set_error_msg(msg)
            character(len=*), intent(in) :: msg
            integer :: i
            integer :: n
            if (quickr_err_msg(1) == c_null_char) then
              n = min(len(msg), 256 - 1)
              quickr_err_msg(1:n) = [(msg(i:i), i = 1, n)]
              quickr_err_msg(n + 1) = c_null_char
            end if
          end subroutine quickr_set_error_msg
      end subroutine
      
      @r: function (x)
        {
            declare(type(x = double(NA, NA)), type(a = double(NA, NA)))
            a <- x
            1
        }
      @c_bridge: #define R_NO_REMAP
        #include <R.h>
        #include <Rinternals.h>
        
        
        extern void f(
          const double* const x__,
          double* const out___,
          const R_len_t x__dim_1_,
          const R_len_t x__dim_2_,
          char* quickr_err_msg);
        
        SEXP f_(SEXP _args) {
          // x
          _args = CDR(_args);
          SEXP x = CAR(_args);
          if (TYPEOF(x) != REALSXP) {
            Rf_error("typeof(x) must be 'double', not '%s'", Rf_type2char(TYPEOF(x)));
          }
          const double* const x__ = REAL(x);
          const int* const x__dim_ = ({
          SEXP dim_ = Rf_getAttrib(x, R_DimSymbol);
          if (Rf_length(dim_) != 2) Rf_error(
            "x must be a 2D-array, but length(dim(x)) is %i",
            (int) Rf_length(dim_));
          INTEGER(dim_);});
          const int x__dim_1_ = x__dim_[0];
          const int x__dim_2_ = x__dim_[1];
        
          const R_xlen_t out___len_ = (1);
          SEXP out_ = PROTECT(Rf_allocVector(REALSXP, out___len_));
          double* out___ = REAL(out_);
        
          char quickr_err_msg[256];
          quickr_err_msg[0] = '\0';
        
        
          f(
            x__,
            out___,
            x__dim_1_,
            x__dim_2_,
            quickr_err_msg);
          if (quickr_err_msg[0] != '\0') {
            Rf_error("%s", quickr_err_msg);
          }
        
          UNPROTECT(1);
          return out_;
        }

