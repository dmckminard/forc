module fftw3
  use, intrinsic :: iso_c_binding
  include 'fftw3.f03'
end module fftw3

module dft
    
  use fftw3
  use utils, only: dp
  implicit none

  private
  public fft, ifft

  contains
  
  subroutine dfft(in, out, kind)
  
    integer(c_int) :: n
    real(dp) :: in(:)
    real(dp), allocatable :: out(:)
    integer(C_FFTW_R2R_KIND), value :: kind
    type(c_ptr) :: plan
    
    n = size(in)
    allocate(out(n))
    
    plan = fftw_plan_r2r_1d(n, in, out, kind, FFTW_ESTIMATE)
    call dfftw_execute(plan)
    call fftw_destroy_plan(plan)
      
  end subroutine dfft
  
  function fft(in) result (out)
    real(dp) :: in(:)
    real(dp), allocatable :: out(:)
    call dfft(in, out, FFTW_FORWARD)
  end function fft

  function ifft(in) result (out)
    real(dp) :: in(:)
    real(dp), allocatable :: out(:)
    call dfft(in, out, FFTW_BACKWARD)
    out = out / real(size(out), kind = 8)
  end function ifft

end module dft
