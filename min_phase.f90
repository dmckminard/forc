module min_phase

  use utils, only: zeros, ones, dp
  use dft, only: fft, ifft
  implicit none

  private
  public mps
  
  contains
  
  function mps(s) result(sm)
  
    real(dp), intent(in) :: s(:)
    real(dp), allocatable :: wn(:), sm(:)
    integer :: n, odd, a, b, c, d
    
    n = size(s)
    odd = mod(n,2)
    
    a = 1
    b = (n+odd)/2-1
    c = 1-mod(n,2)
    d = (n+odd)/2-1 
    
    allocate(wn(a+b+c+d))
    allocate(sm(size(wn)))
    
    wn = [ones(a), 2.0*ones(b), ones(c), zeros(d)]
    sm = ifft(exp(fft(wn*ifft(log(abs(fft(s)))))))
         
  end function mps

end module min_phase