module filter

  use utils, only: dp, zeros, postpad
  implicit none
  
  private
  public lfilter
  
  contains
  
  function lfilter(a, b, x) result(y)
    
    real(dp), allocatable, intent(in) :: a(:), b(:), x(:)
    real(dp), allocatable :: y(:)
    integer :: N,M,L,MN,lw
    
    N = size(a)
    M = size(b)
    L = size(x)
  
    MN = max(N,M)
    lw = MN - 1
  
    call postpad(b, MN)
    
    y = zeros(L)
    
  end function lfilter
  
end module
