! Various signal processing utility functions

module utils

  implicit none
  integer, parameter :: dp=kind(0.d0) ! double precision
  real(dp) :: pi = 4.d0*datan(1.d0)
  
  private
  public dp, zeros, ones, logspace, linspace, postpad, freqpoles
  
  contains
  
  function zeros(n) result(zero)
    integer, intent(in) :: n
    real(dp), allocatable :: zero(:)
    allocate(zero(n))
    zero = 0.0
  end function zeros
  
  function ones(n) result(one)
    integer, intent(in) :: n
    real(dp), allocatable :: one(:)
    allocate(one(n))
    one = 1.0
  end function ones
  
  function dindgen(n) result(r)
    integer :: n, i
    real(dp), dimension(n) :: r
    do i = 1, n
      r(i) = i - 1
    enddo
  end function dindgen
  
  function logspace(a, b, n) result(L)
    real(dp), intent(in) :: a, b
    integer, intent(in) :: n
    real(dp), allocatable :: L (:)
    allocate(L(n))
    L = [dindgen(n-1) / (n - 1.d0) * (b - a) + a, b]
    L = 10**L
  end function logspace
  
  function linspace(a, b, n) result(L)
    real(dp), intent(in) :: a, b
    integer, intent(in) :: n
    real(dp), dimension(n) :: L
    L = [dindgen(n-1) / (n - 1.d0) * (b - a) + a, b]
  end function linspace
  
  ! postpad array with zeros
  subroutine postpad(a, n) 
    real(dp), allocatable :: a(:), pad(:)
    integer, intent(in) :: n
    integer :: a_size
    
    a_size = size(a)
    if(n <= a_size) return
    
    allocate(pad(n-a_size))
    pad = 0.d0
    
    a = reshape(a, [n], PAD = pad)

    !call move_alloc(from=pad, to=a)    
     
  end subroutine postpad
  
  function freqpoles(fr, Fs) result(p)
    
    real(dp), intent(in) :: fr(:)
    integer, intent(in) :: Fs
    real(dp), allocatable :: wp(:), dwp(:)
    complex*16, allocatable :: p(:)
    integer pnum, k
    
    pnum = size(fr)
    allocate(wp(pnum))
    wp = 2*pi*fr/real(Fs) !discrete pole frequencies
    dwp = zeros(pnum)
    
    do k = 2, pnum-1
        dwp(k) = (wp(k+1)-wp(k-1))/2.d0
    enddo
    
    dwp(1) = (wp(2)-wp(1))
    dwp(pnum) = wp(pnum)-wp(pnum-1)
    
    ! computing poles from center frequency wp and bandwidth dwp
    allocate(p(2*size(wp)))
    p = exp(-dwp/2.d0)*exp(cmplx(0.d0,1)*wp)
    p = [p, conjg(p)] !pole pairs one after the other
    
  end function freqpoles

end module
