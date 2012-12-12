module utils

    implicit none
    integer, parameter:: dp=kind(0.d0) ! double precision
    
    private
    public zeros, ones, dp
    
    contains
    
    function zeros(n) result(zero)
      integer, intent(in) :: n
      real(kind(0.d0)), allocatable :: zero(:)
      allocate(zero(n))
      zero = 0.0
    end function zeros
    
    function ones(n) result(one)
      integer, intent(in) :: n
      real(kind(0.d0)), allocatable :: one(:)
      allocate(one(n))
      one = 1.0
    end function ones

end module