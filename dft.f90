module fftw3
    use, intrinsic :: iso_c_binding
    include 'fftw3.f03'
end module fftw3

module dft
    
    implicit none
    
    private
    public fft, ifft
    
    contains
    
    function fft(in) result (out)
        
        use fftw3
        
        implicit none
        
        integer :: n
        type(c_ptr) :: plan
        double complex, allocatable :: in(:), out(:)
        
        n = size(in)
        allocate(out(n))
        
        plan = fftw_plan_dft_1d(n, in, out, FFTW_FORWARD, FFTW_ESTIMATE)
        call fftw_execute_dft(plan, in, out)
        call fftw_destroy_plan(plan)
    
    end function fft
    
    function ifft(in) result (out)
        
        use fftw3
        
        implicit none
        
        integer :: n
        type(c_ptr) :: plan
        double complex, allocatable :: in(:), out(:)
        
        n = size(in)
        allocate(out(n))
        
        plan = fftw_plan_dft_1d(n, in, out, FFTW_BACKWARD, FFTW_ESTIMATE)
        call fftw_execute_dft(plan, in, out)
        call fftw_destroy_plan(plan)
        
        out = out / real(n, kind = 8)
    
    end function ifft


end module dft
