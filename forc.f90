program main

    use sndfile_wrapper
    use dft, only: fft, ifft
    use utils, only: zeros, ones, dp, logspace, linspace, postpad
    use iso_c_binding, only: c_ptr, c_f_pointer, c_double_complex

    implicit none
     
    integer :: frames, sample_rate, i
    type(c_ptr) :: cdata
    real(dp), pointer :: fdata(:)
    real(dp), allocatable :: in(:), out(:), foo(:), h1(:), h2(:), h3(:)
    character(len=*), parameter :: file_name = "l48pNorm.wav"

    h1 = logspace(dble(log10(30.0)), dble(log10(200.0)),5)   
    h2 = linspace(1.d0, 10.d0, 5) 
    
    call postpad(h1, 6) 
    print *, 'forc'
    print *, h1

    call read_wav(file_name, cdata, frames, sample_rate)

    print *, frames
    print *, sample_rate
    print *

    call c_f_pointer(cdata, fdata, [frames])
    call free_array(cdata)

    allocate(in(frames))
    in = fdata

    out = fft(in)

    do i = 1, 5
        write ( *, * ) i, in(i)
    end do

    print *

    do i = 1, 5
       write (*, *) i, out(i)
    end do

    foo = ifft(out)

    print *

    do i = 1, 5
       write (*,*) i, foo(i)
    end do

end program
