program main

    use sndfile_wrapper
    use dft, only: fft, ifft
    use utils, only: zeros, ones, dp
    use iso_c_binding, only: c_ptr, c_f_pointer, c_double_complex

    implicit none
     
    integer :: frames, sample_rate, i
    type(c_ptr) :: cdata
    real(dp), pointer :: fdata(:)
    real(dp), allocatable :: in(:), out(:), foo(:), h1(:), h2(:), h3(:)
    character(len=*), parameter :: file_name = "l48pNorm.wav"

    h1 = ones(2)
    h2 = zeros(3)
    allocate(h3(5))

    h3 = [h1,h2]

    print *, 'forc'
    !print *, mod(7,2)

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

end
