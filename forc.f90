program main

    use sndfile_wrapper
    use dft, only: fft, ifft
    use iso_c_binding, only: c_ptr, c_f_pointer, c_double_complex

    implicit none
     
    integer :: frames, sample_rate, i
    type(c_ptr) :: cdata
    double precision, pointer :: fdata(:)
    double precision, allocatable :: in(:), out(:), foo(:)
    character(len=*), parameter :: file_name = "l48pNorm.wav"
    
    print *, 'forc'
    
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
