program main

    use sndfile_wrapper
    use dft, only: fft, ifft
    use filter, only: lfilter
    use utils, only: zeros, ones, dp, logspace, linspace
    use iso_c_binding, only: c_ptr, c_f_pointer, c_double_complex
    use butter, only: TRbjEqFilter, kHighPass, InitFilter, CalcFilterCoeffs, process
    use min_phase, only: mps
    
    implicit none
    
    type(TRbjEqFilter) lRGJFilter
     
    integer frames, Fs, i
    type(c_ptr) cdata
    real(dp), pointer :: fdata(:)
    real(dp), allocatable :: fplog(:), minphase(:)
    real, allocatable :: output(:)
    !real(dp) :: pi = 3.1415926535
    character(len=*), parameter :: file_name = 'l48pNorm.wav'
      
    call read_wav(file_name, cdata, frames, Fs)
    
    call c_f_pointer(cdata, fdata, [frames])
    call free_array(cdata)

    allocate(minphase(frames))
    ! making the measured response minumum-phase
    minphase = mps(fdata)

    allocate(fplog(25))
    fplog = [logspace(dble(log10(30.0)), dble(log10(200.0)), 13), &
             logspace(dble(log10(250.0)),dble(log10(20000.0)), 12)];
    
    
    output = real(zeros(frames))
    output(1) = 1.0 !target

    ! Bufferworth filter
    ! make the target output a 30 Hz highpass
    call InitFilter(lRGJFilter, real(Fs), 0)
    call CalcFilterCoeffs(lRGJFilter, kHighPass, 30.0, 0.3, 0.0, .FALSE.)
    call process(lRGJFilter, output, frames)
    output = lRGJFilter%out1
    
    print *

    do i = 1, 10
       write (*,*), i, output(i)
    end do

end program
