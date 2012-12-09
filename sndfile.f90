module sndfile_wrapper
  
  implicit none
  
  interface
    subroutine read_wav(file_name, buffer, frames, sample_rate) bind(c)
        use iso_c_binding, only: c_char, c_int, c_ptr
        character(c_char), intent(in) :: file_name(*)
        type(c_ptr), intent(out) :: buffer
        integer(c_int), intent(out) :: frames, sample_rate
    end subroutine 
    subroutine free_array(data) bind(c)
        use, intrinsic :: iso_c_binding
        type(c_ptr) :: data
    end subroutine
  end interface

end module
