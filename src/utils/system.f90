module system_functions

  public

contains

  !> Creates directory structure so that a file with a path file_name
  !! could be created
  !!
  !! @param file_name
  !!
  !! @return
  !!
  ! @bug assuming len=2000
  subroutine new_directory(file_name)
    character(len=*) :: file_name
    character(len=2000) :: cmd = ""

    write(cmd, *) &
         "a=", &
         trim(adjustl(file_name)), &
         '; mkdir -vp ${a%/*}'

    call system(cmd)

  end subroutine new_directory


end module system_functions
