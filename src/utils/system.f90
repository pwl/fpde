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
  subroutine new_directory(dir_name)
    character(len=*) :: dir_name
    character(len=2000) :: cmd = ""

    write(cmd, *) &
         "a=", &
         trim(adjustl(dir_name)), &
         '; mkdir -vp $a'

    call system(cmd)

  end subroutine new_directory

  !> Creates new file with a corresponding directory structure
  !!
  !! @param file_name
  !!
  !! @return
  !!
  subroutine new_file(file_name)
    character(len=*) :: file_name
    character(len=2000) :: cmd = ""

    write(cmd, *) &
         "a=", &
         trim(adjustl(file_name)), &
         '; mkdir -vp ${a%/*} && touch $a'

    call system(cmd)

  end subroutine new_file


  !> Calls
  !! ln -vfns [file_name] [link_name]
  !!
  !! @param file_name
  !! @param link_name
  !!
  !! @return
  !!
  subroutine symlink(file_name, link_name)
    character(len=*), intent(in) :: file_name, link_name
    character(len=2000) :: cmd = ""

    write(cmd, *) &
         "ln -vfns ", trim(file_name), " ", trim(link_name)

    call system(cmd)

  end subroutine symlink

end module system_functions
