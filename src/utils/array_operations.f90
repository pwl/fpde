module utils_array_operations

contains

  !> expands the table a(:) to the given 'size' without erasing the
  !> values
  !!
  !! @param a
  !! @param size
  !!
  !! @return
  !!
  subroutine character_array_realloc( a, n )
    character(len=*), pointer :: a(:)
    integer, intent(in) :: n
    character(len=len(a)), pointer :: acpy(:)


    if( .not. associated(a) ) then
       allocate(a(n))
       return
    else if( size(a) >= n ) then
       return
    else
       allocate(acpy(size(a)))
       acpy = a
       deallocate(a)
       allocate(a(n))
       a = acpy
       deallocate(acpy)
    end if

  end subroutine character_array_realloc


end module utils_array_operations
