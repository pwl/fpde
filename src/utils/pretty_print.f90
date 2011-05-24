module pretty_print

  ! default format for reals
  character(len=*), parameter :: real_format = "f"

contains


  ! n_format generates strings of the form '(nform)' where n should be
  ! a number > 0 and 'form' is either 'form_opt' or 'real_format' when
  ! form_opt is not present
  character(len=30) function n_format(n,form_opt)
    integer, intent(in) :: n
    character(len=*), intent(in), optional :: form_opt
    character(len=10) :: form
    integer :: i

    n_format = ' '

    if( present(form_opt) ) then
       form = form_opt
    else
       form = real_format
    end if

    write (n_format, '(a,i0,a,a)'), '(', n, form, ')'

  end function n_format

  integer function get_unit()
    integer :: unit
    logical :: isOpen

    integer, parameter :: MIN_UNIT_NUMBER = 10
    integer, parameter :: MAX_UNIT_NUMBER = 99

    do unit = MIN_UNIT_NUMBER, MAX_UNIT_NUMBER
       inquire(unit = unit, opened = isOpen)
       if (.not. isOpen) then
          get_unit = unit
          return
       end if
    end do
  end function get_unit

end module pretty_print
