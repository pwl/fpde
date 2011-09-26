program evaluation_test
  integer :: z = 1

  if( a() ) then

  else if( b() ) then

  else

  end if

contains
  function a()
    logical :: a
    print *, "a"
    a = .true.
  end function a

  function b()
    logical :: b
    print *, "b"
    b = .true.
  end function b


end program evaluation_test
