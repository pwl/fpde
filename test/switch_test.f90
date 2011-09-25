program switch_test
  integer :: i = 0

  select case(i)
  case(0)
     i = 1
  case(1)
     i = 0
  end select

  print *, i

end program switch_test
