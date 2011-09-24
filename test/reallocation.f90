program reallocation

  integer, pointer :: a(:)

  allocate(a(2))
  a(1)=1
  a(2)=2

  print *, a

  allocate(a(3))

  print *, a

  deallocate(a)

end program reallocation
