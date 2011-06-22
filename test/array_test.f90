program array_test
  real, pointer :: a(:)
  real, pointer :: b(:,:)
  integer :: n = 2, m = 3
  integer :: i, j

  real, pointer :: array_twod(:,:)
  real, pointer, contiguous :: array_oned(:)
  real, allocatable :: test(:)
  real, allocatable, target :: test2(:,:)

  allocate(a(n*m))

  forall( i = 1:n*m ) a(i) = i

  ! the following does not work in ifort 12 ang gfortran 4.6
  b(1:m, 1:n) => a

  print *, b
  print *, size(b,1)
  print *, b(1,:)
  print *, ""
  print *, b(:,1)
  print *, ""

  allocate( test2(n,m) )
  forall(i=1:n,j=1:m) test2(i,j)=i+10*j

  array_twod => test2
  print *, "array_twod"
  print *, array_twod

  ! allocate ( array_twod (2,2) )

  ! forall( i = 1:2, j=1:2 )&
  !      array_twod (i,j) = i*j

  ! array_oned (1:4) => array_twod

  ! write (*, *) array_oned

end program array_test
