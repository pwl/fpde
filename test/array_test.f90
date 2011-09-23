program array_test
  real, pointer :: a(:)
  real, pointer :: b(:,:)
  integer :: n = 2, m = 3
  integer :: i, j

  integer, allocatable, target :: array_2d(:,:)
  integer, pointer :: array_1d(:)
  integer, pointer :: test1(:), test2(:), test3(:), test4(:)
  ! real, allocatable :: test(:)
  ! real, allocatable, target :: test2(:,:)

  ! allocate(a(n*m))

  ! forall( i = 1:n*m ) a(i) = i

  ! ! the following does not work in ifort 12 ang gfortran 4.6
  ! b(1:m, 1:n) => a

  ! print *, b
  ! print *, "size(b,1)=", size(b,1), "but should be ", n
  ! print *, b(1,:)
  ! print *, ""
  ! print *, b(:,1)
  ! print *, ""

  ! allocate( test2(n,m) )
  ! forall(i=1:n,j=1:m) test2(i,j)=i+10*j

  ! ! array_2d => test2
  ! ! print *, "array_2d"
  ! ! print *, array_2d

  ! nullify(array_2d)

  allocate ( array_2d (2,2) )

  forall( i = 1:2, j=1:2 )&
       array_2d (i,j) = i+10*j

  print *, array_2d

  array_1d(1:size(array_2d)) => array_2d

  ! error with '-check all' enabled
  array_1d (3) = 0

  print *, "array_2d is contiguous : ", is_contiguous(array_2d)
  print *, "array_1d is contiguous : ", is_contiguous(array_1d)

  print *, "array_1d: "
  write (*, *) array_1d
  ! size is not determined correctly?
  print *, "size(array_1d): ", size(array_1d), ", should be 4"

  allocate(test1(1), test2(1))

  test1(1) = 1
  test2(1) = 2

  test3 => test1
  test4 => test3

  test3 => test2

  print *, test4, test3

end program array_test
