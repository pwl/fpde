!>
!! @file   inheritance.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Tue Sep 27 13:42:17 2011
!!
!! @brief  this is a @bug of ifort 12.1 Update 6.
!!
!!
!!
module class_a
  type :: a
     real, pointer :: t => null()
     real :: p = 11.
  end type a
end module class_a

module class_a_extended
  use class_a
  type, extends(a) :: a_extended
  end type a_extended
end module class_a_extended

module class_b
  use class_a
  type :: b
     ! @bug: if `type` is changed to `class` the error occurs
     class(a), pointer :: ap
  end type b
end module class_b

program inheritance_test
  use class_a_extended
  use class_b

  type(a_extended), target :: a_extended_type
  class(b), pointer :: b_class
  type(b) :: b_type

  allocate(a_extended_type % t)
  allocate(b_class)

  a_extended_type % t = 888.

  b_class % ap => a_extended_type % a
  b_type % ap => a_extended_type % a

  ! this does not work
  print *, a_extended_type % t
  print *, b_class % ap % t
  print *, b_type % ap % t

  ! this works just fine
  print *, a_extended_type % p
  print *, b_class % ap % p
  print *, b_type % ap % p

  if ( b_class % ap % t /= a_extended_type % t ) then
     print *, "bug persists!"
  end if


end program inheritance_test
