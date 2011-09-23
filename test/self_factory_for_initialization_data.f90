module self_factory

  type :: d
     character(len=30) :: name = "d"
   contains
     procedure :: init => d_init
  end type d

  type, extends(d) :: d1
     character(len=30) :: d1 = "d1"
  contains
     procedure :: init => d1_init
  end type d1

  type, extends(d) :: d2
     character(len=30) :: d2 = "d2"
  contains
     procedure :: init => d2_init
  end type d2

contains

  function d_new( name ) result(dd)
    class(d), pointer :: dd
    character(*) :: name

    select case(trim(name))
    case( "d1" )
       allocate( d1 :: dd )
    case( "d2" )
       allocate( d2 :: dd )
    case default
       allocate( d :: dd )
    end select
  end function d_new

  subroutine d_init ( dd )
    class(d) :: dd
    dd % name = "d_i"
  end subroutine d_init

  subroutine d1_init ( dd )
    class(d1) :: dd
    dd % name = "d1_i"
  end subroutine d1_init

  subroutine d2_init ( dd )
    class(d2) :: dd
    dd % name = "d2_i"
  end subroutine d2_init

end module self_factory

program self_factory_test
  use self_factory

  class(d), pointer :: dd
  class(d1), pointer :: dd1

  ! dd => d_new("d1")
  ! dd => d1(name="aa")

  ! copying a type of a polymorphic variable from one another works in
  ! ifort 12.1!
  allocate( dd1 )
  allocate( dd, source=dd1 )

  call dd % init

  print *, dd % name

  deallocate( dd1, dd )

end program self_factory_test
