module class_initializable

  private

  type, public :: initializable
     logical :: initialized = .false.
   contains
     procedure :: init
     ! procedure :: free
     procedure :: try_init
  end type initializable

contains

  function init(this) result(r)
    class(initializable) :: this
    logical :: r
    r = .true.
  end function init

  ! subroutine free(this)
  !   class(initializable) :: this
  ! end subroutine free

  !> returns tries to initialize a class instance
  !!
  !! @param m
  !!
  !! @return
  !!
  function try_init(this) result(r)
    class(initializable) :: this
    logical :: r
    r = .false.

    if( this % initialized ) then
       r = .true.
       ! if module is not in uninitialized state we leave it alone
    else if( this % init() ) then
       ! m % init() is not evaluated if one of the previous conditions
       ! holds. Also calling m % init() sets m % initialized to .true.
       r = .true.
       return
    else
       ! @todo error: module failed to initialize
       r = .false.
       return
    end if

  end function try_init


end module class_initializable
