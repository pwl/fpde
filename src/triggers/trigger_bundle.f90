module class_trigger_bundle
  use class_list
  use class_trigger
  use class_solver

  private

  type, public :: trigger_bundle
     class(list), pointer :: triggers
   contains
     procedure :: add
     procedure :: info
     procedure :: init
     procedure :: start
     procedure :: stop
     procedure :: test
  end type trigger_bundle

contains

  subroutine init(this)
    class(trigger_bundle) :: this
    allocate(this%triggers)
  end subroutine init

  subroutine add(this, t)
    class(trigger_bundle) :: this
    class(trigger), pointer :: t
    class(*), pointer :: dummy
    dummy => t
    call this % triggers % add(dummy)
  end subroutine add

  subroutine info(this)
    class(trigger_bundle) :: this
    call this % triggers % map(trigger_info)
  end subroutine info

  subroutine start(this)
    class(trigger_bundle) :: this
    call this % triggers % map(trigger_start)
  end subroutine start

  subroutine stop(this)
    class(trigger_bundle) :: this
    call this % triggers % map(trigger_stop)
  end subroutine stop

  !> Function returning .true. if at least one of the triggers in the
  !> bundle is activated (i.e. trigger%test( ) returns .true. )
  !!
  !! @param this
  !!
  !! @return
  !!
  function test(this) result(r)
    class(trigger_bundle), target :: this
    class(trigger), pointer :: t
    integer :: i
    logical :: r
    r = .false.

    do i = 1, this % triggers % length()
       t => up_to_trigger(this % triggers % element)
       r = r .or. t % test()
    end do


  end function test

  !> Nasty trick used to interpret class(*) as class(trigger)
  !!
  !! @param up
  !!
  !! @return
  !!
  function up_to_trigger( up ) result(r)
    class(*), target :: up
    class(trigger), pointer :: r
    nullify(r)
    select type(up)
    class is (trigger)
       r => up
    end select
  end function up_to_trigger



end module class_trigger_bundle
