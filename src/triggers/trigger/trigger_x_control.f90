module class_trigger_f_control

  use class_solver_data
  use class_trigger

  private

  type, public, extends(trigger) :: trigger_f_control
     real :: max = -1.
     real :: center = 0.
     ! real, pointer :: f(:,:) => null()
   contains
     ! procedure :: info
     procedure :: test
     procedure :: init
  end type trigger_f_control

contains

  function init(this) result(r)
    class(trigger_f_control) :: this
    logical :: r
    r = this % trigger % init()

    if( r ) then
       this % name = "trigger_f_control"
    end if

    ! if( .not. associated( this % f )) then
    !    this % f => this % solver_data % f
    ! end if

  end function init

  function test(t) result(r)
    class(trigger_f_control), target :: t
    logical :: r
    real, pointer :: f(:,:)
    real :: max, center
    r = .false.
    f => t % solver_data % f
    ! f => t % f
    max = t % max
    center = t % center

    if( max > 0. .and. any( abs(f - center) > max) ) then
       r = .true.
       print *, "DEBUG: trigger_f_control: test = .true."
       print *, max, center
       return
    end if

  end function test

end module class_trigger_f_control

