!>
!! @file   trigger_monotonic.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Wed Oct  5 10:05:05 2011
!!
!! @brief  @todo if i == 0 check monotonicity of all functions
!!
!!
!!

module class_trigger_non_monotonic

  use class_solver_data
  use class_trigger

  private

  type, public, extends(trigger) :: trigger_non_monotonic
     logical :: decreasing = .false.
     logical :: increasing = .false.
     integer :: i = 0
   contains
     ! procedure :: info
     procedure :: test
     procedure :: init
  end type trigger_non_monotonic

contains

  function init(this) result(r)
    class(trigger_non_monotonic) :: this
    logical :: r
    r = this % trigger % init()

    if( r ) then
       this % name = "trigger_non_monotonic"
    end if

  end function init

  function test(t) result(r)
    class(trigger_non_monotonic), target :: t
    logical :: r
    real, pointer :: f(:,:)
    logical :: decreasing, increasing
    integer :: i, nx
    r = .false.
    f => t % solver_data % f
    i = t % i
    nx = t % solver_data % nx
    decreasing = t % decreasing
    increasing = t % increasing

    if( i > 0 ) then
       if( decreasing &
            .and. any( f(2:,i) > f(1:nx-1,i) ) &
            .or. &
            increasing &
            .and. any( f(1:nx-1,i) > f(2:,i) ) ) then
          r = .true.
          print *, "DEBUG: trigger_non_monotonic: test = .true."
          return
       end if
    end if



  end function test

end module class_trigger_non_monotonic

