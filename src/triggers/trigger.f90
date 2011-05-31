module class_trigger

  use class_solver

  private

  type, public :: trigger
     class(solver), pointer :: solver
     character(len=300) :: name
   contains
     procedure :: info
     procedure :: start
     procedure :: stop
     procedure :: test
     procedure :: free
  end type trigger

contains

  subroutine info(t)
    class(trigger) :: t
  end subroutine info

  subroutine start(t)
    class(trigger) :: t
  end subroutine start

  subroutine stop(t)
    class(trigger) :: t
  end subroutine stop

  subroutine test(t)
    class(trigger) :: t
  end subroutine test

  subroutine free(t)
    class(trigger) :: t
  end subroutine free



end module class_trigger
