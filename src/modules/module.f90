module class_module
  use class_solver
  use class_trigger_bundle

  private

  type, public :: module
     class(solver), pointer :: solver
     type(trigger_bundle), pointer :: trigger_bundle
     character(len=30) :: name
   contains
     procedure :: start
     procedure :: stop
     procedure :: step
  end type module

contains

  subroutine start( this )
    class(module) :: this
  end subroutine start

  subroutine stop( this )
    class(module) :: this
  end subroutine stop

  subroutine step( this )
    class(module):: this
  end subroutine step

end module class_module

