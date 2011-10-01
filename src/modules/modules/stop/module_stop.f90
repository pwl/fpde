module class_module_solver_stop

  use class_solver_data
  use class_module

  private

  type, public, extends(module) :: module_solver_stop
   contains
     procedure :: init
     procedure :: step
  end type module_solver_stop

contains

  function step(this) result(r)
    class(module_solver_stop) :: this
    logical :: r

    r = .true.

    this % solver_data % status = solver_stopped

  end function step

  function init(this) result(r)
    class(module_solver_stop) :: this
    logical :: r
    r = this % module % init()

    if( r .and. .not. this % named() ) then
       this % name = "module_solver_stop"
    end if

  end function init



end module class_module_solver_stop
