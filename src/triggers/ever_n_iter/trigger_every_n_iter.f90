module class_trigger_every_n_iter

  use class_solver_data
  use class_trigger

  private

  type, public, extends(trigger) :: trigger_every_n_iter
     ! initialization parameters
     integer :: dn = 1
     ! end of initialization parameters
     integer :: n_iter = 0
   contains
     procedure :: test
     procedure :: init
  end type trigger_every_n_iter

contains

  function init(this) result(r)
    class(trigger_every_n_iter) :: this
    logical :: r
    r = this % trigger % init()

    if( r ) then
       this % name = "trigger_every_n_iter"
    end if

  end function init

  function test(t) result(r)
    class(trigger_every_n_iter), target :: t
    logical :: r
    integer, pointer :: n_iter, dn
    r = .false.

    n_iter => t % n_iter
    dn => t % dn

    if ( mod( n_iter, dn ) == 0 ) then
       r = .true.
    end if

    n_iter = n_iter + 1

  end function test

end module class_trigger_every_n_iter
