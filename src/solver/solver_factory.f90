module solver_factory

  use class_solver
  use class_solver_simple

contains

  function solver_new(id) result(s)
    class(solver), pointer :: s
    character(len=*) :: id

    select case(trim(id))
    case( "rk4cs" )
       allocate( solver_simple :: s )
    case default
       print *, "solver_new: invalid id"
       nullify( s )
    end select

  end function solver_new

end module solver_factory
