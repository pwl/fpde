module stepper_factory

  use class_ode_stepper
  use class_ode_stepper_rk4cs

contains

  function stepper_new(id) result(s)
    class(ode_stepper), pointer :: s
    character(len=*) :: id

    select case(trim(id))
    case( "rk4cs" )
       allocate( ode_stepper_rk4cs :: s )
    case default
       print *, "stepper_new: invalid id"
       nullify( s )
    end select

  end function stepper_new

end module stepper_factory
