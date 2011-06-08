module stepper_factory

  use class_stepper
  use class_stepper_rk4cs

contains

  function stepper_new(id) result(s)
    class(ode_stepper_type), pointer :: s
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
