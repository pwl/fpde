module stepper_factory

  use class_ode_stepper
  use class_ode_stepper_rk4cs
  use class_ode_stepper_rkpd54
  use class_ode_stepper_rkf45
  use class_ode_stepper_rkm43

contains

  function stepper_new(id) result(s)
    class(ode_stepper), pointer :: s
    character(len=*) :: id

    select case(trim(id))
    case( "rk4cs" )
       allocate( ode_stepper_rk4cs :: s )
    case( "rkpd54" )
       allocate( ode_stepper_rkpd54 :: s )
    case( "rkf45" )
       allocate( ode_stepper_rkf45 :: s )
    case( "rkm43" )
       allocate( ode_stepper_rkm43 :: s )
    case default
       print *, "stepper_new: invalid id"
       nullify( s )
    end select

  end function stepper_new

end module stepper_factory
