module stepper_factory

  use class_ode_stepper
  use class_ode_stepper_rk4cs
  use class_ode_stepper_rkf45
  use class_ode_stepper_rkm43
  use class_ode_stepper_rkz43

  use class_ode_stepper_rkpd54

  use class_ode_stepper_rkv65
  use class_ode_stepper_rkv65omp
  use class_ode_stepper_rkf78

  use class_ode_stepper_rkimp2

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
    case( "rkz43" )
       allocate( ode_stepper_rkz43 :: s )
    case( "rkv65" )
       allocate( ode_stepper_rkv65 :: s )
    case( "rkv65omp" )
       allocate( ode_stepper_rkv65omp :: s )
    case( "rkf78" )
       allocate( ode_stepper_rkf78 :: s )
    case( "rkimp2" )
       allocate( ode_stepper_rkimp2 :: s )
    case default
       print *, "stepper_new: invalid id"
       nullify( s )
    end select

  end function stepper_new

end module stepper_factory
