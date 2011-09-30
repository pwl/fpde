module control_factory

   use class_ode_step_control
   use class_ode_step_control_standard
   use class_ode_step_control_scaled
   use class_ode_step_control_standard_y
   use class_ode_step_control_standard_dydt

contains

   function control_new(id) result(c)
      class(ode_step_control), pointer :: c
      character(len=*) :: id

      select case(trim(id))
      case( "standard" )
         allocate( ode_step_control_standard :: c )
      case( "scaled" )
         allocate( ode_step_control_scaled :: c )
      case( "standard_y" )
         allocate( ode_step_control_standard_y :: c )
      case( "standard_dydt" )
         allocate( ode_step_control_standard_dydt :: c )
      case default
         print *, "control_new: invalid id"
         nullify( c )
      end select

   end function control_new

end module control_factory
