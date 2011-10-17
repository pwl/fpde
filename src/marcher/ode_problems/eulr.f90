module class_eulr_oivp

   use class_ode_system
   use class_ode_iv_problem
   
   private
   type, public, extends( ode_iv_problem ) :: eulr_oivp
   contains
      procedure :: init
   end type eulr_oivp
contains
   subroutine init( oivp )
      class(eulr_oivp), intent(inout) :: oivp
      ! local variables
      integer, parameter :: n = 3
      ! dimension of the problem
      oivp % dim = n
      ! short description
      oivp % name = 'EULER'
      oivp % desc = 'Eulers equation of rotation of a rigid body'
      ! initial values
      allocate ( oivp % y0( n ) )
      allocate ( oivp % sol_end( n ) )
      oivp % y0(1) = 1.0
      oivp % y0(2) = 0.0
      oivp % y0(3) = 0.9
      ! ode_system initialization
      call ode_system_init( sys = oivp % sys, fun = rhs, dim = n, params = 0 )

   end subroutine init

   subroutine rhs( t, y, dydt, params, status )
      real, intent(in) :: t
      real, pointer, intent(in) :: y(:)
      real, pointer, intent(out) :: dydt(:)
      class(*) :: params
      integer, optional :: status

      ! fixed equation parameters
      ! principal moments of inertia
      real, parameter :: I1 = 0.5, I2 = 2.0, I3 = 3.0
      ! external force
      real :: fx
      real, parameter :: pi = acos(-1.0)

      if ( t .ge. 3.0*pi .and. t .le. 4.0*pi ) then
         fx = sin(t)*sin(t)/4.0
      else
         fx = 0.0
      end if

      dydt(1)  = (I2-I3)/I1*y(2)*y(3)
      dydt(2)  = (I3-I1)/I2*y(3)*y(1)
      dydt(3)  = (I1-I2)/I3*y(1)*y(2) + fx
   end subroutine rhs

end module class_eulr_oivp
