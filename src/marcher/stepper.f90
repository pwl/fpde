! General stepper class
module class_stepper
   
   use ode_system_module
   
   private

   type, public :: ode_stepper_type
      integer :: dim
      logical :: can_use_dydt_in
      logical :: gives_exact_dydt_out
      logical :: gives_estimated_yerr
      integer :: method_order
      character(len=20) :: name
   contains
   !! procedures
      procedure :: init
      procedure :: apply
      procedure :: reset
      procedure :: free
   end type ode_stepper_type
      
contains
   subroutine init( s, dim )
      class(ode_stepper_type), intent(inout) :: s
      integer :: dim
   end subroutine init
   
   subroutine apply( s, dim, t, h, y, yerr, dydt_in, dydt_out, sys )
      class(ode_stepper_type), intent(inout) :: s
      real, intent(in)  :: dim, t, h
      real, intent(inout) :: y(:), yerr(:)
      real, intent(in)  :: dydt_in(:)
      real, intent(inout) :: dydt_out(:)
      class(ode_system) :: sys
   end subroutine apply

   subroutine reset( s )
      class(ode_stepper_type), intent(inout) :: s
   end subroutine reset

   subroutine free( s )
      class(ode_stepper_type), intent(inout) :: s
   end subroutine free

end module class_stepper


! Runge-Kutta 4th order classical
! without error estimation
module class_stepper_rk4cs
   
   use class_stepper
   use ode_system_module
   
   private

   type, public, extends( ode_stepper_type ) :: ode_stepper_rk4cs
      ! void *state
      real, allocatable :: k(:), k1(:), y0(:), ytmp(:)
   contains
      procedure :: init
      procedure :: apply
      procedure :: reset
      procedure :: free
   end type ode_stepper_rk4cs

contains

   subroutine init(s, dim)
      class(ode_stepper_rk4cs), intent(inout) :: s
      integer :: dim
      
      s % dim = dim
      s % can_use_dydt_in = .true.
      s % gives_exact_dydt_out = .true.
      s % gives_estimated_yerr = .false.
      s % method_order = 4
      s % name = "rk4cs"
      
      ! allocate workspace vectors
      allocate( s % k( dim ) )
      allocate( s % k1( dim ) )
      allocate( s % y0( dim ) )
      allocate( s % ytmp( dim ) )
   end subroutine init

   ! @todo now this function do nothing
   subroutine apply( s, dim, t, h, y, yerr, dydt_in, dydt_out, sys )
      class(ode_stepper_rk4cs), intent(inout) :: s
      real, intent(in)  :: dim, t, h
      real, intent(inout) :: y(:), yerr(:)
      real, intent(in)  :: dydt_in(:)
      real, intent(inout) :: dydt_out(:)
      class(ode_system)  :: sys
   end subroutine apply

   subroutine reset( s )
      class(ode_stepper_rk4cs), intent(inout) :: s
      
      s % k = 0.0
      s % k1 = 0.0
      s % y0 = 0.0
      s % ytmp = 0.0
   end subroutine reset

   subroutine free( s )
      class(ode_stepper_rk4cs), intent(inout) :: s
      
      deallocate( s % k )
      deallocate( s % k1 )
      deallocate( s % y0 )
      deallocate( s % ytmp )
   end subroutine free

end module class_stepper_rk4cs
