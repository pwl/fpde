module class_marcher
   use ode_system_module
   use class_stepper
   
   private

   type, public ::  marcher
      integer :: dim
      real, allocatable :: y0(:)
      real, allocatable :: yerr(:)
      real, allocatable :: dydt_in(:)
      real, allocatable :: dydt_out(:)

      integer :: count
      integer :: failed_steps
      real :: last_step

      integer :: status ! @todo status of stepper, marcher

   contains
      procedure :: init
      procedure :: apply
      procedure :: reset
      procedure :: free
   end type marcher

contains
   subroutine init( m, dim )
      class(marcher), intent(inout) :: m
      integer :: dim

      m % dim = dim
      m % count = 0
      m % failed_steps = 0
      m % last_step = 0.0

      ! allocate marcher workspace vectors
      allocate( m % y0( dim ) )
      allocate( m % yerr( dim ) )
      allocate( m % dydt_in( dim ) )
      allocate( m % dydt_out( dim ) )
   end subroutine init

   subroutine apply( m, s, c, sys, t, t1, h, y )
      class(marcher), intent(inout) :: m
      class(ode_stepper_type), intent(inout) :: s
      class(*) :: c
      class(ode_system) :: sys
      real, intent(inout) :: t
      real, intent(in) :: t1
      real, intent(inout) :: h
      real, allocatable :: y(:)
   end subroutine apply

   subroutine reset( m )
      class(marcher), intent(inout) :: m

      m % count = 0
      m % failed_steps = 0
      m % last_step = 0.0
   end subroutine reset

   subroutine free( m )
      class(marcher), intent(inout) :: m

      deallocate( m % y0 )
      deallocate( m % yerr )
      deallocate( m % dydt_in )
      deallocate( m % dydt_out )
   end subroutine free

end module class_marcher
