! General ode_stepper class
module class_ode_stepper

   use class_ode_system

   private

   type, public :: ode_stepper
      integer :: dim
      logical :: can_use_dydt_in
      logical :: gives_exact_dydt_out
      logical :: gives_estimated_yerr
      integer :: method_order ! min(p,pb) for embedded RK
      character(len=20) :: name ! @todo length of this variable
      integer :: status
   contains
   !! procedures
      procedure :: init
      procedure :: apply
      procedure :: reset
      procedure :: free
   end type ode_stepper

contains
   subroutine init( s, dim )
      class(ode_stepper), intent(inout) :: s
      integer :: dim
   end subroutine init

   subroutine apply( s, dim, t, h, y, yerr, dydt_in, dydt_out, sys, status )
      class(ode_stepper), intent(inout) :: s
      integer, intent(in) :: dim
      real, intent(in)  :: t, h
      real, pointer, intent(inout) :: y(:), yerr(:)
      real, pointer, intent(in)  :: dydt_in(:)
      real, pointer, intent(inout) :: dydt_out(:)
      class(ode_system) :: sys
      integer, optional :: status
   end subroutine apply

   subroutine reset( s )
      ! @todo ustawia wartosc wyszystkich wektorow workspace
      ! na 0.0 oraz ustawia status steppera na 1 (czyli status braku bledu)
      class(ode_stepper), intent(inout) :: s
   end subroutine reset

   subroutine free( s )
      class(ode_stepper), intent(inout) :: s
   end subroutine free

end module class_ode_stepper
