! General ode_step_control class
module class_ode_step_control

   use class_ode_stepper

   private

   type, public :: ode_step_control
      real :: eps_abs, eps_rel
      real :: a_y, a_dydt
      character(len=20) :: name ! @todo length of this variable
      integer :: status

   contains
      procedure :: init
      procedure :: apply
      procedure :: free
   end type ode_step_control
   
   
contains
   
   subroutine init ( c, eps_abs, eps_rel, a_y, a_dydt )
      class(ode_step_control), intent(inout) :: c
      real, intent(in) :: eps_abs, eps_rel
      real, intent(in) :: a_y, a_dydt
   end subroutine init

   subroutine apply( c, s, y, yerr, dydt, h)
      class(ode_step_control), intent(inout) :: c
      class(ode_stepper), intent(in) :: s
      real, intent(in) :: y(:)
      real, intent(in) :: yerr(:)
      real, intent(in) :: dydt(:)
      real, intent(inout) :: h
   end subroutine apply

   subroutine free( c )
      class(ode_step_control), intent(inout) :: c
   end subroutine free

end module class_ode_step_control







! Szczegolne typy step controllerow
! standard
module class_ode_step_control_standard

   use class_ode_stepper
   use class_ode_step_control
   
   private

   type, public, extends( ode_step_control ) :: ode_step_control_standard
   contains
      procedure :: init
      procedure :: apply
   end type ode_step_control_standard

contains
   
   subroutine init ( c, eps_abs, eps_rel, a_y, a_dydt )
      class(ode_step_control_standard), intent(inout) :: c
      real, intent(in) :: eps_abs, eps_rel
      real, intent(in) :: a_y, a_dydt

      ! @todo przed zapisaniem przekazywanych jako argumenty funkcji
      ! wartosci do struktury step controlera nalezy sprawdzic
      ! czy sa one wieksze od: zera i najmniejszej reprezentowalnej
      ! liczby typu real? i wyjsc z programu? czy zapisac domyslne
      ! wartosci w strukturze c??
      c % eps_abs = eps_abs
      c % eps_rel = eps_rel
      c % a_y = a_y
      c % a_dydt = a_dydt
      c % name = "standard"

      c % status = 0 !@todo co zrobic ze statusem step controlera
      ! zastanawiam sie czy jest on potrzebny, narazie domyslnie
      ! ustawiam go na stan ktory oznacza wg umowy ze krok pozostal
      ! nie zmieniony
   end subroutine init

   subroutine apply( c, s, y, yerr, dydt, h)
      class(ode_step_control_standard), intent(inout) :: c
      class(ode_stepper), intent(in) :: s
      real, intent(in) :: y(:)
      real, intent(in) :: yerr(:)
      real, intent(in) :: dydt(:)
      real, intent(inout) :: h

      integer :: i, dim, order
      real, parameter :: min_real=epsilon(1.0) ! smallest positive number added to 1.0 /= 1.0
      real, parameter :: sfactor=0.9 ! safety factor
      real :: h_old, rmax, d0, r

      dim = s % dim
      order = s % method_order

      h_old = h
      rmax = min_real
      
      do i=1,dim
         d0 = c % eps_abs + &
              c % eps_rel * ( c % a_y * abs(y(i)) + c%a_dydt * abs( h_old * dydt(i) ) )
         r = abs(yerr(i))/abs(d0)
         rmax = max(r,rmax)
      end do

      ! d0 = eps_abs + eps_rel * ( a_y * abs(y) + a_dydt * abs( h_old * dydt )
      ! r = abs(yerr)/abs(d0)
      ! rmax = maxval( 

      if ( rmax > 1.1 ) then
         ! Zmniejszamy krok, nie wiecej niz czynnik 5, lecz sfactor 
         ! wicej niz sugeruje skalowanie
         r = sfactor/rmax**(1.0/order)
         if ( r < 0.2 ) then
            r = 0.2
         end if
         h = r * h_old;
         c % status = -1 ! status ujemny oznacza ze krok zostal zmniejszony
         return
         
      else if ( rmax < 0.5 ) then
         ! Zwiekszamy krok, nie wiecej niz czynnik 5
         r = sfactor/rmax**(1.0/(order+1))
         if ( r > 5.0 ) then
            r = 5.0
         else if ( r < 1.0 ) then ! sprawdzamy czy sfactor nie spowodowal zmniejszenia kroku
            r = 1.0
         end if
         h = r * h_old;
         c % status = 1 ! status dodatni oznacza ze krok zostal zwiekszony
         return
      
      else 
         ! Krok pozostaje bez zmian
         c % status = 0 ! status rowny zero oznacza ze krok nie zostal zmieniony
      end if
      
   end subroutine apply

end module class_ode_step_control_standard
