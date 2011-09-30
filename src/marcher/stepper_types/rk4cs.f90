! Runge-Kutta 4th order classical
! without error estimation
module class_ode_stepper_rk4cs

   use class_ode_stepper
   use class_ode_system

   private

   type, public, extends( ode_stepper ) :: ode_stepper_rk4cs
      ! @todo workspace nie wszystkie ponizsze wektory sa niezbedne
      ! dla steppera ze stalym krokiem
      real, pointer, contiguous :: k(:), k1(:), y0(:), ytmp(:)
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
      s % status = 1

      ! allocate workspace vectors
      allocate( s % k( dim ) )
      allocate( s % k1( dim ) )
      allocate( s % y0( dim ) )
      allocate( s % ytmp( dim ) )
   end subroutine init

   subroutine apply( s, dim, t, h, y, yerr, dydt_in, dydt_out, sys, status )
      class(ode_stepper_rk4cs), intent(inout) :: s
      integer, intent(in) :: dim
      real, intent(in)  :: t, h
      real, pointer, intent(inout) :: y(:), yerr(:)
      real, pointer, intent(in)  :: dydt_in(:)
      real, pointer, intent(inout) :: dydt_out(:)
      class(ode_system)  :: sys

      integer, optional :: status

      ! Wykonujemy kopie wektora y na wypadek wystapiena bledow
      ! zwracanych przez funkcje sys % fun (prawej strony rownan).
      ! W przypadku ich wystapienia nalezy przywrocic oryginalna
      ! zawartosc wektora y poprzez: y = s % y0, oraz zwrocic
      ! status.
      s % y0 = y

      ! @todo sprawdznie czy zostal podane pochodne wejsciowe
      ! if ( dydt_in /= null() ) then
      !    ! wykorzystujemy juz wyliczone pochodne,
      !    ! kopiujemy je do s%k
      !    s % k = dydt_in
      ! else
      !    ! wyliczamy pochodne
      !    call sys % fun( t, s % y0, s % k, sys % params )
      ! end if

      ! pochodne na wejsciu
      ! @todo narazie zakladam ze jezel s % can_use_dydt_in == .true.
      ! to pochodne musza zostac podane na wejsciu
      if ( s % can_use_dydt_in ) then
         ! wykorzystujemy juz wyliczone pochodne,
         ! kopiujemy je do s%k
         s % k = dydt_in
      else
         ! wyliczamy pochodne
         call sys % fun( t, s % y0, s % k, sys % params, sys % status )
         if ( sys % status /= 1 ) then
            s % status = sys % status
            return
         end if
      end if

      ! krok k1
      y = y + h/6.0*(s % k)
      s % ytmp = s % y0 + 0.5*h*(s % k)

      ! krok k2
      call sys % fun( t+0.5*h, s % ytmp, s % k, sys % params, sys % status )
      if ( sys % status /= 1 ) then
         s % status = sys % status
         return
      end if
      y = y + h/3.0*(s % k)
      s % ytmp = s % y0 + 0.5*h*(s % k)

      ! krok k3
      call sys % fun( t+0.5*h, s % ytmp, s % k, sys % params, sys % status )
      if ( sys % status /= 1 ) then
         s % status = sys % status
         return
      end if
      y = y + h/3.0*(s % k)
      s % ytmp = s % y0 + h*(s % k)

      ! krok k4
      call sys % fun( t+h, s % ytmp, s % k, sys % params, sys % status )
      if ( sys % status /= 1 ) then
         s % status = sys % status
         return
      end if
      y = y + h/6.0*(s % k)

      ! pochodne na wyjsciu


      ! @todo narazie zakladam ze jezel s % gives_exact_dydt_out == .true.
      ! to pochodne musza zostac podane na wejsciu
      if ( s % gives_exact_dydt_out ) then
         ! wyliczamy pochodne
         call sys % fun( t+h, y, dydt_out, sys % params, sys % status )
         if ( sys % status /= 1 ) then
            s % status = sys % status
            return
         end if
      end if

      ! pomyslnie zakonczono subrutyne
      s % status = 1

   end subroutine apply

   subroutine reset( s )
      class(ode_stepper_rk4cs), intent(inout) :: s

      s % k = 0.0
      s % k1 = 0.0
      s % y0 = 0.0
      s % ytmp = 0.0
      s % status = 1
   end subroutine reset

   subroutine free( s )
      class(ode_stepper_rk4cs), intent(inout) :: s

      deallocate( s % k )
      deallocate( s % k1 )
      deallocate( s % y0 )
      deallocate( s % ytmp )
   end subroutine free

end module class_ode_stepper_rk4cs
