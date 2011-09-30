! Runge-Kutta-Fehlberg 4(5)
! @todo referencje
module class_ode_stepper_rkf45

   use class_ode_stepper
   use class_ode_system

   private

   type, public, extends( ode_stepper ) :: ode_stepper_rkf45
      ! workspace
      real, pointer, contiguous :: k1(:), k2(:), k3(:), k4(:), k5(:), k6(:), y0(:), ytmp(:)
      ! RKF constant coefficients. Zero elements left out.
      real :: ah(5) = (/ 1.0/4.0, 3.0/8.0, 12.0/13.0, 1.0, 1.0/2.0 /)
      real :: b3(2) = (/ 3.0/32.0, 9.0/32.0 /)
      real :: b4(3) = (/ 1932.0/2197.0, -7200.0/2197.0, 7296.0/2197.0 /)
      real :: b5(4) = (/ 8341.0/4104.0, -32832.0/4104.0, 29440.0/4104.0, -845.0/4104.0 /)
      real :: b6(5) = (/ -6080.0/20520.0, 41040.0/20520.0, -28352.0/20520.0, 9295.0/20520.0, -5643.0/20520.0 /)

      real :: c1 = 902880.0/7618050.0
      real :: c3 = 3953664.0/7618050.0
      real :: c4 = 3855735.0/7618050.0
      real :: c5 = -1371249.0/7618050.0
      real :: c6 = 277020.0/7618050.0

      ! Error coefficients - pierwsze zero
      ! wyrzucone (roznica konwencji indeksowania elementow tablicy C - FORTRAN)
      real :: ec(6) = (/ 1.0/360.0, 0.0, -128.0/4275.0, -2197.0/75240.0, 1.0/50.0, 2.0/55.0 /)

   contains
      procedure :: init
      procedure :: apply
      procedure :: reset
      procedure :: free
   end type ode_stepper_rkf45

contains

   subroutine init(s, dim)
      class(ode_stepper_rkf45), intent(inout) :: s
      integer :: dim

      s % dim = dim
      s % can_use_dydt_in = .true.
      s % gives_exact_dydt_out = .true.
      s % gives_estimated_yerr = .true. ! @todo
      s % method_order = 5 ! @todo do sprawdzenia razem ze wspolczynnikami b czy bbar
      s % name = "rkf45"
      s % status = 1

      ! allocate workspace vectors
      allocate( s % k1( dim ) )
      allocate( s % k2( dim ) )
      allocate( s % k3( dim ) )
      allocate( s % k4( dim ) )
      allocate( s % k5( dim ) )
      allocate( s % k6( dim ) )
      allocate( s % y0( dim ) )
      allocate( s % ytmp( dim ) )
   end subroutine init

   subroutine apply( s, dim, t, h, y, yerr, dydt_in, dydt_out, sys, status )
      class(ode_stepper_rkf45), intent(inout) :: s
      integer, intent(in) :: dim
      real, intent(in)  :: t, h
      real, pointer, intent(inout) :: y(:), yerr(:)
      real, pointer, intent(in)  :: dydt_in(:)
      real, pointer, intent(inout) :: dydt_out(:)
      class(ode_system)  :: sys

      integer, optional :: status

      integer :: i
      real :: di

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
         ! kopiujemy je do s%k1
         s % k1 = dydt_in
      else
         ! wyliczamy pochodne
         call sys % fun( t, s % y0, s % k1, sys % params, sys % status )
         if ( sys % status /= 1 ) then
            s % status = sys % status
            return
         end if
      end if

      ! krok k1
      s % ytmp = y + s % ah(1)*h*(s % k1)

      ! krok k2
      call sys % fun( t + s % ah(1)*h, s % ytmp, s % k2, sys % params, sys % status )
      if ( sys % status /= 1 ) then
         s % status = sys % status
         return
      end if
      s % ytmp = y + h*( s%b3(1)*(s % k1) + s%b3(2)*(s % k2) )

      ! krok k3
      call sys % fun( t + s%ah(2)*h, s % ytmp, s % k3, sys % params, sys % status )
      if ( sys % status /= 1 ) then
         s % status = sys % status
         return
      end if
      s % ytmp = y + h*( s%b4(1)*(s % k1) + s%b4(2)*(s % k2) + s%b4(3)*(s % k3) )

      ! krok k4
      call sys % fun( t + s%ah(3)*h, s % ytmp, s % k4, sys % params, sys % status )
      if ( sys % status /= 1 ) then
         s % status = sys % status
         return
      end if
      s % ytmp = y + h*( s%b5(1)*(s % k1) + s%b5(2)*(s % k2) + s%b5(3)*(s % k3) &
           + s%b5(4)*(s % k4) )

      ! krok k5
      call sys % fun( t + s%ah(4)*h, s % ytmp, s % k5, sys % params, sys % status )
      if ( sys % status /= 1 ) then
         s % status = sys % status
         return
      end if
      s % ytmp = y + h*( s%b6(1)*(s % k1) + s%b6(2)*(s % k2) + s%b6(3)*(s % k3) &
           + s%b6(4)*(s % k4) + s%b6(5)*(s % k5) )

      ! krok k6 oraz suma koncowa
      call sys % fun( t + s%ah(5)*h, s % ytmp, s % k6, sys % params, sys % status )
      if ( sys % status /= 1 ) then
         s % status = sys % status
         return
      end if

      do i=1,dim
         di = s%c1 * s%k1(i) + s%c3 * s%k3(i) + s%c4 * s%k4(i) &
              + s%c5 * s%k5(i) + s%c6 * s%k6(i)
         y(i) = y(i) + h * di
      end do

      ! pochodne na wyjsciu

      ! @todo narazie zakladam ze jezel s % gives_exact_dydt_out == .true.
      ! to pochodne musza zostac podane na wejsciu
      if ( s % gives_exact_dydt_out ) then
         ! wyliczamy pochodne
         call sys % fun( t+h, y, dydt_out, sys % params, sys % status )
         if ( sys % status /= 1 ) then
            s % status = sys % status

            ! poniewaz wektor y zostal juz nadpisany
            ! musimy go odzyskac z kopi zrobionej na
            ! poczaktu subrutyny
            y = s % y0
            return
         end if
      end if

      ! estymowany blad - roznica pomiedzy 4tym a 5tym rzedem
      yerr = h * ( s%ec(1) * s%k1 + s%ec(3) * s%k3 + s%ec(4) * s%k4 &
           + s%ec(5) * s%k5 + s%ec(6) * s%k6 )

      ! pomyslnie zakonczono subrutyne
      s % status = 1

   end subroutine apply

   subroutine reset( s )
      class(ode_stepper_rkf45), intent(inout) :: s

      s % k1 = 0.0
      s % k2 = 0.0
      s % k3 = 0.0
      s % k4 = 0.0
      s % k5 = 0.0
      s % k6 = 0.0
      s % y0 = 0.0
      s % ytmp = 0.0
      s % status = 1
   end subroutine reset

   subroutine free( s )
      class(ode_stepper_rkf45), intent(inout) :: s

      deallocate( s % k1 )
      deallocate( s % k2 )
      deallocate( s % k3 )
      deallocate( s % k4 )
      deallocate( s % k5 )
      deallocate( s % k6 )
      deallocate( s % y0 )
      deallocate( s % ytmp )
   end subroutine free

end module class_ode_stepper_rkf45
