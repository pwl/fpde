! Merson 4(3)
! @todo referencje
module class_ode_stepper_rkm43

   use class_ode_stepper
   use class_ode_system

   private

   type, public, extends( ode_stepper ) :: ode_stepper_rkm43
      ! workspace
      real, pointer, contiguous :: k1(:), k2(:), k3(:), k4(:), k5(:), y0(:), ytmp(:)
      !
      real :: c(5) = (/ 0.0, 1.0/3.0, 1.0/3.0, 1.0/2.0, 1.0 /)

      real :: a2(1) = (/ 1.0/3.0 /)
      real :: a3(2) = (/ 1.0/6.0, 1.0/6.0 /)
      real :: a4(3) = (/ 1.0/8.0, 0.0, 3.0/8.0 /)
      real :: a5(4) = (/ 1.0/2.0, 0.0, -3.0/2.0, 2.0 /)

      real :: b(5) = (/ 1.0/6.0, 0.0, 0.0, 2.0/3.0, 1.0/6.0 /)

      real :: ec(5) = (/ -1.0/15.0, 0.0, 3.0/10.0, -4.0/15.0, 1.0/30.0 /)

   contains
      procedure :: init
      procedure :: apply
      procedure :: reset
      procedure :: free
   end type ode_stepper_rkm43

contains

   subroutine init(s, dim)
      class(ode_stepper_rkm43), intent(inout) :: s
      integer :: dim

      s % dim = dim
      s % can_use_dydt_in = .true.
      s % gives_exact_dydt_out = .true.
      s % gives_estimated_yerr = .true. ! @todo
      s % method_order = 4
      s % name = "rkm43"
      s % status = 1

      ! allocate workspace vectors
      allocate( s % k1( dim ) )
      allocate( s % k2( dim ) )
      allocate( s % k3( dim ) )
      allocate( s % k4( dim ) )
      allocate( s % k5( dim ) )
      allocate( s % y0( dim ) )
      allocate( s % ytmp( dim ) )
   end subroutine init

   subroutine apply( s, dim, t, h, y, yerr, dydt_in, dydt_out, sys, status )
      class(ode_stepper_rkm43), intent(inout) :: s
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


      ! krok k1

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


      ! krok k2
      s % ytmp = y + h*(s % a2(1))*(s % k1)
      call sys % fun( t + (s % c(2))*h, s % ytmp, s % k2, sys % params, sys % status )
      if ( sys % status /= 1 ) then
         s % status = sys % status
         return
      end if

      ! krok k3
      s % ytmp = y + h*( (s % a3(1) * s % k1) + (s % a3(2) * s % k2) )
      call sys % fun( t + (s % c(3))*h, s % ytmp, s % k3, sys % params, sys % status )
      if ( sys % status /= 1 ) then
         s % status = sys % status
         return
      end if

      ! krok k4
      s % ytmp = y + h*( (s % a4(1) * s % k1) + (s % a4(2) * s % k2) + (s % a4(3) * s % k3))
      call sys % fun( t + (s % c(4))*h, s % ytmp, s % k4, sys % params, sys % status )
      if ( sys % status /= 1 ) then
         s % status = sys % status
         return
      end if


      ! krok k5 oraz suma koncowa
      s % ytmp = y + h*( (s % a5(1) * s % k1) + (s % a5(2) * s % k2) + (s % a5(3) * s % k3) + (s % a5(4) * s % k4))
      call sys % fun( t + (s % c(5))*h, s % ytmp, s % k5, sys % params, sys % status )
      if ( sys % status /= 1 ) then
         s % status = sys % status
         return
      end if

      ! suma koncowa
      s % ytmp = s%b(1) * s%k1 + s%b(2) * s%k2 + s%b(3) * s%k3 + &
              s%b(4) * s%k4 + s%b(5) * s%k5
      y = y + h * s % ytmp

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

      ! estymowany blad - roznica pomiedzy p-tym a pb-tym rzedem
      yerr = h * ( s%ec(1) * s%k1 + s%ec(2) * s%k2 + s%ec(3) * s%k3 &
           + s%ec(4) * s%k4 + s%ec(5) * s%k5 )

      ! pomyslnie zakonczono subrutyne
      s % status = 1

   end subroutine apply

   subroutine reset( s )
      class(ode_stepper_rkm43), intent(inout) :: s

      s % k1 = 0.0
      s % k2 = 0.0
      s % k3 = 0.0
      s % k4 = 0.0
      s % k5 = 0.0
      s % y0 = 0.0
      s % ytmp = 0.0
      s % status = 1
   end subroutine reset

   subroutine free( s )
      class(ode_stepper_rkm43), intent(inout) :: s

      deallocate( s % k1 )
      deallocate( s % k2 )
      deallocate( s % k3 )
      deallocate( s % k4 )
      deallocate( s % k5 )
      deallocate( s % y0 )
      deallocate( s % ytmp )
   end subroutine free

end module class_ode_stepper_rkm43
