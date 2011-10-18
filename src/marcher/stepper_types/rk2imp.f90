!@ Runge-Kutta 2, Gaussian implicit - implicit midpoint
! rule

module class_ode_stepper_rkimp2

   use class_ode_stepper
   use class_ode_system

   private
   type, public, extends( ode_stepper ) :: ode_stepper_rkimp2
      ! workspace
      real, pointer, contiguous :: y1(:), y0(:), ytmp(:)
      real, pointer, contiguous :: yonestep(:), y0orig(:)
      
   contains
      procedure :: init
      procedure :: step
      procedure :: apply
      procedure :: reset
      procedure :: free
   end type ode_stepper_rkimp2

contains
   subroutine init(s, dim)
      class(ode_stepper_rkimp2), intent(inout) :: s
      integer :: dim

      s % dim = dim
      s % can_use_dydt_in = .true.
      s % gives_exact_dydt_out = .true.
      s % gives_estimated_yerr = .true. ! @by step dubbling
      s % method_order = 2
      s % name = "rkimp2"
      s % status = 1

      ! allocate workspace vectors
      allocate( s % y0( dim ) )
      allocate( s % y1( dim ) )
      allocate( s % ytmp( dim ) )
      allocate( s % yonestep( dim ) )
      allocate( s % y0orig( dim ) )
   end subroutine init

   subroutine step( s, dim, t, h, y, sys )
      class(ode_stepper_rkimp2), intent(inout) :: s
      integer, intent(in)                      :: dim
      real, intent(in)                         :: t, h
      real, pointer, intent(out)               :: y(:)
      class(ode_system)                        :: sys
      ! local variables
      integer, parameter :: maxiter = 3 !@ why?
      integer            :: nu

      ! subrutyna wykonujaca adwansujaca rozwiazanie
      ! metoda RK implicit 2'go rzedu z krokiem h

      ! rozwiazanie iteracyjne rownania
      ! Y1 = y0 + h/2*f(t+h/2, Y1)
      do nu=1,maxiter
         s % ytmp = s % y0 + 0.5*h*s % y1

         ! wyliczamy pochodne
         call sys % fun( t + 0.5*h, s % ytmp, s % y1, sys % params, sys % status )
         if ( sys % status /= 1 ) then
            s % status = sys % status
            return
         end if

      end do

      ! przypisanie wynikow
      y = s % y0 + h*s % y1

      ! pomyslnie zakonczono subrutyne
      s % status = 1

   end subroutine step

   subroutine apply( s, dim, t, h, y, yerr, dydt_in, dydt_out, sys, status )
      class(ode_stepper_rkimp2), intent(inout) :: s
      integer, intent(in)                      :: dim
      real, intent(in)                         :: t, h
      real, pointer, intent(inout)             :: y(:), yerr(:)
      real, pointer, intent(in)                :: dydt_in(:)
      real, pointer, intent(inout)             :: dydt_out(:)
      class(ode_system)                        :: sys
      integer, optional                        :: status

      ! Wykonujemy kopie wektora y na wypadek wystapiena bledow
      ! zwracanych przez funkcje sys % fun (prawej strony rownan).
      ! W przypadku ich wystapienia nalezy przywrocic oryginalna
      ! zawartosc wektora y poprzez: y = s % y0, oraz zwrocic
      ! status.
      s % y0 = y
      
      ! Wykonujemy dodatkowa kopie
      s % y0orig = y

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
         ! kopiujemy je do s % y1
         s % y1 = dydt_in
      else
         ! wyliczamy pochodne
         call sys % fun( t, s % y0, s % y1, sys % params, sys % status )
         if ( sys % status /= 1 ) then
            s % status = sys % status
            return
         end if
      end if

      
      ! Adwansujemy rozwiazanie wykonujac jeden krok h
      ! wynik zapisujemy w s % yonestep
      call s % step( dim, t, h, s % yonestep, sys )
      if ( sys % status /= 1 ) then
         s % status = sys % status
         return
      end if
      
      ! Adwansujemy rozwiazanie wykonujac dwa kroki h/2
      ! wynik zapisujemy w y
      !  * pierwszy krok h/2
      call s % step( dim, t, h/2.0, y, sys )
      if ( sys % status /= 1 ) then
         s % status = sys % status
         ! poniewaz wektor y zostal juz nadpisany
         ! musimy go odzyskac z kopi zrobionej na
         ! poczaktu subrutyny
         y = s % y0orig
         return
      end if
      
      s % y0 = y

      ! wyliczamy pochodne w t + h/2
      call sys % fun( t + h/2.0, y, s % y1, sys % params, sys % status )
         if ( sys % status /= 1 ) then
            s % status = sys % status
            ! poniewaz wektor y zostal juz nadpisany
            ! musimy go odzyskac z kopi zrobionej na
            ! poczaktu subrutyny
            y = s % y0orig
            return
         end if

      !  * drugi krok h/2
      call s % step( dim, t + h/2.0, h/2.0, y, sys )
      if ( sys % status /= 1 ) then
         s % status = sys % status
         ! poniewaz wektor y zostal juz nadpisany
         ! musimy go odzyskac z kopi zrobionej na
         ! poczaktu subrutyny
         y = s % y0orig
         return
      end if

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
            y = s % y0orig
            return
         end if
      end if

      ! estymowany blad
      yerr = 4.0*( y - s % yonestep )/3.0

      ! pomyslnie zakonczono subrutyne
      s % status = 1

   end subroutine apply

   subroutine reset( s )
      class(ode_stepper_rkimp2), intent(inout) :: s

      s % y0       = 0.0
      s % y1       = 0.0
      s % ytmp     = 0.0
      s % yonestep = 0.0
      s % y0orig   = 0.0
      s % status   = 1
   end subroutine reset

   subroutine free( s )
      class(ode_stepper_rkimp2), intent(inout) :: s

      deallocate( s % y0 )
      deallocate( s % y1 )
      deallocate( s % ytmp )
      deallocate( s % yonestep )
      deallocate( s % y0orig )
   end subroutine free

end module class_ode_stepper_rkimp2
