! Marcher class
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

   subroutine apply( m, s, sys, t, t1, h, y )
      class(marcher), intent(inout) :: m
      class(ode_stepper_type), intent(inout) :: s
      ! class(*) :: c
      class(ode_system) :: sys
      real, intent(inout) :: t
      real, intent(in) :: t1
      real, intent(inout) :: h
      real, intent(inout) :: y(:)

      logical :: final_step
      integer :: step_status
      real :: h0, t0, dt
      
      h0=h
      t0=t
      dt=t1-t0


      ! Sprawdzanie poprawnosci wymiarow, kierunek calkowania,
      ! calkowania ze zmiennym krokiem ... @todo
      
      ! Wyliczamy pochodne jezeli metoda moze z nich skorzystac
      if ( s % can_use_dydt_in == .true. ) then
         call sys % fun( t, y, m % dydt_in, sys % params )
      end if
      
      
      ! Wykonujemy probny krok

      ! Sprawdzenie czy krok jest ostatnim krokiem 
      ! (w przypadku calkowania do przodu i do tylu)
100   if ( ( dt>=0.0 .and. h0>dt ).or.( dt<0.0 .and. h0<dt ) ) then
         h0=dt
         final_step=.true.
      else
         final_step=.false.
      end if
      
      ! Uruchamiamy stepper z uzyciem dydt_in
      if ( s % can_use_dydt_in == .true. ) then
         ! Kopiujemy wektor y na wypadek wystapienia bledu
         m % y0 = y
         call s % apply( s % dim, t0, h0, y, m % yerr, m % dydt_in, m % dydt_out, sys )
      else
         ! lub bez uzycia dydt_in
         call s % apply( s % dim, t0, h0, y, m % yerr, null(), m % dydt_out, sys )
      end if

      ! Spradzamy czy stepper zwrocil blad @todo
      ! jezeli tak to zwracamy status steppera
      ! i resetujemy krok h

      ! Jezeli stepper nie spowodowal zadnych bledow zwiekszamy 
      ! licznik m%count i zapisujemy krok w m%last_step
      m % count = m % count + 1
      m % last_step = h0

      ! Zwiekszamy aktualny czas
      if ( final_step ) then
         t = t1
      else
         t = t0 + h0
      end if

      ! Jezeli metoda na to pozwala oraz zostal podany step control
      ! uzywamy metody z adaptywnym krokiem
      if ( s % gives_estimated_yerr == .true. ) then
         ! @todo jak sprawdzic czy zostal podany step control
         ! c /= null() powoduje blad kompilacji:
         ! This binary operation is invalid for this data type.
         ! uruchamiamy step control ktory zwraca flage
         ! nalezy ustalic konwencje, np: -1 nalezy zmniejszyc krok,
         ! 1 nalezy zwiekszyc krok, 0 krok czasowy pozostaje taki sam
         ! w przypadku kiedy krok czasowy nalezy zmniejszyc resetujemy
         ! wektor y: y=m%y0, zwiekszamy licznik m%failed_steps
         ! i idziemy do 100
      end if

      ! Zapisujemy sugerowana wielkosc dla nastepnego
      ! kroku czasowego
      h = h0

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
