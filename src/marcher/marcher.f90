! General ode_marcher class
module class_ode_marcher

   use class_ode_system
   use class_ode_stepper
   use class_ode_step_control

   private

   type, public ::  ode_marcher
      integer :: dim
      real, pointer, contiguous :: y0(:)
      real, pointer, contiguous :: yerr(:)
      real, pointer, contiguous :: dydt_in(:)
      real, pointer, contiguous :: dydt_out(:)

      integer :: count
      integer :: failed_steps
      real :: last_step
      integer :: status

   contains
      procedure :: init
      procedure :: apply
      procedure :: reset
      procedure :: free
   end type ode_marcher

contains

   subroutine init( m, dim )
      class(ode_marcher), intent(inout) :: m
      integer :: dim

      m % dim = dim
      m % count = 0
      m % failed_steps = 0
      m % last_step = 0.0
      m % status = 1

      ! allocate marcher workspace vectors
      allocate( m % y0( dim ) )
      allocate( m % yerr( dim ) )
      allocate( m % dydt_in( dim ) )
      allocate( m % dydt_out( dim ) )
   end subroutine init

   subroutine apply( m, s, c, sys, t, t1, h, y )
      class(ode_marcher), intent(inout) :: m
      class(ode_stepper), intent(inout) :: s
      class(ode_step_control), optional :: c
      class(ode_system) :: sys
      real, intent(inout) :: t
      real, intent(in) :: t1
      real, intent(inout) :: h
      real, pointer, intent(inout) :: y(:)

      logical :: final_step
      integer :: step_status
      real :: h0, t0, dt, h_old, t_curr, t_next

      ! h0 zmienna na ktorej operujemy, ewentualna zmiane kroku
      ! czyli zmiennej h dokonujemy na koncu subrutyny
      h0=h
      t0=t
      dt=t1-t0

      ! Sprawdzanie poprawnosci wymiarow, kierunek calkowania,
      ! calkowania ze zmiennym krokiem ... @todo

      ! Sprawdzamy zgodnosc wymiarow marchera oraz steppera
      if ( m % dim /= s % dim ) then
         m % status = -1 ! status bledu
         return
      end if

      ! Sprawdzamy zgodnosc kierunku calkowania
      if ( (dt<0.0 .and. h0>0.0) .or. (dt>0.0 .and. h0<0.0) ) then
         m % status = -2 ! status bledu
         return
      end if

      ! Jezeli calkujemy ze zmiennym krokiem czyli stepper
      ! wylicza blad kroku oraz zostala podana metoda kontrolujaca
      ! krok to wykonujemy kopie wejsciowego wektora y do struktury
      ! matchera m % y0
      if ( s % gives_estimated_yerr .and. present( c ) ) then
         m % y0 = y
      end if

      ! Wyliczamy pochodne jezeli metoda moze z nich skorzystac
      if ( s % can_use_dydt_in ) then
         call sys % fun( t, y, m % dydt_in, sys % params, sys % status )
         if ( sys % status /= 1 ) then
            m % status = sys % status
            return
         end if
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
      if ( s % can_use_dydt_in ) then
         ! Kopiujemy wektor y na wypadek wystapienia bledu
         m % y0 = y
         call s % apply( s % dim, t0, h0, y, m % yerr, m % dydt_in, m % dydt_out, sys, s % status )
      else
         ! lub bez uzycia dydt_in
         call s % apply( s % dim, t0, h0, y, m % yerr, null(), m % dydt_out, sys, s % status )
      end if

      ! Sprawdzamy czy stepper wykonal sie poprawnie
      if ( s % status /= 1 ) then
         ! jezeli wystapil blad przekazujemy taki sam
         ! status bledu do statusu marchera aby mozna go
         ! bylo z zewnatrz odczytac
         m % status = s % status
         h = h0 ! zwracamy krok przy jakim pojawil sie blad
         t = t0 ! przywracamy wartosc t podana na wejsciu
         return
      end if

      ! Jezeli stepper nie spowodowal zadnych bledow zwiekszamy
      ! licznik m % count i zapisujemy krok w m % last_step
      m % count = m % count + 1
      m % last_step = h0

      ! Zapisujemy aktualny czas
      if ( final_step ) then
         t = t1
      else
         t = t0 + h0
      end if

      ! Ponizej kod odpowiadajacy za calkowanie ze zmiennym krokiem

      ! Jezeli metoda na to pozwala oraz zostal podany step control
      ! uzywamy metody z adaptywnym krokiem
      if ( s % gives_estimated_yerr .and. present( c ) ) then
         ! present( c ) zwraca .true. jesli zostal podany step control
         h_old = h0 ! zapamietujemy wielkosc kroku
         call c % apply ( s, y, m % yerr, m % dydt_out, h0 )
         ! po wykonaniu apply step control ustawia swoj status
         ! czyli zmienna c % status w zaleznosci czy krok ma
         ! zostac zmieniony badz nie. Przyjeta konwencja:
         ! c % status = 1   zostal zwiekszony
         ! c % status =-1   zostal zmniejszony
         ! c % status = 0   nie zostal zmieniony

         if ( c % status == -1 ) then
            ! Sprawdzamy poprawnosc sugerowanego kroku:
            ! czy h0 zostalo 'naprawde' zmniejszone
            ! oraz czy sugerowane h0 zmieni czas t conajmniej
            ! o jedna ULP

            ! @todo double coerce?
            t_curr = t
            t_next = t+h0

            if ( abs(h0) < abs(h_old) .and. t_next /= t_curr ) then
               ! Krok zostal zmniejszony, anulujemy wykonany krok
               ! i probujemy znow z nowym krokiem h0
               y = m % y0
               m % failed_steps = m % failed_steps + 1
               go to 100
            else
               ! W przeciwnym wypadku trzymamy aktualny krok
               h0 = h_old
            end if
         end if
      end if

      ! Zapisujemy sugerowana wielkosc dla nastepnego
      ! kroku czasowego
      h = h0

   end subroutine apply

   subroutine reset( m )
      class(ode_marcher), intent(inout) :: m

      m % count = 0
      m % failed_steps = 0
      m % last_step = 0.0
   end subroutine reset

   subroutine free( m )
      class(ode_marcher), intent(inout) :: m

      deallocate( m % y0 )
      deallocate( m % yerr )
      deallocate( m % dydt_in )
      deallocate( m % dydt_out )
   end subroutine free

end module class_ode_marcher
