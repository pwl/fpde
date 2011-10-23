module class_module_stepper_rank

  use class_module

  use stepper_factory
  use class_ode_stepper
  use class_ode_system
  use class_ode_marcher
  use class_solver_data
  use pretty_print
  use system_functions

  private

  type, public, extends(module) :: module_stepper_rank
     ! initialization parameters
     real, pointer, contiguous :: y0(:) => null()
     character(len=30) :: stepper_id = ""
     type(ode_system), pointer :: sys => null()
     ! end of initialization parameters
     integer :: file_handle = stdout_file
     class(ode_stepper), pointer :: stepper => null()
   contains
     procedure :: init
     ! procedure :: start
     ! procedure :: stop
     procedure :: step
  end type module_stepper_rank

contains

  !>
  !!
  !! @param this
  !!
  !! @return
  !!
  function init(this) result(r)
    class(module_stepper_rank) :: this
    logical :: r
    r = this % module % init()
    if(r) then

       if( .not. this % named() ) then
          this % name = "module_stepper_rank"
       end if

       this % stepper => stepper_new(this%stepper_id)
       if( .not. associated(this % stepper) ) then
          r = .false.
       end if

       if( .not. associated(this%y0) ) then
          r = .false.
       end if

       if( .not. associated(this%sys)) then
          r = .false.
       end if

    end if

  end function init

  function step(this) result(r)
    class(module_stepper_rank) :: this
    logical :: r
    integer :: ny
    real :: t1,h,pnorm
    r = .true.
    ny = size(this%y0)
    h = this%solver_data%dt

    call test_stepper_order(&
         stepper = this%stepper,&
         ode = this%sys,&
         ! dim = ny,&
         y0 = this%y0,&
         t0 = this%solver_data%t,&
         t_delta = 4.*h,&
         h = h,&
         verbose = .false.,&
         pnorm = pnorm)

    open(newunit = this%file_handle,&
         file    = "rank.dat",  &
         form    = 'formatted', &
         action  = 'write', &
         position = 'append',&
         ! access = 'direct', &
         recl    = 10000, &
         status  = 'old')

    write(this%file_handle,*) &
         this%solver_data%t, pnorm

    close(this%file_handle)

  end function step

  !>
  !!
  !! @param stepper
  !! @param ode
  !! @param dim
  !! @param y0
  !! @param t0
  !! @param t1
  !! @param h
  !! @param verbose
  !! @param pnorm
  !!
  !! @return
  !!
  subroutine test_stepper_order(stepper,ode,y0,t0,t_delta,h,verbose,pnorm)

   use ieee_arithmetic

   class(ode_stepper), pointer :: stepper
   type(ode_system) :: ode
   real, intent(in) :: y0(:)
   real, intent(in) :: t0, t_delta, h
   logical          :: verbose
   real             :: pnorm
   ! lokalne zmenne

   type(ode_marcher)           :: marcher
   integer                     :: i, j, nsteps
   real, pointer               :: y1h(:), y2h(:), y4h(:)
   real                        :: t1h, t2h, t4h, h1, h2, h4, num, den, p, t1
   real, allocatable           :: ptable(:)
   integer          :: dim

   dim = size(y0)

   nsteps = nint(t_delta/(h*4.0)) ! ilosc iteracji z krokiem 4*h
   t1 = t0 + t_delta

   ! ! wybieramy stepper
   ! stepper => stepper_new(steppername)

   ! alokacja pamieci
   call stepper % init(dim)
   call marcher % init(dim)
   allocate( y1h(dim) )
   allocate( y2h(dim) )
   allocate( y4h(dim) )
   allocate( ptable(nsteps) )

   ! skopiowanie danych poczatkowych
   y1h = y0
   y2h = y0
   y4h = y0

   t1h = t0
   t2h = t0
   t4h = t0

   h1  = 1.0*h
   h2  = 2.0*h
   h4  = 4.0*h

   if ( verbose ) then
      write(*, '(A)') '# step   time   p'
   end if

   do j=1,nsteps
      ! cztery calkowania z krokiem h
      do i=1,4
         call marcher % apply ( s    = stepper, &
                                sys  = ode,     &
                                t    = t1h,     &
                                t1   = t1,      &
                                h    = h1,       &
                                y    = y1h )
         if ( marcher % status /= 1 ) then
            exit
         end if
      end do

      ! dwa calkowania z krokiem 2*h
      do i=1,2
         call marcher % apply ( s    = stepper, &
                                sys  = ode,     &
                                t    = t2h,     &
                                t1   = t1,      &
                                h    = h2,      &
                                y    = y2h )
         if ( marcher % status /= 1 ) then
            exit
         end if
      end do

      ! na koniec jedno calkowanie z krokiem 4*h
         call marcher % apply ( s    = stepper, &
                                sys  = ode,     &
                                t    = t4h,     &
                                t1   = t1,      &
                                h    = h4,      &
                                y    = y4h )
         if ( marcher % status /= 1 ) then
            exit
         end if

         num = norm2(y4h - y1h)
         den = norm2(y2h - y1h)

         p  = log10(num/den)/log10(2.0)

         if ( .not. ieee_is_finite(p) ) p = 0.0

         ptable(j) = p

         if ( verbose ) then
            write(*,10) j, t1h, p
10          FORMAT (I10,e17.10,e17.10)
         end if

   end do

   pnorm = norm2(ptable)/sqrt(real(nsteps))

   call marcher % free()
   deallocate( y1h )
   deallocate( y2h )
   deallocate( y4h )
   deallocate( ptable )

 end subroutine test_stepper_order

end module class_module_stepper_rank
