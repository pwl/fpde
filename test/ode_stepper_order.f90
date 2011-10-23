!@ sprawdzamy zbieznosc stepperow

program test_order_all_steppers

   use class_ode_system
   use class_ode_system
   use class_ode_marcher
   use stepper_factory
   use oivp_factory


   class(ode_iv_problem), pointer :: oivp
   class(ode_stepper), pointer :: stepper
   type(ode_system)            :: ode
   ! number of steppers implemented in fpde
   integer, parameter          :: nsteppers=8
   ! number of ode problems implemented in fpde
   integer, parameter          :: nproblems=3
   
   integer                     :: i, j
   real                        :: t0=0.0, t1=10.0
   real                        :: h = 0.001
   real, allocatable           :: y0(:)
   real                        :: pnorm

   character(len=12), dimension(1:nsteppers) :: step_name
   character(len=36), dimension(1:nproblems) :: prob_name


   prob_name = (/ "BRUSSELATOR", "EULER", "AREN" /)

   step_name = (/ "rk4cs ", "rkpd54", "rkf45 ", "rkm43 ", &
        "rkz43 ",  "rkf78 ", "rkv65 ","rkimp2" /)

   write(*, '(A)') '# id   stepper name   order (computed)   order (expected)'

   do j=1,nproblems
      oivp => oivp_new(trim(prob_name(j)))

      call oivp % init()
      
      print *, '# ODE problem: ', trim(oivp % name)

      do i=1,nsteppers
         stepper => stepper_new(trim(step_name(i)))

         call test_stepper_order( stepper,    &
                                  oivp % sys, &
                                  oivp % dim, &
                                  oivp % y0,  &
                                  t0,         &
                                  t1,         &
                                  h/4.0,      &
                                  .false.,    &
                                  pnorm )

         print '(I4,A,A,A,f16.5,A,I16)', i, '   ', step_name(i), &
              '   ', pnorm, '   ', stepper % method_order
         
         call stepper % free()

      end do

      call oivp % free()

   end do

end program test_order_all_steppers


! program test_order_one_stepper

!    use class_ode_system
!    use class_ode_system
!    use class_ode_marcher
!    use stepper_factory
!    use oivp_factory


!    class(ode_iv_problem), pointer :: oivp
!    class(ode_stepper), pointer :: stepper
!    type(ode_system)            :: ode
  
!    integer                     :: i, j
!    real                        :: t0=0.0, t1=30.0
!    real                        :: h = 0.001
!    real, allocatable           :: y0(:)
!    real                        :: pnorm

!    character(len=12) :: step_name
!    character(len=36) :: prob_name


!    prob_name = "EULER"
!    step_name = "rkpd54"

!    oivp => oivp_new(trim(prob_name))

!    call oivp % init()
      
!    print *, '# ODE problem: ', trim(oivp % name)
!    print *, '# ODE stepper: ', trim(step_name)

!    stepper => stepper_new(trim(step_name))

!    call test_stepper_order( stepper,    &
!                             oivp % sys, &
!                             oivp % dim, &
!                             oivp % y0,  &
!                             t0,         &
!                             t1,         &
!                             h/4.0,      &
!                             .true.,    &
!                             pnorm )

!    call stepper % free()
!    call oivp % free()

! end program test_order_one_stepper



subroutine test_stepper_order(stepper,ode,dim,y0,t0,t1,h,verbose,pnorm)

   use ieee_arithmetic
   use class_ode_system
   use class_ode_marcher
   use class_ode_stepper

   class(ode_stepper), pointer :: stepper
   type(ode_system) :: ode
   integer          :: dim
   real             :: y0(dim)
   real             :: t0, t1, h
   logical          :: verbose
   real             :: pnorm
   ! lokalne zmenne
   
   type(ode_marcher)           :: marcher
   integer                     :: i, j, nsteps
   real, pointer               :: y1h(:), y2h(:), y4h(:)
   real                        :: t1h, t2h, t4h, h1, h2, h4, num, den, p
   real, allocatable           :: ptable(:)
   
   nsteps = nint(t1/(h*4.0)) ! ilosc iteracji z krokiem 4*h

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
