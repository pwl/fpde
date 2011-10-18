!@ Testujemy dokladnosc calkowania problemu
! sztywnego.

program test_accuracy_for_stiff_ode

   use class_ode_system
   use class_ode_marcher
   use stepper_factory
   use control_factory
   use oivp_factory

   class(ode_iv_problem), pointer :: oivp
   class(ode_stepper), pointer :: stepper
   class(ode_step_control), pointer :: step_control
   type(ode_system) :: ode
   
   integer, parameter   :: nsteppers=1 ! steppers to test
   integer, parameter   :: nproblems=1 !

   integer              :: i, j
   real                 :: t0=0.0
   real                 :: h=0.15, err_end
   integer              :: nsteps, failed_steps
   character(len=12), dimension(1:nsteppers) :: step_name
   character(len=36), dimension(1:nproblems) :: prob_name

   prob_name = (/ "STIFF1" /)
   step_name = (/ "rkpd54" /)
   
   step_control => control_new("standard")
   call step_control % init ( eps_abs = 1.0e-10, &
                              eps_rel = 1.0e-7, &
                              a_y     = 1.0, &
                              a_dydt  = 1.0 )

   do j=1,nproblems

      oivp => oivp_new(trim(prob_name(j)))

      call oivp % init()
      
      print *, '# ODE problem: ', trim(oivp % name)

      do i=1,nsteppers

         stepper => stepper_new(trim(step_name(i)))
         

         call test_accuracy ( stepper,      &
                              step_control, &
                              oivp % sys,   &
                              oivp % dim,   &
                              oivp % y0,    &
                              t0,           &
                              h,            &
                              oivp % tend,  &
                              oivp % sol_end, &
                              .false.,      &
                              err_end,      &
                              nsteps,       &
                              failed_steps )
         
         print *, '# ', err_end, nsteps, failed_steps

      end do

      call oivp % free()

   end do

   call step_control % free()

end program test_accuracy_for_stiff_ode


subroutine test_accuracy( stepper, control, sys, dim, y0, t0, hinit, t1, y1, verbose, err_end, nsteps, failed_steps )

   use class_ode_system
   use class_ode_stepper
   use class_ode_step_control
   use class_ode_marcher

   class(ode_stepper), pointer :: stepper
   class(ode_step_control), pointer :: control
   type(ode_system) :: sys
   integer :: dim
   real, pointer :: y0(:), y1(:)
   real :: t0, hinit, t1
   logical :: verbose
   real :: err_end
   integer :: nsteps, failed_steps

   ! local variables
   type(ode_marcher) :: marcher
   real :: t, h
   real, pointer :: y(:)

   allocate(y(dim))
   call stepper % init(dim)
   call marcher % init(dim)
         stepper % test_for_stiffness = .true.

   t = t0
   h = hinit
   y = y0

      if ( verbose ) then
         print *, '# t  y(1)  h  count'
      end if

   do while ( t < t1 )
      call marcher % apply ( s    = stepper, &
                             c    = control, &
                             sys  = sys,     &
                             t    = t,       &
                             t1   = t1,      &
                             h    = h,       &
                             y    = y )

      if ( stepper % stiff_status ) then
         print *, 'this if stiff IVP'
         exit
      end if

      if ( marcher % status /= 1 ) then
         exit
      end if

      if ( verbose ) then
         print 10, t, y(1), h, marcher % count
10       FORMAT (E17.10,E17.10,E17.10,I10)
      end if
   end do

   err_end = norm2(y-y1)/sqrt(real(dim))
   nsteps = marcher % count
   failed_steps = marcher % failed_steps

   deallocate(y)
   call stepper % free()
   call marcher % free()

end subroutine test_accuracy
