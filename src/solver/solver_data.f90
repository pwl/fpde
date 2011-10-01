module class_solver_data

  character(len=30), public, parameter :: &
       solver_started = "started",     &
       solver_stopped = "stopped",     &
       solver_error = "error"


  private

  type, public :: solver_data
     real, pointer                     :: t => null()
     real                              :: dt = 1.e-10
     real                              :: t0 = 0.,t1 = 0.
     real, contiguous, pointer         :: x    (:) => null()
     real, contiguous, pointer         :: f    (:,:) => null()
     real, contiguous, pointer         :: dfdt (:,:) => null()
     real, contiguous, pointer         :: dfdx (:,:,:) => null()
     real                              :: x0=0., x1=1.
     integer                           :: nx = 0, nf = 0, rk = 0
     integer                           :: n_iter = 0
     class(*), pointer                 :: params => null()
     character(len=20)                 :: time_started = ""
     ! solver name
     character(len=30)                 :: name = ""
     ! interface supported by any solver
     integer                           :: rhs_status = 0
     character(len=30)                 :: status = "stopped"
   contains
     procedure                         :: calculate_dfdx
     procedure                         :: pointwise_dfdx
     ! @todo why rhs_for_marcher is here anyway? It is a public
     ! function and it isn't needed as a method
     ! procedure, nopass                 :: rhs_for_marcher
  end type solver_data

    ! public :: rhs_for_marcher

contains

  subroutine calculate_dfdx( s, i )
    class(solver_data) :: s
    integer :: i

    print *, "calculate_dfdx not defined for ", trim(s % name)

  end subroutine calculate_dfdx

  real function pointwise_dfdx( s, i, j, k )
    class(solver_data) :: s
    integer :: i, j, k
    pointwise_dfdx = -1.

    print *, "pointwise_dfdx not defined for ", trim(s % name)

  end function pointwise_dfdx


  ! ! this is a default wrapper for solver%rhs to work with marcher
  ! ! architecture. It should do the right thing for a simple solver,
  ! ! but should be rewritten in a more sophisticated solver
  ! ! implementation
  ! subroutine rhs_for_marcher( t, y, dydt, s, status )
  !   real, intent(in) :: t
  !   real, intent(in) :: y(:)
  !   real, intent(out) :: dydt(:)
  !   class(solver_data) :: s
  !   integer, optional :: status
  !   integer :: nx, nf, i, j

  !   nx = s % nx
  !   nf = s % nf

  !   s % f = reshape( y, [ nx,nf ] )
  !   s % t = t
  !   ! s % y is a one dimensional view of s % f, so this assignment
  !   ! changes s % f
  !   ! s % y = y

  !   ! calculate rhs (with s % f obtained from y)
  !   call s % rhs

  !   if( present(status) ) then
  !      status = s % rhs_status
  !   end if

  !   dydt = reshape( s%dfdt, [nx*nf] )

  ! end subroutine rhs_for_marcher



end module class_solver_data
