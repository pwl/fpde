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
     real, contiguous, pointer         :: data_block (:,:) => null()
     character(len=30), pointer        :: data_block_names (:) => null()
     real, contiguous, pointer         :: data_scalars (:) => null()
     character(len=30), pointer        :: data_scalars_names (:) => null()
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


end module class_solver_data
