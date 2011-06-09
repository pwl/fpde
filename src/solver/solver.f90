module class_solver

  private

  type, public :: solver
     ! interface supported by any solver
     real, pointer                     :: t
     real, pointer                     :: x    (:)
     real, pointer                     :: f    (:,:)
     real, allocatable                 :: dfdt (:,:)
     real, pointer                     :: dfdx (:,:,:)
     integer                           :: rhs_status
     integer                           :: nx,nf,rk
     procedure(interface_rhs), pointer :: rhs
     class(*), pointer                 :: params
     ! solver name
     character(len=30)                 :: name
   contains
     procedure         :: calculate_dfdx
     procedure         :: pointwise_dfdx
     procedure, nopass :: rhs_for_marcher
     procedure         :: info
     procedure         :: free
  end type solver

  ! interface to rhs should be publicly available to all child classes
  public :: interface_rhs, rhs_for_marcher

  abstract interface
     subroutine interface_rhs( s )
       import :: solver
       class(solver) :: s
     end subroutine interface_rhs
  end interface


contains

    subroutine calculate_dfdx( s, i )
    class(solver) :: s
    integer :: i

    print "(a,a)", "calculate_dfdx not defined for ", trim(s % name)
    stop

  end subroutine calculate_dfdx

  real function pointwise_dfdx( s, i, j, k )
    class(solver) :: s
    integer :: i, j, k
    pointwise_dfdx = -1.

    print "(a,a)", "pointwise_dfdx not defined for ", trim(s % name)
    stop

  end function pointwise_dfdx


  subroutine rhs_for_marcher( t, y, dydt, s, status )
    real, intent(in) :: t
    real, intent(in) :: y(:)
    real, intent(out) :: dydt(:)
    class(solver) :: s
    integer, optional :: status
    integer :: nx, nf, i, j

    nx = s % nx
    nf = s % nf

    forall( i = 1:nx, j = 1:nf )
       s % f( i, j ) = y( i + (j-1) * nx )
    end forall

    s % t = t

    ! calculate rhs
    call s % rhs

    if( present(status) ) then
       status = s % rhs_status
    end if

    forall( i = 1:nx, j = 1:nf )
       dydt( i + (j-1) * nx ) = s % dfdt( i, j )
    end forall

  end subroutine rhs_for_marcher

  subroutine info( s )
    class(solver) :: s
  end subroutine info

  subroutine free (s)
    class(solver) :: s
  end subroutine free

end module class_solver
