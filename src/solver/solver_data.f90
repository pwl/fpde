module class_solver_data

  private


  type, public :: solver_data
     real, pointer                     :: t
     real, contiguous, pointer                     :: x    (:)
     real, contiguous, pointer                     :: f    (:,:)
     real, contiguous, pointer                 :: dfdt (:,:)
     real, contiguous, pointer                     :: dfdx (:,:,:)
     integer                           :: nx,nf,rk
     procedure(interface_rhs), pointer :: rhs
     class(*), pointer                 :: params
     ! solver name
     character(len=30)                 :: name
     ! interface supported by any solver
     integer                           :: rhs_status
   contains
     procedure         :: calculate_dfdx
     procedure         :: pointwise_dfdx
     ! @todo why rhs_for_marcher is here anyway? It is a public
     ! function and it isn't needed as a method
     procedure, nopass :: rhs_for_marcher
  end type solver_data

 abstract interface
     subroutine interface_rhs( s )
       import :: solver_data
       class(solver_data) :: s
     end subroutine interface_rhs
  end interface

    public :: interface_rhs, rhs_for_marcher

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


  ! this is a default wrapper for solver%rhs to work with marcher
  ! architecture. It should do the right thing for a simple solver,
  ! but should be rewritten in a more sophisticated solver
  ! implementation
  subroutine rhs_for_marcher( t, y, dydt, s, status )
    real, intent(in) :: t
    real, intent(in) :: y(:)
    real, intent(out) :: dydt(:)
    class(solver_data) :: s
    integer, optional :: status
    integer :: nx, nf, i, j

    nx = s % nx
    nf = s % nf

    s % f = reshape( y, [ nx,nf ] )
    s % t = t
    ! s % y is a one dimensional view of s % f, so this assignment
    ! changes s % f
    ! s % y = y

    ! calculate rhs (with s % f obtained from y)
    call s % rhs

    if( present(status) ) then
       status = s % rhs_status
    end if

    dydt = reshape( s%dfdt, [nx*nf] )

  end subroutine rhs_for_marcher



end module class_solver_data
