module class_solver

  private

  type, public :: solver
     real, pointer :: t
     real, pointer :: x    (:)
     real, pointer :: f    (:,:)
     real, allocatable :: dfdt (:,:)
     real, pointer :: dfdx (:,:,:)
     integer :: nx,nf,maxrk
     procedure(interface_rhs), pointer :: rhs
     class(*), pointer :: params
     character(len=300) :: name
   contains
     procedure :: calculate_dfdx
     procedure :: pointwise_dfdx
     procedure, nopass :: rhs_for_marcher
  end type solver

  ! interface to rhs should be publicly available to all child classes
  public :: interface_rhs

  abstract interface
     subroutine interface_rhs( s, params )
       import :: solver
       class(solver) :: s
       class(*) :: params
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


  subroutine rhs_for_marcher( t, f, dfdt, s )
    real, target :: t
    real, pointer :: f(:), dfdt(:)
    real, pointer :: tmp(:,:)
    class(solver), target :: s

    ! a little bit of magic is involved below
    dfdt(1 : s%nx * s%nf) => s % dfdt
    tmp => s % f
    s % t => t
    s % f(1:s%nx, 1:s%nf) => f
    call s % rhs( s % params )
    s % f => tmp

  end subroutine rhs_for_marcher

end module class_solver
