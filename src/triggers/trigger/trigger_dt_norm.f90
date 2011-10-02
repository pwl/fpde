module class_trigger_dfdt_norm

  use class_solver_data
  use class_trigger

  private

  type, public, extends(trigger) :: trigger_dfdt_norm
     real :: min = -1.
     real :: max = -1.
   contains
     ! procedure :: info
     procedure :: test
     procedure :: init
  end type trigger_dfdt_norm

contains

  function init(this) result(r)
    class(trigger_dfdt_norm) :: this
    logical :: r
    r = this % trigger % init()

    if( r ) then
       this % name = "trigger_dfdt_norm"
    end if

    if( this % min < 0 .and. this % max < 0) then
       print *, "ERROR: trigger_dfdt_norm: ",&
            "neither of min and max is set"
       r = .false.
    end if


  end function init

  function test(t) result(r)
    class(trigger_dfdt_norm), target :: t
    logical :: r
    real, pointer :: dfdt(:,:), x(:)
    real, allocatable :: l2norm(:), dx(:)
    integer :: nf,nx,i
    real :: min, max
    r = .false.
    dfdt => t % solver_data % dfdt
    x => t % solver_data % x
    nf = t % solver_data % nf
    nx = t % solver_data % nx
    min = t % min
    max = t % max

    ! @todo use mesh % integrate
    allocate(l2norm(nf))
    allocate(dx(nx))

    ! calculate measure on the mesh
    dx(2:nx-1) = (x(3:nx) - x(1:nx-2))/2.
    dx(1) = (x(2)-x(1))/2.
    dx(nx) = (x(nx)-x(nx-1))/2.

    forall(i = 1:nf)
       l2norm(i) = sqrt(sum(dfdt(:,i)*dfdt(:,i)*dx(:)))
    end forall

    print *, "DEBUG: trigger_dt_norm: test: l2norm = ", l2norm,&
         "min = ", min

    if( min > 0. .and. any( l2norm < min ) ) then
       r = .true.
       return
    end if

    if( max > 0. .and. any( l2norm > max ) ) then
       r = .true.
       return
    end if

    deallocate(l2norm, dx)

  end function test

end module class_trigger_dfdt_norm

