module class_mesh_sfd3pt
  use class_mesh
  use pretty_print

  ! everything except the type should be private
  private

  type, public, extends( mesh ) :: mesh_sfd3pt
     real :: h = 0.
   contains
     ! overloaded procedures go here (if needed)
     procedure :: init
     procedure :: diff_point
     ! procedure :: diff_global
  end type mesh_sfd3pt

contains

  subroutine init( m )
    class(mesh_sfd3pt), intent(inout) :: m
    integer :: i,j

    call m % mesh % init

    call set_string_if_empty( m % name, "sfd3pt")

    m % rk = 1

  end subroutine init

  subroutine generate_x(m)
    class(mesh_sfd3pt), intent(inout) :: m
    integer :: i

    call m%mesh%generate_x

    ! update the step size
    m % h = (m % x(1) - m % x(m%nx))/(m % nx - 1)

    ! setup a uniform grid
    forall( i = 1 : m % nx )
       m % x(i) = m % x(1) + (i-1) * m % h
    end forall

  end subroutine generate_x

  function diff_point( m, f, k, i ) result(d)
    class(mesh_sfd3pt), intent(inout) :: m
    integer, intent(in) :: i,k
    real, intent(in) :: f(:)
    real :: d
    real :: h

    h = m % h

    if ( k > m % rk ) return

    if( i == 1) then
       d = (f(2)-f(1))/h
    elseif( i == m % nx) then
       d = (f(m%nx)-f(m%nx-1))/h
    else
       d = (f(i+1)-f(i-1))/(2.*h)
    end if



  end function diff_point



end module class_mesh_sfd3pt
