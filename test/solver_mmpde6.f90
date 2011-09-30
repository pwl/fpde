program test_solver_mmpde6

  use class_solver_data
  use class_solver
  use class_solver_standard
  use class_solver_mmpde6

  type(solver_mmpde6) :: s
  real, target :: p = 1.

  s % rhs => my_rhs
  s % t0 = 0.
  s % t1 = 1.
  s % nx = 11
  s % nf = 1
  s % rk = 2
  s % stepper_id = "rk4cs"

  call s % init

  call s % info
  call s % calculate_dfdx(2)

  ! call s % set_pointers( dydt = s % dydt, y = s % y)
  call s % solve

contains

  subroutine my_rhs( s )
    class(solver) :: s
    integer :: i

    ! call s % calculate_dfdx( 2 )

    s % dfdt(:,1) = s % f(:,2)
    s % dfdt(:,2) = s % dfdx(:,1,2)

    s % dfdt(1,:) = 0.
    s % dfdt(s%nx,:) = 0.

  end subroutine my_rhs


end program test_solver_mmpde6
