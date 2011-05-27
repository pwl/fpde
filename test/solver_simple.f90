program solver_simple_program

  use class_mesh
  use class_mesh_sfd3pt

  use class_marcher
  use class_marcher_dummy

  use class_solver
  use class_solver_simple

  ! select a mesh
  type(mesh_sfd3pt) :: m
  ! select a marcher
  type(marcher_dummy) :: march
  ! select a solver
  type(solver_simple) :: s
  ! give a maximal time
  real :: max_t


  call m % init( 10, 2, 2, 0., 1. )

  call march % init( 10 )

  call s % init( m, march, max_t )

  call s % free

end program solver_simple_program
