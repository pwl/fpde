program mesh_factory_test

  use mesh_factory
  use class_mesh

  class(mesh), pointer :: m

  m => mesh_new("sfd3pt")
  call m % init(1,1,1,0.,1.)
  call m % info
  call m % free

  m => mesh_new("nmfdsah")
  call m % init(1,1,1,0.,1.)
  call m % info
  call m % free

end program mesh_factory_test
