module mesh_module
  type mesh
     integer :: dim, nx, nf, maxrk
     real, allocatable :: x(:)
     real, allocatable :: f(:,:)
     real, allocatable :: df(:,:,:)
  end type mesh
contains

  function MeshInit(nx, nf, maxrk)
    integer :: nx,nf,maxrk
    type(mesh) :: MeshInit(1, nx, nf, maxrk)

    allocate(MeshInit%x(nx))
    allocate(MeshInit%f(nf,nx))
    allocate(MeshInit%df(maxrk,nf,nx))
  end function MeshInit

end module mesh_module
