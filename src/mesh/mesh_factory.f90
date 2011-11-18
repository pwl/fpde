module mesh_factory

  use class_mesh
  use class_mesh_sfd3pt

contains

  !> Returns a pointer to a memory block allocated to hold a mesh
  !! corresponding to a given name
  !!
  !! @param id name of a mesh to be initialized
  !!
  !! @return
  !!
  function mesh_new(id) result(m)
    class(mesh), pointer :: m
    character(len=*) :: id

    select case(trim(id))
    case( "sfd3pt" )
       allocate(mesh_sfd3pt :: m)
    case default
       ! if id does not correspond to an implemented mesh we nullify
       ! the pointer
       ! @todo log
       print *, "ERROR: mesh_new: invalid id"
       nullify( m )
    end select

  end function mesh_new

end module mesh_factory
