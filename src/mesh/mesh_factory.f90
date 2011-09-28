module mesh_factory

  use class_mesh
  use class_mesh_sfd3pt
  use class_mesh_afd5pt

contains

  function mesh_new(id) result(m)
    class(mesh), pointer :: m
    character(len=*) :: id

    select case(trim(id))
    case( "sfd3pt" )
       allocate(mesh_sfd3pt :: m)
    case( "afd5pt" )
       allocate(mesh_afd5pt :: m)
    case default
       print *, "mesh_new: invalid id"
       nullify( m )
    end select

  end function mesh_new

end module mesh_factory
