module class_module_bundle

  use class_list
  use class_module

  type, public :: module_bundle
     class(list), pointer :: modules
   contains
     procedure :: add
     procedure :: info
  end type module_bundle

  private
  public module_info

contains

  subroutine add(this, m)
    class(module_bundle) :: this
    class(module), pointer :: m
    class(*), pointer :: dummy
    dummy => m
    call this % modules % add(dummy)
  end subroutine add

  subroutine info(this)
    class(module_bundle) :: this
    call this % modules % map(module_info)
  end subroutine info

  subroutine module_info(m)
    class(module) :: m
    print *, m % name
  end subroutine module_info


end module class_module_bundle
