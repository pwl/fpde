!>
!! @addtogroup Modules
!! @{
!!
!>
!! @file   module_bundle.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Sat Sep 24 21:16:54 2011
!!
!! @brief
!!
!!
!!


module class_module_bundle

  use class_list
  use class_module

  type, public :: module_bundle
     class(list), pointer :: modules
   contains
     procedure :: add
     procedure :: info
     ! procedure :: start
     ! procedure :: stop
     ! procedure :: run
  end type module_bundle

  private
  ! @todo is there any way of not exposing it to the public?
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
!> @}
