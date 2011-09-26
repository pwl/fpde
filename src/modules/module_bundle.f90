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

  private

  ! @todo extends(initializable)
  type, public :: module_bundle
     class(list), pointer :: modules
     integer :: n_modules
   contains
     procedure :: add
     procedure :: info
     procedure :: start
     procedure :: stop
     procedure :: step
     procedure :: init
  end type module_bundle

contains

  subroutine init(this)
    class(module_bundle) :: this
    allocate(this%modules)
    this % n_modules = 0
  end subroutine init

  subroutine add(this, m)
    class(module_bundle) :: this
    class(module), target :: m
    class(*), pointer :: dummy

    if( m % try_init() ) then
       dummy => m
       call this % modules % add(dummy)
       this % n_modules = this % modules % length()
    end if

  end subroutine add

  subroutine info(this)
    class(module_bundle) :: this
    call this % modules % map(module_info)
  end subroutine info

  subroutine start(this)
    class(module_bundle) :: this
    call this % modules % map(module_start)
  end subroutine start

  subroutine stop(this)
    class(module_bundle) :: this
    call this % modules % map(module_stop)
  end subroutine stop

  subroutine step(this)
    class(module_bundle) :: this
    call this % modules % map(module_step)
  end subroutine step

end module class_module_bundle
!> @}
