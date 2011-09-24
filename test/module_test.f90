program module_test
  use class_module_bundle
  use class_module
  use class_module_test1

  type(module_bundle) :: mb
  class(module), pointer :: mt1, mt2

  mt1 => module_test1_init()
  mt2 => module_test1_init()

  allocate(mb % modules)
  call mb % add ( mt1 )
  call mb % add ( mt2 )
  call mb % info

  ! mt1 = module_test1_init()


contains


end program module_test
