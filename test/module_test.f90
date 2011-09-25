program module_test
  ! quite a lot of use statements
  use class_trigger
  use class_trigger_always
  use class_trigger_bundle
  use class_module_bundle
  use class_module
  use class_module_test1
  use class_module_test2

  class(module), pointer :: mt1, mt2
  class(trigger), pointer :: t1, t2

  t1 => trigger_always_init(.false.)
  t2 => trigger_always_init(.true.)

  mt1 => module_test1_init()
  mt2 => module_test2_init()

  call module_info(mt1)

  call mt1 % add(t2)
  call module_info(mt1)

  call module_start(mt1)
  call module_info(mt1)

  call module_step(mt1)
  call module_info(mt1)

  call module_stop(mt1)
  call module_info(mt1)


contains


end program module_test
