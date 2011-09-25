program module_bundle_test
  use class_list
  use class_trigger
  use class_trigger_always
  use class_module
  use class_module_test1
  use class_module_test2
  use class_module_bundle

  class(module), pointer :: m1, m2
  class(trigger), pointer :: t1, t2
  type(module_bundle) :: mb
  class(*), pointer :: m
  class(list), pointer :: l

  call mb % init

  m1 => module_test1_init()
  m2 => module_test2_init()

  t1 => trigger_always_init(.true.)

  call m1 % add(t1)

  call mb % add(m1)
  call mb % add(m2)

  call mb % info

end program module_bundle_test
