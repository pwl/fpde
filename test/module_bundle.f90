program module_bundle_test

  ! @bug this program should work without the class_initializable, it
  ! is a compiler bug
  use class_initializable

  use class_list
  use class_trigger
  use class_trigger_always
  use class_module
  use class_module_test
  use class_module_bundle

  type(module_test) :: m1, m2
  type(module_bundle) :: mb

  call mb % init

  m1 = module_test(name = "test1")
  m2 = module_test(name = "test2")

  ! add two triggers to m1

  call m1 % add(trigger_always(test_result = .true.))
  call m1 % add(trigger_always(test_result = .false.))
  call module_info(m1)

  ! add one trigger to m2
  call m2 % add(trigger_always(test_result = .true.))

  call mb % add(m1)
  call mb % add(m2)

  ! call mb % info

  print *, ""
  print *, "sending start signal to module_bundle"
  call mb % start

  ! call mb % info

end program module_bundle_test
