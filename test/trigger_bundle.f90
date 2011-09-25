program trigger_bundle_test
  use class_trigger_bundle
  use class_trigger
  use class_trigger_always

  type(trigger_bundle) :: tb
  class(trigger), pointer :: t1,t2
  t1 => trigger_always_init(.false.)
  t2 => trigger_always_init(.true.)

  ! this initializes the bundle and the triggers within, if not
  ! initialized already
  call tb % init
  call tb % add(t1)
  call tb % add(t2)
  call tb % info
  call tb % start
  call tb % info
  print *, "test result: ",tb % test()

  print *, "switching off t2"
  select type(t2)
  type is (trigger_always)
     t2 % test_result = .false.
  end select

  print *, "test result: ",tb % test()
  call tb % stop
  call tb % info

end program trigger_bundle_test
