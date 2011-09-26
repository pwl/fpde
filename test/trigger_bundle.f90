program trigger_bundle_test
  use class_initializable
  use class_trigger_bundle
  use class_trigger
  use class_trigger_always

  type(initializable) :: i
  type(trigger_bundle) :: tb
  type(trigger_always) :: t1, t2
  logical :: r

  ! create a trigger by constructing it a trigger
  t1 = trigger_always(test_result=.true.)
  ! t2 has been set to default values test_result=.false.

  ! this initializes the bundle and the triggers within, if not
  ! initialized already
  call tb % init
  ! add a trigger using a constructor with field names specified
  call tb % add(trigger_always(test_result=.false.))
  ! or with default values
  call tb % add(trigger_always())
  ! or using a predefined variable
  call tb % add(t1)
  ! or using a predefined variable, but with default values
  call tb % add(t2)
  call tb % info
  call tb % start
  call tb % info
  print *, "test result: ",tb % test()

  ! the only trigger that will return .true. is t1, so switching it
  ! off should result in failing the test for the whole bundle
  print *, "switching off t2"
  ! if you want to use the trigger components it is recommended that
  ! you call t1%init() first.
  t1 % test_result = .false.

  print *, "test result: ",tb % test()
  call tb % stop
  call tb % info

end program trigger_bundle_test
