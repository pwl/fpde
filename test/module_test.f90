program module_test_
  ! quite a lot of use statements
  use class_trigger
  use class_trigger_always
  ! use class_initializable
  use class_module
  use class_module_test

  type(module_test) :: m1, m2

  ! @bug the test won't compile with ifort 12.1 without those two
  ! lines, reported as issue DPD200174088

  type(trigger) :: t
  ! type(initializable) :: i
  type(trigger_always) :: ta

  ! ta = trigger_always()

  m1 = module_test(name = "test1")
  m2 = module_test(name = "test2")

  if( m1 % try_init() .and. m2 % try_init() ) then

     call module_info(m1)

     call m1 % add(trigger_always(test_result = .true.))
     call module_info(m1)

     call module_start(m1)
     call module_info(m1)

     call module_step(m1)
     call module_info(m1)

     call module_stop(m1)
     call module_info(m1)

  end if



end program module_test_
