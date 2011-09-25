program trigger_program

  use class_trigger
  use class_trigger_always

  class(trigger), pointer :: t

  ! t => trigger_new()

  t => trigger_always_init()

  call trigger_start(t)
  print *, trigger_test(t)
  call trigger_info(t)
  call trigger_stop(t)

end program trigger_program
