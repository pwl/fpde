program trigger_program

  use class_trigger
  use class_trigger_always

  class(trigger), pointer :: t

  t => class_trigger_always_init()

  call t % info

end program trigger_program
