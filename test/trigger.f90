program trigger_program

  ! use class_trigger
  use class_trigger_always

  ! class(trigger), pointer :: t
  type(trigger_always) :: ta

  ta = trigger_always()
  ! t => ta

  ! call trigger_start(t)
  ! print *, trigger_test(t)
  ! call trigger_info(t)
  ! call trigger_stop(t)

end program trigger_program
