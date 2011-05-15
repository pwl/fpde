module module_marcher
  type marcher
     real, allocatable :: f
     real, allocatable :: df
     real, allocatable :: t
     real, allocatable :: dt
  end type marcher
end module module_marcher
