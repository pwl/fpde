TODO
====

- make factory for `ode_control`

- split classes inheriting from `ode_control` to separate files in
  directory `src/marcher/control/control_name`

- default `ode_control` for `solver_simple`

- wait for "pointer bound remapping" feature in `ifort` and
  reimplement how `solver % y` and `solver % f` are handled (now there
  are two allocated tables, one for `solver % y` and one for `solver %
  f` synchronized by `rhs_for_marcher`)

- better (any?) error handling, a singleton class?

- "pointer bound remapping" is fixed in `ifort` 12.1, but there are
  some issues with syncing the three classes: `mesh`, `solver` and
  `marcher`. In particular:

- does `y` stay fixed until the end of `apply` and then it's written
  down?

- cyclic use should make `class_solver_simple_data` much simpler and
  more elegant, possible?

- another idea is to implement general class `class_solver_data` from
  which particular `class_solver_simple_data` could
  inherit. `class_solver_data` should contain solver name and it
  should be able to "generate" a particular solver via a
  factory. `class_solver_data` should contain the most of the
  information required to generate a solver. The problem is how to
  provide additional data required by certain solver, e.g. the number
  of meshes or a monitor function.

- add iterators to factories, to enumerate the products and count
  products number

- move `up_to_`typename to corresponding `class_`typename

- implement `take(i)` as a method in `bundle`s

- compiler bug? see `test/abstract.f90.bug`, due to this in the
  `test/solver_simple.f90` one cannot use `class(solver)` in the
  subroutine `my_rhs`

- give module % add and solver % add several arguments of different
  types (so that you could write solver % add (module1, module2,
  trigger1, trigger2) and this would result in adding module1 and
  module2 to solver both with pointers to the same instances of
  trigger1 and trigger2, so they shall be executed at the same time)

- add an `uninitialized` state?

- add a class `initializable`? Now impossible due to the bug in ifort
  12.1.0 20110811

- `solver_data` and `solver_simple_data` are not related classes. In
  this case one of the names should be changed. Also a way of
  initializing `solver_simple` is not analogous to this of `module`
  and `trigger`.

- module to calculate convergence

- module to print a function of data (or expand `module_data_print` to
  this functionality)

- `solver_mmpde6` (would be best to clean up `solver_simple`)

- modify `CMakeList.txt` to copy the scripts to a build location

- `info_module` to print an information on the simulation, steps/s

- add subroutine `print` to `solver_data` to be used along with
  `module_data_print`?

- logical combinations of triggers, now only "and" is available

- module to calculate convergence?

- include `module_print_data` in `solver`? It is needed almost always

- better solver output directory choice, now its date and time of run,
  and it cannot be changed by user
