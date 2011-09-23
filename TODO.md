TODO
====

- make factory for `ode_control`
- split classes inheriting from `ode_control` to separate files in directory `src/marcher/control/control_name`
- default `ode_control` for `solver_simple`
- wait for "pointer bound remapping" feature in `ifort` and reimplement how `solver % y` and `solver % f` are handled (now there are two allocated tables, one for `solver % y` and one for `solver % f` synchronized by `rhs_for_marcher`)
- better (any?) error handling, a singleton class?

- "pointer bound remapping" is fixed in `ifort` 12.1, but there are some issues with syncing the three classes: `mesh`, `solver` and `marcher`. In particular:
-- does `y` stay fixed until the end of `apply` and then it's written down?


- cyclic use should make `class_solver_simple_data` much simpler and more elegant, possible?
- another idea is to implement general class `class_solver_data` from which particular `class_solver_simple_data` could inherit. `class_solver_data` should contain solver name and it should be able to "generate" a particular solver via a factory. `class_solver_data` should contain the most of the information required to generate a solver. The problem is how to provide additional data required by certain solver, e.g. the number of meshes or a monitor function.
