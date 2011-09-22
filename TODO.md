TODO
====

- make factory for `ode_control`
- split classes inheriting from `ode_control` to separate files in directory `src/marcher/control/control_name`
- default `ode_control` for `solver_simple`
- wait for "pointer bound remapping" feature in `ifort` and reimplement how `solver % y` and `solver % f` are handled (now there are two allocated tables, one for `solver % y` and one for `solver % f` synchronized by `rhs_for_marcher`)
- better (any?) error handling
