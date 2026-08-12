#pragma once

/*

Header file linking the cffi generated header (see MAPL_PythonBridge/fortran_python/bridge.py) to the
iso_c_binding worthy code defined in (bridge.c and bridge.F90)

*/

extern int mapl_fortran_python_bridge_global_initialize_PyHook(int, int, int);

extern int mapl_fortran_python_bridge_user_init_PyHook(char *name, void *mapl, void *import_state, void *export_state);
extern int mapl_fortran_python_bridge_user_run_PyHook(char *name, void *mapl, void *import_state, void *export_state);
extern int mapl_fortran_python_bridge_user_run_with_internal_PyHook(char *name, void *mapl, void *import_state, void *export_state, void *internal_state);
extern int mapl_fortran_python_bridge_user_finalize_PyHook(char *name, void *mapl, void *import_state, void *export_state);
