#pragma once

extern int MAPL_PythonBridge_Py_global_initialize(int, int, int);

extern int pyGEOSBridge_Py_init(char *name, void *grid_comp, void *import_state, void *export_state);
extern int pyGEOSBridge_Py_run(char *name, void *grid_comp, void *import_state, void *export_state);
extern int pyGEOSBridge_Py_finalize(char *name, void *grid_comp, void *import_state, void *export_state);
