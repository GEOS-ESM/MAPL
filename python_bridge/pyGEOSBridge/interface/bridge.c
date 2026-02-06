#include <stdio.h>
#include <time.h>
#include "bridge.h"

void MAPL_PythonBridge_C_global_initialize(int im, int jm, int lm)
{
    printf("Init bridge, C side %d %d %d\n", im, jm, lm);
    int return_code = MAPL_PythonBridge_Py_global_initialize(im, jm, lm);
    if (return_code < 0)
    {
        exit(return_code);
    }
}

void pyGEOSBridge_C_init(
    char *name,
    void *grid_comp,
    void *import_state,
    void *export_state)
{
    int return_code = pyGEOSBridge_Py_init(name, grid_comp, import_state, export_state);
    if (return_code < 0)
    {
        exit(return_code);
    }
}

void pyGEOSBridge_C_run(
    char *name,
    void *grid_comp,
    void *import_state,
    void *export_state)
{
    int return_code = pyGEOSBridge_Py_run(name, grid_comp, import_state, export_state);
    if (return_code < 0)
    {
        exit(return_code);
    }
}

void pyGEOSBridge_C_finalize(
    char *name,
    void *grid_comp,
    void *import_state,
    void *export_state)
{
    int return_code = pyGEOSBridge_Py_finalize(name, grid_comp, import_state, export_state);
    if (return_code < 0)
    {
        exit(return_code);
    }
}
