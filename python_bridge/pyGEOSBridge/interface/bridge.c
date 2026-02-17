#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include "bridge.h"

void MAPL_PythonBridge_C_global_initialize(int im, int jm, int lm)
{
    int return_code = MAPL_PythonBridge_Py_global_initialize(im, jm, lm);
    if (return_code < 0)
    {
        exit(return_code);
    }
}

void pyGEOSBridge_C_init(
    char *name,
    void *mapl,
    void *import_state,
    void *export_state)
{
    int return_code = pyGEOSBridge_Py_init(name, mapl, import_state, export_state);
    if (return_code < 0)
    {
        exit(return_code);
    }
}

void pyGEOSBridge_C_run(
    char *name,
    void *mapl,
    void *import_state,
    void *export_state)
{
    int return_code = pyGEOSBridge_Py_run(name, mapl, import_state, export_state);
    if (return_code < 0)
    {
        exit(return_code);
    }
}

void pyGEOSBridge_C_run_with_internal(
    char *name,
    void *mapl,
    void *import_state,
    void *export_state,
    void *internal_state)
{
    int return_code = pyGEOSBridge_Py_run_with_internal(name, mapl, import_state, export_state, internal_state);
    if (return_code < 0)
    {
        exit(return_code);
    }
}

void pyGEOSBridge_C_finalize(
    char *name,
    void *mapl,
    void *import_state,
    void *export_state)
{
    int return_code = pyGEOSBridge_Py_finalize(name, mapl, import_state, export_state);
    if (return_code < 0)
    {
        exit(return_code);
    }
}
