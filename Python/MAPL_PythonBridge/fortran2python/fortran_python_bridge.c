#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include "fortran_python_bridge.h"

/*

C compiled source linking the iso_c_binding defined in bridge.F90

*/

void mapl_fortran_python_bridge_global_initialize(int im, int jm, int lm)
{
    int return_code = mapl_fortran_python_bridge_global_initialize_PyHook(im, jm, lm);
    if (return_code < 0)
    {
        exit(return_code);
    }
}

void mapl_fortran_python_bridge_user_init(
    char *name,
    void *mapl,
    void *import_state,
    void *export_state)
{
    int return_code = mapl_fortran_python_bridge_user_init_PyHook(name, mapl, import_state, export_state);
    if (return_code < 0)
    {
        exit(return_code);
    }
}

void mapl_fortran_python_bridge_user_run(
    char *name,
    void *mapl,
    void *import_state,
    void *export_state)
{
    int return_code = mapl_fortran_python_bridge_user_run_PyHook(name, mapl, import_state, export_state);
    if (return_code < 0)
    {
        exit(return_code);
    }
}

void mapl_fortran_python_bridge_user_run_with_internal(
    char *name,
    void *mapl,
    void *import_state,
    void *export_state,
    void *internal_state)
{
    int return_code = mapl_fortran_python_bridge_user_run_with_internal_PyHook(name, mapl, import_state, export_state, internal_state);
    if (return_code < 0)
    {
        exit(return_code);
    }
}

void mapl_fortran_python_bridge_user_finalize(
    char *name,
    void *mapl,
    void *import_state,
    void *export_state)
{
    int return_code = mapl_fortran_python_bridge_user_finalize_PyHook(name, mapl, import_state, export_state);
    if (return_code < 0)
    {
        exit(return_code);
    }
}
