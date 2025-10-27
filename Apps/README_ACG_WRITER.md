`MAPL_GridCompSpecs_ACG_writer.py`
===

**`MAPL_GridCompSpecs_ACG_writer.py`** is a Python script that processes
a MAPL(2) Fortran source code file and generates an ACG(2) spec file. It
determines the columns for the spec file from the arguments of
**`MAPL_Add_STATE_Specs`** subroutine calls in the source code file,
where **`_STATE_`** is the **`ESMF_State`**.  It determines the ESMF State
from the subroutine name and processes the text in the subroutine
argument list.

The script requires one command line parameter:

    -o SPEC_FILE | --output SPEC_FILE

which is the filename for the spec file, as well as one command line
positional argument:

    SOURCE_FILE

which is the filename of the source code to be processed.

By default, the script determines the component name from the input file
name, but it will use the value of the optional command line parameter:

    -c COMPONENT_NAME | --component COMPONENT_NAME

if the parameter is given.

The script accepts:

    -d DEBUG_FILE | --debug DEBUG_FILE
    
The value of this optional command line parameter is the filename of an
optional debugging file to be used by users to troubleshoot results.
The debugging file contains all the lines that were skipped. The skipped
lines are partially processed, so they will not match the lines in the
original source code file.

The script accepts an optional command line flag:

    --run-tests
    
development testing. It is not intended for users, and if it is present,
it will run tests instead of generating a spec file.

Finally, the script accepts the standard:

    -h  or --help
   
command line parameter, which produces usage (help) information.
