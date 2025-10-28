# `MAPL_GridCompSpecs_ACG_writer.py`

## Introduction

**`MAPL_GridCompSpecs_ACG_writer.py`** writes a spec file for the MAPL2
Automatic Code Generator based on **`MAPL_Add_STATE_Specs`** subroutine calls
in Fortran source code that uses MAPL2. **`STATE`** is "`IMPORT`", "`EXPORT`",
or "`INTERNAL`", and the writer uses this string to determine the
**`ESMF_State`** of the **`ESMF_Field`** in the subroutine call. The writer
determines the columns for the spec file from the arguments of the subroutine
calls.

## Usage

The writer requires one command line parameter and one positional argument. The 
basic syntax to run the writer is:

    MAPL_GridCompSpecs_ACG_writer.py -o SPEC_FILE SOURCE_FILE
 
 or

    MAPL_GridCompSpecs_ACG_writer.py --output SPEC_FILE SOURCE_FILE

`SPEC_FILE` is the filename for the spec file the writer produces, and
`SOURCE_FILE` is the filename of the source code to be processed.

The script determines the component name from the input file name, by default,
but the user can specify the component explicitly with the optional command
line parameter:

    -c COMPONENT_NAME | --component COMPONENT_NAME

The script accepts two additional optional command line parameters:

    -d DEBUG_FILE | --debug DEBUG_FILE

and

    --run-tests

The value of the `-d`/ `--debug` parameter is the filename of a debugging file
that users can use to troubleshoot results.  The debugging file contains all
the lines that were skipped. The skipped lines are partially processed, so they
will not match the lines in the original source code file.

The `--run-tests` parameter is used for development of the writer itself, and
it is not intended for users.  If it is present, it will run tests instead of
generating a spec file.

Finally, the script accepts the standard:

    -h  or --help
   
command line parameter, which produces usage (help) information.

## Further Notes

The writer is not a true parser of Fortran code. It is a Python script that
parses a restricted subset of MAPL2 subroutine calls that add **`ESMF_Field`**
instances in Fortran source code.  It treats the code as text without any
Fortran syntax comprehension.

It will process subroutine calls that span multiple lines with "&" continuation
characters, and it will ignore Fortran comments. It will not interpret Fortran
control statements like `IF/ELSEIF/ELSE/END` blocks or `DO` loops. 

It produces spec files with columns ordered based on the order of arguments in
the subroutine calls, but it orders the initial and final columns to match the
column order found in typical spec files.  The spec file is formatted for
readability.
