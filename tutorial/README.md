# MAPL Tutorials Overview

For user education we have provided some simple tutorials. These demonstrate how to create simple gridded componnts and hierachies of components and drive them via the MAPL_Cap just like the real GEOSgcm model. Each "tutorial" will consist of a set of input files that can be run with our the Example_Driver.x and will itself contain a REAMDE file with explanation about what that particular tutorial is demonstrating. In addition, each will suggest exercises that you can do to extend them.

Before embarking on these tutorials the new users should review the MAPL/ESMF powerpoint presentation here to get a general sense of what these frameworks are about and become familiar with the terminology.

In addition, to use these tutorials you will have to have either built MAPL as a standalone fixture or as part of another fixture like the GEOSgcm. If you are reading this and have not built either, see the instructions for how to build MAPL here: [How to Build MAPL](https://github.com/GEOS-ESM/MAPL/wiki/Building-and-Testing-MAPL-as-a-standalone). For these exercises we highly recommend building MAPL by itself as your build will be much faster, expecially when you change the code. The build itself takes a short time. We also recommend using the "debug" build rather than the "release" build.

Once you have installed either MAPL or the full GEOSgcm, you will have an installation directory whose full path I will refer to as INSTALL_DIR.

Once you have this, you will find a script run_tutorial_case.sh that is in INSTALL_DIR/bin we have created for your convinience.

This script takes two arguments, the path to INSTALL_DIR and the directory name of the test case you wish to run. To run this, go to a tempoary directory then run the script with the arguments. It will copy the input files to that directory and run the Example_Driver.x for that set of input files. Note that if you are at NCCS or NAS you will need to be on an interactive slurm job with a single node.

The following tutorials are available in the recommended order and represent the tutorial name you would use in the run script.
- hello_world
- parent_no_children
- parent_one_child_no_imports
- parent_two_siblings_connect_import_export
- parent_one_child_import_via_extdata


As a concrete example, suppose you have installed MAPL here at /discover/nobackup/auser/MAPL/install and want to run hello_world you run this on the command line:

```
/discover/nobackup/auser/MAPL/install/bin/run_tutorial_case.sh /discover/nobackup/auser/MAPL/install hello_world
```


# Note for the Curious
The astute user might ask, how is it each tutorial is running the same executable yet using different gridded components? The answer is that each gridded component is compiled as a shared object library. Each time you run Example_Driver.x, you pass in the actual name of the shared object library that will be used as the top level gridded component. This was done to make the life of the humble developer writing this tutorial easier. 

Note that this technology, while used at places in the full GEOSgcm model to handle mom5 and mom6, it is not ubiquitous.  You might notice that a few calls in these tutorails, particularly MAPL_AddChild calls in most gridded components and the "program" itself, aka where you have something like this:
```
program Example_Driver.x

! we have some source code

end program Example_Driver.x
```
may look a little different if you look at the corresponding program file for GEOSgcm.x. Do not worry. Please come ask your nearest SI team member.
