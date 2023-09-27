

MAPL has the following features:

- Easy specification of import, export, and internal states
- Easy addition of child components
- Default implementation for checkpoint/restart.
- Wrap user-specified ESMF entry points to
   - Manage instantiation of fields, esp. field connections
   - Profile (time and memory)
   - Insert couplers (when needed)
- Enforce conventions.

The above features are implemented through a set of a framework of sub-systems:

-  __MAPL_Core__ is a collection of routines and conventions used to
    build __ESMF_GridComps__ (or to wrap legacy codes as __ESMF_GridComps__). 
    In particular, it includes the means of describing a component's Import and Export states
    as well as the new Internal state.
- __MAPL_Connect__ is a collection of routines and conventions used for
    organizing MAPL-ESMF Gridded Components into a MAPL hierarchy.
- __MAPL_History__ is an ESMF Gridded Component that sits inside MAPL and
    _can_ be instantiated to provide data writing services for a MAPL
    hierarchy.
- __MAPL_ExtData__ is an ESMF Gridded Component that sits inside MAPL and
    _can_ be instantiated to provide data services to the IMPORT states
    of MAPL components in a hierarchy.
- __MAPL_Utils__ is a set of support utillities for commonly performed
    tasks in global climate models. MAPL itself uses some of these, but,
    like MAPL_History, MAPL components or applications need not use them.
- __PFIO__ is an I/O layer that reads or writes NetCDF files in parallel.


The distinction between MAPL_Core and MAPL_Connect, which in
the MAPL code are mostly within the __MAPL\_Generic__ module,
is important. One may use MAPL_Core alone as a means of facilitating the
introduction of ESMF, with no intention of ever coupling the component to a
MAPL hierarchy. A component so contructed is a perfectly good
ESMF component and, other than having to access the MAPL  library
to build and execute, is not special in any way. The code in
an application instantiating it would not need to know it was built
with MAPL  machinery.