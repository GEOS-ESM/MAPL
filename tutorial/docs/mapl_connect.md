MAPL adopts ESMF’s natural hierarchical topology for component connectivity, following the model illustrated in Figure ??. 
The leaf components (no children: at the bot- tom of the figure) contain the bulk of the computational code. 
These are things like physical parameterizations or dynamical cores, and they are grouped in composite com- ponents (their parents). 
In a typical application, a composite component (parent) spawns other (children) components. 
In our Mapl example (3.9), the parent gridded component GEOS_AgcmSimpleGridComp spawns two children components 
FVdycore_GridCompMod and GEOS_hsGridCompMod. 
The registration of the children with Mapl is accomplished by the following calls in the parent’s SetServices.

```fortran
      dyn = MAPL_AddChild( gc, name='FVDYNAMICS', ss=DYN_SetServices, rc=status)
      phs = MAPL_AddChild( gc, name='HSPHYSICS',  ss=PHS_SetServices, rc=status)
```

Each parent's constituent components (its _children_)
can then be connected to each other by ESMF couplers (ESMF_CplComp).
_It is in these couplers that the more automatable coupling functions,
such as grid transformation, accumulation, etc., are performed_.
Note that in this hierarchical scheme, all couplings - whether physical
or automatable - occur between _siblings_. This simplifies the
placement of couplers, which is important since we want this
to be done automatically by MAPL, but it does require some
means of making connections between _cousins_. This is done by
adopting some rules that define the parent-child relationship. Since a
parent `owns' its children components and their __IM__/__EX__
states (it declares them!?!?!), it has access to them. In \ggn, we
take advantage of this by having the \emph{parent explicitly declare} what
connections it wants between its children's __Import__ and __Export__
states. The following call,
made by the parent component, would let \ggn\ know that it needs
certain connectivity services between these children; \ggn\
will provide these by __automatically__ generating the appropriate
couplers (ESMF_CplComp) (__Or does it just swap pointers!?!?!__),
extracting some of the needed information from the data
services provided by the children. Once again, this is done in \ssv.

```fortran
      call MAPL_AddConnectivity                         &
           (GC,                                         &
            SHORT_NAME  = (/ 'DUDT', 'DVDT', 'DTDT' /), &
            SRC_ID      =  PHS,                         &
            DST_ID      =  DYN,                         &
            RC=STATUS)
```

Here, __DUDT__, __DVDT__ and __DTDT__ are Import states of
__FVdycore_GridComp__ and Export states of __GEOS_hsGridComp__.
After all connections between the children are processed, their Import
states may still contain some unsatisfied items (such as those that
would be provided by cousins). MAPL adds these to the
parent's Import state. This occurs recursively up the hierarchy
until, in a well-coupled application, all \im s are satisfied.
__Unresolved__ Imports at the parent level have to be terminated.
In the Held-Suarez example, the child __DYN__ of the parent component
\texttt{GEOS\_AgcmSimple} has unresolved Import __PHIS__, __DPEDT__
which are terminated in the SetServices routine of GEOS_AgcmSimple:

```fortran
      call MAPL_TerminateImport
           (GC,
            SHORT_NAME = (/ 'PHIS', 'DPEDT' /), &
            CHILD = DYN,                        &
            RC=STATUS)
```

In order to have the cousin's Export available to the parents, MAPL
places all of the children's Export s in the parent's Export
state. This also continues recursively up the hierarchy.

#### What MAPL_GenericSetServices  Does with the Children

- Allocates an ESMF_GridComp  and an Import and Export state for each child
- Creates each child's ESMF_GridComp using the inherited grid and
   configuration. The $i^{th}$ child is named __GCNames(I)__.
- Creates each child's Import and Export states. These are named
   __GCNames(I)//_"IMPORT"__ and __GCNames(I)//"_EXPORT"__
- Invokes each child's SetServices. [These are chosen from the five possible
    externals specified, depending on the value of {\tt SSptr(I)}(!?!?!). By
    convention, if {\tt SSptr} is not present, there can be at most as many
    children as optional externals, and these are associated in the order
    they appear in {\tt GCNames} and the argument list] - __EXPLAIN__.
- 'Wires' the children. This resolves all child \im s that are satisfied
   by siblings. All such connections must have been added explicitly
   in SetServices.
- Propagates each child's Export state to the component's Export state.
- Propagate the childrens's unresolved Imports to the component's Import state.


#### Rules for MAPL Application

- __Rule 25:__ Every Mapl application will have one and only one Root component, 
  which will be an ancestor of every component except the History component.
- __Rule 26:__ The Cap component is the main program; 
  it has no parent and exactly three children: Root, ExtData, and History. 
  The application component creates and initializes the configuration.

### Configuration


MAPL requires that the application’s configuration be propagated down from parents to children, 
and that it be present in the component as soon as the component is created. 
It effectively treats the configuration as though it was a UNIX environment available to all components in an application.

The behavior of an application is controlled through three resource (or configuration) files. 
The MAPL_Cap (main program) opens the configuration files for itself and its three children (Root and History). 
These have the default names CAP.rc, ROOT.rc, ExtData.rc, and HISTORY.rc. 
They must be present in the run directory at run time. 
The name of MAPL_Cap’s own resource file is fixed as Cap.rc, since this is the resource from which the application ‘boots up’. 
The other two may be renamed in Cap.rc. The table below lists the resources in the Cap.rc.


| __Name__ | __Description__ | __Units__ | __Default__ |
| ----  |  ---- |  ---- |  ---- |
| CF_FILE:    |  Name of ROOT's config file             |  none    |  `Root.rc' | 
| CF_FILE:    |  Name of HISTORY's config file          |  none   | `HISTORY.rc' | 
| TICK_FIRST: |  Determines when clock is advanced      |  1 or 0  |  none | 
| BEG_YY:     |  Beginning year (integer)               |  year    |  1 | 
| BEG_MM:     |  Beginning month (integer 1-12)         |  month   | 1 | 
| BEG_DD:     |  Beginning day of month (integer 1-31)  |  day     |  1 | 
| BEG_H:      |  Beginning hour of day (integer 0-23)   |  hour    |  0 | 
| BEG_M:      |  Beginning minute (integer 0-59)        |  minute  |  0 | 
| BEG_S:      |  Beginning second (integer 0-59)        |  second  |  0 | 
| END_YY:     |  Ending year (integer)                  |  year    |  1 | 
| END_MM:     |  Ending month (integer 1-12)            |  month   |  1 | 
| END_DD:     |  Ending day of month (integer 1-31)     |  day     |  1 | 
| END_H:      |  Ending hour of day (integer 0-23)      |  hour    |  0 | 
| END_M:      |  Ending minute (integer 0-59)           |  minute  |  0 | 
| END_S:      |  Ending second (integer 0-59)           |  second  |  0 | 
| RUN_DT:     |  App Clock Interval (the Heartbeat)     |  second  |  none | 
| LATLON:     |  1 -> regular lat-lon; 0 -> custom grid |  0 or 1  |  1 | 
| NX:         |  Processing elements in 1st dimension   |  none    |  1 | 
| NY:         |  Processing elements in 2nd dimension   |  none    |  1 | 
| IM_WORLD:   |  Grid size in 1st dimension             |  none    |  none | 
| JM_WORLD:   |  Grid size in 2nd dimension             |  none    |  none | 
| LM:         |  Grid size in 3rd dimension             |  none    |  1 | 
| GRIDNAME:   |  Optional grid name                     |  none    |  `APPGRID' | 
| IMS:        |  Gridpoints in each PE along 1st dimension |  none |  IMS | 
| JMS:        |  Gridpoints in each PE along 2nd dimension |  none |  JMS | 
| POLEEDGE:   |  1->gridedge at pole; 0->gridpoint at pole |  0 or 1 |  0 | 
| LON0:       |  Longituce of center of first gridbox      |  degree  |  -90. | 

An example configuration file  `CAP.rc`:

```
  NX:     2
  NY:     2
  IM:     72
  JM:     46
  LM:     72
  BEG_YY: 1991
  BEG_MM: 03
  BEG_DD: 01
  BEG_H:  0
  END_YY: 1991
  END_MM: 03
  END_DD: 02
  END_H:  0
  RUN_DT: 1800
  ROOT_RC_FILE: HelloWorld.rc
```

An example `HelloWorld.rc`` configuration file is simply:

```
  RUN_DT: 1800
```

This would perform a one day simulation with 30 minute time steps on a
$4^o \times  5^o$ grid, using a $2 \times 2$ decomposition element layout.

For an ESMF_GridComp, the configuration may be obtained by querying using the standard ESMF interface, as shown in the run method of Example 2 (3.2.4.2). 
It can also be queried through the Mapl object by calling MAPL_GetResource. 
This is the preferred way. When the configuration is queried this way, Mapl first tries to match a label that has been made instance-specific by prepending the instance’s full name and an underscore to the specified label; in Example 2, Mapl would first look for `trim(COMP_NAME)//’_DT:’`. 
If this is not found, it would then look for a type-specific label by prepending only the last name, if the instance has one. If this fails, it would look for the unqualified label, DT:; finally, if this also fails, it would set it to the default value, which in the example is the application’s time step, RUN_DT.