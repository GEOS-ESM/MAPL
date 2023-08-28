MAPL's original intention, and its core function, is
to provide assistance in writing ESMF Gridded components. It does this
in the following ways:

1. It makes it easier to write a component's __IRF__ methods. In fact
   in some cases they need not be written at all.
2. It adds an Internal (IN) __ESMF\_State__
   to the component, supplementing the Import (IM) and Export (EX)
   states required by ESMF.
3. It provides a means of describing the contents of the three states
   so that MAPL  can help manage them.
4. It adopts ground rules for the behavior of a component
   and its treatment of the three states.
5. It defines a standard recipe for writing
   MAPL-based ESMF Gridded Components.

Each of these items is discussed in more detail in the following subsections.
It will be helpful to refer to the complete MAPL example in section.

### Writing the IRF method
After writing some gridded components, one realizes that, except
the actual insertion of the user code, most SetServices and __IRF__
methods are very similar, and that it would be economical to
generalize this `boilerplate' code. MAPL provides
three ways of doing this.


1. The first way is to use the generic versions of SetServices and the IRF methods 
   provided by MAPL as the component’s methods. When MAPL_GenericSetServices is invoked, 
   it registers the three Generic IRF methods. 
   If not overridden, these become the component’s actual methods.
2. A second way of using Mapl is to simply call the generic versions of the methods from 
   the component-specific versions, allowing them to perform the boilerplate functions.
3. Should this really be included!?!?! A third way is to simply use the source code of the 
   generic versions as templates for the specific versions. 
   Taking this approach is dangerous and not allowed for MAPL-compliant components.


So what do the Generic IRF methods do? This will be described in detail in subsequent sec- tions, 
but simply stated, they manage the IM/EX States and a third ESMF_State ’Internal’
that we will discuss below. We will refer to these three states as the IM/EX/IN states. 
Note that they are all ordinary ESMF_States. 
From the description of the three states provided in the data services, 
MAPL is able to create, allocate, initialize, and destroy all items in these states; 
it can also checkpoint and restart the Internal and Import states. 
The IRF methods also implement connectivity of children components, creating the appropriate couplers, 
registering their services, and executing their IRF methods.

### The new Internal (IN) State

ESMF requires that the control is passed back to it at the end of a component’s Run method. 
In the spirit of having as unintrusive a design as possible, ESMF says nothing about 
a component’s internal state which will probably be needed during subsequent executions of the component’s Run. 
MAPL provides a mechanism to place parts of its true internal state in an ESMF_State 
called an Internal state, that is similar to the IM/EX states.

Since it is desirable that gridded components be as object-oriented as possible, 
the framework has to allow them to be fully instantiatable. 
This requires that whatever the component defines as its internal state be attached 
(in the object-oriented paralance) to the instance of the ESMF_GridComp. 
ESMF provides such a mechanism - effectively a hook on which a component can hang 
the current instance of its internal state.


Accordingly, this new IN state does not appear explicitly in the argument list of IRF meth- ods, 
as is the case with the IM/EX states; instead it is attached to the ESMF_GridComp and, in principle, 
is accessible only through Mapl and can be queried.


All of the mechanisms for registering and manipulating data that are already available in MAPL
for the IM/EX States, are extended to the IN state. 
The default accessibility rules for this state are that its items can be written only by the 
component and can be read only by its parent. 
All data registered in this state by the component’s SetServices are, of course, 
automatically allocated, checkpointed, and restarted by the MAPL Initialize and Finalize methods.

### Description of State Contents

The simplest ESMF gridded component consists of the IRF methods encapsulating the user’s computational code. 
These methods are private to the component, but are callable by the framework; 
in fact, they can only be called by the framework. 
This is accomplished by having in each component a public method (SetServices) that tells 
the framework what functions it can perform (initialize the component, run it, etc.).
The framework can then invoke these functional services when they are required.

The interface to these services is prescribed by ESMF and includes Import and Export (IM/EX) States, 
through which all data is exchanged between the components.
THese states can contain only ESMF objects (primarily ESMF_Fields and other ESMF_States), 
but ESMF says nothing about how they are to be used. 
MAPL assumes that IM/EX states consist only of ESMF_Fields and other ESMF_States.
It also adopts the convention that, by default, items in its Export state are not modified 
by other components and that a component cannot modify items in its Import state 
(this default behavior can be changed by adding a ‘FRIENDLY_TO’ attribute to an IM state).

A major innovation in MAPL is a means of describing the contents of the IM/EX states. 
MAPL takes the view that 

> a component, in addition to giving the framework access to its functional services, should also tell the framework about its data services, i.e., what it needs from others and what it can provide.

MAPL extends the use of SetServices to accomplish this. The SetServices method of a 
MAPL-based gridded component will contain spec calls like the following:

Adding Import state:

```fortran
    call MAPL_AddImportSpec  (GC,                                &
                              SHORT_NAME = 'PLE',                &
                              LONG_NAME  = 'air_pressure',       &
                              UNITS      = 'Pa',                 &
                              DIMS       = MAPL_DimsHorzVert,    &
                              VLOCATION  = MAPL_VLocationEdge,   &
                              RC         = STATUS)
```

Adding Export state:

```fortran
    call MAPL_AddExportSpec  (GC,                                &
                              SHORT_NAME = 'U',                  &
                              LONG_NAME  = 'eastward_wind',      &
                              UNITS      = 'm s-1',              &
                              DIMS       = MAPL_DimsHorzVert,    &
                              VLOCATION  = MAPL_VLocationCenter, &
                              RC         = STATUS)
```

Adding Internal state:

```fortran
    call MAPL_AddInternalSpec(GC,                                &
                              SHORT_NAME = 'PKZ',                &
                              LONG_NAME  = 'pressure_to_kappa',  &
                              UNITS      = 'Pa$^\kappa$',        &
                              PRECISION  = ESMF_KIND_R8,         &
                              DIMS       = MAPL_DimsHorzVert,    &
                              VLOCATION  = MAPL_VLocationCenter, &
                              RC         = STATUS)
```

Note that some of the attributes being set for the ESMF_Fields, such as units, 
likely reflect assumptions made by the component and are usually static; 
others may be set at run time, say from a configuration file.

The information provided in setting data services is used by MAPL to allocate and initialize the states, 
to couple to other components, and to help build the component’s __IRF__ methods, as described below.

### Rules for Components

The first thing to clarify is what we mean by a MAPL-based ESMF_GridComp. 
The following general rules apply to MAPL-compliant components:

- __Rule 1:__ The component must be a fully-compliant ESMF_GridComp. 
    This implies that its only public method is SetServices and it registers IRF methods with ESMF.
- __Rule 2:__ Associated with each instance of a MAPL-compliant ESMF_GridComp there is an ESMF_Grid 
    that Mapl will use to allocate data.
- __Rule 3:__ Every ESMF_GridComp has a configuration (that stores parameters). 
    A Mapl grid- ded component will expect it to be open (accessible!?!?!) when SetServices is called.
- __Rule 4:__ Components can be run sequentially or concurrently; 
    however, their Run methods must return control at RUN_DT intervals.
- __Rule 5:__ A Mapl-compliant ESMF_GridComp can be simple (called a leaf ) or composite. 
- __Rule 6:__ The component must obey all Mapl rules pertaining to its grid, as defined below.
- __Rule 7:__ The component must obey all Mapl access rules to the IM/EX/IN states, as defined below.
- __Rule 8:__ The MAPL_GenericSetServices, MAPL_GenericInitialize, and MAPL_GenericFinalize 
    methods must be invoked once, and only once, for each instance of the gridded component.
- __Rule 9:__ Component instances must have unique names of the form: `‘first[:last]’`. 
    Neither first nor last name can have a colon. Example: `Ens01:TURBULENCE`.

The following Fortran 95 codes show simple MAPL components.

#### Example 1: Using the Generic Component

MaAPL has built-in ESMF_GridComps. 
The most fundamental of these is the MAPL_Generic component, whose SetServices and __IRF__ methods 
we normally use in building other components. 
It is possible, however, to instantiate MAPL_Generic itself. 
Currently such an instance is useful only as a null leaf component, which does nothing. 
Nevertheless, it is a perfectly valid ESMF_GridComp.

The following example is a main program that runs MAPL_Generic for a year. 
It also illustrates the basic steps that an ESMF main program (called Cap) contains. 
This is a fully-compliant ESMF_GridComp. 
It has a public SetServices taken from Mapl, and this is its only public object (method?). 
Of course, it does nothing; but it can be run as a null component anywhere an ESMF_GridComp can be run. 
Since it uses the generic IRF methods, it has a single stage of each. 
The rules about grids and states are not too relevant, but it has a natural grid - the ESMF_Grid 
is assumed to be given to it when the instance of the ESMF_GridComp is created. 
It has IM/EX/IN states, which are silently created by the implicit generic methods; but all three state are empty.

```fortran
  Program Example1

    use ESMF
    use MAPLBase_Mod, only: SetServices => MAPL_GenericSetServices

    type(ESMF_GridComp)    :: GC
    type(ESMF_State)       :: Import, Export
    type(ESMF_Clock)       :: Clock
    type(ESMF_Time)        :: StartTime
    type(ESMF_Time)        :: StopTime
    type(ESMF_TimeInterval):: DT
    integer                :: RC

    RC = ESMF_SUCCESS

    ! Initialize ESMF
    !----------------
    call ESMF_Initialize(defaultCalendar=ESMF_CALKIND_GREGORIAN, rc=rc)
    if(rc==ESMF_FAILURE) call exit(rc)

    ! Initial and final time of run and time step
    !--------------------------------------------
    call ESMF_TimeSet(StartTime, YY = 2007, rc=RC)
    if(RC==ESMF_FAILURE) call exit(RC)
    call ESMF_TimeSet(StopTime,  YY = 2008, rc=RC)
    if(RC==ESMF_FAILURE) call exit(RC)
    call ESMF_TimeIntervalSet(DT, S=1800,   rc=RC)
    if(RC==ESMF_FAILURE) call exit(RC)

    ! Create the Clock
    !-----------------
    clock = ESMF_ClockCreate( name='MyClock', timeStep=DT, &
             startTime=StartTime, stopTime=StopTime, userRC=STATUS )
    if(RC==ESMF_FAILURE) call exit(RC)

    ! Create the gridded component
    !-----------------------------
    GC = ESMF_GridCompCreate(name='ExampleGC', rc=rc)
    if(RC==ESMF_FAILURE) call exit(RC)


    ! SetServices
    !------------
    call ESMF_GridCompSetServices(GC, SetServices, RC)
    if(RC==ESMF_FAILURE) call exit(RC)

    ! Initialize
    !-----------
    call ESMF_GridCompInitialize(GC, importState=Import, exportState=Export, clock=Clock, RC)
    if(RC==ESMF_FAILURE) call exit(RC)

    ! Time loop
    do while (.not. ESMF_ClockIsDone(Clock))
      ! Run
      !----
      call ESMF_GridCompRun(GC, importState=Import, exportState=Export, clock=Clock, userRC=RC)
      if(RC==ESMF_FAILURE) call exit(RC)


      ! Tick the Clock
      !---------------
      call ESMF_ClockAdvance(Clock, rc=RC)
      if(RC==ESMF_FAILURE) call exit(RC)
    enddo

    ! Finalize grid comp
    !-------------------
    call ESMF_GridCompFinalize(GC, importState=Import, exportState=Export, clock=Clock, userRC=RC)
    if(RC==ESMF_FAILURE) call exit(RC)

    ! Don't we need to destroy the components!?!?!
    !------------------------------------------

    ! Finalize ESMF
    !--------------
    call ESMF_Finalize (rc=rc)
    if(RC==ESMF_FAILURE) call exit(RC)

    ! All Done
        !---------
    call exit(RC)

  end Program Example1

  #### Example 2: HelloWorldMod

  The second example illustrates a more typical use of MAPL to help write a gridded component.

  ```fortran
    module HelloWorldMod

    ! We always have this preamble
    !-----------------------------
    use ESMF
    use MAPLBase_Mod

    implicit none

    ! Make sure only SetServices is public.
    ! This is a hallmark of ESMF gridded components.
    !-----------------------------------------------
    private
    public SetServices

  contains

    ! a simple SetServices to register our custom
    ! run method (run_hello) with MAPL.
    !--------------------------------------------
    subroutine SetServices(gc,rc)
      ! input/output parameters
      !------------------------
      type(ESMF_GridComp), intent(INOUT) :: gc      ! gridded component
      integer, optional,    intent( OUT) :: rc      ! return code

      ! register custom run method
      call MAPL_GridCompSetEntryPoint(gc, ESMF_SETRUN, run_hello, rc)

      !IMPORTANT step - call GenericSetServices
      call MAPL_GenericSetServices(gc, rc)
    end subroutine SetServices

    ! The Run method
    !---------------
    subroutine run_hello(gc, import, export, clock, rc )

      ! input/output parameters
      !------------------------
      type(ESMF_GridComp),  intent(inout) :: gc     ! gridden component
      type(ESMF_State),     intent(inout) :: import ! import state
      type(ESMF_State),     intent(inout) :: export ! export state
      type(ESMF_Clock),     intent(inout) :: clock  ! the clock
      integer, optional,    intent(  out) :: rc     ! return code
                                                    ! 0 - all is well

      ! local
      !------
      type(ESMF_Config)                   :: cf     ! config
      character(len=ESMF_MAXSTR)          :: comp_name
      real                                :: dt     ! time step

      ! Get my name
      !------------
      call ESMF_GridCompGet(gc, name=comp_name, config=cf)

      ! Query configuration to get time step
      !-------------------------------------
      call ESMF_ConfigGetAttribute(cf, dt, label='run_dt:')

      print *, 'Hello World. I am ', trim(comp_name), &
               ', and my timestep is ',dt

      ! All done - successfully
      !------------------------
      rc = ESMF_SUCCESS

    end subroutine run_hello

  end module HelloWorldMod
  ```

  This example needs a custom Run method (run_hello). Since this method can only be registered 
  in a SetServices that is in the module, we must also write an explicit SetServices. 
  Notice that the registration of the Run method is with Mapl, not directly with ESMF. 
  The component does not explicitly register Initialize and Finalize methods, 
  so the generic ones will be used. 
  Normally, we would also register data and connectivities at this point, but in this example, 
  we have none. Also note that MAPL_GenericSetServices is called at the end, 
  after all registration with Mapl is completed. We rely on MAPL_GenericSetServices to do the heavy work.

  The Run method is simple, but it does illustrate that every instance of an ESMF_GridComp has a name, 
  and the IRF methods can access it to know which instance they are working on.

  Notice also that we have assumed that there is an open configuration (ESMF_Config - see section 3.3.3) 
  in the gridded component, from which we are getting the time step. 
  This is also typical of Mapl components and is crucial to the successful use of this and other examples is the. 
  Mapl treats the configuration in the component object like an environment from which 
  it can always query for predefined metadata. 
  MAPL requires certain configuration variables to be set in order to properly execute any application.

  The situation illustrated by this example is quite common. Most simple components will follow this template: 
  define a custom Run method, a SetServices that registers it and calls MAPL_GenericSetServices, 
  and default the Initialize and Finalize methods.

  #### Additional Rules for Grids and States

  Most MAPL_GridComps will receive a fully populated grid from its parent. 
  Some, however, may need be written to receive an empty grid that they populate themselves 
  or to replace the grid they receive with one of their own creation.

In its current implementation, Mapl severely restricts the nature of ESMF_Grids reflecting 
in part the state of ESMF’s own development. We will discuss this at length later (where!?!?!).

The following are some of the grid related rules:

Rule 10 A component’s grid must be fully formed before MAPL_GenericInitialize is invoked.
Rule 11 Once MAPL_GenericInitialize is invoked, the grid may not be changed and must remain as the instance’s ESMF_Grid.
Rule 12 An instance’s grid can be either an ESMF_Grid or a MAPL_LocationStream that has an associated ESMF_Grid. Thus there is always an ESMF_Grid associated (at- tached) with each instance of a Mapl-compliant ESMF_GridComp.

A component can operate on data on various grids. 
These can be ESMF_Grids or grids defined with the user’s own conventions and ESMF Infrasructure 
can be used to manipulate this data internally. 
But to the outside world and to Mapl a MAPL-compliant component should ‘look’ as though it has only one grid.

The following are the ESMF_State related rules:

- __Rule 13:__ Items in the IM/EX/IN states must be one of ESMF_States, ESMF_Bundles or ESMF_Fields.
- __Rule 14:__ MAPL places items in the IM/EX/IN states only through appropriate ‘spec’ calls from its SetServices.
- __Rule 15:__ All items the component places in the IM/EX/IN states must be defined on its grid. 
   If the grid is a MAPL_LocationStream, these items can be either at locations or on the associated ESMF_Grid. 
   Only in this sense can a component appear to expose two grids.
- __Rule 16:__ In addition to the ESMF Internal state that Mapl places in the component, 
   a component can have any number of privately defined ‘internal’ states. 
   We will refer to these as the component’s private states.
- __Rule 17:__ The private states, together with IN, fully define the component’s instantiatable state. 
   Private states must, therefore, be attached to the ESMF_GridComp.
- __Rule 18:__ Private states must be ‘named’ states when attached to the ESMF_GridComp. 
   MAPL uses the unnamed internal state in the component for its own purposes.
- __Rule 19:__ Items in the IM/EX/IN states may have Mapl and user attributes.
- __Rule 20:__ Items in the IN state can be given a FRIENDLY_TO_Mapl attribute that consists of 
   a list of other component’s names. 
   MAPL then places these items in the component’s EX state, and it is an error to add another 
   item with the same name to the Export. Why don’t we make this a regular EX state!?!?!
- __Rule 21:__ Items in the IM state are ‘read-only’ to the component, unless the component’s name 
   appears in the item’s FRIENDLY_TO attribute.
- __Rule 22:__ Items in the EX state can be assumed to be ‘read-only’ to other components, 
   unless a non-empty FRIENDLY_TO is present.
- __Rule 23:__ Components can only create or modify the FRIENDLY_TO attribute of items in its Import state.
- __Rule 24:__ Values of all Mapl attributes can be set only in SetServices.

Note that the restriction on items being on the component’s grid applies only to 
the items explicitly placed in the states by the component; 
Mapl itself may place other items in these states that are not ‘visible’ to the component. 
It is in this sense that the component ‘looks’ as though it has a single grid, even when its children use different grids.

### The recipe for writing a MAPL_GridComp

Writing an ESMF_GridComp consists of writing a SetServices and at least one phase of each of the registered IRF methods. 
Mapl provides a recipe for each of these tasks. 
We will focus first on the writing of a leaf component (an ESMF_GridComp that is a simple container for user code) 
and defer the discussion of how to extend the recipe to composite components and to putting together 
hierarchies to the Mapl_Connect section.

#### Writing a SetServices

Every non-trivial MAPL_GridComp has a SetServices from which MAPL_GenericSetServices is called, 
as illustrated in Example 2 (3.2.4.2). In this section we provide a complete recipe for writing 
SetServices and explain the role of MAPL_GenericSetServices 
(for variable declarations, please refer to the actual code).

The minimum we must do in SetServices is registering the private IRF methods (Run in Example 2) 
and then call MAPL_GenericSetServices. Everything else is optional. 
The following is a complete list in the order in which they would normally appear:

1. Get instance name and set-up traceback handle (Mapl_Utils: only used for optional error handling)

```fortran
    Iam = "SetServices"
    call ESMF_GridCompGet( gc, name=COMP_NAME, RC=STATUS )
    Iam = trim(COMP_NAME) // Iam
```

2. If using a private state, allocate it and put it in the gridded component with a unique name.
    Why can’t we just use an IN state!?!?!

```fortran
    allocate( dyn_internal_state, stat=status )
    wrap%dyn_state => dyn_internal_state
    call ESMF_UserCompSetInternalState ( gc,'FVstate',wrap,status )
```

3. Register any custom __IRF__ methods with MAPL. 
    MAPL will register them with ESMF. This step is present in practically all components.

```fortran
    call MAPL_GridCompSetEntryPoint ( gc, ESMF_SETINIT,  Initialize, rc=status)
    call MAPL_GridCompSetEntryPoint ( gc, ESMF_SETRUN,   Run1, rc=status)
    call MAPL_GridCompSetEntryPoint ( gc, ESMF_SETRUN,   Run2, rc=status)
    call MAPL_GridCompSetEntryPoint ( gc, ESMF_SETFINAL, Finalize, rc=status)
    call MAPL_GridCompSetEntryPoint ( gc, ESMF_SETREADRESTART, Coldstart, rc=status)
```
4. Set Data Services for the gridded component. 
    Data services are the heart of MAPL and practically all components will have to 
    do some state description. 
    An exception would be a composite component that serves only as a container for its children. 
    We explain the setting of data services in detail below.

```fortran
    call MAPL_AddImportSpec ( gc,               &
         SHORT_NAME = 'DUDT',                   &
         LONG_NAME  = 'eastward_wind_tendency', &
         UNITS      = 'm s-2',                  &
         DIMS       = MAPL_DimsHorzVert,        &
         VLOCATION  = MAPL_VLocationCenter,     &
         RC=STATUS )
```

Similarly for Export and Internal variables.

5. all MAPL_GenericSetServices. This is required. We discuss what it does next.

```fortran
    call MAPL_GenericSetServices( GC, RC=STATUS )
```

6. Set the Profiling timers (Mapl_Utils). This, of course, is optional.

```fortran
    call MAPL_TimerAdd( GC, name="INITIALIZE", RC=STATUS )
```

#### What does MAPL_GenericSetServices do?

As we showed in a previous example, MAPL_GenericSetServices can be used as a component’s SetServices, 
but this is not particularly useful. 
Its typical use is as a set-up routine for MAPL and is one of the last things called from the 
component’s own SetServices (step 5 above).
 MAPL_GenericSetServices performs the following tasks:

- If the Mapl object does not exist in the component, it allocates it and places it in the component. 
   Usually the object already exists at this point.
- Sets any of ESMF_GridComp’s IRF methods that have not been registered to the generic versions.
- Deals with the children. This is discussed further in Mapl_Connect.

#### Data Services

A crucial aspect of writing a MAPL component is describing the three states (IM/EX/IN). 
These are all ESMF_States. The IM/EX states are those passed in the calls to the IRF methods. 
The IN state is attached to the MAPL object by MAPL. 
In SetServices we must describe all items in all the three states. 
This will allow MAPL to create, initialze, and otherwise manipulate these data.

MAPL assumes that items in these states are either ESMF_Fields or ESMF_Bundles. 
Each item is described by a call to MAPL_AddxxxSpec, where ‘xxx’ stands for either Import/Export or Internal. 
These calls do not modify these states or create the items; 
they merely update tables of item specifications for the three states. 
The interface of MAPL_AddInternalSpec is as follows:

```fortran
  subroutine MAPL_AddInternalSpec(GC,                 &
                                  SHORT_NAME,         &
                                  LONG_NAME,          &
                                  UNITS,              &
                                  DIMS,               &
                                  VLOCATION,          &
                                  DATATYPE,           &
                                  NUM_SUBTITILES,     &
                                  REFRESH_INTERVAL,   &
                                  AVERAGING_INTERVAL, &
                                  DEFAULT,            &
                                  RESTART,            &
                                  HALOWIDTH,          &
                                  PRECISION,          &
                                  FRIENDLYTO,         &
                                  ADD2EXPORT,         &
                                  ATTR_RNAMES,        &
                                  ATTR_INAMES,        &
                                  ATTR_RVALUES,       &
                                  ATTR_IVALUES,       &
                                  UNGRIDDED_DIMS,     &
                                  RC)


    type (ESMF_GridComp)            , intent(INOUT)   :: GC
    character (len=*)               , intent(IN)      :: SHORT_NAME
    character (len=*)  , optional   , intent(IN)      :: LONG_NAME
    character (len=*)  , optional   , intent(IN)      :: UNITS
    integer            , optional   , intent(IN)      :: DIMS
    integer            , optional   , intent(IN)      :: DATATYPE
    integer            , optional   , intent(IN)      :: VLOCATION
    integer            , optional   , intent(IN)      :: NUM_SUBTILES
    integer            , optional   , intent(IN)      :: REFRESH_INTERVAL
    integer            , optional   , intent(IN)      :: AVERAGING_INTERVAL
    integer            , optional   , intent(IN)      :: PRECISION
    real               , optional   , intent(IN)      :: DEFAULT
    logical            , optional   , intent(IN)      :: RESTART
    character (len=*)  , optional   , intent(IN)      :: HALOWIDTH
    character (len=*)  , optional   , intent(IN)      :: FRIENDLYTO
    logical            , optional   , intent(IN)      :: ADD2EXPORT
    character (len=*)  , optional   , intent(IN)      :: ATTR_INAMES(:)
    character (len=*)  , optional   , intent(IN)      :: ATTR_RNAMES(:)
    integer            , optional   , intent(IN)      :: ATTR_IVALUES(:)
    real               , optional   , intent(IN)      :: ATTR_RVALUES(:)
    integer            , optional   , intent(IN)      :: UNGRIDDED_DIMS(:)
    integer            , optional   , intent(OUT)     :: RC
```

Only the ESMF_GridComp object GC and the SHORT_NAME are required. 
The latter is the handle used to access the variable; 
it is also the name used for the variable by MAPL in checkpoint files. 
For a description of the remaining optional arguments (as well as interfaces to MAPL_AddImportSpec and MAPL_ExportSpec),
please see the Mapl Reference Manual.

#### Writing and Initialize Method

Every MAPL component must make a call to MAPL_GenericInitialize. 
This can be done by letting the method default or by writing a component-specific 
Initialize method that invokes MAPL_GenericInitialize. 
In this section we provide a complete recipe for writing an Initialize routine and explain 
exactly what MAPL_GenericInitialize does.

The main reason to write a component-specific Initalize is to handle a private internal state. 
If all internal state variables can be put in the MAPL IN and checkpointed, 
using MAPL_GenericInitialize should suffice, at least for a simple component. 
A composite component may have other considerations; these will be discussed in later sections.

The following is a complete recipe in the order they would normally appear. Add commands for each step!?!?!

1. Get the instance name and setup traceback handle (Mapl_Utils: used for optional error handling)

```fortran
      Iam = "Initialize"
      call ESMF_GridCompGet(GC, name=COMP_NAME, CONFIG=CF, RC=STATUS )
      Iam = trim(COMP_NAME) // Iam
```

2. Get the Mapl object from the ESMF_GridComp. 
    _It will almost certainly be convenient to query this object during Initialization._

```fortran
      call MAPL_GetObjectFromGC(GC, MAPL,  RC=STATUS )
```

3. If profiling, turn on timer (Mapl_Utils)

```fortran
      call MAPL_TimerOn(MAPL,"TOTAL")
      call MAPL_TimerOn(MAPL,"INITIALIZE")
```

4. If you will use the configuration, get it from the ESMF_GridComp.
    _The configuration is to a component what the environment is to a UNIX process. We use it to keep all parameters, and so it is likely to be needed in Initalize. ‘Resource’ in MAPL parlance is the same as ‘Attribute’ in ESMF._

```fortran
      ! can use call ESMF_ConfigGetAttribute(cf, ...) but
      ! the preferred way is the following
      !--------------------------------------------------
      call MAPL_GetResource(MAPL, ...)
```

5. Get the component’s private Internal state from the ESMF_GridComp.
   _If you are writing your own Initialize you will almost certainly be using a private internal state._

```fortran
      call ESMF_UserCompGetInternalState(GC, 'FVstate', wrap, status)
      state => wrap%dyn_state
```

6. If you are changing the grid, it has to be done before invoking MAPL_GenericInitialize.
    _Remember, by default the component’s natural grid will be the one it was
    given at creation. If an Internal and/or an Import state is being restarted
    (as described in the next section - where!?!?!), the grids on those restarts
    will overide whatever is present when MAPL_GenericInitialize is called in
    the next step. So it only makes sense to change the grid if you are not doing
    restarts in MAPL_GenericInitialize. After returning from MAPL_GenericInitialize, the natural grid cannot be changed._

7. Invoke MAPL_GenericInitialize. 
   _his will do the automatic state initializations as described below. In the case of a composite component, it will also initialize the children._

```fortran
      call MAPL_GenericInitialize(GC, IMPORT, EXPORT, CLOCK,  RC=STATUS)
```

8. If you have put items that need to be explicitly initialized in the MAPL Internal state, get it from the MAPL object.
    _tems in the Mapl Internal state that were checkpointed will be restored by MAPL_GenericInitialize; other items will be set to their DEFAULT value. We need access to Internal only if we wish to overide these in Initialize. An example of this would be setting static arrays, like map factors, Coriolis, etc._

9. Query the Mapl object for information you need to do initialization. 
    _You probably need to know what the world looks like, so get LATS and LONS._

10. Query the configuration for parameters you need to do initialization.

11. Get pointers from the Mapl Internal and/or the private internal states.
     _These are the quantities you need to initialize._

12. Do the Initialization. 
     _For Internal items, you are overriding Mapl’s initialization, which was either from a restart or a default; for a private state you are on your own._

13. If you are profiling, turn off timer.

#### What does MAPL_GenericInitialize do?

MAPL_GenericInitialize does most of the instance-specific initializations of the Mapl objects. 
It also creates, and possibly allocates and initializes, items in the IM/EX/IN states. 
MAPL_GenericInitialize also makes the final decision on what will be the natural grid. 
And, as is the case for all generic IRF methods, it calls the children’s Initialize. 
The following list discusses these tasks in more detail: where is the list!?!?!

#### Writing a Finalize Method

Finalize parallels the Initialize. It is usually only needed if there is a private internal state.

MAPL_GenericFinalize does most of the instance-specific finalizations of the Mapl objects. 
It checkpoints the Import and Export states if a checkpoint file has been provided. 
It also destroys, and possibly deallocates items in the IM/EX(?)/IN states. 
MAPL_GenericFinalize also calls the children’s Finalize routines.
