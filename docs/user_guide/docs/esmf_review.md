The [Earth System Modeling Framework (ESMF)](http://earthsystemmodeling.org/) is a
software package designed to provide some of the essential functions
needed by parallel, scalable earth system models in a
machine-independent way. ESMF is implemented as a collection of very
general programming classes that can be used both to construct ESMF
components and to connect them to one another.
These classes thus support modelers in building interoperable and portable codes. 
This design is illustrated
by the ESMF `sandwich' diagram below, where the
user's computational code sits between the two ESMF layers.


| ![fig_esmf_sandwich](figs/esmf_sandwich.png 'ESMF Sandwich') |
| :---: |
| Figure 2: *Schematic of the ESMF 'sandwich' architecture.* |
| *The framework consists of two parts, an upper level superstructure layer and a lower level infrastructure layer. User code is sandwiched between these two layers.* |



ESMF provides a way of structuring code components so that they can be used in many different user-written applications and contexts with minimal code modification, and so they can be coupled together in new configurations with relative ease. The idea is to create many components across a broad community, and so to encourage new collaborations and combinations.

The simplest ESMF implementation consists of building a Gridded
Component (an ESMF superstructure class) that encapsulates the user's
code, interfacing it to the framework by defining the ESMF callable
methods (hereafter, __IRF__ methods)):

- __Initialize__: called once to initialze all the parameters and variables (boundary conditions for instance) needed by the application. 
- __Run__: called for time stepping integration.
- __Finalize__: called once to clean (deallocate variables, close opened files, finalize MPI, etc.) the application.

This can actually be done without using any of the ESMF Infrastructure - a
strategy that fails to capitalize on some of ESMF's greatest strengths. 
Such `encapsulation' implementations have dominated the early adoptions of ESMF.

More sophisticated implementations put the user's data in ESMF
infrastructure objects (primarily ESMF_Fields) which can then be
manipulated by a wide array of ESMF methods to facilitate the coupling
of components with different data structures (i.e., that are on
different grids) and to insulate the user
from the architecture-specific implementation layers that are used for
inter-process or inter-processor communication, I/O, etc.

An ESMF component (represented by a box in Figure 1,
e.g. _solar_) consists of four (_or just one --- SetServices!?!?!_) public component interface functions performing specific roles:


* __SetServices:__ A component's __SetServices__ function is called when an
    instance (object) of the component is created and is the 
    _only required public interface_ of the component. It takes the instantiated component as
    the first argument, and an integer return code as the second. The goal of
    __SetServices__ is to register with the framework the component's user-defined routines
    that satisfy the __IRF__ requirements.
* __Initialize:__ A component's __Initialize__ function
    is called to configure an instance (object) of the component (allocate
    space, initialize data etc.). In addition to
    the component's instance, the arguments to this function include two
    __ESMF_States__  (one Import and one Export) and an __ESMF_Clock__. 
    The __ESMF_State__ variables are used to pass data between components. 
    The `Clock` is used to pass the simulation time counter to the component instance.
* __Run:__ A component's __Run__ function is called to
    carry out a cycle of the iteration that makes up the kernel of a component's
    computational algorithm. This function contains the kernel of user code
    and is called repeatedly as part of the
    component instance's life cycle. It takes the same argument list as
    __Initialize__.
__Finalize:__ A component's __Finalize__ function is
    called to terminate the the component instance cleanly (release space,
    write results etc.). It takes the same argument list as __Initialize__.


Some important aspects of the ESMF API that are relevant to
MAPL are:

- Data structures added to an __ESMF_State__ can have _arbitrary meta tags_ 
   associated with them
- An __ESMF_State__ can contain an __ESMF_State__ variable allowing _recursive nesting_
   of __ESMF_State__ variables.
- _Hierarchical organization of gridded components_: __ESMF_GridComps__ can be
   simple containers for user code (leaf components) or they can contain other
   gridded __ESMF_GridComps__ (composite components). The notion of composite components
   allows a straightforward way of organizing applications as a hierarchy of
   components. ESMF does not require a hierarchical organization, but it is
   the most natural way of connecting ESMF components.
- ESMF also defines the notion of _Coupler Components_.
   These are similar to gridded components, but are not intended for user code;
   rather, they house the transformations necessary to convert between \ex s
   of one component and Import s of another.


In designing ESMF, a deliberate decision was made to have the
framework provide these services in a very general way, and not to
prejudge how future models would use it or what programming models
would best suit future computer architectures. This generality is an
important strength of ESMF, but it is also an impediment to many users
that would prefer a more specific formulation for porting existing
codes or a better defined recipe for building new codes with ESMF. The
generality also impacts the interoperability of applications, since
the ESMF interfaces to the __IRF__ methods are general purpose, and they
carry little information (other than the grid definition) about the
physical content of the data moving in and out of the gridded
component.

The middle-ware layer implemented in MAPL  includes the following design elements:

1. Aides in constructing a component's __IRF__ methods
2. Provides easy-to-use tools for describing the contents of a
    component's Import and Export states, as well as adopting conventions for what
    must be described. But in no way specifying what the contents must be.
    MAPL extends the __ESMF_State__ concept to a component's __Internal__ state, and
    help it manage its persistent data.
3. Facilitates the use of __ESMF_Fields__ and thus of the ESMF Infrastructure layer.
4. Facilitates the coupling of components into complex applications. This
    requires a means of describing the connectivity between components and
    of using the description of the Import and Export states to couple
    components - MAPL adopts the hierarchical organization as its
    architecture for making complex applications and uses both composite
    gridded components and ESMF coupler components to establish connections
    between members of the hierarchy.

