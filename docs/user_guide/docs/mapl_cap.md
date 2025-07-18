## MAPL Cap

The main program (or, in ESMF lingo, the Cap) of any ESMF application is
provided by the user. In MAPL, it initiates the execution of each of the
sub-hierarchies of the application (`SetServices`, `Initialize`, `Run`,
`Finalize`, and the new `Record`).
Usually, each of these, except `Run` and `Record`, is executed only once.

 In MAPL applications, the Cap contains the time loop.
 The hierarchy of Run methods is called each time through the loop,
 returning control to the Cap after each round trip down and back up
 the hierarchy. The Run hierarchy must be invoked once and only once
 each time through the time loop.

 The time loop advances the current time of the Application Clock -- the ESMF
 Clock that is passed down to all registered methods of all components in
 the hierarchy. MAPL applications require that the Application Clock be
 _ticked_ after the `Run` method is invoked, but before the `Record`.
 The time interval of the Application Clock is called the heartbeat in MAPL.

 Since the Cap is a main program, it is not an ESMF Gridded Component.
 A MAPL component's cap, however, has "children" and treats them much
 as any Composite Component would. In particular, it registers them with
 MAPL by invoking a `MAPL_AddChild` for each one.

 The Cap for a MAPL application has only two children: a single instance
 of the Root Component of a MAPL hierarchy and a single instance of MAPL's
 own History Component. The History Component services the computational
 components' diagnostic output.

 As might be expected from this simple set of rules, all MAPL Caps are
 very similar. We have therefore gathered the basic MAPL Cap functionality
 in a single Fortran subroutine (MAPL_Cap) that is included in the
 MAPL library.

 For basic MAPL Caps, a call to this subroutine is the only required
 executable statement of the main program. As an example, the following code
 is the entire main program of the Held-Suarez example:

```fortran
 #define I_AM_MAIN
 #include "MAPL_Generic.h"

 Program Main

   use MAPLBase_Mod
   use GEOS_AgcmSimpleGridCompMod, only:  ROOT_SetServices => SetServices

   implicit none
   integer           :: STATUS
   character(len=18) :: Iam="Main"

   call MAPL_CAP(ROOT_SetServices, rc=STATUS)
   _VERIFY(STATUS)

   call exit(0)

 end Program Main
```

Notice that, besides calling the MAPL_Cap subroutine, the only purpose
of this program is to identify the root component of the MAPL hierarchy
by accessing its SetServices through use association. The rest of the code
is a bit of MAPL boilerplate. In fact, doing away with MAPL and Fortran
niceties, the code can be reduced to:

```fortran
 Program Main

   use GEOS_AgcmSimpleGridCompMod, only:  ROOT_SetServices => SetServices
   call MAPL_CAP(ROOT_SetServices)

 end Program Main
```

In either case, only the name `GEOS_AgcmSimpleGridCompMod` needs to be
modified to use these codes in another application.

In using MAPL, it is important to know exactly what boilerplate routines,
such as MAPL_Cap, are doing for you, so that you can supplement or replace
them with custom code, if necessary. MAPL_Cap is simple enough that it is
probably easier to look at its full code than to try to describe its
functioning in detail. Studying this code should also be useful if one
decides to write a more specialized custom version to replace it.

For additional information, please consult the document
[CapGridComp.md](../../../gridcomps/Cap/CapGridComp.md).
