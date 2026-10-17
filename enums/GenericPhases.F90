module mapl_GenericPhases_mod
   implicit none(type,external)
   private
   
   ! enum, bind(c) is used (rather than integer parameters) so that
   ! ESMF/NUOPC SetPhase routines receive C-interoperable integer kinds
   ! without requiring an explicit kind= argument at every call site.

   ! Named constants
   ! Init phases
   public :: GENERIC_INIT_PHASE_SEQUENCE
   public :: GENERIC_INIT_SET_CLOCK
   public :: GENERIC_INIT_GEOM_A
   public :: GENERIC_INIT_GEOM_B
   public :: GENERIC_INIT_ADVERTISE
   public :: GENERIC_INIT_MODIFY_ADVERTISED
   public :: GENERIC_INIT_REALIZE_PROVIDED
   public :: GENERIC_INIT_ACCEPT_TRANSFER
   public :: GENERIC_INIT_REALIZE_ACCEPTED
   public :: GENERIC_INIT_GRAPH_FREEZE
   public :: GENERIC_INIT_READ_RESTART
   public :: GENERIC_INIT_USER

   ! Run phases
   public :: GENERIC_RUN_OFFSET
   public :: GENERIC_RUN_CLOCK_ADVANCE
   public :: GENERIC_RUN_USER

   ! Finalize phases
   public :: GENERIC_FINALIZE_USER

   ! Internal (in-memory) checkpoint phases
   public :: GENERIC_INTERNAL_READ_RESTART
   public :: GENERIC_INTERNAL_WRITE_RESTART

   enum, bind(c)
      !!!! IMPORTANT: USER phase must be "1" !!!!
      enumerator :: GENERIC_INIT_USER = 1
      ! Phases that should be within NUOPC initialize_advertise
      enumerator :: GENERIC_INIT_SET_CLOCK
      enumerator :: GENERIC_INIT_GEOM_A
      enumerator :: GENERIC_INIT_GEOM_B
      enumerator :: GENERIC_INIT_ADVERTISE
      ! Phases that should be within NUOPC modify_advertised
      enumerator :: GENERIC_INIT_MODIFY_ADVERTISED
      ! Phases that should be within NUOPC realize
      enumerator :: GENERIC_INIT_REALIZE_PROVIDED
      enumerator :: GENERIC_INIT_ACCEPT_TRANSFER
      enumerator :: GENERIC_INIT_REALIZE_ACCEPTED
      ! openspec/changes/hierarchy-wide-component-graph, tasks.md Group 9
      ! (design.md D7, "Freeze becomes one hierarchy-wide call"): a
      ! dedicated, explicitly NON-RECURSIVE phase - the handler
      ! (OuterMetaComponent%initialize_graph_freeze) does not call
      ! recurse() the way every other init phase does; it is a no-op for
      ! any non-root component and, only at the true hierarchy root,
      ! performs its own single explicit whole-tree walk
      ! (GraphBuilder%freeze_hierarchy) that freezes every component's
      ! ComponentGraph exactly once. Appears exactly ONCE in
      ! GENERIC_INIT_PHASE_SEQUENCE below (unlike GENERIC_INIT_
      ! ACCEPT_TRANSFER, which appears twice for the realize/accept/
      ! realize two-pass schedule) - this single occurrence is what makes
      ! "freeze once, after everything else is done" require no
      ! additional idempotency guard at this call site. Placed after the
      ! second GENERIC_INIT_REALIZE_ACCEPTED (the last point any graph
      ! structure is still being mutated) and before GENERIC_INIT_
      ! READ_RESTART (which only reads graph-adjacent state, never
      ! mutates structure).
      enumerator :: GENERIC_INIT_GRAPH_FREEZE
      enumerator :: GENERIC_INIT_READ_RESTART
   end enum

   ! Internal restart phases are registered directly against
   ! ESMF_METHOD_READRESTART / ESMF_METHOD_WRITERESTART and are
   ! intentionally NOT part of GENERIC_INIT_PHASE_SEQUENCE below.
   !
   ! GENERIC_INTERNAL_WRITE_RESTART must differ from the default
   ! (ESMF_SINGLEPHASE == 1) phase already used by the existing
   ! netCDF write_restart registration for ESMF_METHOD_WRITERESTART;
   ! otherwise the two phases would collide on that method.
   ! GENERIC_INTERNAL_READ_RESTART has no such collision since no
   ! prior phase is registered for ESMF_METHOD_READRESTART.
   enum, bind(c)
      enumerator :: GENERIC_INTERNAL_READ_RESTART = 1
      enumerator :: GENERIC_INTERNAL_WRITE_RESTART = 2
   end enum

   ! We start the generic run phases at a high index to allow for
   ! multiple user run phases.  And we want to avoid computing
   ! offests.
   integer, parameter :: GENERIC_RUN_OFFSET = 1000
   enum, bind(c)
      enumerator :: GENERIC_RUN_USER = 1
      enumerator :: GENERIC_RUN_CLOCK_ADVANCE = GENERIC_RUN_OFFSET + 1
   end enum

   enum, bind(c)
      !!!! IMPORTANT: USER phase must be "1" !!!!
      enumerator :: GENERIC_FINALIZE_USER = 1
   end enum

   integer, parameter :: GENERIC_INIT_PHASE_SEQUENCE(*) = &
        [ &

        ! label_Advertise
        GENERIC_INIT_SET_CLOCK, & ! 1
        GENERIC_INIT_ADVERTISE, & ! 2

        ! label_ModifyAdvertise
        GENERIC_INIT_MODIFY_ADVERTISED, & ! 3

        ! label_RealizeProvided
        GENERIC_INIT_GEOM_A, &
        GENERIC_INIT_GEOM_B, &
        GENERIC_INIT_REALIZE_PROVIDED, &

        ! label_AcceptTransfer
        GENERIC_INIT_ACCEPT_TRANSFER, & ! 7

        ! label_RealizeAccepted
        GENERIC_INIT_REALIZE_ACCEPTED, & ! 8

        ! Second pass
        GENERIC_INIT_REALIZE_PROVIDED, & ! 9
        GENERIC_INIT_ACCEPT_TRANSFER, &  ! 10
        GENERIC_INIT_REALIZE_ACCEPTED, & ! 11

        ! openspec/changes/hierarchy-wide-component-graph, tasks.md
        ! Group 9: exactly one occurrence, after the second (and last)
        ! GENERIC_INIT_REALIZE_ACCEPTED above - see this phase's own
        ! enumerator comment above for why it needs no idempotency
        ! guard the way GENERIC_INIT_ACCEPT_TRANSFER's own two
        ! occurrences do.
        GENERIC_INIT_GRAPH_FREEZE, &     ! 12

        ! label_DataInitialize
        GENERIC_INIT_READ_RESTART, & ! IMPORTANT: Goes before INIT_USER
        GENERIC_INIT_USER &
        ]

end module mapl_GenericPhases_mod
