module mapl3g_GenericPhases
   implicit none
   private
   
   ! Named constants
   ! Init phases
   public :: GENERIC_INIT_PHASE_SEQUENCE
   public :: GENERIC_INIT_SET_CLOCK
   public :: GENERIC_INIT_GEOM_A
   public :: GENERIC_INIT_GEOM_B
   public :: GENERIC_INIT_ADVERTISE
   public :: GENERIC_INIT_MODIFY_ADVERTISED
   public :: GENERIC_INIT_REALIZE
   public :: GENERIC_INIT_READ_RESTART
   public :: GENERIC_INIT_USER

   ! Run phases
   public :: GENERIC_RUN_OFFSET
   public :: GENERIC_RUN_CLOCK_ADVANCE
   public :: GENERIC_RUN_USER

   ! Finalize phases
   public :: GENERIC_FINALIZE_USER

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
      enumerator :: GENERIC_INIT_REALIZE
      enumerator :: GENERIC_INIT_READ_RESTART
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
        GENERIC_INIT_SET_CLOCK, &
        GENERIC_INIT_GEOM_A, &
        GENERIC_INIT_GEOM_B, &
        GENERIC_INIT_ADVERTISE, &
        GENERIC_INIT_MODIFY_ADVERTISED, &
        GENERIC_INIT_REALIZE, &
        GENERIC_INIT_READ_RESTART, & ! IMPORTANT: Goes before INIT_USER
        GENERIC_INIT_USER &
        ]

end module mapl3g_GenericPhases
