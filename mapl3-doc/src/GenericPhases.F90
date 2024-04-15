module mapl3g_GenericPhases
   implicit none
   private
   
   ! Named constants
   ! Init phases
   public :: GENERIC_INIT_PHASE_SEQUENCE
   public :: GENERIC_INIT_ADVERTISE_GEOM
   public :: GENERIC_INIT_REALIZE_GEOM
   public :: GENERIC_INIT_ADVERTISE
   public :: GENERIC_INIT_POST_ADVERTISE
   public :: GENERIC_INIT_REALIZE
   public :: GENERIC_INIT_USER

   ! Run phases
   public :: GENERIC_RUN_CLOCK_ADVANCE
   public :: GENERIC_RUN_USER

   ! Finalize phases
   public :: GENERIC_FINALIZE_USER

   enum, bind(c)
      !!!! IMPORTANT: USER phase must be "1" !!!!
      enumerator :: GENERIC_INIT_USER = 1
      enumerator :: GENERIC_INIT_ADVERTISE_GEOM
      enumerator :: GENERIC_INIT_REALIZE_GEOM
      enumerator :: GENERIC_INIT_ADVERTISE
      enumerator :: GENERIC_INIT_POST_ADVERTISE
      enumerator :: GENERIC_INIT_REALIZE
   end enum

   enum, bind(c)
      enumerator :: GENERIC_RUN_CLOCK_ADVANCE = 1
      enumerator :: GENERIC_RUN_USER
   end enum

   enum, bind(c)
      !!!! IMPORTANT: USER phase must be "1" !!!!
      enumerator :: GENERIC_FINALIZE_USER = 1
   end enum

   integer, parameter :: GENERIC_INIT_PHASE_SEQUENCE(*) = &
        [ &
        GENERIC_INIT_ADVERTISE_GEOM, &
        GENERIC_INIT_REALIZE_GEOM, &
        GENERIC_INIT_ADVERTISE, &
        GENERIC_INIT_POST_ADVERTISE, &
        GENERIC_INIT_REALIZE, &
        GENERIC_INIT_USER &
        ]


end module mapl3g_GenericPhases
