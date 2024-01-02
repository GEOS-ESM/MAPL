module mapl3g_GenericPhases
   implicit none
   private
   
   ! Named constants
   public :: GENERIC_INIT_PHASE_SEQUENCE
   public :: GENERIC_INIT_CLOCK
   public :: GENERIC_INIT_GEOM
   public :: GENERIC_INIT_ADVERTISE
   public :: GENERIC_INIT_POST_ADVERTISE
   public :: GENERIC_INIT_REALIZE
   public :: GENERIC_INIT_USER

   public :: GENERIC_FINALIZE_USER
   enum, bind(c)
      !!!! IMPORTANT: USER phase must be "1" !!!!
      enumerator :: GENERIC_INIT_USER = 1
      enumerator :: GENERIC_INIT_CLOCK
      enumerator :: GENERIC_INIT_GEOM
      enumerator :: GENERIC_INIT_ADVERTISE
      enumerator :: GENERIC_INIT_POST_ADVERTISE
      enumerator :: GENERIC_INIT_REALIZE
   end enum

   enum, bind(c)
      !!!! IMPORTANT: USER phase must be "1" !!!!
      enumerator :: GENERIC_FINALIZE_USER = 1
   end enum

   integer, parameter :: GENERIC_INIT_PHASE_SEQUENCE(*) = [ &
        GENERIC_INIT_GEOM, &
        GENERIC_INIT_ADVERTISE, &
        GENERIC_INIT_POST_ADVERTISE, &
        GENERIC_INIT_REALIZE, &
        GENERIC_INIT_USER &
        ]

end module mapl3g_GenericPhases
