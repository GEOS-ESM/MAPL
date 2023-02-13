module mapl3g_GenericPhases
   implicit none
   private
   
   ! Named constants
   public :: GENERIC_INIT_PHASE_SEQUENCE
   public :: GENERIC_INIT_GRID
   public :: GENERIC_INIT_ADVERTISE
   public :: GENERIC_INIT_REALIZE
   public :: GENERIC_INIT_USER

   enum, bind(c)
      !!!! IMPORTANT: USER phase must be "1" !!!!
      enumerator :: GENERIC_INIT_USER = 1
      enumerator :: GENERIC_INIT_GRID
      enumerator :: GENERIC_INIT_ADVERTISE
      enumerator :: GENERIC_INIT_REALIZE
   end enum

   integer, parameter :: GENERIC_INIT_PHASE_SEQUENCE(*) = [ &
        GENERIC_INIT_GRID, &
        GENERIC_INIT_ADVERTISE, &
        GENERIC_INIT_REALIZE, &
        GENERIC_INIT_USER &
        ]

end module mapl3g_GenericPhases
