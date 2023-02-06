module mapl3g_GenericPhases
   implicit none
   private
   
   ! Named constants
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


end module mapl3g_GenericPhases
