module mapl_CouplerPhases

   implicit none(type,external)
   private

   ! enum, bind(c) is used (rather than integer parameters) so that
   ! ESMF/NUOPC SetPhase routines receive C-interoperable integer kinds
   ! without requiring an explicit kind= argument at every call site.

   ! Phase indices
   public :: GENERIC_COUPLER_INITIALIZE
   public :: GENERIC_COUPLER_UPDATE
   public :: GENERIC_COUPLER_INVALIDATE
   public :: GENERIC_COUPLER_CLOCK_ADVANCE

   enum, bind(c)
      enumerator :: GENERIC_COUPLER_INITIALIZE = 1
      enumerator :: GENERIC_COUPLER_UPDATE
      enumerator :: GENERIC_COUPLER_INVALIDATE
      enumerator :: GENERIC_COUPLER_CLOCK_ADVANCE
   end enum

end module mapl_CouplerPhases
