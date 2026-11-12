!------------------------------------------------------------------------------
! Concrete stand-in for MockPayload, so gFTL map smoke tests have something
! allocatable to insert as the polymorphic mapped value.
!------------------------------------------------------------------------------
module MockPayloadConcrete_mod
   use MockPayload_mod, only: MockPayload
   implicit none
   private

   public :: MockPayloadConcrete

   type, extends(MockPayload) :: MockPayloadConcrete
      integer :: tag = 0
   end type MockPayloadConcrete

end module MockPayloadConcrete_mod
