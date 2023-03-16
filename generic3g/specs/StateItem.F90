module mapl3g_StateItem
   use esmf
   implicit none
   private

   public :: MAPL_STATEITEM_UNKNOWN
   public :: MAPL_STATEITEM_FIELD
   public :: MAPL_STATEITEM_FIELDBUNDLE
   public :: MAPL_STATEITEM_STATE
   public :: MAPL_STATEITEM_SERVICE_PROVIDER
   public :: MAPL_STATEITEM_SERVICE_SUBSCRIBER

   ! This following must be public for internal MAPL use, but should not be
   ! exported to the public API of MAPL

   type(ESMF_StateItem_Flag), parameter :: &
        MAPL_STATEITEM_UNKNOWN = ESMF_STATEITEM_UNKNOWN, &
        MAPL_STATEITEM_FIELD = ESMF_STATEITEM_FIELD, &
        MAPL_STATEITEM_FIELDBUNDLE = ESMF_STATEITEM_FIELDBUNDLE, &
        MAPL_STATEITEM_STATE = ESMF_STATEITEM_STATE, &
        MAPL_STATEITEM_SERVICE_PROVIDER = ESMF_StateItem_Flag(201), &
        MAPL_STATEITEM_SERVICE_SUBSCRIBER = ESMF_StateItem_Flag(202)

end module Mapl3g_StateItem
