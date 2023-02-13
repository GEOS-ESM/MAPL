module mapl3g_StateItemSpecTypeId
   implicit none
   private

   public :: MAPL_TYPE_ID_INVALID
   public :: MAPL_TYPE_ID_FIELD
   public :: MAPL_TYPE_ID_BUNDLE
   public :: MAPL_TYPE_ID_STATE
   public :: MAPL_TYPE_ID_SERVICE_PROVIDER
   public :: MAPL_TYPE_ID_SERVICE_SUBSCRIBER

   ! This following must be public for internal MAPL use, but should not be
   ! exported to the public API of MAPL
   public :: StateItemSpecTypeId
   public :: operator(==)
   public :: operator(/=)


   type :: StateItemSpecTypeId
      private
      integer :: id = -1
   end type StateItemSpecTypeId

   type(StateItemSpecTypeId), parameter :: &
        MAPL_TYPE_ID_INVALID = StateItemSpecTypeId(-1), &
        MAPL_TYPE_ID_FIELD = StateItemSpecTypeId(1), &
        MAPL_TYPE_ID_BUNDLE = StateItemSpecTypeId(2), &
        MAPL_TYPE_ID_STATE = StateItemSpecTypeId(3), &
        MAPL_TYPE_ID_SERVICE_PROVIDER = StateItemSpecTypeId(4), &
        MAPL_TYPE_ID_SERVICE_SUBSCRIBER = StateItemSpecTypeId(5)

   interface operator(==)
      module procedure :: equal_to
   end interface operator(==)

   interface operator(/=)
      module procedure :: not_equal_to
   end interface operator(/=)

contains

   pure logical function equal_to(a, b)
      type(StateItemSpecTypeId), intent(in) :: a, b

      equal_to = (a%id == b%id)
   end function equal_to
   
   pure logical function not_equal_to(a, b)
      type(StateItemSpecTypeId), intent(in) :: a, b

      not_equal_to = .not. (a == b)
   end function not_equal_to

end module Mapl3g_StateItemSpecTypeId
