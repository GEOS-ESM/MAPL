module mapl3g_StateItemAllocation
   implicit none(type, external)
   private

   ! Type
   public :: StateItemAllocation
   ! Operators
   public :: operator(==)
   public :: operator(/=)
   public :: operator(<)
   public :: operator(>=)
   ! Parameters
   public :: STATEITEM_ALLOCATION_INVALID
   public :: STATEITEM_ALLOCATION_CREATED
   public :: STATEITEM_ALLOCATION_INACTIVE
   public :: STATEITEM_ALLOCATION_ACTIVE
   public :: STATEITEM_ALLOCATION_CONNECTED
   public :: STATEITEM_ALLOCATION_ALLOCATED
   
   type :: StateItemAllocation
      private
      integer :: id = 0
   contains
      procedure :: to_string
   end type StateItemAllocation

   type(StateItemAllocation), parameter :: STATEITEM_ALLOCATION_INVALID = StateItemAllocation(-1)
   type(StateItemAllocation), parameter :: STATEITEM_ALLOCATION_CREATED = StateItemAllocation(0)
   type(StateItemAllocation), parameter :: STATEITEM_ALLOCATION_INACTIVE = StateItemAllocation(1)
   type(StateItemAllocation), parameter :: STATEITEM_ALLOCATION_ACTIVE = StateItemAllocation(2)
   type(StateItemAllocation), parameter :: STATEITEM_ALLOCATION_CONNECTED = StateItemAllocation(3)
   type(StateItemAllocation), parameter :: STATEITEM_ALLOCATION_ALLOCATED = StateItemAllocation(4)
   
   interface operator(==)
      procedure equal
   end interface operator(==)

   interface operator(/=)
      procedure not_equal
   end interface operator(/=)

   interface operator(<)
      procedure less_than
   end interface operator(<)
 
   interface operator(>=)
      procedure greater_than_or_equal
   end interface operator(>=)


   interface StateItemAllocation
      procedure new_StateItemAllocation
   end interface StateItemAllocation

contains

   function new_StateItemAllocation(str) result(allocation_status)
      type(StateItemAllocation) :: allocation_status
      character(*), intent(in) :: str

      select case (str)
      case ('INVALID')
         allocation_status = STATEITEM_ALLOCATION_INVALID
      case ('CREATED')
         allocation_status = STATEITEM_ALLOCATION_CREATED
      case ('INACTIVE')
         allocation_status = STATEITEM_ALLOCATION_INACTIVE
      case ('ACTIVE')
         allocation_status = STATEITEM_ALLOCATION_ACTIVE
      case ('CONNECTED')
         allocation_status = STATEITEM_ALLOCATION_CONNECTED
      case ('ALLOCATED')
         allocation_status = STATEITEM_ALLOCATION_ALLOCATED
      case default
         allocation_status = STATEITEM_ALLOCATION_INVALID
      end select
         
   end function new_StateItemAllocation

   function to_string(this) result(s)
      character(:), allocatable :: s
      class(StateItemAllocation), intent(in) :: this

      integer :: id

      id = this%id
      select case(id)
      case (STATEITEM_ALLOCATION_INVALID%id)
         s = "INVALID"
      case (STATEITEM_ALLOCATION_CREATED%id)
         s = "CREATED"
      case (STATEITEM_ALLOCATION_INACTIVE%id)
         s = "INACTIVE"
      case (STATEITEM_ALLOCATION_ACTIVE%id)
         s = "ACTIVE"
      case (STATEITEM_ALLOCATION_CONNECTED%id)
         s = "CONNECTED"
      case (STATEITEM_ALLOCATION_ALLOCATED%id)
         s = "ALLOCATED"
      case default
         s = "UNKNOWN"
      end select

   end function to_string


   elemental logical function equal(a, b)
      class(StateItemAllocation), intent(in) :: a, b
      equal = a%id == b%id
   end function equal

   elemental logical function not_equal(a, b)
      class(StateItemAllocation), intent(in) :: a, b
      not_equal = .not. (a%id == b%id)
   end function not_equal

   elemental logical function less_than(a, b)
      class(StateItemAllocation), intent(in) :: a, b
      less_than = a%id < b%id
   end function less_than

   elemental logical function greater_than_or_equal(a, b)
      class(StateItemAllocation), intent(in) :: a, b
      greater_than_or_equal = .not. (a%id < b%id)
   end function greater_than_or_equal

end module mapl3g_StateItemAllocation
