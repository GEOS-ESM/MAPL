module mapl3g_AspectId
   implicit none(type, external)
   private

   ! Type
   public :: AspectId
   ! Operators
   public :: operator(==)
   public :: operator(/=)
   public :: operator(<)
   ! Parameters
   public :: CLASS_ASPECT_ID
   public :: GEOM_ASPECT_ID
   public :: UNITS_ASPECT_ID
   public :: ATTRIBUTES_ASPECT_ID
   public :: UNGRIDDED_DIMS_ASPECT_ID
   public :: VERTICAL_GRID_ASPECT_ID
   public :: FREQUENCY_ASPECT_ID
   public :: TYPEKIND_ASPECT_ID
   public :: INVALID_ASPECT_ID
   public :: MOCK_ASPECT_ID
   
   type :: AspectId
      private
      integer :: id
   contains
      procedure :: to_string
   end type AspectId

   type(AspectId), parameter :: INVALID_ASPECT_ID = AspectId(-1)
   type(AspectId), parameter :: CLASS_ASPECT_ID = AspectId(1)
   type(AspectId), parameter :: GEOM_ASPECT_ID = AspectId(2)
   type(AspectId), parameter :: UNITS_ASPECT_ID = AspectId(3)
   type(AspectId), parameter :: ATTRIBUTES_ASPECT_ID = AspectId(4)
   type(AspectId), parameter :: UNGRIDDED_DIMS_ASPECT_ID = AspectId(5)
   type(AspectId), parameter :: VERTICAL_GRID_ASPECT_ID = AspectId(6)
   type(AspectId), parameter :: FREQUENCY_ASPECT_ID = AspectId(7)
   type(AspectId), parameter :: TYPEKIND_ASPECT_ID = AspectId(8)

   type(AspectId), parameter :: MOCK_ASPECT_ID = AspectId(99)
   
   interface operator(==)
      procedure equal
   end interface operator(==)

   interface operator(/=)
      procedure not_equal
   end interface operator(/=)

   interface operator(<)
      procedure less_than
   end interface operator(<)
   
contains

   function to_string(this) result(s)
      character(:), allocatable :: s
      class(AspectId), intent(in) :: this

      integer :: id

      id = this%id
      select case(id)
      case (CLASS_ASPECT_ID%id)
         s = "CLASS"
      case (GEOM_ASPECT_ID%id)
         s = "GEOM"
      case (UNITS_ASPECT_ID%id)
         s = "UNITS"
      case (ATTRIBUTES_ASPECT_ID%id)
         s = "ATTRIBUTES"
      case (UNGRIDDED_DIMS_ASPECT_ID%id)
         s = "UNGRIDDED_DIMS"
      case (VERTICAL_GRID_ASPECT_ID%id)
         s = "VERTICAL_GRID"
      case (FREQUENCY_ASPECT_ID%id)
         s = "FREQUENCY"
      case (TYPEKIND_ASPECT_ID%id)
         s = "TYPEKIND"
      case default
         s = "UNKNOWN"
      end select
   end function to_string


   logical function equal(a, b)
      class(AspectId), intent(in) :: a, b
      equal = a%id == b%id
   end function equal

   logical function not_equal(a, b)
      class(AspectId), intent(in) :: a, b
      not_equal = .not. (a%id == b%id)
   end function not_equal

   logical function less_than(a, b)
      class(AspectId), intent(in) :: a, b
      less_than = a%id < b%id
   end function less_than

end module mapl3g_AspectId
