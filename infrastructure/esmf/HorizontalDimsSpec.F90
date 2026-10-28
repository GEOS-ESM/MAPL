module mapl_HorizontalDimsSpec_mod

   implicit none
   private

   public :: HorizontalDimsSpec
   public :: to_HorizontalDimsSpec
   public :: operator(==)
   public :: operator(/=)

   public :: HORIZONTAL_DIMS_UNKNOWN
   public :: HORIZONTAL_DIMS_NONE
   public :: HORIZONTAL_DIMS_GEOM
   public :: HORIZONTAL_DIMS_MIRROR

   ! Users should not be able to invent their own staggering, but we
   ! need to be able to declare type components of this type, so we
   ! cannot simply make the type private.  Instead we give it a
   ! default value that is invalid.  This class does not check the
   ! value, but higher level logic should check that returned values
   ! are of one of the defined parameters.
  
   type :: HorizontalDimsSpec
      private
      integer :: id = -1
   contains
      procedure :: to_string
   end type HorizontalDimsSpec

   type(HorizontalDimsSpec), parameter :: HORIZONTAL_DIMS_UNKNOWN = HorizontalDimsSpec(-1)
   type(HorizontalDimsSpec), parameter :: HORIZONTAL_DIMS_NONE = HorizontalDimsSpec(0)
   type(HorizontalDimsSpec), parameter :: HORIZONTAL_DIMS_GEOM = HorizontalDimsSpec(1)
   ! An import that takes the horizontal dims spec of what it connects to.
   ! Never a valid spec for a field being created.
   type(HorizontalDimsSpec), parameter :: HORIZONTAL_DIMS_MIRROR = HorizontalDimsSpec(2)

   interface operator(==)
      procedure equal_to
   end interface operator(==)

   interface operator(/=)
      procedure not_equal_to
   end interface operator(/=)
      
   
contains

   elemental logical function equal_to(a, b)
      type(HorizontalDimsSpec), intent(in) :: a
      type(HorizontalDimsSpec), intent(in) :: b
      equal_to = (a%id == b%id)
   end function equal_to
   
   elemental logical function not_equal_to(a, b)
      type(HorizontalDimsSpec), intent(in) :: a
      type(HorizontalDimsSpec), intent(in) :: b
      not_equal_to = .not. (a == b)
   end function not_equal_to
   
   function to_string(this) result(string)
      class(HorizontalDimsSpec), intent(in) :: this
      character(len=:), allocatable :: string

      select case(this%id)
      case(0)
         string = "HORIZONTAL_DIMS_NONE"
      case(1)
         string = "HORIZONTAL_DIMS_GEOM"
      case(2)
         string = "HORIZONTAL_DIMS_MIRROR"
      case default
         string = "HORIZONTAL_DIMS_UNKNOWN"
      end select
   end function to_string

   function to_HorizontalDimsSpec(string) result(horizontal_dims_spec)
      character(len=*), intent(in) :: string
      type(HorizontalDimsSpec) :: horizontal_dims_spec

      select case(string)
      case("HORIZONTAL_DIMS_NONE")
         horizontal_dims_spec = HORIZONTAL_DIMS_NONE
      case("HORIZONTAL_DIMS_GEOM")
         horizontal_dims_spec = HORIZONTAL_DIMS_GEOM
      case("HORIZONTAL_DIMS_MIRROR")
         horizontal_dims_spec = HORIZONTAL_DIMS_MIRROR
      case default
         horizontal_dims_spec = HORIZONTAL_DIMS_UNKNOWN
      end select
   end function to_HorizontalDimsSpec

end module mapl_HorizontalDimsSpec_mod
