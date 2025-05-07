module mapl3g_TransformId
   implicit none(type, external)
   private

   ! Type
   public :: TransformId
   ! Operators
   public :: operator(==)
   public :: operator(/=)
   public :: operator(<)
   ! Parameters
   public :: INVALID_TRANSFORM_ID
   public :: NULL_TRANSFORM_ID
   public :: TIME_INTERP_TRANSFORM_ID
   public :: GEOM_TRANSFORM_ID
   public :: UNITS_TRANSFORM_ID
   public :: VERTICAL_GRID_TRANSFORM_ID
   public :: FREQUENCY_TRANSFORM_ID
   public :: TYPEKIND_TRANSFORM_ID
   public :: EVAL_TRANSFORM_ID
   public :: EXTEND_TRANSFORM_ID
   
   type :: TransformId
      private
      integer :: id
   contains
      procedure :: to_string
   end type TransformId

   type(TransformId), parameter :: INVALID_TRANSFORM_ID = TransformId(-1)
   type(TransformId), parameter :: NULL_TRANSFORM_ID = TransformId(0)
   type(TransformId), parameter :: TIME_INTERP_TRANSFORM_ID = TransformId(1)
   type(TransformId), parameter :: GEOM_TRANSFORM_ID = TransformId(2)
   type(TransformId), parameter :: UNITS_TRANSFORM_ID = TransformId(3)
   type(TransformId), parameter :: VERTICAL_GRID_TRANSFORM_ID = TransformId(4)
   type(TransformId), parameter :: FREQUENCY_TRANSFORM_ID = TransformId(5)
   type(TransformId), parameter :: TYPEKIND_TRANSFORM_ID = TransformId(6)
   type(TransformId), parameter :: EVAL_TRANSFORM_ID = TransformId(7)
   type(TransformId), parameter :: EXTEND_TRANSFORM_ID = TransformId(8)
   
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
      class(TransformId), intent(in) :: this

      integer :: id

      id = this%id
      select case(id)
      case (NULL_TRANSFORM_ID%id)
         s = "NULL"
      case (TIME_INTERP_TRANSFORM_ID%id)
         s = "TIME_INTERP"
      case (GEOM_TRANSFORM_ID%id)
         s = "GEOM"
      case (UNITS_TRANSFORM_ID%id)
         s = "UNITS"
      case (VERTICAL_GRID_TRANSFORM_ID%id)
         s = "VERTICAL_GRID"
      case (FREQUENCY_TRANSFORM_ID%id)
         s = "FREQUENCY"
      case (TYPEKIND_TRANSFORM_ID%id)
         s = "TYPEKIND"
      case (EVAL_TRANSFORM_ID%id)
         s = "EVAL"
      case (EXTEND_TRANSFORM_ID%id)
         s = "EXTEND"
      case default
         s = "UNKNOWN"
      end select
   end function to_string


   elemental logical function equal(a, b)
      type(TransformId), intent(in) :: a, b
      equal = a%id == b%id
   end function equal

   elemental logical function not_equal(a, b)
      type(TransformId), intent(in) :: a, b
      not_equal = .not. (a%id == b%id)
   end function not_equal

   elemental logical function less_than(a, b)
      type(TransformId), intent(in) :: a, b
      less_than = a%id < b%id
   end function less_than

end module mapl3g_TransformId
