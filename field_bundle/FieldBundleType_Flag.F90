module mapl3g_FieldBundleType_Flag
   implicit none
   private

   public :: FieldBundleType_Flag
   public :: FIELDBUNDLETYPE_BASIC
   public :: FIELDBUNDLETYPE_VECTOR
   public :: FIELDBUNDLETYPE_BRACKET
   public :: FIELDBUNDLETYPE_VECTOR_BRACKET
   public :: FIELDBUNDLETYPE_INVALID

   public :: operator(==)
   public :: operator(/=)

   type :: FieldBundleType_Flag
      private
      integer :: id = -1
      character(32) :: name = "FIELDBUNDLETYPE_INVALID"
   contains
      procedure :: to_string
   end type Fieldbundletype_Flag

   interface FieldBundleType_Flag
      procedure new_FieldBundleType_Flag
   end interface FieldBundleType_Flag

   interface operator(==)
      procedure equal_to
   end interface operator(==)

   interface operator(/=)
      procedure not_equal_to
   end interface operator(/=)

   type(FieldBundleType_Flag), parameter :: FIELDBUNDLETYPE_BASIC = FieldBundleType_Flag(1, "FIELDBUNDLETYPE_BASIC")
   type(FieldBundleType_Flag), parameter :: FIELDBUNDLETYPE_VECTOR = FieldBundleType_Flag(2, "FIELDBUNDLETYPE_VECTOR")
   type(FieldBundleType_Flag), parameter :: FIELDBUNDLETYPE_BRACKET = FieldBundleType_Flag(3, "FIELDBUNDLETYPE_BRACKET")
   type(FieldBundleType_Flag), parameter :: FIELDBUNDLETYPE_VECTOR_BRACKET = FieldBundleType_Flag(4, "FIELDBUNDLETYPE_VECTOR_BRACKET")
   type(FieldBundleType_Flag), parameter :: FIELDBUNDLETYPE_INVALID = FieldBundleType_Flag(-1, "FIELDBUNDLETYPE_INVALID")

contains

   function new_FieldBundleType_Flag(name) result (type_flag)
      character(*), intent(in) :: name
      type(FieldBundleType_Flag) :: type_flag

      select case (name)
      case ("FIELDBUNDLETYPE_BASIC")
         type_flag = FIELDBUNDLETYPE_BASIC
      case ("FIELDBUNDLETYPE_VECTOR")
         type_flag = FIELDBUNDLETYPE_VECTOR
      case ("FIELDBUNDLETYPE_BRACKET")
         type_flag = FIELDBUNDLETYPE_BRACKET
      case ("FIELDBUNDLETYPE_VECTOR_BRACKET")
         type_flag = FIELDBUNDLETYPE_VECTOR_BRACKET
      case default
         type_flag = FIELDBUNDLETYPE_INVALID
      end select

   end function new_FieldBundleType_Flag

   function to_string(this) result(s)
      character(:), allocatable :: s
      class(FieldBundleType_Flag), intent(in) :: this

      s = trim(this%name)

   end function to_string


   elemental logical function equal_to(a,b)
      type(FieldBundleType_Flag), intent(in) :: a,b
      equal_to = a%id == b%id
   end function equal_to

   elemental logical function not_equal_to(a,b)
      type(FieldBundleType_Flag), intent(in) :: a,b
      not_equal_to = .not. (a%id == b%id)
   end function not_equal_to
   
end module mapl3g_FieldBundleType_Flag
