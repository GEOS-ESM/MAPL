module mapl3g_ValidationMode
   implicit none(type, external)
   private

   ! Type
   public :: ValidationMode
   ! Operators
   public :: operator(==)
   public :: operator(/=)
   ! Parameters
   public :: VALIDATION_MODE_PERMISSIVE
   public :: VALIDATION_MODE_STRICT

   type :: ValidationMode
      private
      integer :: id = -1
   contains
      procedure :: to_string
   end type ValidationMode

   ! Define parameter instances
   type(ValidationMode), parameter :: &
        VALIDATION_MODE_PERMISSIVE = ValidationMode(0), &
        VALIDATION_MODE_STRICT     = ValidationMode(1)

   interface ValidationMode
      procedure new_from_string
   end interface ValidationMode

   interface operator(==)
      procedure equal
   end interface operator(==)

   interface operator(/=)
      procedure not_equal
   end interface operator(/=)

contains

   function new_from_string(str) result(mode)
      type(ValidationMode) :: mode
      character(*), intent(in) :: str

      select case (trim(str))
      case ('permissive', 'PERMISSIVE')
         mode = VALIDATION_MODE_PERMISSIVE
      case ('strict', 'STRICT')
         mode = VALIDATION_MODE_STRICT
      case default
         mode = VALIDATION_MODE_PERMISSIVE
      end select
   end function new_from_string

   function to_string(this) result(s)
      character(:), allocatable :: s
      class(ValidationMode), intent(in) :: this

      select case (this%id)
      case (VALIDATION_MODE_PERMISSIVE%id)
         s = 'permissive'
      case (VALIDATION_MODE_STRICT%id)
         s = 'strict'
      case default
         s = 'permissive'
      end select
   end function to_string

   elemental logical function equal(a, b)
      class(ValidationMode), intent(in) :: a, b
      equal = a%id == b%id
   end function equal

   elemental logical function not_equal(a, b)
      class(ValidationMode), intent(in) :: a, b
      not_equal = .not. (a%id == b%id)
   end function not_equal

end module mapl3g_ValidationMode
