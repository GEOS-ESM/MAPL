module mapl3g_ConservationType
   implicit none(type, external)
   private
   
   ! Type
   public :: ConservationType
   
   ! Helper function
   public :: conservation_type_from_string
   
   ! ConservationType parameters
   public :: CONSERVE_NONE
   public :: CONSERVE_MASS
   public :: CONSERVE_ENERGY
   public :: CONSERVE_MOMENTUM
   
   ! Operators
   public :: operator(==)
   public :: operator(/=)
   
   !---------------------------------------------------------------------------
   ! ConservationType - Describes what physical quantity is conserved
   !---------------------------------------------------------------------------
   type :: ConservationType
      private
      integer :: id = 0
      character(32) :: name = ""
   contains
      procedure :: to_string => conservation_type_to_string
      procedure :: is_valid => conservation_type_is_valid
   end type ConservationType
   
   interface ConservationType
      procedure new_ConservationType
   end interface ConservationType
   
   type(ConservationType), parameter :: CONSERVE_NONE = ConservationType(0, "CONSERVE_NONE")
   type(ConservationType), parameter :: CONSERVE_MASS = ConservationType(1, "CONSERVE_MASS")
   type(ConservationType), parameter :: CONSERVE_ENERGY = ConservationType(2, "CONSERVE_ENERGY")
   type(ConservationType), parameter :: CONSERVE_MOMENTUM = ConservationType(3, "CONSERVE_MOMENTUM")
   
   !---------------------------------------------------------------------------
   ! Equality operators
   !---------------------------------------------------------------------------
   interface operator(==)
      procedure conservation_type_equal
   end interface operator(==)
   
   interface operator(/=)
      procedure conservation_type_not_equal
   end interface operator(/=)
   
contains

   !---------------------------------------------------------------------------
   ! ConservationType procedures
   !---------------------------------------------------------------------------
   
   function new_ConservationType(name) result(ctype)
      character(*), intent(in) :: name
      type(ConservationType) :: ctype
      
      select case (trim(name))
      case ("CONSERVE_MASS")
         ctype = CONSERVE_MASS
      case ("CONSERVE_ENERGY")
         ctype = CONSERVE_ENERGY
      case ("CONSERVE_MOMENTUM")
         ctype = CONSERVE_MOMENTUM
      case default
         ctype = CONSERVE_NONE
      end select
   end function new_ConservationType
   
   function conservation_type_to_string(this) result(s)
      character(:), allocatable :: s
      class(ConservationType), intent(in) :: this
      
      s = trim(this%name)
   end function conservation_type_to_string
   
   elemental logical function conservation_type_equal(a, b)
      type(ConservationType), intent(in) :: a, b
      conservation_type_equal = (a%id == b%id)
   end function conservation_type_equal
   
   elemental logical function conservation_type_not_equal(a, b)
      type(ConservationType), intent(in) :: a, b
      conservation_type_not_equal = .not. (a%id == b%id)
   end function conservation_type_not_equal
   
   logical function conservation_type_is_valid(this)
      class(ConservationType), intent(in) :: this
      conservation_type_is_valid = (this%id >= 0 .and. this%id <= 3)
   end function conservation_type_is_valid
   
   function conservation_type_from_string(name, rc) result(ctype)
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(ConservationType) :: ctype
      
      ctype = ConservationType(name)
      if (present(rc)) rc = 0
   end function conservation_type_from_string
   
end module mapl3g_ConservationType
