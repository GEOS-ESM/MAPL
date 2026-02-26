module mapl3g_NormalizationType
   implicit none(type, external)
   private
   
   ! Types
   public :: NormalizationType
   
   ! Helper functions
   public :: normalization_type_from_string
   
   ! NormalizationType parameters
   public :: NORMALIZE_NONE
   public :: NORMALIZE_DELP
   public :: NORMALIZE_DZ
   
   ! Operators
   public :: operator(==)
   public :: operator(/=)
   
   !---------------------------------------------------------------------------
   ! NormalizationType - Describes the normalization method for conservative regridding
   !---------------------------------------------------------------------------
   type :: NormalizationType
      private
      integer :: id = 0
      character(24) :: name = "NORMALIZE_NONE"
   contains
      procedure :: to_string => normalization_type_to_string
      procedure :: is_valid => normalization_type_is_valid
   end type NormalizationType
   
   interface NormalizationType
      procedure new_NormalizationType
   end interface NormalizationType
   
   type(NormalizationType), parameter :: NORMALIZE_NONE = NormalizationType(0, "NORMALIZE_NONE")
   type(NormalizationType), parameter :: NORMALIZE_DELP = NormalizationType(1, "NORMALIZE_DELP")
   type(NormalizationType), parameter :: NORMALIZE_DZ = NormalizationType(2, "NORMALIZE_DZ")
   
   !---------------------------------------------------------------------------
   ! Equality operators for NormalizationType
   !---------------------------------------------------------------------------
   interface operator(==)
      procedure normalization_type_equal
   end interface operator(==)
   
   interface operator(/=)
      procedure normalization_type_not_equal
   end interface operator(/=)
   
contains

   !---------------------------------------------------------------------------
   ! NormalizationType procedures
   !---------------------------------------------------------------------------
   
   function new_NormalizationType(name) result(ntype)
      character(*), intent(in) :: name
      type(NormalizationType) :: ntype
      
      select case (trim(name))
      case ("NORMALIZE_DELP")
         ntype = NORMALIZE_DELP
      case ("NORMALIZE_DZ")
         ntype = NORMALIZE_DZ
      case default
         ntype = NORMALIZE_NONE
      end select
   end function new_NormalizationType
   
   function normalization_type_to_string(this) result(s)
      character(:), allocatable :: s
      class(NormalizationType), intent(in) :: this
      
      s = trim(this%name)
   end function normalization_type_to_string
   
   elemental logical function normalization_type_equal(a, b)
      type(NormalizationType), intent(in) :: a, b
      normalization_type_equal = (a%id == b%id)
   end function normalization_type_equal
   
   elemental logical function normalization_type_not_equal(a, b)
      type(NormalizationType), intent(in) :: a, b
      normalization_type_not_equal = .not. (a%id == b%id)
   end function normalization_type_not_equal
   
   logical function normalization_type_is_valid(this)
      class(NormalizationType), intent(in) :: this
      normalization_type_is_valid = (this%id >= 0 .and. this%id <= 2)
   end function normalization_type_is_valid
   
   function normalization_type_from_string(name, rc) result(ntype)
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(NormalizationType) :: ntype
      
      ntype = NormalizationType(name)
      if (present(rc)) rc = 0
   end function normalization_type_from_string

end module mapl3g_NormalizationType
