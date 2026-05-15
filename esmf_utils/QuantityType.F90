module mapl3g_QuantityType
   implicit none(type, external)
   private
   
    ! Types
    public :: QuantityType
    public :: MixingRatioBasis
    
    ! Helper functions
    public :: quantity_type_from_string
    public :: mixing_ratio_basis_from_string
    
    ! QuantityType parameters
   public :: QUANTITY_UNKNOWN
   public :: QUANTITY_MIXING_RATIO
   public :: QUANTITY_CONCENTRATION
   public :: QUANTITY_TEMPERATURE
   public :: QUANTITY_PRESSURE
   public :: QUANTITY_EXTENSIVE
   
   ! MixingRatioBasis parameters
   public :: BASIS_NONE
   public :: BASIS_WET_MASS
   public :: BASIS_DRY_MASS
   public :: BASIS_VOLUME
   
   ! Operators
   public :: operator(==)
   public :: operator(/=)
   
   !---------------------------------------------------------------------------
   ! QuantityType - Describes the physical nature of a field quantity
   !---------------------------------------------------------------------------
    type :: QuantityType
       private
       integer :: id = -1
       character(32) :: name = "QUANTITY_UNKNOWN"
    contains
       procedure :: to_string => quantity_type_to_string
       procedure :: is_valid => quantity_type_is_valid
    end type QuantityType
   
   interface QuantityType
      procedure new_QuantityType
   end interface QuantityType
   
   type(QuantityType), parameter :: QUANTITY_UNKNOWN = QuantityType(-1, "QUANTITY_UNKNOWN")
   type(QuantityType), parameter :: QUANTITY_MIXING_RATIO = QuantityType(1, "QUANTITY_MIXING_RATIO")
   type(QuantityType), parameter :: QUANTITY_CONCENTRATION = QuantityType(2, "QUANTITY_CONCENTRATION")
   type(QuantityType), parameter :: QUANTITY_TEMPERATURE = QuantityType(3, "QUANTITY_TEMPERATURE")
   type(QuantityType), parameter :: QUANTITY_PRESSURE = QuantityType(4, "QUANTITY_PRESSURE")
   type(QuantityType), parameter :: QUANTITY_EXTENSIVE = QuantityType(5, "QUANTITY_EXTENSIVE")
   
   !---------------------------------------------------------------------------
   ! MixingRatioBasis - Describes the basis for mixing ratio quantities
   !---------------------------------------------------------------------------
    type :: MixingRatioBasis
       private
       integer :: id = 0
       character(24) :: name = "BASIS_NONE"
    contains
       procedure :: to_string => basis_to_string
       procedure :: is_valid => basis_is_valid
    end type MixingRatioBasis
   
   interface MixingRatioBasis
      procedure new_MixingRatioBasis
   end interface MixingRatioBasis
   
   type(MixingRatioBasis), parameter :: BASIS_NONE = MixingRatioBasis(0, "BASIS_NONE")
   type(MixingRatioBasis), parameter :: BASIS_WET_MASS = MixingRatioBasis(1, "BASIS_WET_MASS")
   type(MixingRatioBasis), parameter :: BASIS_DRY_MASS = MixingRatioBasis(2, "BASIS_DRY_MASS")
   type(MixingRatioBasis), parameter :: BASIS_VOLUME = MixingRatioBasis(3, "BASIS_VOLUME")
   
   !---------------------------------------------------------------------------
   ! Equality operators
   !---------------------------------------------------------------------------
   interface operator(==)
      procedure quantity_type_equal
      procedure basis_equal
   end interface operator(==)
   
   interface operator(/=)
      procedure quantity_type_not_equal
      procedure basis_not_equal
   end interface operator(/=)
   
contains

   !---------------------------------------------------------------------------
   ! QuantityType procedures
   !---------------------------------------------------------------------------
   
   function new_QuantityType(name) result(qtype)
      character(*), intent(in) :: name
      type(QuantityType) :: qtype
      
      select case (trim(name))
      case ("QUANTITY_MIXING_RATIO")
         qtype = QUANTITY_MIXING_RATIO
      case ("QUANTITY_CONCENTRATION")
         qtype = QUANTITY_CONCENTRATION
      case ("QUANTITY_TEMPERATURE")
         qtype = QUANTITY_TEMPERATURE
      case ("QUANTITY_PRESSURE")
         qtype = QUANTITY_PRESSURE
      case ("QUANTITY_EXTENSIVE")
         qtype = QUANTITY_EXTENSIVE
      case default
         qtype = QUANTITY_UNKNOWN
      end select
   end function new_QuantityType
   
   function quantity_type_to_string(this) result(s)
      character(:), allocatable :: s
      class(QuantityType), intent(in) :: this
      
      s = trim(this%name)
   end function quantity_type_to_string
   
   elemental logical function quantity_type_equal(a, b)
      type(QuantityType), intent(in) :: a, b
      quantity_type_equal = (a%id == b%id)
   end function quantity_type_equal
   
    elemental logical function quantity_type_not_equal(a, b)
       type(QuantityType), intent(in) :: a, b
       quantity_type_not_equal = .not. (a%id == b%id)
    end function quantity_type_not_equal
    
    logical function quantity_type_is_valid(this)
       class(QuantityType), intent(in) :: this
       quantity_type_is_valid = (this%id >= -1 .and. this%id <= 5)
    end function quantity_type_is_valid
    
    function quantity_type_from_string(name, rc) result(qtype)
       character(*), intent(in) :: name
       integer, optional, intent(out) :: rc
       type(QuantityType) :: qtype
       
       qtype = QuantityType(name)
       if (present(rc)) rc = 0
    end function quantity_type_from_string
    
    !---------------------------------------------------------------------------
    ! MixingRatioBasis procedures
    !---------------------------------------------------------------------------
   
   function new_MixingRatioBasis(name) result(basis)
      character(*), intent(in) :: name
      type(MixingRatioBasis) :: basis
      
      select case (trim(name))
      case ("BASIS_WET_MASS")
         basis = BASIS_WET_MASS
      case ("BASIS_DRY_MASS")
         basis = BASIS_DRY_MASS
      case ("BASIS_VOLUME")
         basis = BASIS_VOLUME
      case default
         basis = BASIS_NONE
      end select
   end function new_MixingRatioBasis
   
   function basis_to_string(this) result(s)
      character(:), allocatable :: s
      class(MixingRatioBasis), intent(in) :: this
      
      s = trim(this%name)
   end function basis_to_string
   
   elemental logical function basis_equal(a, b)
      type(MixingRatioBasis), intent(in) :: a, b
      basis_equal = (a%id == b%id)
   end function basis_equal
   
    elemental logical function basis_not_equal(a, b)
       type(MixingRatioBasis), intent(in) :: a, b
       basis_not_equal = .not. (a%id == b%id)
    end function basis_not_equal
    
    logical function basis_is_valid(this)
       class(MixingRatioBasis), intent(in) :: this
       basis_is_valid = (this%id >= 0 .and. this%id <= 3)
    end function basis_is_valid
    
    function mixing_ratio_basis_from_string(name, rc) result(basis)
       character(*), intent(in) :: name
       integer, optional, intent(out) :: rc
       type(MixingRatioBasis) :: basis
       
       basis = MixingRatioBasis(name)
       if (present(rc)) rc = 0
    end function mixing_ratio_basis_from_string
    
end module mapl3g_QuantityType
