module mapl3g_VectorBasisKind
   implicit none(type, external)
   private
   
   ! Type
   public :: VectorBasisKind
   ! Operators
   public :: operator(==)
   public :: operator(/=)
   ! Parameters
   public :: VECTOR_BASIS_KIND_INVALID
   public :: VECTOR_BASIS_KIND_GRID      ! Grid-aligned (i,j components)
   public :: VECTOR_BASIS_KIND_NS        ! North-South / East-West (geographic)
   
   type :: VectorBasisKind
      private
      integer :: id = -1
   contains
      procedure :: to_string
   end type VectorBasisKind

   ! Define parameter instances
   type(VectorBasisKind), parameter :: VECTOR_BASIS_KIND_INVALID = VectorBasisKind(-1)
   type(VectorBasisKind), parameter :: VECTOR_BASIS_KIND_GRID = VectorBasisKind(0)
   type(VectorBasisKind), parameter :: VECTOR_BASIS_KIND_NS = VectorBasisKind(1)
   
   interface VectorBasisKind
      procedure new_VectorBasisKind
   end interface VectorBasisKind
   
   interface operator(==)
      procedure equal
   end interface operator(==)
   
   interface operator(/=)
      procedure not_equal
   end interface operator(/=)

contains

   function new_VectorBasisKind(str) result(basis_kind)
      type(VectorBasisKind) :: basis_kind
      character(*), intent(in) :: str

      select case (trim(str))
      case ('GRID', 'grid')
         basis_kind = VECTOR_BASIS_KIND_GRID
      case ('NS', 'ns')
         basis_kind = VECTOR_BASIS_KIND_NS
      case default
         basis_kind = VECTOR_BASIS_KIND_INVALID
      end select
   end function new_VectorBasisKind

   function to_string(this) result(s)
      character(:), allocatable :: s
      class(VectorBasisKind), intent(in) :: this

      select case(this%id)
      case (VECTOR_BASIS_KIND_GRID%id)
         s = "GRID"
      case (VECTOR_BASIS_KIND_NS%id)
         s = "NS"
      case default
         s = "INVALID"
      end select
   end function to_string

   elemental logical function equal(a, b)
      class(VectorBasisKind), intent(in) :: a, b
      equal = a%id == b%id
   end function equal

   elemental logical function not_equal(a, b)
      class(VectorBasisKind), intent(in) :: a, b
      not_equal = .not. (a%id == b%id)
   end function not_equal

end module mapl3g_VectorBasisKind
