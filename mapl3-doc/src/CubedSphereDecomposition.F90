module mapl3g_CubedSphereDecomposition
   use mapl_KeywordEnforcer
   use esmf
   implicit none
   private

   public :: CubedSphereDecomposition
   public :: make_CubedSphereDecomposition
   public :: operator(==)
   public :: operator(/=)

   type :: CubedSphereDecomposition
      private
      integer, allocatable :: x_distribution(:)
      integer, allocatable :: y_distribution(:)
   contains
      procedure :: get_x_distribution
      procedure :: get_y_distribution
   end type CubedSphereDecomposition

   interface CubedSphereDecomposition
      procedure :: new_CubedSphereDecomposition_basic
      procedure :: new_CubedSphereDecomposition_petcount
      procedure :: new_CubedSphereDecomposition_topo
   end interface CubedSphereDecomposition

   interface make_CubedSphereDecomposition
      procedure :: make_CubedSphereDecomposition_current_vm
      procedure :: make_CubedSphereDecomposition_vm
   end interface make_CubedSphereDecomposition

   interface operator(==)
      procedure equal_to
   end interface operator(==)

   interface operator(/=)
      procedure not_equal_to
   end interface operator(/=)

   integer, parameter :: R8 = ESMF_KIND_R8
   interface

      ! Constructors
      pure module function new_CubedSphereDecomposition_basic(x_distribution, y_distribution) result(decomp)
         type(CubedSphereDecomposition) :: decomp
         integer, intent(in) :: x_distribution(:)
         integer, intent(in) :: y_distribution(:)
      end function new_CubedSphereDecomposition_basic

      ! Keyword enforced to avoid ambiguity with '_topo' interface
      pure module function new_CubedSphereDecomposition_petcount(dims, unusable, petCount) result(decomp)
         type(CubedSphereDecomposition) :: decomp
         integer, intent(in) :: dims(2)
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, intent(in) :: petCount
      end function new_CubedSphereDecomposition_petcount

      ! Keyword enforced to avoid ambiguity with '_petcount' interface
      pure module function new_CubedSphereDecomposition_topo(dims, unusable, topology) result(decomp)
         type(CubedSphereDecomposition) :: decomp
         integer, intent(in) :: dims(2)
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, intent(in) :: topology(2)
      end function new_CubedSphereDecomposition_topo

      ! accessors
      pure module function get_x_distribution(decomp) result(x_distribution)
         integer, allocatable :: x_distribution(:)
         class(CubedSphereDecomposition), intent(in) :: decomp
      end function get_x_distribution

      pure module function get_y_distribution(decomp) result(y_distribution)
         integer, allocatable :: y_distribution(:)
         class(CubedSphereDecomposition), intent(in) :: decomp
      end function get_y_distribution

      ! Static factory methods
      module function make_CubedSphereDecomposition_current_vm(dims, rc) result(decomp)
         type(CubedSphereDecomposition) :: decomp
         integer, intent(in) :: dims(2)
         integer, optional, intent(out) :: rc
      end function make_CubedSphereDecomposition_current_vm

      module function make_CubedSphereDecomposition_vm(dims, vm, rc) result(decomp)
         type(CubedSphereDecomposition) :: decomp
         integer, intent(in) :: dims(2)
         type(ESMF_VM), intent(in) :: vm
         integer, optional, intent(out) :: rc
      end function make_CubedSphereDecomposition_vm

      elemental module function equal_to(decomp1, decomp2)
         logical :: equal_to
         type(CubedSphereDecomposition), intent(in) :: decomp1
         type(CubedSphereDecomposition), intent(in) :: decomp2
      end function equal_to

      elemental module function not_equal_to(decomp1, decomp2)
         logical :: not_equal_to
         type(CubedSphereDecomposition), intent(in) :: decomp1
         type(CubedSphereDecomposition), intent(in) :: decomp2
      end function not_equal_to
      
   end interface

end module mapl3g_CubedSphereDecomposition

