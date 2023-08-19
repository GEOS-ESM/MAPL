module mapl3g_LatLonDecomposition
   use mapl_KeywordEnforcer
   use esmf
   implicit none
   private

   public :: LatLonDecomposition
   public :: make_LatLonDecomposition
   public :: operator(==)
   public :: operator(/=)

   type :: LatLonDecomposition
      private
      integer, allocatable :: lon_distribution(:)
      integer, allocatable :: lat_distribution(:)
   contains
      procedure :: get_lon_distribution
      procedure :: get_lat_distribution
      procedure :: get_lon_subset
      procedure :: get_lat_subset
   end type LatLonDecomposition

   interface LatLonDecomposition
      procedure :: new_LatLonDecomposition_basic
      procedure :: new_LatLonDecomposition_petcount
      procedure :: new_LatLonDecomposition_topo
   end interface LatLonDecomposition

   interface make_LatLonDecomposition
      procedure :: make_LatLonDecomposition_current_vm
      procedure :: make_LatLonDecomposition_vm
   end interface make_LatLonDecomposition

   interface operator(==)
      procedure equal_to
   end interface operator(==)

   interface operator(/=)
      procedure not_equal_to
   end interface operator(/=)

   integer, parameter :: R8 = ESMF_KIND_R8
   interface

      ! Constructors
      pure module function new_LatLonDecomposition_basic(lon_distribution, lat_distribution) result(decomp)
         type(LatLonDecomposition) :: decomp
         integer, intent(in) :: lon_distribution(:)
         integer, intent(in) :: lat_distribution(:)
      end function new_LatLonDecomposition_basic

      ! Keyword enforced to avoid ambiguity with '_topo' interface
      pure module function new_LatLonDecomposition_petcount(dims, unusable, petCount) result(decomp)
         type(LatLonDecomposition) :: decomp
         integer, intent(in) :: dims(2)
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, intent(in) :: petCount
      end function new_LatLonDecomposition_petcount

      ! Keyword enforced to avoid ambiguity with '_petcount' interface
      pure module function new_LatLonDecomposition_topo(dims, unusable, topology) result(decomp)
         type(LatLonDecomposition) :: decomp
         integer, intent(in) :: dims(2)
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, intent(in) :: topology(2)
      end function new_LatLonDecomposition_topo

      ! accessors
      pure module function get_lon_distribution(decomp) result(lon_distribution)
         integer, allocatable :: lon_distribution(:)
         class(LatLonDecomposition), intent(in) :: decomp
      end function get_lon_distribution

      pure module function get_lat_distribution(decomp) result(lat_distribution)
         integer, allocatable :: lat_distribution(:)
         class(LatLonDecomposition), intent(in) :: decomp
      end function get_lat_distribution

      pure module function get_lon_subset(this, coordinates, rank) result(subset)
         real(kind=R8), allocatable :: subset(:)
         class(LatLonDecomposition), intent(in) :: this
         real(kind=R8), intent(in) :: coordinates(:)
         integer, intent(in) :: rank
      end function get_lon_subset

      pure module function get_lat_subset(this, coordinates, rank) result(subset)
         real(kind=R8), allocatable :: subset(:)
         class(LatLonDecomposition), intent(in) :: this
         real(kind=R8), intent(in) :: coordinates(:)
         integer, intent(in) :: rank
      end function get_lat_subset

      ! Static factory methods
      module function make_LatLonDecomposition_current_vm(dims, rc) result(decomp)
         type(LatLonDecomposition) :: decomp
         integer, intent(in) :: dims(2)
         integer, optional, intent(out) :: rc
      end function make_LatLonDecomposition_current_vm

      module function make_LatLonDecomposition_vm(dims, vm, rc) result(decomp)
         type(LatLonDecomposition) :: decomp
         integer, intent(in) :: dims(2)
         type(ESMF_VM), intent(in) :: vm
         integer, optional, intent(out) :: rc
      end function make_LatLonDecomposition_vm

      elemental module function equal_to(decomp1, decomp2)
         logical :: equal_to
         type(LatLonDecomposition), intent(in) :: decomp1
         type(LatLonDecomposition), intent(in) :: decomp2
      end function equal_to

      elemental module function not_equal_to(decomp1, decomp2)
         logical :: not_equal_to
         type(LatLonDecomposition), intent(in) :: decomp1
         type(LatLonDecomposition), intent(in) :: decomp2
      end function not_equal_to
      
   end interface

end module mapl3g_LatLonDecomposition

