#include "MAPL_ErrLog.h"

module mapl3g_LatLonDecomposition
   use mapl3g_LonAxis
   use mapl3g_LatAxis
   use mapl_Partition
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

      pure module function get_lon_subset(this, axis, rank) result(local_axis)
         type(LonAxis) :: local_axis
         class(LatLonDecomposition), intent(in) :: this
         type(LonAxis), intent(in) :: axis
         integer, intent(in) :: rank
      end function get_lon_subset

      pure module function get_lat_subset(this, axis, rank) result(local_axis)
         type(LatAxis) :: local_axis
         class(LatLonDecomposition), intent(in) :: this
         type(LatAxis), intent(in) :: axis
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

      pure module function get_subset(coordinates, i_0, i_1) result(subset)
         real(kind=R8), allocatable :: subset(:)
         real(kind=R8), intent(in) :: coordinates(:)
         integer, intent(in) :: i_0, i_1
      end function get_subset

      pure module subroutine get_idx_range(distribution, rank, i_0, i_1)
         integer, intent(in) :: distribution(:)
         integer, intent(in) :: rank
         integer, intent(out) :: i_0, i_1
      end subroutine get_idx_range

   end interface


   CONTAINS

   function new_LatLonDecomposition_basic(lon_distribution, lat_distribution) result(decomp)
      use mapl_KeywordEnforcer
      type(LatLonDecomposition) :: decomp
      integer, intent(in) :: lon_distribution(:)
      integer, intent(in) :: lat_distribution(:)
 
      decomp%lon_distribution = lon_distribution
      decomp%lat_distribution = lat_distribution
 
   end function new_LatLonDecomposition_basic

   function new_LatLonDecomposition_petcount(dims, unusable, petCount) result(decomp)
      use mapl_KeywordEnforcer
      type(LatLonDecomposition) :: decomp
      integer, intent(in) :: dims(2)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, intent(in) :: petCount
      
      integer :: nx, nx_start 
 
      associate (aspect_ratio => real(dims(1))/dims(2))
        nx_start = max(1, floor(sqrt(petCount * aspect_ratio)))
        do nx = nx_start, 1, -1
           if (mod(petcount, nx) == 0) then ! found a decomposition
              exit
           end if
        end do
      end associate
 
      decomp = LatLonDecomposition(dims, topology=[nx, petCount/nx])
 
   end function new_LatLonDecomposition_petcount

   function new_LatLonDecomposition_topo(dims, unusable, topology) result(decomp)
      use mapl_KeywordEnforcer
      type(LatLonDecomposition) :: decomp
      integer, intent(in) :: dims(2)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, intent(in) :: topology(2)

      decomp%lon_distribution = mapl_GetPartition(dims(1), k=topology(1), min_extent=2)
      decomp%lat_distribution = mapl_GetPartition(dims(2), k=topology(2), min_extent=2)

   end function new_LatLonDecomposition_topo

   pure function get_lat_distribution(decomp) result(lat_distribution)
      integer, allocatable :: lat_distribution(:)
      class(LatLonDecomposition), intent(in) :: decomp
      lat_distribution = decomp%lat_distribution
   end function get_lat_distribution

   ! accessors
   pure function get_lon_distribution(decomp) result(lon_distribution)
      integer, allocatable :: lon_distribution(:)
      class(LatLonDecomposition), intent(in) :: decomp
      lon_distribution = decomp%lon_distribution
   end function get_lon_distribution

   elemental function not_equal_to(decomp1, decomp2)
      logical :: not_equal_to
      type(LatLonDecomposition), intent(in) :: decomp1
      type(LatLonDecomposition), intent(in) :: decomp2

      not_equal_to = .not. (decomp1 == decomp2)

   end function not_equal_to

end module mapl3g_LatLonDecomposition

