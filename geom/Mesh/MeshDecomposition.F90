#include "MAPL_ErrLog.h"

module mapl3g_MeshDecomposition

   use mapl_Partition
   use mapl_KeywordEnforcer
   use esmf

   implicit none
   private

   public :: MeshDecomposition
   public :: make_MeshDecomposition
   public :: operator(==)
   public :: operator(/=)

   type :: MeshDecomposition
      private
      integer, allocatable :: point_distribution(:)
   contains
      procedure :: get_point_distribution
      procedure :: get_local_indices
   end type MeshDecomposition

   interface MeshDecomposition
      procedure :: new_MeshDecomposition_basic
      procedure :: new_MeshDecomposition_petcount
   end interface MeshDecomposition

   interface make_MeshDecomposition
      procedure :: make_MeshDecomposition_current_vm
      procedure :: make_MeshDecomposition_vm
   end interface make_MeshDecomposition

   interface operator(==)
      procedure equal_to
   end interface operator(==)

   interface operator(/=)
      procedure not_equal_to
   end interface operator(/=)

   integer, parameter :: R8 = ESMF_KIND_R8

   interface

      module subroutine get_local_indices(this, rank, i_0, i_1)
         class(MeshDecomposition), intent(in) :: this
         integer, intent(in) :: rank
         integer, intent(out) :: i_0, i_1
      end subroutine get_local_indices

      ! Static factory methods
      module function make_MeshDecomposition_current_vm(npoints, rc) result(decomp)
         type(MeshDecomposition) :: decomp
         integer, intent(in) :: npoints
         integer, optional, intent(out) :: rc
      end function make_MeshDecomposition_current_vm

      module function make_MeshDecomposition_vm(npoints, vm, rc) result(decomp)
         type(MeshDecomposition) :: decomp
         integer, intent(in) :: npoints
         type(ESMF_VM), intent(in) :: vm
         integer, optional, intent(out) :: rc
      end function make_MeshDecomposition_vm

      elemental module function equal_to(decomp1, decomp2)
         logical :: equal_to
         type(MeshDecomposition), intent(in) :: decomp1
         type(MeshDecomposition), intent(in) :: decomp2
      end function equal_to

   end interface

contains

   function new_MeshDecomposition_basic(point_distribution) result(decomp)
      type(MeshDecomposition) :: decomp
      integer, intent(in) :: point_distribution(:)

      decomp%point_distribution = point_distribution
   end function new_MeshDecomposition_basic

   function new_MeshDecomposition_petcount(npoints, unusable, petCount) result(decomp)
      use mapl_KeywordEnforcer
      type(MeshDecomposition) :: decomp
      integer, intent(in) :: npoints
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, intent(in) :: petCount

      ! Distribute points as evenly as possible across PETs
      decomp%point_distribution = mapl_GetPartition(npoints, k=petCount, min_extent=1)

      _UNUSED_DUMMY(unusable)
   end function new_MeshDecomposition_petcount

   pure function get_point_distribution(decomp) result(point_distribution)
      integer, allocatable :: point_distribution(:)
      class(MeshDecomposition), intent(in) :: decomp

      point_distribution = decomp%point_distribution
   end function get_point_distribution

   elemental function not_equal_to(decomp1, decomp2)
      logical :: not_equal_to
      type(MeshDecomposition), intent(in) :: decomp1
      type(MeshDecomposition), intent(in) :: decomp2

      not_equal_to = .not. (decomp1 == decomp2)
   end function not_equal_to

end module mapl3g_MeshDecomposition
