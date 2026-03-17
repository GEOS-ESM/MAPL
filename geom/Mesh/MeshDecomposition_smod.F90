#include "MAPL_ErrLog.h"

submodule (mapl3g_MeshDecomposition) MeshDecomposition_smod

   use mapl_Partition
   use mapl_ErrorHandlingMod

   implicit none

contains

   ! Static factory methods
   module procedure make_MeshDecomposition_current_vm
      type(ESMF_VM) :: vm
      integer :: status

      call ESMF_VMGetCurrent(vm, _RC)
      decomp = make_MeshDecomposition(npoints, vm, _RC)

      _RETURN(_SUCCESS)
   end procedure make_MeshDecomposition_current_vm

   module procedure make_MeshDecomposition_vm
      integer :: status
      integer :: petCount

      call ESMF_VMGet(vm, petCount=petCount, _RC)
      decomp = MeshDecomposition(npoints, petCount=petCount)

      _RETURN(_SUCCESS)
   end procedure make_MeshDecomposition_vm

   ! Comparison operators
   module procedure equal_to
      
      ! Both must have allocated distributions
      if (.not. allocated(decomp1%point_distribution)) then
         equal_to = .not. allocated(decomp2%point_distribution)
         return
      end if

      if (.not. allocated(decomp2%point_distribution)) then
         equal_to = .false.
         return
      end if

      ! Check if distributions are the same
      equal_to = all(decomp1%point_distribution == decomp2%point_distribution)

   end procedure equal_to

   ! Get local indices for a given rank
   module procedure get_local_indices
      integer :: i

      i_0 = 1
      do i = 1, rank
         i_0 = i_0 + this%point_distribution(i)
      end do

      i_1 = i_0 + this%point_distribution(rank + 1) - 1

   end procedure get_local_indices

   ! Get local node indices for a given rank (Phase 3)
   module procedure get_local_node_indices
      integer :: i

      ! If node distribution is not set, fallback to point distribution
      if (.not. allocated(this%node_distribution)) then
         call get_local_indices(this, rank, i_0, i_1)
         return
      end if

      i_0 = 1
      do i = 1, rank
         i_0 = i_0 + this%node_distribution(i)
      end do

      i_1 = i_0 + this%node_distribution(rank + 1) - 1

   end procedure get_local_node_indices

end submodule MeshDecomposition_smod
