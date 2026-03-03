#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonDecomposition) get_lat_subset_smod
   use mapl_ErrorHandlingMod
   implicit none (type, external)

contains

   pure module function get_lat_subset(this, axis, rank) result(local_axis)
      type(LatAxis) :: local_axis
      class(LatLonDecomposition), intent(in) :: this
      type(LatAxis), intent(in) :: axis
      integer, intent(in) :: rank

      real(kind=R8), allocatable :: centers(:)
      real(kind=R8), allocatable :: corners(:)
      
      integer :: j_0, j_1, j_n

      call get_idx_range(this%lat_distribution, rank, j_0, j_1)
      j_n = j_1 ! unless

      associate (ny => size(this%get_lat_distribution()))
        if (1+rank == ny) then
           j_n = j_n + 1
        end if
      end associate
      
      centers = get_subset(axis%get_centers(), j_0, j_1)
      corners = get_subset(axis%get_corners(), j_0, j_n)
         
      local_axis = LatAxis(centers, corners)

   end function get_lat_subset

end submodule get_lat_subset_smod

