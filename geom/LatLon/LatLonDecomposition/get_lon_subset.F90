#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonDecomposition) get_lon_subset_smod
   use mapl_ErrorHandlingMod
   use MAPL_Base
   implicit none (type, external)

contains

   pure module function get_lon_subset(this, axis, rank) result(local_axis)
      type(LonAxis) :: local_axis
      class(LatLonDecomposition), intent(in) :: this
      type(LonAxis), intent(in) :: axis
      integer, intent(in) :: rank

      real(kind=R8), allocatable :: centers(:)
      real(kind=R8), allocatable :: corners(:)

      integer :: i_0, i_1, i_n

      call get_idx_range(this%lon_distribution, rank, i_0, i_1)
      i_n = i_1 ! unless

      associate (nx => size(this%get_lon_distribution()))
        if (.not. axis%is_periodic() .and. (1+rank == nx)) then
           i_n = i_n + 1
        end if
      end associate
      
      centers = get_subset(axis%get_centers(), i_0, i_1)
      corners = get_subset(axis%get_corners(), i_0, i_n)
         
      local_axis = LonAxis(centers, corners)

   end function get_lon_subset

end submodule get_lon_subset_smod

