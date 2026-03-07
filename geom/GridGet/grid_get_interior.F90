#include "MAPL.h"

submodule (mapl3g_GridGet) grid_get_interior_smod

   use mapl3g_DistGridGet, only: DistGridGet

   implicit none(type, external)

contains

   module subroutine grid_get_interior(grid, interior, rc)

      type(ESMF_Grid), intent(in) :: grid
      integer, allocatable, intent(out) :: interior(:)
      integer, optional, intent(out) :: rc

      type(ESMF_DistGrid) :: dist_grid
      type(ESMF_DELayout) :: layout
      type(ESMF_Info) :: infoh
      integer, allocatable :: AL(:, :), AU(:, :)
      integer, allocatable :: local_de_to_demap(:), global_grid_info(:)
      integer :: de_count, local_de_count, de_id, grid_rank
      integer :: j1, jn, status
      logical :: is_present

      call ESMF_InfoGetFromHost(grid, infoh, _RC)
      is_present = ESMF_InfoIsPresent(infoh, 'GLOBAL_GRID_INFO', _RC)
      if (is_present) then
         call ESMF_InfoGetAlloc(infoh, key="GLOBAL_GRID_INFO", values=global_grid_info, _RC)
         interior = global_grid_info(7:10)
         deallocate(global_grid_info, _STAT)
         _RETURN(_SUCCESS)
      end if

      call ESMF_GridGet(grid, dimCount=grid_rank, distGrid=dist_grid, _RC)
      call ESMF_DistGridGet(dist_grid, delayout=layout, _RC)
      call ESMF_DELayoutGet(layout, deCount=de_count, localDECount=local_de_count, _RC)
      if (local_de_count > 0) then
         allocate(local_de_to_demap(local_de_count), _STAT)
         call ESMF_DELayoutGet(layout, localDeToDeMap=local_de_to_demap, _RC)
         de_id = local_de_to_demap(1)
         allocate(AL(grid_rank, 0:de_count - 1), _STAT)
         allocate(AU(grid_rank, 0:de_count - 1), _STAT)
         call DistGridGet(dist_grid, min_index=AL, max_index=AU, _RC)
         interior = [AL(1, de_id), AU(1, de_id)] ! i1, in
         ! _ASSERT(grid_rank > 1, 'tilegrid is 1d (without RC this only for info')
         j1 = 1
         jn = 1
         if (grid_rank > 1) then
            j1 = AL(2, de_id)
            jn = AU(2, de_id)
         end if
         interior = [interior, j1, jn] ! i1, in, j1, jn
         deallocate(AU, AL, local_de_to_demap)
      end if

      _RETURN(_SUCCESS)
   end subroutine grid_get_interior

end submodule grid_get_interior_smod
