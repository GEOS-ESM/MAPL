#include "MAPL.h"

module mapl3g_DistGrid

  use ESMF, only: ESMF_DistGrid, ESMF_DistGridGet
  use MAPL_ErrorHandlingMod
  ! use MAPL_KeywordEnforcerMod
  ! use MAPL_ConstantsMod, only : MAPL_PI_R8, MAPL_UnitsRadians

  implicit none
  private

  public :: DistGridGet

contains

  subroutine DistGridGet(dist_grid, min_index, max_index, rc)
    type(ESMF_DistGrid), intent(inout) :: dist_grid
    integer, intent(inout) :: min_index(:,:)
    integer, intent(inout) :: max_index(:,:)
    integer, optional, intent(out) :: rc

    integer :: status

    integer :: i, tile_size, tile_count, tile, de_count
    logical :: ESMFCubedSphere
    integer, allocatable  :: elementCountPTile(:)
    integer, allocatable :: de_to_tile_map(:)
    integer, allocatable :: old_min_index(:,:), old_max_index(:,:)

    ESMFCubedSphere = .false.
    call ESMF_DistGridGet(dist_grid, tileCount=tile_count, _RC)
    if (tile_count == 6) ESMFCubedSphere = .true.

    if (ESMFCubedSphere) then

       allocate(elementCountPTile(tile_count), _STAT)
       call ESMF_DistGridGet(dist_grid, elementCountPTile=elementCountPTile, _RC)
       ! All tile should have same number of elements
       tile_size = elementCountPTile(1)
       tile_size = sqrt(real(tile_size))
       deallocate(elementCountPTile)

       de_count = size(min_index,2)

       allocate(de_to_tile_map(de_count), _STAT)
       allocate(old_min_index(2, de_count), old_max_index(2, de_count), _STAT)
       call ESMF_DistGridGet(dist_grid, &
            maxIndexPDe=old_max_index, &
            minIndexPDe=old_min_index, &
            deToTileMap=de_to_tile_map, _RC)
       do i = 1, de_count
          tile = de_to_tile_map(i)
          select case (tile)
          case (1)
             min_index(:, i) = old_min_index(:, i)
             max_index(:, i) = old_max_index(:, i)
          case (2)
             min_index(1, i) = old_min_index(1, i) -  tile_size
             min_index(2, i) = old_min_index(2, i) +  tile_size
             max_index(1, i) = old_max_index(1, i) -  tile_size
             max_index(2, i) = old_max_index(2, i) +  tile_size
          case (3)
             min_index(1, i) = old_min_index(1, i) -  tile_size
             min_index(2, i) = old_min_index(2, i) +  tile_size
             max_index(1, i) = old_max_index(1, i) -  tile_size
             max_index(2, i) = old_max_index(2, i) +  tile_size
          case (4)
             min_index(1, i) = old_min_index(1, i) -2*tile_size
             min_index(2, i) = old_min_index(2, i) +2*tile_size
             max_index(1, i) = old_max_index(1, i) -2*tile_size
             max_index(2, i) = old_max_index(2, i) +2*tile_size
          case (5)
             min_index(1, i) = old_min_index(1, i) -2*tile_size
             min_index(2, i) = old_min_index(2, i) +2*tile_size
             max_index(1, i) = old_max_index(1, i) -2*tile_size
             max_index(2, i) = old_max_index(2, i) +2*tile_size
          case (6)
             min_index(1, i) = old_min_index(1, i) -3*tile_size
             min_index(2, i) = old_min_index(2, i) +3*tile_size
             max_index(1, i) = old_max_index(1, i) -3*tile_size
             max_index(2, i) = old_max_index(2, i) +3*tile_size
          end select
       enddo
       deallocate(de_to_tile_map)
       deallocate(old_max_index, old_min_index)

    else

       call ESMF_DistGridGet(dist_grid, minIndexPDe=min_index, maxIndexPDe=max_index, _RC)

    end if

    _RETURN(_SUCCESS)
  end subroutine DistGridGet

end module mapl3g_DistGrid
