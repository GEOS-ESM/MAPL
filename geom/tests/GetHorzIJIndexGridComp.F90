#include "MAPL.h"
#include "MAPL_Exceptions.h"


module GetHorzIJIndexGridComp

  use ESMF
  use MAPL2
  use mapl3g_CubedSphereGeomSpec, only: CubedSphereGeomSpec, make_CubedSphereGeomSpec

  implicit none
  private

  public setservices

  contains

  subroutine setservices(gc, rc)
     type(ESMF_GridComp), intent(inout) :: gc
     integer, optional,   intent(out)   :: rc

     type(MAPL_MetaComp), pointer :: MAPL
     integer :: status

     call MAPL_GetObjectFromGC(gc, MAPL, _RC)
     call MAPL_GridCompSetEntryPoint(gc, ESMF_METHOD_INITIALIZE, initialize, _RC)
     call MAPL_GridCompSetEntryPoint(gc, ESMF_METHOD_RUN,        run,        _RC)
     call MAPL_GenericSetServices(gc, _RC)

     _RETURN(_SUCCESS)
  end subroutine setservices


  subroutine initialize(gc, import, export, clock, rc)
     type(ESMF_GridComp), intent(inout) :: gc
     type(ESMF_State),    intent(inout) :: import
     type(ESMF_State),    intent(inout) :: export
     type(ESMF_Clock),    intent(inout) :: clock
     integer, optional,   intent(out)   :: rc

     type(MAPL_MetaComp), pointer :: MAPL
     integer :: status

     call MAPL_GridCreate(gc, _RC)
     call MAPL_GetObjectFromGC(gc, MAPL, _RC)
     print *, 'Num threads = ', MAPL_get_num_threads(), ' for this run'
     call MAPL_GenericInitialize(gc, import, export, clock, _RC)

     _RETURN(_SUCCESS)
  end subroutine initialize


  ! Build a CubedSphereGeomSpec from a live ESMF_Grid.
  ! Reads im_world from global dims; nx_face/ny_face from the local count
  ! (each DE owns one face tile, so local count == face tile size).
  function make_spec_from_grid_(grid, rc) result(spec)
     type(ESMF_Grid), intent(inout) :: grid
     integer, optional, intent(out) :: rc
     type(CubedSphereGeomSpec) :: spec

     integer :: status, dims(3), local_dims(3)
     integer :: im_world, nx_face, ny_face
     type(ESMF_HConfig) :: hconfig
     character(len=256) :: content

     call MAPL_GridGet(grid, globalCellCountPerDim=dims, localCellCountPerDim=local_dims, _RC)
     im_world = dims(1)
     _ASSERT(dims(1)*6 == dims(2), 'Expected cubed-sphere: JM_World /= 6*IM_World')
     nx_face = local_dims(1)
     ny_face = local_dims(2)

     write(content, '(a,i0,a,i0,a,i0,a)') &
          '{class: CubedSphere, im_world: ', im_world, &
          ', nx_face: ', nx_face, &
          ', ny_face: ', ny_face, '}'
     hconfig = ESMF_HConfigCreate(content=trim(content), _RC)
     spec = make_CubedSphereGeomSpec(hconfig, _RC)
     call ESMF_HConfigDestroy(hconfig, _RC)

     _RETURN(_SUCCESS)
  end function make_spec_from_grid_


  subroutine check_dim(grid, rc)
     type(ESMF_Grid), intent(in) :: grid
     integer, optional, intent(out) :: rc

     integer :: status, dims(3)
     call MAPL_GridGet(grid, globalCellCountPerDim=dims, _RC)
     _ASSERT(dims(1)*6 == dims(2), 'Mini grid should act as a cubed sphere grid for global extents')
     _RETURN(_SUCCESS)
  end subroutine check_dim


  subroutine check_expected_index(grid, rc)
     type(ESMF_Grid), intent(inout) :: grid
     integer, optional, intent(out) :: rc

     integer :: status, i, j
     integer, allocatable :: II(:), JJ(:)
     real, dimension(:,:), pointer :: lats, lons
     real :: lon(1), lat(1)
     type(CubedSphereGeomSpec) :: spec

     spec = make_spec_from_grid_(grid, _RC)

     call ESMFL_GridCoordGet(grid, LATS,          &
                             Name     = "Latitude",  &
                             Location = ESMF_STAGGERLOC_CENTER, &
                             Units    = MAPL_UnitsRadians, _RC)
     call ESMFL_GridCoordGet(grid, LONS,           &
                             Name     = "Longitude", &
                             Location = ESMF_STAGGERLOC_CENTER, &
                             Units    = MAPL_UnitsRadians, _RC)
     _ASSERT(all(shape(LATS) == shape(LONS)), 'LATS and LONS must have the same shape')

     do j = 1, size(LATS, 2)
        do i = 1, size(LATS, 1)
           lon = lons(i, j)
           lat = lats(i, j)
           call spec%get_horz_ij_index(lon=lon, lat=lat, ii=II, jj=JJ, _RC)
           _ASSERT(II(1) == i, 'I-index of cell center (i,j) should be i')
           _ASSERT(JJ(1) == j, 'J-index of cell center (i,j) should be j')
        end do
     end do

     _RETURN(_SUCCESS)
  end subroutine check_expected_index


  subroutine check_unexpected_index(grid, rc)
     type(ESMF_Grid), intent(inout) :: grid
     integer, optional, intent(out) :: rc

     integer :: status, i, j
     integer, allocatable :: II(:), JJ(:)
     real, dimension(:,:), pointer :: lats, lons
     real :: lon(1), lat(1)
     type(CubedSphereGeomSpec) :: spec

     spec = make_spec_from_grid_(grid, _RC)

     call ESMFL_GridCoordGet(grid, lats,           &
                             Name     = "Latitude",  &
                             Location = ESMF_STAGGERLOC_CENTER, &
                             Units    = MAPL_UnitsRadians, _RC)
     call ESMFL_GridCoordGet(grid, lons,            &
                             Name     = "Longitude", &
                             Location = ESMF_STAGGERLOC_CENTER, &
                             Units    = MAPL_UnitsRadians, _RC)
     ! antipodal points — should not be in the local domain
     lons = lons + MAPL_PI
     lats = -lats
     _ASSERT(all(shape(LATS) == shape(LONS)), 'LATS and LONS must have the same shape')

     do j = 1, size(lats, 2)
        do i = 1, size(lats, 1)
           lon = lons(i, j)
           lat = lats(i, j)
           call spec%get_horz_ij_index(lon=lon, lat=lat, ii=II, jj=JJ, _RC)
           _ASSERT(II(1) == -1, 'Expected -1 for point outside of domain')
           _ASSERT(JJ(1) == -1, 'Expected -1 for point outside of domain')
        end do
     end do

     _RETURN(_SUCCESS)
  end subroutine check_unexpected_index


  subroutine run(gc, import, export, clock, rc)
     type(ESMF_GridComp), intent(inout) :: gc
     type(ESMF_State),    intent(inout) :: import
     type(ESMF_State),    intent(inout) :: export
     type(ESMF_Clock),    intent(inout) :: clock
     integer, optional,   intent(out)   :: rc

     type(MAPL_MetaComp), pointer :: mapl
     type(ESMF_Grid) :: grid
     integer :: status

     call MAPL_GetObjectFromGC(gc, MAPL, _RC)
     call MAPL_Get(MAPL, grid=grid, _RC)
     call ESMF_GridValidate(grid, _RC)

     call check_dim(grid, _RC)
     call check_expected_index(grid, _RC)
     call check_unexpected_index(grid, _RC)

     _UNUSED_DUMMY(import)
     _UNUSED_DUMMY(export)
     _UNUSED_DUMMY(clock)

     _RETURN(_SUCCESS)
  end subroutine run

end module GetHorzIJIndexGridComp
