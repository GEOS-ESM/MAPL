#include "MAPL.h"
#include "MAPL_Exceptions.h"


module GridComp

  use ESMF
  use MAPL

  implicit none
  private

  public setservices

  contains

  subroutine setservices(gc,rc)

     type(ESMF_GridComp), intent(inout)  :: gc
     integer, optional :: rc

     type (MAPL_MetaComp),       pointer    :: MAPL
     type (ESMF_Config) :: cf
     integer :: status
     logical :: use_threads
     integer :: num_threads

     call MAPL_GetObjectFromGC (gc, MAPL, _RC)
     call ESMF_GridCompGet(gc, config=cf, _RC)
     call ESMF_ConfigGetAttribute(cf, use_threads, label='use_threads:', default=.FALSE., _RC)
     call MAPL%set_use_threads(use_threads)

     call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_INITIALIZE,  initialize, _RC)
     call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_RUN,  run, _RC)
     call MAPL_GenericSetServices(gc, _RC)

     _RETURN(_SUCCESS)

  end subroutine setservices


  subroutine initialize(gc, import, export, clock, rc)
     type(ESMF_GridComp), intent(inout) :: gc
     type(ESMF_State), intent(inout) :: import
     type(ESMF_State), intent(inout) :: export
     type(ESMF_Clock), intent(inout) :: clock
     integer, intent(out), optional :: rc

     type (MAPL_MetaComp),       pointer    :: MAPL
     integer :: status

     call MAPL_GridCreate(gc, _RC)
     call MAPL_GetObjectFromGC (gc, MAPL, _RC)
     print *, 'Num threads = ', MAPL_get_num_threads(), ' for this run'
     call MAPL_GenericInitialize(gc, import, export, clock, _RC)

     _RETURN(_SUCCESS)

  end subroutine initialize

  subroutine check_dim(grid, rc)
     type(ESMF_Grid), intent(in) :: grid
     integer, intent(out), optional :: rc

! locals
     integer :: status
     integer :: dims(3)

      call MAPL_GridGet(grid, globalCellCountPerDim=dims,_RC)

      _ASSERT(dims(1)*6 == dims(2), 'Mini grid should act as a cubed sphere grid for global extents')

     _RETURN(_SUCCESS)

  end subroutine check_dim

  subroutine check_expected_index(grid, rc)
     type(ESMF_Grid), intent(inout) :: grid
     integer, intent(out), optional :: rc

! locals
     integer :: status, i, j, II(1), JJ(1)
     real, dimension(:,:), pointer :: lats, lons
     real :: lon(1), lat(1)


      call ESMFL_GridCoordGet(   grid, LATS                      , &
                              Name     = "Latitude"              , &
                              Location = ESMF_STAGGERLOC_CENTER  , &
                              Units    = MAPL_UnitsRadians    , _RC)

      call ESMFL_GridCoordGet(   grid, LONS                      , &
                              Name     = "Longitude"             , &
                              Location = ESMF_STAGGERLOC_CENTER  , &
                              Units    = MAPL_UnitsRadians    , _RC)
      _ASSERT(all(shape(LATS) == shape(LONS)), 'LATS and LONS must have the same shape')

      do j = 1, size(LATS,2)
         do i = 1, size(LATS, 1)
            lon = lons(i,j)
            lat = lats(i,j)
            call MAPL_GetHorzIJIndex(1, II, JJ,  &
                grid=grid, lon=lon, lat=lat, _RC)
            _ASSERT(II(1) == i, 'I-index of cell center (i,j) should be i')
            _ASSERT(JJ(1) == j, 'J-index of cell center (i,j) should be j')
         end do
      end do

     _RETURN(_SUCCESS)

  end subroutine check_expected_index

  subroutine check_unexpected_index(grid, rc)
     type(ESMF_Grid), intent(inout) :: grid
     integer, intent(out), optional :: rc

! locals
     integer :: status, i, j, II(1), JJ(1)
     real, dimension(:,:), pointer :: lats, lons
     real :: lon(1), lat(1)


      call ESMFL_GridCoordGet(   grid, lats                      , &
                              Name     = "Latitude"              , &
                              Location = ESMF_STAGGERLOC_CENTER  , &
                              Units    = MAPL_UnitsRadians    , _RC)

      call ESMFL_GridCoordGet(   grid, lons                      , &
                              Name     = "Longitude"             , &
                              Location = ESMF_STAGGERLOC_CENTER  , &
                              Units    = MAPL_UnitsRadians    , _RC)
      ! centers of antipodal points
       lons = lons + MAPL_PI
       lats = -lats
      _ASSERT(all(shape(LATS) == shape(LONS)), 'LATS and LONS must have the same shape')

      do j = 1, size(lats,2)
         do i = 1, size(lats, 1)
            lon = lons(i,j)
            lat = lats(i,j)
            call MAPL_GetHorzIJIndex(1, II(1), JJ(1),  &
                grid=grid, lon=lon, lat=lat, _RC)
            _ASSERT(II(1) == -1, 'Expected -1 for point outside of domain')
            _ASSERT(JJ(1) == -1, 'Expected -1 for point outside of domain') 
         end do         
      end do

     _RETURN(_SUCCESS)

  end subroutine check_unexpected_index

  subroutine run(gc, import, export, clock, rc)
     type(ESMF_GridComp), intent(inout) :: gc
     type(ESMF_State), intent(inout) :: import
     type(ESMF_State), intent(inout) :: export
     type(ESMF_Clock), intent(inout) :: clock
     integer, intent(out), optional :: rc

! locals
     type (MAPL_MetaComp), pointer     :: mapl
     type (ESMF_Grid)                  :: grid
     integer :: status

      call MAPL_GetObjectFromGC (gc, MAPL, _RC)
      call MAPL_Get (MAPL, grid=grid, _RC)
      call ESMF_GridValidate(grid,_RC)

      call check_dim(grid, _RC)
      call check_expected_index(grid, _RC)
      call check_unexpected_index(grid, _RC)

     _UNUSED_DUMMY(import)
     _UNUSED_DUMMY(export)
     _UNUSED_DUMMY(clock)

     _RETURN(_SUCCESS)

  end subroutine run

end module GridComp
