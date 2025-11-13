#define I_AM_MAIN
#include "MAPL_ErrLog.h"

program main
   use mapl3g_Geom_API
   use mapl_ErrorHandling
   use esmf
   implicit none(type,external)

   type(ESMF_Mesh) :: surf_mesh
   type(ESMF_Geom) :: atm_geom, out_geom
   type(ESMF_Grid) :: atm_grid, out_grid
   type(ESMF_Xgrid) :: surf_xgrd
   type(ESMF_RouteHandle) :: rh, rh2
   integer :: status

   type(ESMF_Field) :: f_mesh, f_grid, f_xgrd, f_out
   real(kind=ESMF_KIND_R8), allocatable :: centroid(:,:)
   real(kind=ESMF_KIND_R4), pointer :: x_mesh(:)
   real(kind=ESMF_KIND_R4), pointer :: x_xgrd(:)
   real(kind=ESMF_KIND_R4), pointer :: x_grid(:,:)
   real(kind=ESMF_KIND_R4), pointer :: x_out(:,:)
   integer, allocatable :: elementMask(:)
   integer :: elementCount

   type(GeomManager) :: geom_mgr
   type(ESMF_HConfig) :: hconfig
   type(MaplGeom), pointer :: mapl_geom
   type(ESMF_Geom) :: geom

   call ESMF_Initialize(_RC)
   geom_mgr = GeomManager()
   call geom_mgr%initialize()

   hconfig = ESMF_HConfigCreate(content="{class: CubedSphere, im_world: 90, nx: 1, ny: 1}", rc=status)
   mapl_geom => geom_mgr%get_mapl_geom(hconfig, _RC)
   atm_geom = mapl_geom%get_geom()
   call ESMF_GeomGet(atm_geom, grid=atm_grid, _RC)

   hconfig = ESMF_HConfigCreate(content="{class: latlon, im_world: 360, jm_world: 180, pole: PC, dateline: DC, nx: 3, ny: 2}", _RC)
!#   hconfig = ESMF_HConfigCreate(content="{class: latlon, im_world: 1440, jm_world: 720, pole: PC, dateline: DC, nx: 3, ny: 2}", _RC)
   mapl_geom => geom_mgr%get_mapl_geom(hconfig, _RC)
   out_geom = mapl_geom%get_geom()
   call ESMF_GeomGet(out_geom, grid=out_grid, _RC)

   surf_mesh = ESMF_MeshCreate('surface_types.nc', fileFormat=ESMF_FILEFORMAT_ESMFMESH, _RC)

   surf_xgrd = ESMF_XgridCreate(sideAGrid=[atm_grid], sideBMesh=[surf_mesh], storeOverlay=.true., _RC)

   f_mesh = ESMF_FieldCreate(name='f_mesh', mesh=surf_mesh, typekind=ESMF_TYPEKIND_R4, meshloc=ESMF_MESHLOC_ELEMENT, _RC)
   f_grid = ESMF_FieldCreate(name='f_grid', grid=atm_grid, typekind=ESMF_TYPEKIND_R4, _RC)
   f_xgrd = ESMF_FieldCreate(name='f_xgrd', xgrid=surf_xgrd, typekind=ESMF_TYPEKIND_R4, _RC)
   f_out  = ESMF_FieldCreate(name='f_out',  grid=out_grid, typekind=ESMF_TYPEKIND_R4, _RC)

   call ESMF_FieldGet(f_mesh, fArrayPtr=x_mesh, _RC)
   call ESMF_FieldGet(f_xgrd, fArrayPtr=x_xgrd, _RC)
   call ESMF_FieldGet(f_grid, fArrayPtr=x_grid, _RC)
   call ESMF_FieldGet(f_out, fArrayPtr=x_out, _RC)

   x_xgrd = -1
   x_grid = -1
   
   call ESMF_MeshGet(surf_mesh, elementCount=elementCount, _RC)
   allocate(elementMask(elementCount))
   call ESMF_MeshGet(surf_mesh, elementMask=elementMask, _RC)
   x_mesh = elementMask
   _HERE, minval(elementMask), maxval(elementMask)

   call ESMF_FieldRegridStore(surf_xgrd, srcField=f_mesh, dstField=f_xgrd, routeHandle=rh, regridMethod=ESMF_REGRIDMETHOD_CONSERVE, _RC)
   call ESMF_FieldRegrid(f_mesh, f_xgrd, routeHandle=rh, _RC)
   _HERE, size(x_grid), minval(x_xgrd), maxval(x_xgrd), count(x_xgrd > 5)

   call ESMF_XgridGet(surf_xgrd, elementCount=elementCount, _RC)
   allocate(centroid(elementCount,2))
   call ESMF_XgridGet(surf_xgrd, centroid=centroid, _RC)

   block
     ! print the centroig of the element that has the largest value.
     integer :: loc
     loc = maxloc(x_xgrd, 1)
     _HERE, loc,  centroid(loc,:)
   end block
   
   call ESMF_FieldRegridStore(srcField=f_xgrd, dstField=f_out, routeHandle=rh2, regridMethod=ESMF_REGRIDMETHOD_CONSERVE, _RC)
   call ESMF_FieldRegrid(f_xgrd, f_out, routehandle=rh2, _RC)
   _HERE, minval(x_out), maxval(x_out)

   call ESMF_FieldWrite(f_out, fileName='crazy_latlon.nc', &
        variableName='surf_type', overwrite=.true., iofmt=ESMF_IOFMT_NETCDF4P, _RC)
   
   call ESMF_Finalize(_RC)
end program main

