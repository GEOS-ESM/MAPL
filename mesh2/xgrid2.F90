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
   type(ESMF_RouteHandle) :: rh, rh2, rh3
   integer :: status

   type(ESMF_Field) :: f_mesh, f_agrid, f_xgrid, f_out
   real(kind=ESMF_KIND_R8), allocatable :: centroid(:,:)
   real(kind=ESMF_KIND_R4), pointer :: f_mesh_Ptr(:)
   real(kind=ESMF_KIND_R4), pointer :: f_xgrid_Ptr(:)
   real(kind=ESMF_KIND_R4), pointer :: f_agrid_Ptr(:,:)
   real(kind=ESMF_KIND_R4), pointer :: f_out_ptr(:,:)
   integer, allocatable :: elementMask(:)
   integer :: elementCount

   type(GeomManager) :: geom_mgr
   type(ESMF_HConfig) :: hconfig
   type(MaplGeom), pointer :: mapl_geom
   type(ESMF_Geom) :: geom

   call ESMF_Initialize(_RC)
   geom_mgr = GeomManager()
   call geom_mgr%initialize()

   hconfig = ESMF_HConfigCreate(content="{class: CubedSphere, im_world: 90, nx: 1, ny: 6}", rc=status)
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

   f_mesh  = ESMF_FieldCreate(name='f_mesh',  mesh=surf_mesh, typekind=ESMF_TYPEKIND_R4, meshloc=ESMF_MESHLOC_ELEMENT, _RC)
   f_agrid = ESMF_FieldCreate(name='f_agrid', grid=atm_grid, typekind=ESMF_TYPEKIND_R4, _RC)
   f_xgrid  = ESMF_FieldCreate(name='f_xgrd',  xgrid=surf_xgrd, typekind=ESMF_TYPEKIND_R4, _RC)
   f_out  = ESMF_FieldCreate(name='f_out',    grid=out_grid, typekind=ESMF_TYPEKIND_R4, _RC)

   call ESMF_FieldGet(f_mesh, fArrayPtr=f_mesh_Ptr, _RC)
   call ESMF_FieldGet(f_xgrid, fArrayPtr=f_xgrid_Ptr, _RC)
   call ESMF_FieldGet(f_agrid, fArrayPtr=f_agrid_Ptr, _RC)
   call ESMF_FieldGet(f_out, fArrayPtr=f_out_ptr, _RC)

   f_xgrid_Ptr  = -1
   f_agrid_Ptr  = -1
   f_out_ptr    = -1
   
   call ESMF_MeshGet(surf_mesh, elementCount=elementCount, _RC)
   allocate(elementMask(elementCount))
   call ESMF_MeshGet(surf_mesh, elementMask=elementMask, _RC)
   f_mesh_Ptr = elementMask
   _HERE, minval(elementMask), maxval(elementMask)

   call ESMF_FieldRegridStore(xgrid=surf_xgrd, srcField=f_mesh, dstField=f_agrid, routeHandle=rh, regridMethod=ESMF_REGRIDMETHOD_CONSERVE, _RC)
   call ESMF_FieldRegrid(f_mesh, f_agrid, routeHandle=rh, _RC)
   _HERE,  minval(f_agrid_Ptr), maxval(f_agrid_Ptr), count(f_agrid_Ptr > 5)

   !call ESMF_FieldRegridStore(xgrid=surf_xgrd, srcField=f_mesh, dstField=f_xgrid, routeHandle=rh, regridMethod=ESMF_REGRIDMETHOD_CONSERVE, _RC)
   !call ESMF_FieldRegrid(f_mesh, f_xgrid, routeHandle=rh, _RC)
   !_HERE,  minval(f_xgrid_Ptr), maxval(f_xgrid_Ptr), count(f_xgrid_Ptr > 5)

   call ESMF_XgridGet(surf_xgrd, elementCount=elementCount, _RC)
   allocate(centroid(elementCount,2))
   call ESMF_XgridGet(surf_xgrd, centroid=centroid, _RC)

   block
     ! print the centroig of the element that has the largest value.
     integer :: loc
     loc = maxloc(f_xgrid_Ptr, 1)
     _HERE, loc,  centroid(loc,:)
   end block
   
   call ESMF_FieldRegridStore( srcField=f_xgrid, dstField=f_out, routeHandle=rh2, regridMethod=ESMF_REGRIDMETHOD_CONSERVE, _RC)
   call ESMF_FieldRegrid(f_xgrid, f_out, routehandle=rh2, _RC)
   _HERE, minval(f_out_ptr), maxval(f_out_ptr)

   call ESMF_FieldWrite(f_out, fileName='crazy_latlon.nc', &
        variableName='surf_type', overwrite=.true., iofmt=ESMF_IOFMT_NETCDF4P, _RC)

   call ESMF_FieldRegridStore( srcField=f_xgrid, dstField=f_agrid, routeHandle=rh3, regridMethod=ESMF_REGRIDMETHOD_CONSERVE, _RC)
   call ESMF_FieldRegrid(f_xgrid, f_agrid, routehandle=rh3, _RC)
   _HERE, minval(f_agrid_ptr), maxval(f_agrid_ptr)
   !call ESMF_FieldWrite(f_agrid, fileName='crazy_cubed.nc', &
   !     variableName='surf_type', overwrite=.true., iofmt=ESMF_IOFMT_NETCDF4P, _RC)
  
    
   call ESMF_Finalize(_RC)
end program main

