#define I_AM_MAIN
#include "MAPL_ErrLog.h"

program main
   use mapl3g_Geom_API
   use mapl_ErrorHandling
   use esmf
   use, intrinsic :: iso_fortran_env, only: INT64, REAL64
   
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

   integer(kind=INT64) :: c0, c1, crate
   integer :: pet
   type(ESMF_VM) :: vm
   type(ESMF_DistGrid) :: dist_grid

!!$   character(*), parameter :: cs_config ="{class: CubedSphere, im_world: 90, nx: 1, ny: 1}
!!$   character(*), parameter :: cs_config ="{class: CubedSphere, im_world: 1440, nx: 5, ny: 1}"
!!$   character(*), parameter :: cs_config ="{class: CubedSphere, im_world: 1440, nx: 5, ny: 2}"
!!$   character(*), parameter :: cs_config ="{class: CubedSphere, im_world: 1440, nx: 5, ny: 4}"
   character(*), parameter :: cs_config ="{class: CubedSphere, im_world: 1440, nx: 10, ny: 4}"
!!$   character(*), parameter :: cs_config ="{class: CubedSphere, im_world: 1440, nx: 10, ny: 8}"
!!$   character(*), parameter :: cs_config ="{class: CubedSphere, im_world: 1440, nx: 10, ny: 16}"
   
   
   call ESMF_Initialize(_RC)
   call ESMF_VMGetGlobal(vm, _RC)
   call ESMF_VMget(vm, localPet=pet, _RC)
   geom_mgr = GeomManager()
   call geom_mgr%initialize()

   hconfig = ESMF_HConfigCreate(content=cs_config, rc=status)

   mapl_geom => geom_mgr%get_mapl_geom(hconfig, _RC)
   atm_geom = mapl_geom%get_geom()
   call ESMF_GeomGet(atm_geom, grid=atm_grid, _RC)

!#   hconfig = ESMF_HConfigCreate(content="{class: latlon, im_world: 360, jm_world: 180, pole: PC, dateline: DC, nx: 3, ny: 2}", _RC)
!#   hconfig = ESMF_HConfigCreate(content="{class: latlon, im_world: 360, jm_world: 180, pole: PC, dateline: DC, nx: 9, ny: 6}", _RC)

!#   hconfig = ESMF_HConfigCreate(content="{class: latlon, im_world: 1440, jm_world: 720, pole: PC, dateline: DC, nx: 6, ny: 5}", _RC)
!#   hconfig = ESMF_HConfigCreate(content="{class: latlon, im_world: 1440, jm_world: 720, pole: PC, dateline: DC, nx: 15, ny: 4}", _RC)
!#   hconfig = ESMF_HConfigCreate(content="{class: latlon, im_world: 1440, jm_world: 720, pole: PC, dateline: DC, nx: 15, ny: 8}", _RC)
   hconfig = ESMF_HConfigCreate(content="{class: latlon, im_world: 1440, jm_world: 720, pole: PC, dateline: DC, nx: 30, ny: 8}", _RC)
!!$   hconfig = ESMF_HConfigCreate(content="{class: latlon, im_world: 1440, jm_world: 720, pole: PC, dateline: DC, nx: 30, ny: 16}", _RC)
!!$   hconfig = ESMF_HConfigCreate(content="{class: latlon, im_world: 1440, jm_world: 720, pole: PC, dateline: DC, nx: 60, ny: 16}", _RC)

   mapl_geom => geom_mgr%get_mapl_geom(hconfig, _RC)
   out_geom = mapl_geom%get_geom()
   call ESMF_GeomGet(out_geom, grid=out_grid, _RC)

!#   elementCount = 8077482
!#   dist_grid = ESMF_DistGridCreate(minIndex=[1], maxIndex=[elementCount], _RC)
   call system_clock(c0, crate)
   surf_mesh = ESMF_MeshCreate('surface_types.nc', fileFormat=ESMF_FILEFORMAT_ESMFMESH, _RC)
   block
     real(kind=REAL64), allocatable :: nodeCoords(:)
     integer :: elementCount
     call ESMF_MeshGet(surf_mesh, elementCount=elementCount, _RC)
     allocate(nodeCoords(2*elementCount))
     nodeCoords = -HUGE(1.d0)
     call ESMF_MeshGet(surf_mesh, nodeCoords=nodeCoords, _RC)
     if (any(abs(nodeCoords(::2))> 90.1)) then
        _HERE, pet, elementCount
        _HERE, pet, maxloc(nodeCoords(::2)), nodeCoords(2*maxloc(nodeCoords(::2),1))
     end if
   end block
   call system_clock(c1)
   if (pet == 0) _HERE,'time to create mesh (read file): ', real(c1-c0)/crate

   call system_clock(c0, crate)
   surf_xgrd = ESMF_XgridCreate(sideAGrid=[atm_grid], sideBMesh=[surf_mesh], storeOverlay=.true., _RC)
   call system_clock(c1)
   if (pet == 0) _HERE,'time for xgrid create: ', real(c1-c0)/crate

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

   call system_clock(c0, crate)
   call ESMF_FieldRegridStore(surf_xgrd, srcField=f_mesh, dstField=f_xgrd, routeHandle=rh, regridMethod=ESMF_REGRIDMETHOD_CONSERVE, _RC)
   call ESMF_FieldRegrid(f_mesh, f_xgrd, routeHandle=rh, _RC)
   call system_clock(c1)
   if (pet==0)_HERE,'time for xgrid regrid store: ', real(c1-c0)/crate


   if (pet==0) _HERE
   call ESMF_XgridGet(surf_xgrd, elementCount=elementCount, _RC)
   allocate(centroid(elementCount,2))
   call ESMF_XgridGet(surf_xgrd, centroid=centroid, _RC)
   if (pet==0) _HERE

   block
     ! print the centroid of the element that has the largest value.
     integer :: loc
     loc = maxloc(x_xgrd, 1)
     if (abs(centroid(loc,2)) > 100) then
        _HERE, pet, loc,  centroid(loc,:)
     end if
   end block
   
   call system_clock(c0, crate)
   if (pet==0) _HERE
   call ESMF_FieldRegridStore(srcField=f_xgrd, dstField=f_out, routeHandle=rh2, regridMethod=ESMF_REGRIDMETHOD_CONSERVE, _RC)
   if (pet==0) _HERE
   call ESMF_FieldRegrid(f_xgrd, f_out, routehandle=rh2, _RC)
   if (pet==0) _HERE
   call system_clock(c1)
   if (pet==0) then
      _HERE,'time for last regrid store: ', real(c1-c0)/crate
      _HERE, minval(x_out), maxval(x_out)
   end if

   call ESMF_FieldWrite(f_out, fileName='crazy_latlon.nc', &
        variableName='surf_type', overwrite=.true., iofmt=ESMF_IOFMT_NETCDF4P, _RC)
   
   call ESMF_Finalize(_RC)

contains

!##undef I_AM_MAIN
!##include "MAPL_ErrLog.h"
!#
!#
!#   ! 0)
!#   !    - Create empty mesh
!#   !    - read file metadata (dim sizes)
!#   !    - notionally assign elements to PETs   locElements=[...]
!#   !       dont split "original" polygon
!#
!#
!#   ! 1) MeshAddNodes
!#   !    - each nodes in elementConn(1,locElements) are *assigned* to local PET
!#   !         - table(
!#   !    - each nodes in elementConn(:,locElements) is in local call
!#   !    - remaining nodes - assign to pet of first element in which they appear
!#   !    - use local offset in call
!#
!#   ! 2) MeshAddElements
!#   !    locElements
!#   !    elementConn(:,locElements)  but adjust for local id.
!#
!#   function make_mesh(filename, rc) result(m)
!#      type(ESMF_Mesh) :: m
!#      character(*), intent(in) :: filename
!#      integer, optional, intent(out) :: rc
!#
!#      integer :: status
!#      type(FileMetadata) :: filemd
!#      type(NetCDF4_FileFormatter) :: formatter
!#      integer, allocatable :: elementCount
!#      integer, allocatable :: nodeCount
!#      int      integer, allocatable :: elementConn(:,:)
!#      integer, allocatable :: elementMask(:)
!#      real(kind=REAL64), allocatable :: nodeCoords
!#      integer :: i
!#
!#      if (pet == 0) then
!#         call formatter%read(file=filename, mode=pFIO_READ, _RC)
!#
!#         elementCount = ...
!#         nodeCount = ...
!#         allocate(nodeCoords(2,nodeCount)
!#         call formatter%get_var('nodeCoords', nodeCoords, _RC)
!#
!#      end if
!#
!#      
!#      central_node_ids = < 1st column of conn >
!#
!#      call scatter(nodeCoords)
!#      allocate(
!#      m = ESMF_MeshCreate(...)
!#      call ESM_MeshAddNodes(m, nodeIds=[(i,i=1,loc_nodeCount)], nodeCoords=..., _RC)
!#      deallocate(nodeCoords)
!#
!#      if (pet == 0) then
!#         allocate(elementConn(3,elementCount))
!#         allocate(elementMask(elementCount))
!#         
!#         call formatter%read('elementConn', elementConn, _RC)
!#         call formatter%read('elementMask', elementConn, _RC)
!#      end if
!#
!#      call scatter(elementConn, _RC)
!#      call scatter(elementMask, _RC)
!#      call ESMF_MeshAddElements(m, elementConn=elementConn, _RC)
!#
!#      deallocate(elementConn, elementMask)
!#
!#   end function make_mesh
!#      
!#      _RETURN(_SUCCESS)
!#   end function make_mesh
!#
end program main

