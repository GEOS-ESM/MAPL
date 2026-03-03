#define I_AM_MAIN
#include "MAPL_ErrLog.h"

program main
   use mapl3g_Geom_API
   use mapl_ErrorHandling
   use esmf
   use mpi

   implicit none(type,external)

   type(ESMF_Mesh) :: surf_mesh
   type(ESMF_Geom) :: atm_geom
   type(ESMF_Grid) :: atm_grid
   type(ESMF_Xgrid) :: surf_xgrd
   type(ESMF_RouteHandle) :: rh
   integer :: status
   type(ESMF_VM)    :: vm
   type(ESMF_Field) :: f_mesh, f_agrid, f_xgrid
   real(kind=ESMF_KIND_R4), pointer :: f_mesh_Ptr(:)
   real(kind=ESMF_KIND_R4), pointer :: f_xgrid_Ptr(:)
   real(kind=ESMF_KIND_R4), pointer :: f_agrid_Ptr(:,:)
   integer, allocatable :: elementMask(:)
   integer :: elementCount, localPet, petCount
   real(kind=ESMF_KIND_R8) :: start_time, end_time, elapsed_time
   type(GeomManager) :: geom_mgr
   type(ESMF_HConfig) :: hconfig
   type(MaplGeom), pointer :: mapl_geom
   type(ESMF_Geom) :: geom

   integer           :: i, nargs
   character(len=16) :: im_world, nx, ny, arg
   character(len=256) :: infile
   character(:), allocatable :: content

   nargs = command_argument_count()

   if (nargs == 0) then
     call print_usage()
     stop
   end if
   i = 1
   do while ( i <= nargs)
      call get_command_argument(i, arg, status=status)
      if (status /=0) then
        print*, "Error, cannot retrieve argument"
         stop 1
      endif
      select case ( trim(arg))
        case('-i', '--input')
          i = i + 1
          if (i > nargs) then
             print *, 'Error: -i/--input requires filename '
             stop 1
          end if
          call get_command_argument(i, infile, status=status)
        case('-x', '--nx')
          i = i + 1
          if (i > nargs) then
             print *, 'Error: -x/--nx requires nx'
             stop 1
          end if
          call get_command_argument(i, nx, status=status)
        case('-y', '--ny')
          i = i + 1
          if (i > nargs) then
             print *, 'Error: -y/--ny requires ny'
             stop 1
          end if
          call get_command_argument(i, ny, status=status)
        case('-m', '--im_world')
          i = i + 1
          if (i > nargs) then
             print *, 'Error: -m/--im_world requires target im_world'
             stop 1
          end if
          call get_command_argument(i, im_world, status=status)
        case ('-h', '--help')
           call print_usage()
           stop
        case default
           print *, 'Error: Unknown option: ', trim(arg)
           call print_usage()
           stop 1
        end select
        i = i + 1
   end do


   call ESMF_Initialize(_RC)

   call ESMF_VMGetGlobal(VM, _RC)
   call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, _RC)

   content = "{class: CubedSphere, im_world: "//trim(im_world)//", nx: "//trim(nx)// ", ny: "//trim(ny)//"}"

   if (localPet == 0) then
      print*, content
      print*, trim(infile)
   endif

   geom_mgr = GeomManager()
   call geom_mgr%initialize()

   hconfig = ESMF_HConfigCreate(content=content, rc=status)
   mapl_geom => geom_mgr%get_mapl_geom(hconfig, _RC)
   atm_geom = mapl_geom%get_geom()
   call ESMF_GeomGet(atm_geom, grid=atm_grid, _RC)

   surf_mesh = ESMF_MeshCreate( trim(infile), fileFormat=ESMF_FILEFORMAT_ESMFMESH, _RC)
   call ESMF_VMBarrier(vm, _RC)
   start_time = MPI_WTIME()
   surf_xgrd  = ESMF_XgridCreate(sideAGrid=[atm_grid], sideBMesh=[surf_mesh], storeOverlay=.true., _RC)
   call ESMF_VMBarrier(vm, _RC)
   end_time   = MPI_WTIME()
   ! Calculate elapsed time
   elapsed_time = end_time - start_time
    ! Print the result for a specific rank
   IF (localPet == 0) THEN
      PRINT *, "Elapsed time for creating xgrid: ", elapsed_time, " seconds"
   END IF

   f_mesh  = ESMF_FieldCreate(name='f_mesh',  mesh=surf_mesh, typekind=ESMF_TYPEKIND_R4, meshloc=ESMF_MESHLOC_ELEMENT, _RC)
   f_agrid = ESMF_FieldCreate(name='f_agrid', grid=atm_grid,   typekind=ESMF_TYPEKIND_R4, _RC)
   f_xgrid = ESMF_FieldCreate(name='f_xgrd',  xgrid=surf_xgrd, typekind=ESMF_TYPEKIND_R4, _RC)

   call ESMF_FieldGet(f_mesh, fArrayPtr=f_mesh_Ptr, _RC)
   call ESMF_FieldGet(f_xgrid, fArrayPtr=f_xgrid_Ptr, _RC)
   call ESMF_FieldGet(f_agrid, fArrayPtr=f_agrid_Ptr, _RC)

   f_xgrid_Ptr  = -1
   f_agrid_Ptr  = -1
   
   call ESMF_MeshGet(surf_mesh, elementCount=elementCount, _RC)
   allocate(elementMask(elementCount))
   call ESMF_MeshGet(surf_mesh, elementMask=elementMask, _RC)
   f_mesh_Ptr = elementMask
   
   call ESMF_VMBarrier(vm, _RC)
   start_time = MPI_WTIME()
   call ESMF_FieldRegridStore(xgrid=surf_xgrd, srcField=f_mesh, dstField=f_xgrid, routeHandle=rh, regridMethod=ESMF_REGRIDMETHOD_CONSERVE, _RC)
   call ESMF_VMBarrier(vm, _RC)
   end_time   = MPI_WTIME()
   ! Calculate elapsed time
   elapsed_time = end_time - start_time
    ! Print the result for a specific rank
   IF (localPet == 0) THEN
      PRINT *, "Elapsed time for creating route handle: ", elapsed_time, " seconds"
   END IF
   
   call ESMF_VMBarrier(vm, _RC)
   start_time = MPI_WTIME()
   call ESMF_FieldRegrid(f_mesh, f_xgrid, routeHandle=rh, _RC)
   call ESMF_VMBarrier(vm, _RC)
   end_time   = MPI_WTIME()
   ! Calculate elapsed time
   elapsed_time = end_time - start_time
    ! Print the result for a specific rank
   IF (localPet == 0) THEN
      PRINT *, "Elapsed time for regridding from mesh to xgrid: ", elapsed_time, " seconds"
   END IF

   call ESMF_FieldRegridRelease(routehandle=rh, _RC)
   call ESMF_VMBarrier(vm, _RC)
   start_time = MPI_WTIME()
   call ESMF_FieldRegridStore( xgrid=surf_xgrd, srcField=f_xgrid, dstField=f_agrid, routeHandle=rh, regridMethod=ESMF_REGRIDMETHOD_CONSERVE, _RC)
   call ESMF_VMBarrier(vm, _RC)
   end_time   = MPI_WTIME()
   ! Calculate elapsed time
   elapsed_time = end_time - start_time
    ! Print the result for a specific rank
   IF (localPet == 0) THEN
      PRINT *, "Elapsed time for creating handle from xgrid to agrid: ", elapsed_time, " seconds"
   END IF


   call ESMF_VMBarrier(vm, _RC)
   start_time = MPI_WTIME()
   call ESMF_FieldRegrid(f_xgrid, f_agrid, routehandle=rh, _RC)
   call ESMF_VMBarrier(vm, _RC)
   end_time   = MPI_WTIME()
   ! Calculate elapsed time
   elapsed_time = end_time - start_time
    ! Print the result for a specific rank
   IF (localPet == 0) THEN
      PRINT *, "Elapsed time for regriding from xgrid to agrid: ", elapsed_time, " seconds"
   END IF


   call ESMF_FieldRegridRelease(routehandle=rh, _RC)
   call ESMF_VMBarrier(vm, _RC)
   start_time = MPI_WTIME()
   call ESMF_FieldRegridStore( srcField=f_mesh, dstField=f_agrid, routeHandle=rh, regridMethod=ESMF_REGRIDMETHOD_CONSERVE, _RC)
   call ESMF_VMBarrier(vm, _RC)
   end_time   = MPI_WTIME()
   ! Calculate elapsed time
   elapsed_time = end_time - start_time
    ! Print the result for a specific rank
   IF (localPet == 0) THEN
      PRINT *, "Elapsed time for creating handle from mesh to agrid: ", elapsed_time, " seconds"
   END IF


   call ESMF_VMBarrier(vm, _RC)
   start_time = MPI_WTIME()
   call ESMF_FieldRegrid(f_mesh, f_agrid, routehandle=rh, _RC)
   call ESMF_VMBarrier(vm, _RC)
   end_time   = MPI_WTIME()
   ! Calculate elapsed time
   elapsed_time = end_time - start_time
    ! Print the result for a specific rank
   IF (localPet == 0) THEN
      PRINT *, "Elapsed time for regriding from mesh to agrid: ", elapsed_time, " seconds"
   END IF

   call ESMF_FieldDestroy(f_mesh,  _RC)
   call ESMF_FieldDestroy(f_agrid, _RC)
   call ESMF_FieldDestroy(f_xgrid, _RC)

   call ESMF_GridDestroy(atm_grid,  _RC)
   call ESMF_XGridDestroy(surf_xgrd, _RC)
   call ESMF_MeshDestroy(surf_mesh, _RC)
   call ESMF_FieldRegridRelease(routehandle=rh, _RC)
   call ESMF_Finalize(_RC)

contains
  subroutine print_usage()
        print *, 'Usage: xgrid to benchmark performance'
        print *, ''
        print *, 'Options:'
        print *, '  -i, --input    File        Input file that contains mesh info'
        print *, '  -m, --im_world im_world    target grid resolution'
        print *, '  -x, --nx       nx          nx distribution '
        print *, '  -y, --ny       ny          ny distribution '
        print *, '  -h, --help             Display this help message'
        print *, ''
        print *, 'Example:'
        print *, 'mpirun -np 96 ./xgrid.x -i sufrace_mesh.nc4 -m 1440 -x 4 -y 24'
   end subroutine print_usage


end program main

