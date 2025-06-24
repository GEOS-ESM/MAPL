#define I_AM_MAIN
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"
!------------------------------------------------------------------------------
!># Standalone Program for Testing PFIO
!
! Program to write out several records of 2D & 3D geolocated variables in a netCDF file.
! It mimics the prgramming steps of MAPL_Cap and can be used as reference to implement
! PFIO in a non-GEOS model.
!
!#### Usage:
!
!   If we reserve 2 haswell nodes (28 cores in each), want to run the model on 28 cores
!   and use 1 MultiGroup with 5 backend processes, then the execution command is:
!```
! mpiexec -np 56 pfio_MAPL_demo.x --npes_model 28 --oserver_type multigroup --nodes_output_server 1 --npes_backend_pernode 5
!```
!------------------------------------------------------------------------------
program main
      use, intrinsic :: iso_fortran_env, only: REAL32
      use mpi
      use MAPL
      use ESMF
      use pFIO_UnlimitedEntityMod
      use pFIO_ClientManagerMod, only: o_Clients

      implicit none

      ! PARAMETERS:
      real,              PARAMETER ::             pfio_vmin = -MAPL_UNDEF
      real,              PARAMETER ::             pfio_vmax =  MAPL_UNDEF
      real,              PARAMETER ::    pfio_missing_value =  MAPL_UNDEF
      real,              PARAMETER ::       pfio_fill_value =  MAPL_UNDEF
      real,           DIMENSION(2) ::      pfio_valid_range = (/-MAPL_UNDEF, MAPL_UNDEF/)
      integer,           parameter ::     MAX_STRING_LENGTH = 256
      real(kind=REAL64), parameter ::                    PI = 4.0d0*ATAN(1.0d0)
      integer,           parameter ::      num_time_records = 6
      integer,           parameter ::              num_dims = 2 ! number of dimension to decompose

      ! PFIO specific variables
      type(MAPL_FargparseCLI) :: cli
      type(MAPL_CapOptions)   :: cap_options
      type(ServerManager)     :: ioserver_manager
      type(SplitCommunicator) :: split_comm
      type(ArrayReference)    :: ref
      type(FileMetadata)      :: fmd
      Type(Variable)          :: v
      type(StringVariableMap) :: var_map

      ! Global domain variables
      real, parameter :: lon_min = -180.0, lon_max = 180.0
      real, parameter :: lat_min =  -90.0, lat_max =  90.0

      integer, parameter :: IM_WORLD = 360
      integer, parameter :: JM_WORLD = 181
      integer, parameter :: KM_WORLD =  72

      real :: lons(IM_WORLD)
      real :: lats(JM_WORLD)
      real :: levs(KM_WORLD)

      ! Domain decomposition variables
      integer :: NX, NY
      integer :: global_indexX(num_dims)
      integer :: global_indexY(num_dims)
      integer, allocatable :: points_per_procX(:)
      integer, allocatable :: points_per_procY(:)
      integer, allocatable :: map_domainX(:,:,:)
      integer, allocatable :: map_domainY(:,:,:)
      integer, allocatable :: map_proc(:,:)

      integer :: client_comm, npes, ierror
      integer :: status, pe_id, rc
      integer :: i, j, k, hist_id, n, num_steps
      integer :: i1, i2, j1, j2, k1, k2
      real :: hh
      real, allocatable :: local_temp(:,:,:)
      real, allocatable :: local_tracer(:,:)
      character(len=MAX_STRING_LENGTH) :: file_name, var_name
      character(len=4) :: cday
      integer :: day, record_id
      integer :: npes_world, pe_rank
      integer :: subcommunicator

!------------------------------------------------------------------------------

      ! Read and parse the command line, and set parameters
      cli = MAPL_FargparseCLI()
      cap_options = MAPL_CapOptions(cli)

      ! Initialize MPI if MPI_Init has not been called
      call initialize_mpi(MPI_COMM_WORLD)

      call MPI_Comm_size(MPI_COMM_WORLD, npes, __IERROR)
      if ( cap_options%npes_model == -1) then
          cap_options%npes_model = npes
      endif

      ! Initialize the IO Server Manager using parameters defined above
      subcommunicator = create_member_subcommunicator(MPI_COMM_WORLD, 1, npes)
      if (subcommunicator /= MPI_COMM_NULL) then
         call initialize_ioserver(subcommunicator)

         call ioserver_manager%get_splitcomm(split_comm)

         SELECT CASE(split_comm%get_name())
         CASE('model')

            ! Get the model MPI communicator
            client_comm = split_comm%get_subcommunicator()

            CALL ESMF_Initialize(logKindFlag=ESMF_LOGKIND_NONE, mpiCommunicator=client_comm, rc=status)

            ! Get the number of PEs used for the model
            call MPi_Comm_size(client_comm, npes, __IERROR)

            ! Get the PE id
            call MPI_Comm_rank(client_comm, pe_id, __IERROR)
            if (npes /= cap_options%npes_model) stop "sanity check failed"

            !------------------------------------------------
            ! ---> Perform domain decomposition for the model
            !------------------------------------------------
            call perform_domain_deposition()

            ! Allocate model variables
            !-------------------------
            ALLOCATE(local_tracer(i1:i2, j1:j2))
            ALLOCATE(local_temp(i1:i2, j1:j2, k1:k2))

            ! if there are multiple oserver, split it into large and small pool
            call o_clients%split_server_pools()

            call create_file_metada()

            !---------------------------------------------
            ! ---> Model time stepping and writing outputs
            !---------------------------------------------
            call run_model()

            deallocate(local_temp)
            deallocate(local_tracer)
            deallocate(points_per_procX)
            deallocate(points_per_procY)
            deallocate(map_proc)
            deallocate(map_domainX)
            deallocate(map_domainY)

            call i_Clients%terminate()
            call o_Clients%terminate()

            call ESMF_Finalize(endflag=ESMF_END_KEEPMPI, rc=status)
         END SELECT
      END IF

      call ioserver_manager%finalize()

      call MPI_finalize(__IERROR)

!------------------------------------------------------------------------------
CONTAINS
!------------------------------------------------------------------------------
!>
! `create_member_subcommunicator` -- Create a subcommunicator
!
   integer function create_member_subcommunicator(comm, n_members, npes_member, rc) result(subcommunicator)
      use MAPL_SimpleCommSplitterMod
      integer, intent(in) :: comm
      integer :: n_members, npes_member
      integer, optional, intent(out) :: rc

      type(SimpleCommSplitter) :: splitter
      type (SplitCommunicator) :: split_comm

      integer :: status

      subcommunicator = MPI_COMM_NULL ! in case of failure
      splitter = SimpleCommSplitter(comm, n_members, npes_member)
      split_comm = splitter%split(rc=status)
      ! __VERIFY(status)
      subcommunicator = split_comm%get_subcommunicator()

      __UNUSED_DUMMY(rc)

   end function create_member_subcommunicator
!------------------------------------------------------------------------------
!>
! `initialize_mpi` -- Initialized MPI is MPI_Init has not been called yet.
!
   subroutine initialize_mpi(comm)
      integer, intent(in) :: comm
      logical :: mpi_already_initialized
      integer :: ierror
      integer :: provided

      call MPI_Initialized(mpi_already_initialized, ierror)
      if (.not. mpi_already_initialized) then
         call MPI_Init_thread(MPI_THREAD_SINGLE, provided, ierror)
         __VERIFY(ierror)
         __ASSERT(provided == MPI_THREAD_SINGLE, "MPI_THREAD_SINGLE not supported by this MPI.")
      end if

      call MPI_Comm_rank(comm, pe_rank, ierror); __VERIFY(ierror)
      call MPI_Comm_size(comm, npes_world, ierror); __VERIFY(ierror)

   end subroutine initialize_mpi
!------------------------------------------------------------------------------
!>
! `initialize_ioserver` -- Initialize the IO Server using the command line options
!
   subroutine initialize_ioserver(comm)
      integer, intent(in) :: comm
      call ioserver_manager%initialize(comm, &
                    application_size     = cap_options%npes_model, &
                    nodes_input_server   = cap_options%nodes_input_server, &
                    nodes_output_server  = cap_options%nodes_output_server, &
                    npes_input_server    = cap_options%npes_input_server, &
                    npes_output_server   = cap_options%npes_output_server, &
                    oserver_type         = cap_options%oserver_type, &
                    npes_backend_pernode = cap_options%npes_backend_pernode, &
                    isolate_nodes        = cap_options%isolate_nodes, &
                    fast_oclient         = cap_options%fast_oclient, &
                    with_profiler        = cap_options%with_io_profiler, &
                 rc=status)
      __VERIFY(status)
   end subroutine initialize_ioserver
!------------------------------------------------------------------------------
!>
! `perform_domain_deposition` -- Perfom the domain decomposition
!
   subroutine perform_domain_deposition()
      integer, allocatable :: proc_sizes(:)
      !------------------------------------------------
      ! ---> Perform domain decomposition for the model
      !------------------------------------------------
      ! determine the number of processors in each direction
      ALLOCATE(proc_sizes(1:num_dims))
      call decompose_proc(npes, proc_sizes)
      NX = proc_sizes(1)
      NY = proc_sizes(2)
      DEALLOCATE(proc_sizes)

      ! determine the number of grid points each processor will have along the x-direction
      allocate(points_per_procX(0:NX-1))
      call decompose_dim(IM_WORLD, points_per_procX, NX)

      ! determine the number of grid points each processor will have along the y-direction
      allocate(points_per_procY(0:NY-1))
      call decompose_dim(JM_WORLD, points_per_procY, NY)

      allocate(map_proc(0:NX-1, 0:NY-1))
      allocate(map_domainX(0:NX-1, 0:NY-1, 2))
      allocate(map_domainY(0:NX-1, 0:NY-1, 2))

      ! determime the grid local domain corners with respect to the global domain
      call mapping_domain(map_proc, map_domainX, map_domainY, &
                           points_per_procX, points_per_procY, NX, NY, &
                           pe_id, global_indexX, global_indexY)

      i1 = global_indexX(1)
      i2 = global_indexX(2)
      j1 = global_indexY(1)
      j2 = global_indexY(2)
      k1 = 1
      k2 = KM_WORLD

      print '(a7,i5,a5,4i5)', 'pe_id: ', pe_id, '-->', i1, i2, j1, j2
   end subroutine perform_domain_deposition
!------------------------------------------------------------------------------
!>
! `create_file_metada` -- Create the file metada using PFIO methods and the file collection identifier
!
   subroutine create_file_metada()
      !--------------------------------------------------------------
      ! ---> Define dimensions and create variables for a netCDF file
      !--------------------------------------------------------------

      !fmd = FileMetadata()

      ! Define dimensions
      !----------------------
      call fmd%add_dimension('lon', IM_WORLD, rc=status)
      call fmd%add_dimension('lat', JM_WORLD, rc=status)
      call fmd%add_dimension('lev', KM_WORLD, rc=status)
      call fmd%add_dimension('time', pFIO_UNLIMITED, rc=status)

      ! Define variables
      !-----------------
      hh = (lon_max - lon_min)/(IM_WORLD)
      do i = 1, IM_WORLD
         lons(i) = lon_min + (i-1)*hh
      enddo

      v = Variable(type=PFIO_REAL32, dimensions='lon')
      call v%add_attribute('long_name', 'Longitude')
      call v%add_attribute('units', 'degrees_east')
      call v%add_const_value(UnlimitedEntity(lons))
      call fmd%add_variable('lon', v)

      hh = (lat_max - lat_min)/(JM_WORLD-1)
      do j = 1, JM_WORLD
         lats(j) = lat_min + (j-1)*hh
      enddo

      v = Variable(type=PFIO_REAL32, dimensions='lat')
      call v%add_attribute('long_name', 'Latitude')
      call v%add_attribute('units', 'degrees_north')
      call v%add_const_value(UnlimitedEntity(lats))
      call fmd%add_variable('lat', v)

      levs = (/(k, k=1,KM_WORLD)/)

      v = Variable(type=PFIO_REAL32, dimensions='lev')
      call v%add_attribute('long_name', 'Vertical level')
      call v%add_attribute('units', 'layer')
      call v%add_attribute('positive', 'down')
      call v%add_attribute('coordinate', 'eta')
      call v%add_attribute('standard_name', 'model_layers')
      call v%add_const_value(UnlimitedEntity(levs))
      call fmd%add_variable('lev', v)

      v = Variable(type=PFIO_REAL32, dimensions='time')
      call v%add_attribute('long_name', 'time')
      call v%add_attribute('units', 'minutes since 2010-01-03 00:00:00')
      call v%add_attribute('time_increment', 30000)
      call v%add_attribute('begin_date', 20100103)
      call v%add_attribute('begin_time', 0)
      call fmd%add_variable('time', v)

      var_name = "temperature"
      call add_fvar(fmd, TRIM(var_name), PFIO_REAL32, 'lon,lat,lev,time', &
                          units     = 'K', &
                          long_name = TRIM(var_name), &
                          rc        = status)

      var_name = "tracer"
      call add_fvar(fmd, TRIM(var_name), PFIO_REAL32, 'lon,lat,time', &
                          units     = 'mol mol-1', &
                          long_name = TRIM(var_name), &
                          rc        = status)

      ! Set File attributes
      call fmd%add_attribute('Convention', 'COARDS')
      call fmd%add_attribute('Source', 'GMAO')
      call fmd%add_attribute('Title', 'Sample code to test PFIO')
      call fmd%add_attribute('HISTORY', 'File writtem by PFIO vx.x.x')

      hist_id = o_clients%add_hist_collection(fmd)
   end subroutine create_file_metada
!------------------------------------------------------------------------------
!>
! `run_model` -- Run the model and write out the data
!
   subroutine run_model()
      day = 1
      num_steps = 18
      do n = 1, num_steps
         IF (pe_id == 0) PRINT*, "In Stepping: ", n

         ! Check if a new file (in the same collection) needs to be created.
         IF (MOD(n, num_time_records) == 1) THEN
            record_id = 1
            write(cday, '(I4.4)') day
            file_name = 'Nsample_pfio_file_Day'//cday//'.nc4'

            v = Variable(type=PFIO_REAL32, dimensions='time')
            call v%add_attribute('long_name', 'time')
            call v%add_attribute('units', 'minutes since 2010-01-03 00:00:00')
            call v%add_attribute('time_increment', 30000)
            call v%add_attribute('begin_date', 20100103)
            call v%add_attribute('begin_time', 0)
            call var_map%insert('time', v)
            call o_clients%modify_metadata(hist_id, var_map=var_map, rc=status)
            IF (pe_id == 0) print '(i5,a,a)', n, '  Created: ', file_name
         ENDIF

         ! Update variables
         !-----------------
         do k = k1, k2
            call set_temperature(local_temp(:,:,k))
         enddo

         call set_tracer(local_tracer(:,:))

         ! Write variables in netCDF file using PFIO methods
         !--------------------------------------------------
         call o_clients%set_optimal_server(nwriting=1)

         var_name = "temperature"
         ref =  ArrayReference(local_temp)
         call o_clients%collective_stage_data(hist_id, TRIM(file_name), &
                                   TRIM(var_name), ref, &
                                   start        = [i1,j1,k1,1], &
                                   global_start = [1,1,1,record_id], &
                                   global_count = [IM_WORLD,JM_WORLD,KM_WORLD,1])

         var_name = "tracer"
         ref =  ArrayReference(local_tracer)
         call o_clients%collective_stage_data(hist_id, TRIM(file_name), &
                                   TRIM(var_name), ref, &
                                   start        = [i1,j1,1], &
                                   global_start = [1,1,record_id], &
                                   global_count = [IM_WORLD,JM_WORLD,1])

         ! Is this the last record?
         IF (MOD(n, num_time_records) == 0) THEN
            day = day + 1
            record_id = 0
            ! write in the file and close it
            call o_clients%done_collective_stage()
         ENDIF
         call o_clients%post_wait()
         record_id = record_id + 1
         IF (pe_id == 0) PRINT*, "Out Stepping: ", n
      enddo
   end subroutine run_model
!------------------------------------------------------------------------------
!>
! `add_fvar` -- PFIO utility routine to create a variable and set attributes
!
   subroutine add_fvar(cf, vname, vtype, dims, units, long_name ,rc)
      type(FileMetadata), intent(inout) :: cf
      integer,          intent(in) :: vtype
      character(len=*), intent(in) :: vname
      character(len=*), intent(in) :: dims
      character(len=*), optional, intent(in) :: units
      character(len=*), optional, intent(in) :: long_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(Variable) :: fvar

      fvar = Variable(type=vtype, dimensions=TRIM(dims))
      if (present(units))     call fvar%add_attribute('units', TRIM(units))
      if (present(long_name)) call fvar%add_attribute('long_name', TRIM(long_name))
      call fvar%add_attribute("scale_factor", 1.0)
      call fvar%add_attribute("add_offset", 0.0)
      !if (mod(pe_id,2) == 0) then
      !   call fvar%add_attribute("missing_value", -9999.0)
      !else
      !   call fvar%add_attribute("missing_value", pfio_missing_value)
      !endif
      call fvar%add_attribute("missing_value", pfio_missing_value)
      call fvar%add_attribute("_FillValue", pfio_fill_value)
      call fvar%add_attribute('valid_range', pfio_valid_range)
      call fvar%add_attribute("vmin", pfio_vmin)
      call fvar%add_attribute("vmax", pfio_vmax)
      call cf%add_variable(TRIM(vname), fvar, rc=status)
      __VERIFY(status)
   end subroutine add_fvar
!------------------------------------------------------------------------------
!>
! `decompose_dim` --
! For a given number of grid points along a dimension and a number of
! available processors for that diemsion,, !! determine the number of
! grid points assigned to each processor.
!
   subroutine decompose_dim(dim_world, dim_array, num_procs )
!
      integer, intent(in)  :: dim_world                !! total number of grid points
      integer, intent(in)  :: num_procs                !! number of processors
      integer, intent(out) :: dim_array(0:num_procs-1)
!
      integer ::   n, im, rm
!------------------------------------------------------------------------------
      im = dim_world/num_procs
      rm = dim_world-num_procs*im
      do n = 0, num_procs-1
                      dim_array(n) = im
      if( n.le.rm-1 ) dim_array(n) = im+1
      enddo
   end subroutine decompose_dim
!------------------------------------------------------------------------------
!>
! `decompose_proc` --
!  Given the total number of available processors and the number of dimensions,
! determine the number of processors along each dimension.
!
   subroutine decompose_proc(num_procs, proc_sizes)
!
      integer, intent(in)  :: num_procs      !! number of processors
      integer, intent(out) :: proc_sizes(2)  !! array for the num of procs in each dimension
!------------------------------------------------------------------------------
      proc_sizes(1) = INT(FLOOR(SQRT(num_procs*1.0)))
      DO
         IF (MOD(num_procs, proc_sizes(1)) == 0) THEN
            proc_sizes(2) = INT(num_procs / proc_sizes(1))
            EXIT
         END IF
         proc_sizes(1) = proc_sizes(1) + 1
      END DO
   end subroutine decompose_proc
!------------------------------------------------------------------------------
!>
! `mapping_domain` --
! Determime the indices of the local domain corners
! with respect to the global domain.
!
   subroutine mapping_domain(map_proc, map_domainX, map_domainY, &
                        points_per_procX, points_per_procY, NX, NY, &
                        proc_id, global_indexX, global_indexY)

      integer, intent(in)  :: proc_id
      integer, intent(in)  :: NX                       !! num of procs along x-axis
      integer, intent(in)  :: NY                       !! num of procs along y-axis
      integer, intent(in)  :: points_per_procX(0:NX-1) !! num of grid points/proc along x-axis
      integer, intent(in)  :: points_per_procY(0:NY-1) !! num of grid points/proc along y-axis
!
      integer, intent(out) :: map_proc(0:NX-1, 0:NY-1)       !! mapping procs to subdomains
      integer, intent(out) :: map_domainX(0:NX-1, 0:NY-1, 2) !! sub-domain x-corners
      integer, intent(out) :: map_domainY(0:NX-1, 0:NY-1, 2) !! sub-domain y-corners
      integer, intent(out) :: global_indexX(2)               !! Global sub-domain x-corners for proc_id
      integer, intent(out) :: global_indexY(2)               !! Global sub-domain y-corners for proc_id
!
      integer ::  ix, iy, prevX, prevY, lastY
      logical :: firstX, firstY
!------------------------------------------------------------------------------
      firstX = .TRUE.
      firstY = .TRUE.
      do iy = 0, NY-1
         if (firstY) then
            prevY = 1
            firstY = .FALSE.
         else
            prevY = lastY
         endif
         do ix = 0, NX-1
            map_proc(ix,iy) = ix + iy*NX

            if (firstX) then
               prevX = 1
               firstX = .FALSE.
            endif
            map_domainX(ix,iy, 1) = prevX
            map_domainX(ix,iy, 2) = prevX + points_per_procX(ix) - 1
            prevX = map_domainX(ix,iy, 2) + 1

            map_domainY(ix,iy, 1) = prevY
            map_domainY(ix,iy, 2) = prevY + points_per_procY(iy) - 1
            lastY = map_domainY(ix,iy, 2) + 1

            if ( map_proc(ix,iy) == proc_id ) then
               global_indexX(1) = map_domainX(ix,iy, 1)
               global_indexX(2) = map_domainX(ix,iy, 2)
               global_indexY(1) = map_domainY(ix,iy, 1)
               global_indexY(2) = map_domainY(ix,iy, 2)
            endif
         enddo
         firstX = .TRUE.
      enddo
   end subroutine mapping_domain
!------------------------------------------------------------------------------
!>
! `set_tracer` -- Arbitrary set values for a field
!
   subroutine set_tracer(var)
      real , intent(out) ::    var(i1:i2, j1:j2)
      integer :: i, j

      do j = j1, j2
         do i = i1,i2
            !var(i,j) = lons(i)
            var(i,j) = 10.0 + 5.0*sin(lons(i)/lon_max*PI) + 2.0*sin(lats(j)/lat_max*PI)
         enddo
      enddo

   end subroutine set_tracer
!------------------------------------------------------------------------------
!>
! `set_temperature` -- Arbitrary set values for the temperature field.
!
   subroutine set_temperature(var)

      real , intent(out) ::    var(i1:i2, j1:j2)
      integer :: i, j

      do j = j1, j2
         do i = i1,i2
            var(i,j) =  2.0 + cos(2.0*lons(i)/lon_max*PI) &
                        *cos(lats(j)/lat_max*PI)*cos(lats(j)/lat_max*PI)
         enddo
      enddo

   end subroutine set_temperature
!------------------------------------------------------------------------------
end program main
