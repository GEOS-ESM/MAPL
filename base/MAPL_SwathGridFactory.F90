#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_SwathGridFactoryMod
   use MAPL_AbstractGridFactoryMod
   use MAPL_MinMaxMod
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_ShmemMod
   use mapl_ErrorHandlingMod
   use MAPL_Constants
   use MAPL_Base, only : MAPL_GridGetInterior
   use ESMF
   use pFIO
   use MAPL_CommsMod
   !!use netcdf
   !!use Plain_netCDF_Time
   use MAPL_ObsUtilMod
   use pflogger,    only : Logger, logging
   use, intrinsic :: iso_fortran_env, only: REAL32
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none
   integer, parameter :: gridLabel_max = 20
   integer, parameter :: mx_file = 300
   private

   public :: SwathGridFactory

   type, extends(AbstractGridFactory) :: SwathGridFactory
      private
      character(len=:), allocatable :: grid_name
      character(len=:), allocatable :: grid_file_name
      character(len=ESMF_MAXSTR)    :: filenames(mx_file)
      integer                       :: M_file

      integer :: cell_across_swath
      integer :: cell_along_swath
      integer :: im_world = MAPL_UNDEFINED_INTEGER
      integer :: jm_world = MAPL_UNDEFINED_INTEGER
      integer :: lm = MAPL_UNDEFINED_INTEGER
      logical :: force_decomposition = .false.

      integer :: epoch                         ! unit: second
      integer(ESMF_KIND_I8) :: epoch_index(4)  ! is,ie,js,je
      real(ESMF_KIND_R8), allocatable:: t_alongtrack(:)
      ! note: this var is not deallocated in swathfactory, use caution
      character(len=ESMF_MAXSTR)     :: tunit
      character(len=ESMF_MAXSTR)     :: index_name_lon
      character(len=ESMF_MAXSTR)     :: index_name_lat
      character(len=ESMF_MAXSTR)     :: var_name_lon
      character(len=ESMF_MAXSTR)     :: var_name_lat
      character(len=ESMF_MAXSTR)     :: var_name_time
      character(len=ESMF_MAXSTR)     :: input_template
      logical                        :: found_group

      type(ESMF_Time)                :: obsfile_start_time   ! user specify
      type(ESMF_TimeInterval)        :: obsfile_interval
      type(ESMF_TimeInterval)        :: EPOCH_FREQUENCY
      integer                        :: obsfile_Ts_index     ! for epoch
      integer                        :: obsfile_Te_index
      logical                        :: is_valid

      ! Domain decomposition:
      integer :: nx = MAPL_UNDEFINED_INTEGER
      integer :: ny = MAPL_UNDEFINED_INTEGER
      integer, allocatable :: ims(:)
      integer, allocatable :: jms(:)
      ! Used for halo
      type (ESMF_DELayout) :: layout
      logical :: initialized_from_metadata = .false.
   contains
      procedure :: make_new_grid
      procedure :: create_basic_grid
      procedure :: add_horz_coordinates_from_file
      procedure :: init_halo
      procedure :: halo

      procedure :: initialize_from_file_metadata
      procedure :: initialize_from_config_with_prefix
      procedure :: initialize_from_esmf_distGrid

      procedure :: equals
      procedure :: check_and_fill_consistency
      procedure :: generate_grid_name
      procedure :: to_string

      procedure :: append_metadata
      procedure :: get_grid_vars
      procedure :: get_file_format_vars
      procedure :: append_variable_metadata
      procedure :: check_decomposition
      procedure :: generate_newnxy
      procedure :: generate_file_bounds
      procedure :: generate_file_corner_bounds
      procedure :: generate_file_reference2D
      procedure :: generate_file_reference3D
      procedure :: decomps_are_equal
      procedure :: physical_params_are_equal

      procedure :: get_xy_subset
      procedure :: destroy
      procedure :: get_obs_time
   end type SwathGridFactory

   character(len=*), parameter :: MOD_NAME = 'MAPL_SwathGridFactory::'

   interface SwathGridFactory
      module procedure SwathGridFactory_from_parameters
   end interface SwathGridFactory

   interface set_with_default
      module procedure set_with_default_integer
      module procedure set_with_default_real
      module procedure set_with_default_real64
      module procedure set_with_default_character
      module procedure set_with_default_bounds
   end interface set_with_default

contains

   function SwathGridFactory_from_parameters(unusable, grid_name, &
        & im_world, jm_world, lm, nx, ny, ims, jms, rc) result(factory)
      type (SwathGridFactory) :: factory
      class (KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: grid_name

      ! grid details:
      integer, optional, intent(in) :: im_world
      integer, optional, intent(in) :: jm_world
      integer, optional, intent(in) :: lm

      ! decomposition:
      integer, optional, intent(in) :: nx
      integer, optional, intent(in) :: ny
      integer, optional, intent(in) :: ims(:)
      integer, optional, intent(in) :: jms(:)

      integer, optional, intent(out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      call set_with_default(factory%grid_name, grid_name, MAPL_GRID_NAME_DEFAULT)
      call set_with_default(factory%nx, nx, MAPL_UNDEFINED_INTEGER)
      call set_with_default(factory%ny, ny, MAPL_UNDEFINED_INTEGER)
      call set_with_default(factory%im_world, im_world, MAPL_UNDEFINED_INTEGER)
      call set_with_default(factory%jm_world, jm_world, MAPL_UNDEFINED_INTEGER)
      call set_with_default(factory%lm, lm, MAPL_UNDEFINED_INTEGER)

      ! default is unallocated
      if (present(ims)) factory%ims = ims
      if (present(jms)) factory%jms = jms

      call factory%check_and_fill_consistency(_RC)

      _RETURN(_SUCCESS)
   end function SwathGridFactory_from_parameters


   function make_new_grid(this, unusable, rc) result(grid)
      type (ESMF_Grid) :: grid
      class (SwathGridFactory), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status

      _UNUSED_DUMMY(unusable)

      grid = this%create_basic_grid(_RC)
      call this%add_horz_coordinates_from_file(grid,_RC)
      _RETURN(_SUCCESS)
   end function make_new_grid


   function create_basic_grid(this, unusable, rc) result(grid)
      type (ESMF_Grid) :: grid
      class (SwathGridFactory), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_Info) :: infoh

      _UNUSED_DUMMY(unusable)

      grid = ESMF_GridCreateNoPeriDim( &
           & name = this%grid_name, &
           & countsPerDEDim1=this%ims, &
           & countsPerDEDim2=this%jms, &
           & indexFlag=ESMF_INDEX_DELOCAL, &
           & coordDep1=[1,2], &
           & coordDep2=[1,2], &
           & coordSys=ESMF_COORDSYS_SPH_RAD, &
           & _RC)

      ! Allocate coords at default stagger location
      call ESMF_GridAddCoord(grid, _RC)
      call ESMF_InfoGetFromHost(grid,infoh,_RC)

      if (this%lm /= MAPL_UNDEFINED_INTEGER) then
         call ESMF_InfoSet(infoh, 'GRID_LM', this%lm, _RC)
      end if
      call ESMF_InfoSet(infoh, 'GridType', 'Swath', _RC)
      call ESMF_InfoSet(infoh, 'Global', .false., _RC)

      _RETURN(_SUCCESS)
   end function create_basic_grid


   subroutine add_horz_coordinates_from_file(this, grid, unusable, rc)
      use MAPL_BaseMod, only: MAPL_grid_interior
      implicit none
      class (SwathGridFactory), intent(in) :: this
      type (ESMF_Grid), intent(inout) :: grid
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status

      real(kind=ESMF_KIND_R8), pointer :: fptr(:,:)
      real(kind=ESMF_KIND_R8), allocatable :: lon_true(:,:)
      real(kind=ESMF_KIND_R8), allocatable :: lat_true(:,:)
      real(kind=ESMF_KIND_R8), allocatable :: time_true(:,:)
      real(kind=ESMF_KIND_R8), pointer :: arr_lon(:,:)
      real(kind=ESMF_KIND_R8), pointer :: arr_lat(:,:)

      integer :: i, j, k
      integer :: Xdim, Ydim
      integer :: Xdim_full, Ydim_full
      integer :: nx, ny

      integer :: IM, JM
      integer :: IM_WORLD, JM_WORLD
      integer :: COUNTS(3), DIMS(3)
      integer :: i_1, i_n, j_1, j_n  ! regional array bounds
      type(Logger), pointer :: lgr

      ! debug
      type(ESMF_VM) :: vm
      integer :: mypet, petcount
      integer :: nsize, count
      integer :: mpic

      _UNUSED_DUMMY(unusable)

      call ESMF_VMGetCurrent(vm,_RC)
!!      call ESMF_VMGet(vm, mpiCommunicator=mpic, localPet=mypet, petCount=petCount, _RC)

      Xdim=this%im_world
      Ydim=this%jm_world
      count = Xdim * Ydim

      call MAPL_grid_interior(grid, i_1, i_n, j_1, j_n)
      call MAPL_AllocateShared(arr_lon,[Xdim,Ydim],transroot=.true.,_RC)
      call MAPL_AllocateShared(arr_lat,[Xdim,Ydim],transroot=.true.,_RC)
      call MAPL_SyncSharedMemory(_RC)

      if (mapl_am_i_root()) then
         allocate( lon_true(0,0), lat_true(0,0), time_true(0,0) )
         call read_M_files_4_swath (this%filenames(1:this%M_file), nx, ny, &
              this%index_name_lon, this%index_name_lat, &
              var_name_lon=this%var_name_lon, &
              var_name_lat=this%var_name_lat, &
              var_name_time=this%var_name_time, &
              lon=lon_true, lat=lat_true, time=time_true, &
              Tfilter=.true., _RC)
         k=0
         do j=this%epoch_index(3), this%epoch_index(4)
            k=k+1
            arr_lon(1:Xdim, k) = lon_true(1:Xdim, j)
            arr_lat(1:Xdim, k) = lat_true(1:Xdim, j)
         enddo
         arr_lon=arr_lon*MAPL_DEGREES_TO_RADIANS_R8
         arr_lat=arr_lat*MAPL_DEGREES_TO_RADIANS_R8
         deallocate( lon_true, lat_true, time_true )

!         write(6,*) 'in root'
!         write(6,'(11x,100f10.1)')  arr_lon(::5,189)
      end if
!      call MPI_Barrier(mpic, status)
      call MAPL_SyncSharedMemory(_RC)

      call MAPL_BcastShared (VM, data=arr_lon, N=count, Root=MAPL_ROOT, RootOnly=.false., _RC)
      call MAPL_BcastShared (VM, data=arr_lat, N=count, Root=MAPL_ROOT, RootOnly=.false., _RC)

!      write(6,'(2x,a,2x,i5,4x,100f10.1)')  'PET', mypet, arr_lon(::5,189)
!      call MPI_Barrier(mpic, status)

      call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=fptr, _RC)
      fptr=real(arr_lon(i_1:i_n,j_1:j_n), kind=ESMF_KIND_R8)
      call MAPL_SyncSharedMemory(_RC)

      call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, &
           farrayPtr=fptr, rc=status)
      fptr=real(arr_lat(i_1:i_n,j_1:j_n), kind=ESMF_KIND_R8)

      if(MAPL_ShmInitialized) then
         call MAPL_DeAllocNodeArray(arr_lon,_RC)
         call MAPL_DeAllocNodeArray(arr_lat,_RC)
      else
         deallocate(arr_lon)
         deallocate(arr_lat)
      end if

!      if (mapl_am_I_root()) then
!         write(6,'(2x,a,10i8)')  &
!              'ck: Xdim, Ydim', Xdim, Ydim
!         write(6,'(2x,a,10i8)')  &
!              'ck: i_1, i_n, j_1, j_n', i_1, i_n, j_1, j_n
!      end if

!      write(6,*) 'MAPL_AmNodeRoot, MAPL_ShmInitialized=', MAPL_AmNodeRoot, MAPL_ShmInitialized
!      if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
!         write(6,'(2x,a,2x,i10)')  'add_horz_coord: MAPL_AmNodeRoot:  mypet=', mypet
!      end if

      _RETURN(_SUCCESS)

   end subroutine add_horz_coordinates_from_file


   subroutine initialize_from_file_metadata(this, file_metadata, unusable, force_file_coordinates, rc)
      use MAPL_KeywordEnforcerMod
      use MAPL_BaseMod, only: MAPL_DecomposeDim

      class (SwathGridFactory), intent(inout)  :: this
      type (FileMetadata), target, intent(in) :: file_metadata
      class (KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in) :: force_file_coordinates
      integer, optional, intent(out) :: rc

      integer :: status

      class (CoordinateVariable), pointer :: v
      class (*), pointer :: ptr(:)

      character(:), allocatable :: lon_name
      character(:), allocatable :: lat_name
      character(:), allocatable :: lev_name
      integer :: i
      logical :: hasLon, hasLat, hasLongitude, hasLatitude, hasLev,hasLevel,regLat,regLon
      real(kind=REAL64) :: del12,delij

      integer :: i_min, i_max
      real(kind=REAL64) :: d_lat, d_lat_temp, extrap_lat
      logical :: is_valid, use_file_coords, compute_lons, compute_lats

      _UNUSED_DUMMY(unusable)

      if (present(force_file_coordinates)) then
         use_file_coords = force_file_Coordinates
      else
         use_file_coords = .false.
      end if

      ! Cannot assume that lats and lons are evenly spaced

      associate (im => this%im_world, jm => this%jm_world, lm => this%lm)
         lon_name = 'lon'
         hasLon = file_metadata%has_dimension(lon_name)
         if (hasLon) then
            im = file_metadata%get_dimension(lon_name, _RC)
         else
            lon_name = 'longitude'
            hasLongitude = file_metadata%has_dimension(lon_name)
            if (hasLongitude) then
               im = file_metadata%get_dimension(lon_name, _RC)
            else
               _FAIL('no longitude coordinate')
            end if
         end if
         lat_name = 'lat'
         hasLat = file_metadata%has_dimension(lat_name)
         if (hasLat) then
            jm = file_metadata%get_dimension(lat_name, _RC)
         else
            lat_name = 'latitude'
            hasLatitude = file_metadata%has_dimension(lat_name)
            if (hasLatitude) then
               jm = file_metadata%get_dimension(lat_name, _RC)
            else
               _FAIL('no latitude coordinate')
            end if
         end if
         hasLev=.false.
         hasLevel=.false.
         lev_name = 'lev'
         hasLev = file_metadata%has_dimension(lev_name)
         if (hasLev) then
            lm = file_metadata%get_dimension(lev_name,_RC)
         else
            lev_name = 'levels'
            hasLevel = file_metadata%has_dimension(lev_name)
            if (hasLevel) then
               lm = file_metadata%get_dimension(lev_name,_RC)
            end if
         end if
    end associate

    call this%make_arbitrary_decomposition(this%nx, this%ny, _RC)

    ! Determine IMS and JMS with constraint for ESMF that each DE has at least an extent
    ! of 2.  Required for ESMF_FieldRegrid().
    allocate(this%ims(0:this%nx-1))
    allocate(this%jms(0:this%ny-1))
    call MAPL_DecomposeDim(this%im_world, this%ims, this%nx, min_DE_extent=2)
    call MAPL_DecomposeDim(this%jm_world, this%jms, this%ny, min_DE_extent=2)

    call this%check_and_fill_consistency(_RC)

    _RETURN(_SUCCESS)

   end subroutine initialize_from_file_metadata


   subroutine initialize_from_config_with_prefix(this, config, prefix, unusable, rc)
      use MPI
      implicit none
      class (SwathGridFactory), intent(inout) :: this
      type (ESMF_Config), intent(inout) :: config
      character(len=*), intent(in) :: prefix
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status

      type(ESMF_VM) :: VM
      integer:: mpic
      integer:: irank, ierror
      integer :: nlon, nlat, tdim
      integer :: Xdim, Ydim, ntime
      integer :: nx, ny
      character(len=ESMF_MAXSTR) :: key_lon, key_lat, key_time
      character(len=ESMF_MAXSTR) :: tunit, grp1, grp2
      character(len=ESMF_MAXSTR) :: filename, STR1, tmp
      character(len=ESMF_MAXSTR) :: symd, shms

      real(ESMF_KIND_R8), allocatable :: scanTime(:,:)
      real(ESMF_KIND_R8), allocatable :: lon_true(:,:)
      real(ESMF_KIND_R8), allocatable :: lat_true(:,:)
      integer :: yy, mm, dd, h, m, s, sec, second
      integer :: i, j, L
      integer :: ncid, ncid2, varid
      integer :: fid_s, fid_e
      integer :: M_file

      type(ESMF_Time) :: currTime
      integer (ESMF_KIND_I8) :: j0, j1, jt, jt1, jt2
      real(ESMF_KIND_R8) :: jx0, jx1
      real(ESMF_KIND_R8) :: x0, x1
      integer :: khi, klo, k, nstart, nend, max_iter
      type(Logger), pointer :: lgr
      logical :: ispresent

      type(ESMF_TimeInterval) :: Toff, obs_time_span

      _UNUSED_DUMMY(unusable)
      lgr => logging%get_logger('HISTORY.sampler')

      call ESMF_VmGetCurrent(VM, _RC)

      !   input :  config
      !   output:  this%epoch_index,  nx, ny
      !
      !   Read in specs, crop epoch_index based on scanTime


      !__ s1. read in file spec.
      !
      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'GRIDNAME:', default=MAPL_GRID_NAME_DEFAULT)
      this%grid_name = trim(tmp)
      call ESMF_ConfigGetAttribute(config, this%nx,  label=prefix//'NX:', default=MAPL_UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%ny,  label=prefix//'NY:', default=MAPL_UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%lm,  label=prefix//'LM:', default=MAPL_UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%input_template, label=prefix//'GRID_FILE:', default='unknown.txt', _RC)
      call ESMF_ConfigGetAttribute(config, this%epoch, label=prefix//'Epoch:', default=300, _RC)
      call ESMF_ConfigGetAttribute(config, tmp,      label=prefix//'Epoch_init:', default='2006', _RC)
      _ASSERT (this%lm /= MAPL_UNDEFINED_INTEGER, 'LM: is undefined in swath grid')

      call lgr%debug(' %a  %a', 'CurrTime =', trim(tmp))


      if ( index(tmp, 'T') /= 0 .OR. index(tmp, '-') /= 0 ) then
         call ESMF_TimeSet(currTime, timeString=tmp, _RC)
      else
         read(tmp,'(i4,5i2)') yy,mm,dd,h,m,s
         call ESMF_Timeset(currTime, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, _RC)
      endif
      second = hms_2_s(this%Epoch)
      call ESMF_TimeIntervalSet(this%epoch_frequency, s=second, _RC)


      call ESMF_ConfigGetAttribute(config, value=STR1, default="", &
           label= prefix// 'ref_time:', _RC)
      _ASSERT (trim(STR1)/='', 'ref_time missing, critical for data with 5 min interval!')
      call ESMF_TimeSet(this%obsfile_start_time, timestring=STR1, _RC)

      if (mapl_am_I_root()) then
         write(6,105) 'ref_time provided: ', trim(STR1)
      end if

      call ESMF_ConfigGetAttribute(config, value=STR1, default="", &
           label= prefix// 'frequency:', _RC)
      _ASSERT(STR1/='', 'fatal error: frequency not provided in RC file')
      if (mapl_am_I_root()) write(6,105) 'frequency:', trim(STR1)
      if (mapl_am_I_root()) write(6,106) 'Epoch (second)   :', second

      i= index( trim(STR1), ' ' )
      if (i>0) then
         symd=STR1(1:i-1)
         shms=STR1(i+1:)
      else
         symd=''
         shms=trim(STR1)
      endif
      call convert_twostring_2_esmfinterval (symd, shms,  this%obsfile_interval, _RC)

      call ESMF_ConfigGetAttribute(config, value=this%index_name_lon, default="", &
           label=prefix // 'index_name_lon:', _RC)
      call ESMF_ConfigGetAttribute(config, value=this%index_name_lat, default="", &
                 label=prefix // 'index_name_lat:', _RC)
      call ESMF_ConfigGetAttribute(config, this%var_name_lon, &
           label=prefix // 'var_name_lon:', default="", _RC)
      call ESMF_ConfigGetAttribute(config, this%var_name_lat, &
           label=prefix // 'var_name_lat:', default="", _RC)
      call ESMF_ConfigGetAttribute(config, this%var_name_time, default="", &
           label=prefix//'var_name_time:',  _RC)
      call ESMF_ConfigGetAttribute(config, this%tunit, default="", &
           label=prefix//'tunit:',  _RC)

      call lgr%debug(' %a  %a', 'input_template =', trim(this%input_template))


      !__ s2. find obsFile even if missing on disk and get array: this%t_alongtrack(:)
      !
      call ESMF_VMGet(vm, mpiCommunicator=mpic, _RC)
      call MPI_COMM_RANK(mpic, irank, ierror)
      _VERIFY(ierror)

      if (irank==0) &
           write(6,'(10(2x,a20,2x,a40,/))') &
           'index_name_lon:', trim(this%index_name_lon), &
           'index_name_lat:', trim(this%index_name_lat), &
           'var_name_lon:',   trim(this%var_name_lon), &
           'var_name_lat:',   trim(this%var_name_lat), &
           'var_name_time:',  trim(this%var_name_time), &
           'tunit:',          trim(this%tunit)

      if (irank==0) then
         call ESMF_TimeIntervalSet(Toff, h=0, m=0, s=0, _RC)
         call Find_M_files_for_currTime (currTime, &
              this%obsfile_start_time, this%obsfile_interval, &
              this%epoch_frequency,  this%input_template, M_file, this%filenames, &
              T_offset_in_file_content = Toff,  _RC)
         this%M_file = M_file
         write(6,'(10(2x,a20,2x,i40))') &
              'M_file:', M_file
         do i=1, M_file
            write(6,'(10(2x,a14,i4,a2,2x,a))') &
                 'filenames(', i, '):', trim(this%filenames(i))
         end do

         !------------------------------------------------------------
         !  QC for obs files:
         !
         !  1.  redefine nstart to skip un-defined time value
         !  2.  Scan_Start_Time =  -9999, -9999, -9999,
         !      ::  eliminate this row of data
         !------------------------------------------------------------

         allocate(lon_true(0,0), lat_true(0,0), scanTime(0,0))
         call read_M_files_4_swath (this%filenames(1:M_file), nx, ny, &
              this%index_name_lon, this%index_name_lat, &
              var_name_lon=this%var_name_lon, &
              var_name_lat=this%var_name_lat, &
              var_name_time=this%var_name_time, &
              lon=lon_true, lat=lat_true, time=scanTime, &
              Tfilter=.true., _RC)

         nlon=nx
         nlat=ny
         allocate(this%t_alongtrack(nlat))
         do j=1, nlat
            this%t_alongtrack(j) = scanTime(1,j)
         end do

         !!write(6,'(a)')  'this%t_alongtrack(::50)='
         !!write(6,'(5f20.2)')  this%t_alongtrack(::50)


         nstart = 1
         !
         ! If the t_alongtrack contains undefined values, use this code
         !
         x0 = this%t_alongtrack(1)
         x1 = 1.d16
         if (x0 > x1) then
            !
            ! bisect backward finding the first index arr[n] < x1
            klo=1
            khi=nlat
            max_iter = int( log( real(nlat) ) / log(2.d0) ) + 2
            do i=1, max_iter
               k = (klo+khi)/2
               if ( this%t_alongtrack(k) < x1 ) then
                  khi=k
               else
                  nstart = khi
                  exit
               endif
            enddo
            call lgr%debug('%a %i4', 'nstart', nstart)
            call lgr%debug('%a %i4', 'this%t_alongtrack(nstart)',  this%t_alongtrack(nstart))
         endif

         this%cell_across_swath = nlon
         this%cell_along_swath = nlat
         deallocate(scanTime)



         ! P2.
         ! determine im_world from Epoch
         ! -----------------------------
         ! t_axis = t_alongtrack = t_a
         ! convert currTime to j0
         ! use Epoch to find j1
         ! search j0, j1 in t_a

         call time_esmf_2_nc_int (currTime, this%tunit, j0, _RC)
         sec = hms_2_s (this%Epoch)
         j1= j0 + sec
         jx0= j0
         jx1= j1

         this%epoch_index(1)= 1
         this%epoch_index(2)= this%cell_across_swath
         nend = this%cell_along_swath
         call bisect( this%t_alongtrack, jx0, jt1, n_LB=int(nstart, ESMF_KIND_I8), n_UB=int(nend, ESMF_KIND_I8), rc=rc)
         call bisect( this%t_alongtrack, jx1, jt2, n_LB=int(nstart, ESMF_KIND_I8), n_UB=int(nend, ESMF_KIND_I8), rc=rc)

         call lgr%debug ('%a %i20 %i20', 'nstart, nend', nstart, nend)
         call lgr%debug ('%a %f20.1 %f20.1', 'j0[currT]    j1[T+Epoch]  w.r.t. timeunit ', jx0, jx1)
         call lgr%debug ('%a %f20.1 %f20.1', 'x0[times(1)] xn[times(N)] w.r.t. timeunit ', &
              this%t_alongtrack(1), this%t_alongtrack(nend))
         call lgr%debug ('%a %i20 %i20', 'jt1, jt2 [final intercepted position]', jt1, jt2)

         call lgr%debug ('%a %i20 %i20', 'nstart, nend', nstart, nend)
         call lgr%debug ('%a %f20.1 %f20.1', 'j0[currT]    j1[T+Epoch]  w.r.t. timeunit ', jx0, jx1)
         call lgr%debug ('%a %f20.1 %f20.1', 'x0[times(1)] xn[times(N)] w.r.t. timeunit ', &
              this%t_alongtrack(1), this%t_alongtrack(nend))
         call lgr%debug ('%a %i20 %i20', 'jt1, jt2 [final intercepted position]', jt1, jt2)

         if (jt1==jt2) then
            _FAIL('Epoch Time is too small, empty swath grid is generated, increase Epoch')
         endif

         jt1 = jt1 + 1               ! (x1,x2]  design
         this%epoch_index(3)= jt1
         this%epoch_index(4)= jt2
         _ASSERT( jt1 < jt2, 'Swath grid fail : epoch_index(3) > epoch_index(4)')
         Xdim = this%cell_across_swath
         Ydim = this%epoch_index(4) - this%epoch_index(3) + 1

         call lgr%debug ('%a %i4 %i4', 'bisect for j0:  rc, jt', rc, jt1)
         call lgr%debug ('%a %i4 %i4', 'bisect for j1:  rc, jt', rc, jt2)
         call lgr%debug ('%a %i4 %i4', 'Xdim, Ydim', Xdim, Ydim)
         call lgr%debug ('%a %i4 %i4 %i4 %i4', 'this%epoch_index(4)', &
              this%epoch_index(1), this%epoch_index(2), &
              this%epoch_index(3), this%epoch_index(4))

         this%im_world = Xdim
         this%jm_world = Ydim
      end if


      call MPI_bcast(this%M_file, 1, MPI_INTEGER, 0, mpic, ierror)
      _VERIFY(ierror)
      do i=1, this%M_file
         call MPI_bcast(this%filenames(i), ESMF_MAXSTR, MPI_CHARACTER, 0, mpic, ierror)
         _VERIFY(ierror)
      end do
      call MPI_bcast(this%epoch_index, 4, MPI_INTEGER8, 0, mpic, ierror)
      _VERIFY(ierror)
      call MPI_bcast(this%im_world, 1, MPI_INTEGER, 0, mpic, ierror)
      _VERIFY(ierror)
      call MPI_bcast(this%jm_world, 1, MPI_INTEGER, 0, mpic, ierror)
      _VERIFY(ierror)
      call MPI_bcast(this%cell_across_swath, 1, MPI_INTEGER, 0, mpic, ierror)
      _VERIFY(ierror)
      call MPI_bcast(this%cell_along_swath, 1, MPI_INTEGER, 0, mpic, ierror)
      _VERIFY(ierror)
      ! donot need to bcast this%along_track (root only)


      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'IMS_FILE:', rc=status)
      if ( status == _SUCCESS ) then
         call get_ims_from_file(this%ims, trim(tmp),this%nx, _RC)
      else
         call get_multi_integer(this%ims, 'IMS:', _RC)
      endif
      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'JMS_FILE:', rc=status)
      if ( status == _SUCCESS ) then
         call get_ims_from_file(this%jms, trim(tmp),this%ny, _RC)
      else
         call get_multi_integer(this%jms, 'JMS:', _RC)
      endif
      ! ims is set at here
      call this%check_and_fill_consistency(_RC)
      call lgr%debug(' %a  %i5  %i5', 'nx, ny (check_and_fill_consistency) = ', this%nx, this%ny)

      _RETURN(_SUCCESS)

105      format (1x,a,2x,a)
106      format (1x,a,2x,10i8)

   contains

      subroutine get_multi_integer(values, label, rc)
         integer, allocatable, intent(out) :: values(:)
         character(len=*) :: label
         integer, optional, intent(out) :: rc

         integer :: i
         integer :: n
         integer :: tmp
         integer :: status
         logical :: isPresent

         call ESMF_ConfigFindLabel(config, label=prefix//label, isPresent=isPresent, _RC)

         if (.not. isPresent) then
            _RETURN(_SUCCESS)
         end if

         ! First pass:  count values
         n = 0
         do
            call ESMF_ConfigGetAttribute(config, tmp, rc=status)
            if (status /= _SUCCESS) then
               exit
            else
               n = n + 1
            end if
         end do


         ! Second pass: allocate and fill
         allocate(values(n), stat=status) ! no point in checking status
         _VERIFY(status)
         call ESMF_ConfigFindLabel(config, label=prefix//label,_RC)
         do i = 1, n
            call ESMF_ConfigGetAttribute(config, values(i), _RC)
            write(6,*) 'values(i)=', values(i)
         end do

         _RETURN(_SUCCESS)

      end subroutine get_multi_integer

      subroutine get_ims_from_file(values, file_name, n, rc)
         integer, allocatable, intent(out) :: values(:)
         character(len=*), intent(in) :: file_name
         integer, intent(in) :: n
         integer, optional, intent(out) :: rc

         logical :: FileExists
         integer :: i, total, unit
         integer :: status

         inquire(FILE = trim(file_name), EXIST=FileExists)
         allocate(values(n), stat=status) ! no point in checking status
         _VERIFY(status)

         _ASSERT(FileExists, "File <"//trim(file_name)//"> not found")
         if (MAPL_AM_I_Root(VM)) then
            open(newunit=UNIT, file=trim(file_name), form="formatted", iostat=status )
            _VERIFY(STATUS)
            read(UNIT,*) total
            _ASSERT(total == n, trim(file_name) // " n is different from total")
            do i = 1,total
                read(UNIT,*) values(i)
            enddo
            close(UNIT)
         endif

         call MAPL_CommsBcast(VM, values, n=N, ROOT=MAPL_Root, _RC)
         _RETURN(_SUCCESS)

      end subroutine get_ims_from_file

      subroutine get_range(range, label, rc)
         type(RealMinMax), intent(out) :: range
         character(len=*) :: label
         integer, optional, intent(out) :: rc

         integer :: i
         integer :: n
         integer :: status
         logical :: isPresent

         call ESMF_ConfigFindLabel(config, label=prefix//label,isPresent=isPresent,_RC)
         if (.not. isPresent) then
            _RETURN(_SUCCESS)
         end if

         ! Must be 2 values: min and max
         call ESMF_ConfigGetAttribute(config, range%min, _RC)
         call ESMF_ConfigGetAttribute(config, range%max, _RC)

         _RETURN(_SUCCESS)

      end subroutine get_range


   end subroutine initialize_from_config_with_prefix



   function to_string(this) result(string)
      character(len=:), allocatable :: string
      class (SwathGridFactory), intent(in) :: this

      _UNUSED_DUMMY(this)
      string = 'SwathGridFactory'

   end function to_string


   subroutine check_and_fill_consistency(this, unusable, rc)
      use MAPL_BaseMod, only: MAPL_DecomposeDim
      class (SwathGridFactory), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: verify_decomp

      _UNUSED_DUMMY(unusable)

      if (.not. allocated(this%grid_name)) then
         this%grid_name = MAPL_GRID_NAME_DEFAULT
      end if

      ! Check decomposition/bounds
      ! WY notes: should not have this assert
      !_ASSERT(allocated(this%ims) .eqv. allocated(this%jms), 'inconsistent options')
      call verify(this%nx, this%im_world, this%ims, rc=status)
      call verify(this%ny, this%jm_world, this%jms, rc=status)

      if (.not.this%force_decomposition) then
         verify_decomp = this%check_decomposition(_RC)
         if ( (.not.verify_decomp) ) then
            call this%generate_newnxy(_RC)
         end if
      end if
      _RETURN(_SUCCESS)

   contains

      subroutine verify(n, m_world, ms, rc)
         integer, intent(inout) :: n
         integer, intent(inout) :: m_world
         integer, allocatable, intent(inout) :: ms(:)
         integer, optional, intent(out) :: rc

         integer :: status

         if (allocated(ms)) then
            _ASSERT(size(ms) > 0, 'degenerate topology')

            if (n == MAPL_UNDEFINED_INTEGER) then
               n = size(ms)
            else
               _ASSERT(n == size(ms), 'inconsistent topology')
            end if

            if (m_world == MAPL_UNDEFINED_INTEGER) then
               m_world = sum(ms)
            else
               _ASSERT(m_world == sum(ms), 'inconsistent decomponsition')
            end if

         else

            _ASSERT(n /= MAPL_UNDEFINED_INTEGER, 'uninitialized topology')
            _ASSERT(m_world /= MAPL_UNDEFINED_INTEGER,'uninitialized dimension')
            allocate(ms(n), stat=status)
            _VERIFY(status)
            !call MAPL_DecomposeDim(m_world, ms, n, min_DE_extent=2)
            call MAPL_DecomposeDim(m_world, ms, n)

         end if

         _RETURN(_SUCCESS)

      end subroutine verify

   end subroutine check_and_fill_consistency


   elemental subroutine set_with_default_integer(to, from, default)
      integer, intent(out) :: to
      integer, optional, intent(in) :: from
      integer, intent(in) :: default

      if (present(from)) then
         to = from
      else
         to = default
      end if

   end subroutine set_with_default_integer

   elemental subroutine set_with_default_real64(to, from, default)
      real(REAL64), intent(out) :: to
      real(REAL64), optional, intent(in) :: from
      real(REAL64), intent(in) :: default

      if (present(from)) then
         to = from
      else
         to = default
      end if

   end subroutine set_with_default_real64

   elemental subroutine set_with_default_real(to, from, default)
      real, intent(out) :: to
      real, optional, intent(in) :: from
      real, intent(in) :: default

      if (present(from)) then
         to = from
      else
         to = default
      end if

   end subroutine set_with_default_real

   subroutine set_with_default_character(to, from, default)
      character(len=:), allocatable, intent(out) :: to
      character(len=*), optional, intent(in) :: from
      character(len=*), intent(in) :: default

      if (present(from)) then
         to = from
      else
         to = default
      end if

   end subroutine set_with_default_character


   elemental subroutine set_with_default_bounds(to, from, default)
      type (RealMinMax), intent(out) :: to
      type (RealMinMax), optional, intent(in) :: from
      type (RealMinMax), intent(in) :: default

      if (present(from)) then
         to = from
      else
         to = default
      end if

   end subroutine set_with_default_bounds


   ! MAPL uses values in lon_array and lat_array only to determine the
   ! general positioning.  Actual coordinates are then recomputed.
   ! This helps to avoid roundoff differences from slightly different
   ! input files.
   subroutine initialize_from_esmf_distGrid(this, dist_grid, lon_array, lat_array, unusable, rc)
      use MAPL_ConfigMod
      use MAPL_Constants, only: PI => MAPL_PI_R8
      class (SwathGridFactory), intent(inout)  :: this
      type (ESMF_DistGrid), intent(in) :: dist_grid
      type (ESMF_LocalArray), intent(in) :: lon_array
      type (ESMF_LocalArray), intent(in) :: lat_array
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: dim_count, tile_count
      integer, allocatable :: max_index(:,:)
      integer :: status
      character(len=2) :: pole ,dateline

      type (ESMF_Config) :: config
      type (ESMF_VM) :: vm
      integer :: nPet
      real(kind=REAL32), pointer :: lon(:)
      real(kind=REAL32), pointer :: lat(:)
      integer :: nx_guess,nx,ny
      integer :: i

      real, parameter :: tiny = 1.e-4

      _FAIL ('stop: not implemented: subroutine initialize_from_esmf_distGrid')

      _UNUSED_DUMMY(unusable)

      call ESMF_DistGridGet(dist_grid, dimCount=dim_count, tileCount=tile_count)
      allocate(max_index(dim_count, tile_count))
      call ESMF_DistGridGet(dist_grid, maxindexPTile=max_index)

      config = MAPL_ConfigCreate(_RC)
      call MAPL_ConfigSetAttribute(config, max_index(1,1), 'IM_WORLD:', _RC)
      call MAPL_ConfigSetAttribute(config, max_index(2,1), 'JM_WORLD:', _RC)
      call MAPL_ConfigSetAttribute(config, max_index(3,1), 'LM:', _RC)

      lon => null()
      lat => null()
      call ESMF_LocalArrayGet(lon_array, farrayPtr=lon, _RC)
      call ESMF_LocalArrayGet(lat_array, farrayPtr=lat, _RC)

      call ESMF_VMGetCurrent(vm, _RC)
      call ESMF_VMGet(vm, PETcount=nPet, _RC)

      nx_guess = nint(sqrt(real(nPet)))
      do nx = nx_guess,1,-1
         ny=nPet/nx
         if (nx*ny==nPet) then
            call MAPL_ConfigSetAttribute(config, nx, 'NX:')
            call MAPL_ConfigSetAttribute(config, ny, 'NY:')
            exit
         end if
      enddo

      call this%initialize(config, _RC)

   end subroutine initialize_from_esmf_distGrid


   function decomps_are_equal(this,a) result(equal)
      class (SwathGridFactory), intent(in) :: this
      class (AbstractGridFactory), intent(in) :: a
      logical :: equal

      select type (a)
         class default
         equal = .false.
         return
      class is (SwathGridFactory)
         equal = .true.


         equal = size(a%ims)==size(this%ims) .and. size(a%jms)==size(this%jms)
         if (.not. equal) return

         ! same decomposition
         equal = all(a%ims == this%ims) .and. all(a%jms == this%jms)
         if (.not. equal) return

      end select

   end function decomps_are_equal


   function physical_params_are_equal(this, a) result(equal)
      class (SwathGridFactory), intent(in) :: this
      class (AbstractGridFactory), intent(in) :: a
      logical :: equal

      select type (a)
         class default
         equal = .false.
         return
      class is (SwathGridFactory)
         equal = .true.

         equal = (a%im_world == this%im_world) .and. (a%jm_world == this%jm_world)
         if (.not. equal) return
      end select

   end function physical_params_are_equal

   logical function equals(a, b)
      class (SwathGridFactory), intent(in) :: a
      class (AbstractGridFactory), intent(in) :: b

      select type (b)
         class default
         equals = .false.
         return
      class is (SwathGridFactory)
         equals = .true.

         equals = (a%lm == b%lm)
         if (.not. equals) return

         equals = a%decomps_are_equal(b)
         if (.not. equals) return

         equals = a%physical_params_are_equal(b)
         if (.not. equals) return

      end select

   end function equals


   function generate_grid_name(this) result(name)
      character(len=:), allocatable :: name
      class (SwathGridFactory), intent(in) :: this
! from tclune: This needs thought. I suspect we want something that indicates this is a swath grid.
      character(len=4) :: im_string, jm_string
      name = im_string // 'x' // jm_string
   end function generate_grid_name


   function check_decomposition(this,unusable,rc) result(can_decomp)
      class (SwathGridFactory), target, intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      logical :: can_decomp
      integer :: n
      _UNUSED_DUMMY(unusable)

      can_decomp = .true.
      if (this%im_world==1 .and. this%jm_world==1) then
         _RETURN(_SUCCESS)
      end if
      n = this%im_world/this%nx
      if (n < 2) can_decomp = .false.
      n = this%jm_world/this%ny
      if (n < 2) can_decomp = .false.
      _RETURN(_SUCCESS)
   end function check_decomposition


   subroutine generate_newnxy(this,unusable,rc)
      use MAPL_BaseMod, only: MAPL_DecomposeDim
      class (SwathGridFactory), target, intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: n
      integer :: j, pet_count

      _UNUSED_DUMMY(unusable)

      pet_count = this%nx * this%ny
      n = this%im_world/this%nx
      if (n < 2) then
         do j = this%im_world/2, 1, -1
            if ( mod(pet_count, j) == 0 .and. this%im_world/j >= 2 ) then
               exit  ! found a decomposition
            end if
         end do
         this%nx = j
         this%ny = pet_count/j
      end if

      n = this%jm_world/this%ny
      if (n < 2) then
         do j = this%jm_world/2, 1, -1
            if ( mod(pet_count, j) == 0 .and. this%jm_world/j >=2 ) then
               exit  ! found a decomposition
            end if
         end do
         this%ny = j
         this%nx = pet_count/j
      end if

      if ( this%im_world/this%nx < 2 .OR. this%jm_world/this%ny < 2 ) then
         _FAIL ('Algorithm failed')
      end if

      if (allocated(this%ims)) deallocate(this%ims)
      allocate(this%ims(0:this%nx-1))
      call MAPL_DecomposeDim(this%im_world, this%ims, this%nx)
      if (allocated(this%jms)) deallocate(this%jms)
      allocate(this%jms(0:this%ny-1))
      call MAPL_DecomposeDim(this%jm_world, this%jms, this%ny)

      _RETURN(_SUCCESS)

   end subroutine generate_newnxy


   subroutine init_halo(this, unusable, rc)
      class (SwathGridFactory), target, intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      _FAIL('Stop: subroutine init_halo is not needed for SwathGridFactory')
   end subroutine init_halo

   subroutine halo(this, array, unusable, halo_width, rc)
      use MAPL_CommsMod
      class (SwathGridFactory), intent(inout) :: this
      real(kind=REAL32), intent(inout) :: array(:,:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: halo_width
      integer, optional, intent(out) :: rc
      _FAIL( 'Stop: subroutine halo is not needed for SwathGridFactory')
   end subroutine halo


   subroutine append_metadata(this, metadata)
      use MAPL_Constants
      class (SwathGridFactory), intent(inout) :: this
      type (FileMetadata), intent(inout) :: metadata

      type (Variable) :: v
      real(kind=REAL64), allocatable :: temp_coords(:)

      character(len=ESMF_MAXSTR) :: key_lon
      character(len=ESMF_MAXSTR) :: key_lat

      ! Horizontal grid dimensions
      call metadata%add_dimension('lon', this%im_world)
      call metadata%add_dimension('lat', this%jm_world)

      ! Coordinate variables
      v = Variable(type=PFIO_REAL64, dimensions='lon,lat')
      call v%add_attribute('long_name', 'longitude')
      call v%add_attribute('units', 'degrees_east')
      call metadata%add_variable('lons', v)

      v = Variable(type=PFIO_REAL64, dimensions='lon,lat')
      call v%add_attribute('long_name', 'latitude')
      call v%add_attribute('units', 'degrees_north')
      call metadata%add_variable('lats', v)

   end subroutine append_metadata


   function get_grid_vars(this) result(vars)
      class (SwathGridFactory), intent(inout) :: this

      character(len=:), allocatable :: vars
      character(len=ESMF_MAXSTR) :: key_lon
      character(len=ESMF_MAXSTR) :: key_lat
      _UNUSED_DUMMY(this)

      !!key_lon=trim(this%var_name_lon)
      !!key_lat=trim(this%var_name_lat)
      vars = 'lon,lat'

   end function get_grid_vars


   function get_file_format_vars(this) result(vars)
      class (SwathGridFactory), intent(inout) :: this

      character(len=:), allocatable :: vars
      _UNUSED_DUMMY(this)

      vars = 'lon,lat'
   end function get_file_format_vars


   subroutine append_variable_metadata(this,var)
      class (SwathGridFactory), intent(inout) :: this
      type(Variable), intent(inout) :: var
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(var)
   end subroutine append_variable_metadata


   subroutine generate_file_bounds(this,grid,local_start,global_start,global_count,metadata,rc)
      use MAPL_BaseMod
      class(SwathGridFactory), intent(inout) :: this
      type(ESMF_Grid),      intent(inout) :: grid
      integer, allocatable, intent(out) :: local_start(:)
      integer, allocatable, intent(out) :: global_start(:)
      integer, allocatable, intent(out) :: global_count(:)
      type(FileMetaData), intent(in), optional :: metaData
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: global_dim(3), i1,j1,in,jn

      _UNUSED_DUMMY(this)

      call MAPL_GridGet(grid,globalCellCountPerDim=global_dim,_RC)
      call MAPL_GridGetInterior(grid,i1,in,j1,jn)
      allocate(local_start,source=[i1,j1])
      allocate(global_start,source=[1,1])
      allocate(global_count,source=[global_dim(1),global_dim(2)])

      _RETURN(_SUCCESS)

   end subroutine generate_file_bounds


   subroutine generate_file_corner_bounds(this,grid,local_start,global_start,global_count,rc)
      use esmf
      class (SwathGridFactory), intent(inout) :: this
      type(ESMF_Grid), intent(inout)      :: grid
      integer, allocatable, intent(out) :: local_start(:)
      integer, allocatable, intent(out) :: global_start(:)
      integer, allocatable, intent(out) :: global_count(:)
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(grid)
      _UNUSED_DUMMY(local_start)
      _UNUSED_DUMMY(global_start)
      _UNUSED_DUMMY(global_count)

      _FAIL('unimplemented')
      _RETURN(_SUCCESS)
   end subroutine generate_file_corner_bounds

   function generate_file_reference2D(this,fpointer) result(ref)
      use pFIO
      type(ArrayReference) :: ref
      class(SwathGridFactory), intent(inout) :: this
      real, pointer, intent(in) :: fpointer(:,:)
      _UNUSED_DUMMY(this)
      ref = ArrayReference(fpointer)
   end function generate_file_reference2D

   function generate_file_reference3D(this,fpointer,metaData) result(ref)
      use pFIO
      type(ArrayReference) :: ref
      class(SwathGridFactory), intent(inout) :: this
      real, pointer, intent(in) :: fpointer(:,:,:)
      type(FileMetaData), intent(in), optional :: metaData
      _UNUSED_DUMMY(this)
      ref = ArrayReference(fpointer)
   end function generate_file_reference3D


   subroutine get_xy_subset(this, interval, xy_subset, rc)
      use MPI
      class(SwathGridFactory), intent(in) :: this
      type(ESMF_Time), intent(in) :: interval(2)
      integer, intent(out) :: xy_subset(2,2)
      integer, optional, intent(out) :: rc

      type(ESMF_VM) :: VM
      integer:: mpic
      integer:: irank, ierror

      integer :: status
      type(ESMF_Time) :: T1, T2
      integer(ESMF_KIND_I8) :: i1, i2
      real(ESMF_KIND_R8) :: iT1, iT2
      integer(ESMF_KIND_I8) :: index1, index2
      integer :: jlo, jhi, je


      call ESMF_VmGetCurrent(VM, _RC)
      call ESMF_VMGet(vm, mpiCommunicator=mpic, _RC)
      call MPI_COMM_RANK(mpic, irank, ierror)
      _VERIFY(ierror)

      if (irank==0) then
         ! xtrack
         xy_subset(1:2,1)=this%epoch_index(1:2)

         ! atrack
         T1= interval(1)
         T2= interval(2)

         ! this%t_alongtrack
         !
         call time_esmf_2_nc_int (T1, this%tunit, i1, _RC)
         call time_esmf_2_nc_int (T2, this%tunit, i2, _RC)
         iT1 = i1   ! int to real*8
         iT2 = i2
         if (this%epoch_index(3) > 2) then
            jlo = this%epoch_index(3) - 2
         else
            jlo = this%epoch_index(3)
         end if
         jhi = this%epoch_index(4) + 1
         !
         ! -- it is possible some obs files are missing
         !
         call bisect( this%t_alongtrack, iT1, index1, n_LB=int(jlo, ESMF_KIND_I8), n_UB=int(jhi, ESMF_KIND_I8), rc=rc)
         call bisect( this%t_alongtrack, iT2, index2, n_LB=int(jlo, ESMF_KIND_I8), n_UB=int(jhi, ESMF_KIND_I8), rc=rc)

         !! complex version
         !!      ! (x1, x2]  design in bisect
         !!      if (index1==jlo-1) then
         !!         je = index1 + 1
         !!      else
         !!         je = index1
         !!      end if
         !!      xy_subset(1, 2) = je
         !!      if (index2==jlo-1) then
         !!         je = index2 + 1
         !!      else
         !!         je = index2
         !!      end if
         !!      xy_subset(2, 2) = je

         ! simple version
         xy_subset(1,  2)=index1+1                 ! atrack
         xy_subset(2,  2)=index2

         !
         !- relative
         !
         xy_subset(1,2)= xy_subset(1,2) - this%epoch_index(3) + 1
         xy_subset(2,2)= xy_subset(2,2) - this%epoch_index(3) + 1
      end if

      call MPI_bcast(xy_subset, 4, MPI_INTEGER, 0, mpic, ierror)
      _VERIFY(ierror)

      _RETURN(_SUCCESS)
    end subroutine get_xy_subset


    subroutine destroy(this, rc)
      class(SwathGridFactory), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: i
      return
    end subroutine destroy


    !   here  grid ==  external_grid
    !   because  this%grid is protected in AbstractGridFactory
    subroutine get_obs_time(this, grid, obs_time,  rc)
      use MAPL_BaseMod, only: MAPL_grid_interior
      class(SwathGridFactory), intent(inout) :: this
      type (ESMF_Grid), intent(in) :: grid
      real(ESMF_KIND_R4), intent(out) :: obs_time(:,:)
      integer, optional, intent(out) :: rc
      integer :: status

      real(kind=ESMF_KIND_R8), pointer :: fptr(:,:)
      real(kind=ESMF_KIND_R8), pointer :: centers(:,:)
      real(kind=ESMF_KIND_R8), allocatable :: lon_true(:,:)
      real(kind=ESMF_KIND_R8), allocatable :: lat_true(:,:)
      real(kind=ESMF_KIND_R8), allocatable :: time_true(:,:)
      real(kind=ESMF_KIND_R8), pointer :: arr_time(:,:)

      integer :: i, j, k
      integer :: Xdim, Ydim, count
      integer :: nx, ny
      integer :: i_1, i_n, j_1, j_n ! regional array bounds

      ! debug
      type(ESMF_VM) :: vm
      integer :: mypet, petcount
      integer :: mpic

      call ESMF_VMGetCurrent(vm,_RC)
      call ESMF_VMGet(vm, mpiCommunicator=mpic, localPet=mypet, petCount=petCount, _RC)

      Xdim=this%im_world
      Ydim=this%jm_world
      count=Xdim*Ydim

      call MAPL_grid_interior(grid, i_1, i_n, j_1, j_n)
      call MAPL_AllocateShared(arr_time,[Xdim,Ydim],transroot=.true.,_RC)
      call MAPL_SyncSharedMemory(_RC)

      if (mapl_am_i_root()) then
         allocate( lon_true(0,0), lat_true(0,0), time_true(0,0) )
         call read_M_files_4_swath (this%filenames(1:this%M_file), nx, ny, &
              this%index_name_lon, this%index_name_lat, &
              var_name_lon=this%var_name_lon, &
              var_name_lat=this%var_name_lat, &
              var_name_time=this%var_name_time, &
              lon=lon_true, lat=lat_true, time=time_true, &
              Tfilter=.true., _RC)
         k=0
         do j=this%epoch_index(3), this%epoch_index(4)
            k=k+1
            arr_time(1:Xdim, k) = time_true(1:Xdim, j)
         enddo
         deallocate( lon_true, lat_true, time_true )

!         write(6,*) 'in root, time'
!         write(6,'(11x,100E12.5)')  arr_time(::5,189)
      end if
      call MAPL_SyncSharedMemory(_RC)

      call MAPL_BcastShared (VM, data=arr_time, N=count, Root=MAPL_ROOT, RootOnly=.false., _RC)

!      write(6,'(2x,a,2x,i5,4x,100E12.5)')  'PET, time', mypet, arr_time(::5,189)
!      call MPI_Barrier(mpic, status)

      !(Xdim, Ydim)
      obs_time = arr_time(i_1:i_n,j_1:j_n)

      if(MAPL_ShmInitialized) then
         call MAPL_DeAllocNodeArray(arr_time,_RC)
      else
         deallocate(arr_time)
      end if

      _RETURN(_SUCCESS)
    end subroutine get_obs_time


end module MAPL_SwathGridFactoryMod
