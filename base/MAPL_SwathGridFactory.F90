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
   use Plain_netCDF_Time
   use MAPL_Base, only : MAPL_GridGetInterior
   use ESMF
   use pFIO
   use MAPL_CommsMod
   use netcdf
   use, intrinsic :: iso_fortran_env, only: REAL32
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none
   integer, parameter :: gridLabel_max = 20
   private

   public :: SwathGridFactory
   
   type, extends(AbstractGridFactory) :: SwathGridFactory
      private
      character(len=:), allocatable :: grid_name
      character(len=:), allocatable :: grid_file_name      

      integer :: cell_across_swath
      integer :: cell_along_swath
      integer :: im_world = MAPL_UNDEFINED_INTEGER
      integer :: jm_world = MAPL_UNDEFINED_INTEGER
      integer :: lm = MAPL_UNDEFINED_INTEGER
      logical :: force_decomposition = .false.

      integer :: epoch                         ! unit: second
      integer(ESMF_KIND_I8) :: epoch_index(4)  ! is,ie,js,je
      character(len=ESMF_MAXSTR) :: tunit
      real(ESMF_KIND_R8), allocatable :: t_alongtrack(:)
      character(len=ESMF_MAXSTR)     :: nc_index
      character(len=ESMF_MAXSTR)     :: nc_time
      character(len=ESMF_MAXSTR)     :: nc_latitude
      character(len=ESMF_MAXSTR)     :: nc_longitude
      character(len=ESMF_MAXSTR)     :: var_name_time
      character(len=ESMF_MAXSTR)     :: var_name_lat
      character(len=ESMF_MAXSTR)     :: var_name_lon
      character(len=ESMF_MAXSTR)     :: input_template
      logical                        :: found_group


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

      if (this%lm /= MAPL_UNDEFINED_INTEGER) then
         call ESMF_AttributeSet(grid, name='GRID_LM', value=this%lm, _RC)
      end if
      call ESMF_AttributeSet(grid, 'GridType', 'LatLon', _RC)
      call ESMF_AttributeSet(grid, 'Global', .false., _RC)

      _RETURN(_SUCCESS)      
   end function create_basic_grid


   subroutine add_horz_coordinates_from_file(this, grid, unusable, rc)
      use MAPL_BaseMod, only: MAPL_grid_interior
      use pflogger,    only : Logger, logging
      implicit none
      class (SwathGridFactory), intent(in) :: this
      type (ESMF_Grid), intent(inout) :: grid
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status

      real(kind=ESMF_KIND_R8), pointer :: fptr(:,:)
      real, pointer :: centers(:,:)
      real, allocatable :: centers_full(:,:)

      integer :: i, j, k
      integer :: Xdim, Ydim
      integer :: Xdim_full, Ydim_full
      
      integer :: IM, JM
      integer :: IM_WORLD, JM_WORLD
      integer :: COUNTS(3), DIMS(3)
      integer :: i_1, i_n, j_1, j_n  ! regional array bounds
      !      character(len=:), allocatable :: lon_center_name, lat_center_name, time_name
      character(len=ESMF_MAXSTR) :: lon_center_name, lat_center_name, time_name      
      type(Logger), pointer :: lgr

      _UNUSED_DUMMY(unusable)

      ! keywords in netCDF
      lon_center_name = "clon"
      lat_center_name = "clat"
      time_name = "scanTime"      
      Xdim=this%im_world
      Ydim=this%jm_world
      Xdim_full=this%cell_across_swath
      Ydim_full=this%cell_along_swath
      
      call MAPL_grid_interior(grid, i_1, i_n, j_1, j_n)      
      call MAPL_AllocateShared(centers,[Xdim,Ydim],transroot=.true.,_RC)
      call MAPL_SyncSharedMemory(_RC)

      ! read longitudes
       if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
          allocate( centers_full(Xdim_full, Ydim_full))
          call get_v2d_netcdf(this%grid_file_name, lon_center_name, centers_full, Xdim_full, Ydim_full)
          k=0
          do j=this%epoch_index(3), this%epoch_index(4)
             k=k+1
             centers(1:Xdim, k) = centers_full(1:Xdim, j)
          enddo
          centers=centers*MAPL_DEGREES_TO_RADIANS_R8
          deallocate (centers_full)
       end if
       call MAPL_SyncSharedMemory(_RC)       
       call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
          staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=fptr, _RC)
       fptr=real(centers(i_1:i_n,j_1:j_n), kind=ESMF_KIND_R8)

       ! read latitudes 
       if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
          allocate( centers_full(Xdim_full, Ydim_full))
          call get_v2d_netcdf(this%grid_file_name, lat_center_name, centers_full, Xdim_full, Ydim_full)
          k=0
          do j=this%epoch_index(3), this%epoch_index(4)
             k=k+1
             centers(1:Xdim, k) = centers_full(1:Xdim, j)
          enddo
          centers=centers*MAPL_DEGREES_TO_RADIANS_R8
          deallocate (centers_full)
       end if
       call MAPL_SyncSharedMemory(_RC)
       call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
          staggerloc=ESMF_STAGGERLOC_CENTER, &
          farrayPtr=fptr, rc=status)
       fptr=real(centers(i_1:i_n,j_1:j_n), kind=ESMF_KIND_R8)

       if(MAPL_ShmInitialized) then
          call MAPL_DeAllocNodeArray(centers,_RC)
       else
          deallocate(centers)
       end if

      lgr => logging%get_logger('HISTORY.sampler')
      call lgr%debug('%a', 'test')
      call lgr%debug('%a %i8 %i8',  'Xdim, Ydim', Xdim, Ydim)
      call lgr%debug('%a %i8 %i8', 'Xdim_full, Ydim_full', Xdim_full, Ydim_full)
      call lgr%debug('%a %i8 %i8 %i8 %i8', 'epoch_index(1:4)', &
           this%epoch_index(1), this%epoch_index(2), &
           this%epoch_index(3), this%epoch_index(4))
       
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
      use esmf
      use pflogger, only : Logger, logging
      implicit none
      class (SwathGridFactory), intent(inout) :: this
      type (ESMF_Config), intent(inout) :: config
      character(len=*), intent(in) :: prefix
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status

      type(ESMF_VM) :: VM
      integer :: nlon, nlat, tdim
      integer :: Xdim, Ydim, ntime
      character(len=ESMF_MAXSTR) :: key_lon, key_lat, key_time
      character(len=ESMF_MAXSTR) :: filename, tunit, tmp, grp_name
      real, allocatable :: scanTime(:,:)
      integer :: yy, mm, dd, h, m, s, sec
      integer :: i, j

      type(ESMF_Time) :: time0
      integer (ESMF_KIND_I8) :: j0, j1, jt, jt1, jt2
      real(ESMF_KIND_R8) :: jx0, jx1
      real(ESMF_KIND_R8) :: x0, x1
      integer :: khi, klo, k, nstart, max_iter
      type(Logger), pointer :: lgr
      logical :: ispresent

      _UNUSED_DUMMY(unusable)
      lgr => logging%get_logger('HISTORY.sampler')
      
      call ESMF_VmGetCurrent(VM, _RC)

      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'GRIDNAME:', default=MAPL_GRID_NAME_DEFAULT)
      this%grid_name = trim(tmp)
      call ESMF_ConfigGetAttribute(config, this%nx,  label=prefix//'NX:', default=MAPL_UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%ny,  label=prefix//'NY:', default=MAPL_UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%lm,  label=prefix//'LM:', default=MAPL_UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%input_template, label=prefix//'GRID_FILE:', default='unknown.txt', _RC)
      call ESMF_ConfigGetAttribute(config, this%epoch, label=prefix//'Epoch:', default=300, _RC)
      call ESMF_ConfigGetAttribute(config, tmp,      label=prefix//'Epoch_init:', default='2006', _RC)

      call lgr%debug(' %a  %a', 'input_template =', trim(this%input_template))

      print*,__FILE__, __LINE__
      !!write(6,'(2x,a,/,4i8,/,5(2x,a))') 'nx,ny,lm,epoch -- filename,tmp', &
      !!     this%nx,this%ny,this%lm,this%epoch,&
      !!     trim(filename),trim(tmp)
      !!print*, 'ck: Epoch_init:', trim(tmp)

!      filename
      
      if ( index(tmp, 'T') /= 0 .OR. index(tmp, '-') /= 0 ) then
         call ESMF_TimeSet(time0, timeString=tmp, _RC)
      else
         read(tmp,'(i4,5i2)') yy,mm,dd,h,m,s
         call ESMF_Timeset(time0, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, _RC)
      endif
      this%grid_file_name = trim(filename)
      
      call ESMF_ConfigGetAttribute(config, value=this%nc_index, default="", &
                 label=prefix // 'nc_Index:', _RC)      
      call ESMF_ConfigGetAttribute(config, value=this%nc_time, default="", &
           label=prefix//'nc_Time:',  _RC)
      call ESMF_ConfigGetAttribute(config, this%nc_longitude, &
           label=prefix // 'nc_Longitude:', default="", _RC)
      call ESMF_ConfigGetAttribute(config, this%nc_latitude, &
           label=prefix // 'nc_Latitude:', default="", _RC)

      write(6,'((2x,a),10(2x,a15))') 'nc_time =', trim(this%nc_time)
      write(6,'((2x,a),10(2x,a15))') 'nc_lon  =',            trim(this%nc_longitude)
      write(6,'((2x,a),10(2x,a15))') 'nc_lat  =',            trim(this%nc_latitude)     

      
      i=index(this%nc_longitude, '/')
      if (i>0) then
         this%found_group = .true.
         grp_name = this%nc_longitude(1:i-1)
      else
         this%found_group = .false.
         grp_name = ''
      endif
      this%var_name_lat = this%nc_latitude(i+1:)
      this%var_name_lon = this%nc_longitude(i+1:)
      this%var_name_time= this%nc_time(i+1:)

      write(6,'(10(2x,a))') 'name lat, lon, time',  &
           trim(this%var_name_lat),  trim(this%var_name_lon), trim(this%var_name_time)

      ! read global dim from nc file
      ! ----------------------------
      key_lon=this%var_name_lon
      key_lat=this%var_name_lat
      key_time=this%var_name_time
      !      CALL get_ncfile_dimension(filename, nlon, nlat, tdim, key_lon, key_lat, key_time, _RC)

      filename='/Users/yyu11/ModelData/earthData/flk_modis_MOD04_2017_090/MOD04_L2.A2017090.0010.051.NRT.hdf'

      
      CALL get_ncfile_dimension(filename, nlon=nlon, &
           key_lon=key_lon, _RC)
      print*, trim(key_lon), ' nlon ', nlon

      _FAIL('stop')

      CALL get_ncfile_dimension(filename, nlon=nlon, nlat=nlat, &
           key_lon=key_lon, key_lat=key_lat, _RC)
      allocate(scanTime(nlon, nlat))
      allocate(this%t_alongtrack(nlat))
      
      lgr => logging%get_logger('HISTORY.sampler')
      call lgr%debug('%a  %a', &
           'swath Epoch init time:', trim(tmp) )
      call lgr%debug('%a  %a', &
           'swath obs filename:   ', trim(filename) )
      call lgr%debug('%a  %i8  %i8  %i8', &
           'swath obs nlon,nlat,tdim:', nlon,nlat,tdim )

      call get_v2d_netcdf(filename, 'scanTime', scanTime, nlon, nlat)
      do j=1, nlat
         this%t_alongtrack(j)= scanTime(1,j)
      enddo
      !
      ! skip un-defined time value
      !
      !
      nstart = 1
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
      
      deallocate(scanTime)
      
      this%cell_across_swath = nlon
      this%cell_along_swath = nlat

      
      ! determine im_world from Epoch
      ! -----------------------------
      ! t_axis = t_alongtrack = t_a
      ! convert time0 to j0
      ! use Epoch to find j1
      ! search j0, j1 in t_a


      ! this is a bug
      !
      tunit='seconds since 1993-01-01 00:00:00'
      this%tunit = tunit
      call time_esmf_2_nc_int (time0, tunit, j0, _RC)
      sec = hms_2_s (this%Epoch)
      j1= j0 + sec
      jx0= j0
      jx1= j1
      !!call lgr%debug ('%a %f8 %f8', 'jx0, jx1', jx0, jx1)
      call lgr%debug ('%a %i16 %i16', 'j0,  j1 ', j0,  j1)

      
      this%epoch_index(1)= 1
      this%epoch_index(2)= this%cell_across_swath
      call bisect( this%t_alongtrack, jx0, jt1, n_LB=int(nstart, ESMF_KIND_I8), n_UB=int(this%cell_along_swath, ESMF_KIND_I8), rc=rc)
      call bisect( this%t_alongtrack, jx1, jt2, n_LB=int(nstart, ESMF_KIND_I8), n_UB=int(this%cell_along_swath, ESMF_KIND_I8), rc=rc)


      if (jt1==jt2) then
         _FAIL('Epoch Time is too small, empty swath grid is generated, increase Epoch')
      endif
      jt1 = jt1 + 1               ! (x1,x2]  design
      this%epoch_index(3)= jt1
      this%epoch_index(4)= jt2
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


      _RETURN(_SUCCESS)


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

      _UNUSED_DUMMY(unusable)

      n = this%im_world/this%nx
      if (n < 2) then
         this%nx = generate_new_decomp(this%im_world,this%nx)
         deallocate(this%ims)
         allocate(this%ims(0:this%nx-1))
         call MAPL_DecomposeDim(this%im_world, this%ims, this%nx)
      end if
      n = this%jm_world/this%ny
      if (n < 2) then
         this%ny = generate_new_decomp(this%jm_world,this%ny)
         deallocate(this%jms)
         allocate(this%jms(0:this%ny-1))
         call MAPL_DecomposeDim(this%jm_world, this%jms, this%ny)
      end if

      _RETURN(_SUCCESS)

   end subroutine generate_newnxy

   function generate_new_decomp(im,nd) result(n)
      integer, intent(in) :: im, nd
      integer :: n
      logical :: canNotDecomp

      canNotDecomp = .true.
      n = nd
      do while(canNotDecomp)
         if ( (im/n) < 2) then
            n = n/2
         else
            canNotDecomp = .false.
         end if
      enddo
   end function generate_new_decomp

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
      
      key_lon=trim(this%var_name_lon)
      key_lat=trim(this%var_name_lat)
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
      class(SwathGridFactory), intent(in) :: this
      type(ESMF_Time), intent(in) :: interval(2)
      integer, intent(out) :: xy_subset(2,2)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Time) :: T1, T2      
      integer(ESMF_KIND_I8) :: i1, i2
      real(ESMF_KIND_R8) :: iT1, iT2
      integer(ESMF_KIND_I8) :: index1, index2
      integer :: jlo, jhi, je

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
      jlo = this%epoch_index(3) - 2
      jhi = this%epoch_index(4) + 1
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

      integer :: i_1, i_n, j_1, j_n ! regional array bounds

      !! shared mem
      real(kind=ESMF_KIND_R8), pointer :: fptr(:,:)
      real, pointer :: centers(:,:)
      real, allocatable :: centers_full(:,:)
      
      integer :: i, j, k
      integer :: Xdim, Ydim
      integer :: Xdim_full, Ydim_full
      
      integer :: IM_WORLD, JM_WORLD
      character(len=:), allocatable :: time_name


      ! keywords in netCDF
      time_name = "scanTime"
      call MAPL_grid_interior(grid, i_1, i_n, j_1, j_n)

      !- shared mem case in MPI
      !
      Xdim=this%im_world
      Ydim=this%jm_world

      Xdim_full=this%cell_across_swath
      Ydim_full=this%cell_along_swath
      
      call MAPL_AllocateShared(centers,[Xdim,Ydim],transroot=.true.,_RC)
      call MAPL_SyncSharedMemory(_RC)
      

       ! read Time and set
       if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
          allocate( centers_full(Xdim_full, Ydim_full))
          call get_v2d_netcdf(this%grid_file_name, time_name, centers_full, Xdim_full, Ydim_full)
          k=0
          do j=this%epoch_index(3), this%epoch_index(4)
             k=k+1
             centers(1:Xdim, k) = centers_full(1:Xdim, j)
          enddo
          deallocate (centers_full)
       end if
       call MAPL_SyncSharedMemory(_RC)

       !(Xdim, Ydim)
       obs_time = centers(i_1:i_n,j_1:j_n)

       if(MAPL_ShmInitialized) then
          call MAPL_DeAllocNodeArray(centers,_RC)
       else
          deallocate(centers)
       end if
      
      _RETURN(_SUCCESS)
    end subroutine get_obs_time


end module MAPL_SwathGridFactoryMod
