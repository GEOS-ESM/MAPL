#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

! Swath: (misisng time and long-axes for scan)
! This module generates ESMF_Grids for logically-rectangular grid based on observation (netCDF)
! Spacing between lats (lons) needs not be constant (imhomogenous)
! LatLon concepts such as corners, pole and dataline are eliminated

module MAPL_SwathGridFactoryMod
   use MAPL_AbstractGridFactoryMod
   use MAPL_MinMaxMod
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_ShmemMod
   use mapl_ErrorHandlingMod
   use MAPL_Constants
   use ESMF
   use pFIO
   use MAPL_CommsMod
   use, intrinsic :: iso_fortran_env, only: REAL32
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none
   private

   public :: SwathGridFactory

   integer, parameter :: NUM_DIM = 2
   
   type, extends(AbstractGridFactory) :: SwathGridFactory
      private
      character(len=:), allocatable :: grid_name
      character(len=:), allocatable :: grid_file_name      
      ! Grid dimensions
      integer :: im_world = MAPL_UNDEFINED_INTEGER
      integer :: jm_world = MAPL_UNDEFINED_INTEGER
      integer :: lm = MAPL_UNDEFINED_INTEGER
      ! prepare to delete
      !real(kind=REAL64), allocatable :: lon_centers(:)
      !real(kind=REAL64), allocatable :: lat_centers(:)
      logical :: force_decomposition = .false.

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

      STOP 'Stop:  SwathGridFactory_from_parameters is not tested'
      
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
      call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CORNER, _RC)

      if (this%lm /= MAPL_UNDEFINED_INTEGER) then
         call ESMF_AttributeSet(grid, name='GRID_LM', value=this%lm, _RC)
      end if

      call ESMF_AttributeSet(grid, 'GridType', 'LatLon', _RC)   ! grid=ESMF_grid

      call ESMF_AttributeSet(grid, 'Global', .false., rc=status)


      _RETURN(_SUCCESS)
   end function create_basic_grid

   

   subroutine add_horz_coordinates_from_file(this, grid, unusable, rc)
      use MAPL_BaseMod, only: MAPL_grid_interior
      class (SwathGridFactory), intent(in) :: this
      type (ESMF_Grid), intent(inout) :: grid
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: i_1, i_n, j_1, j_n ! regional array bounds
      integer :: ic_1,ic_n,jc_1,jc_n ! regional corner bounds

      !! shared mem
      real(kind=ESMF_KIND_R8), pointer :: fptr(:,:)
      real, pointer :: centers(:,:)               ! check R4 or R8

      integer :: status
      integer :: i, j, ij(4)
      integer :: Xdim, Ydim
      
      integer :: IM, JM
      integer :: IM_WORLD, JM_WORLD
      integer :: COUNTS(3), DIMS(3)
      character(len=:), allocatable :: lon_center_name, lat_center_name
      
      
      _UNUSED_DUMMY(unusable)

      ! keywords in netCDF
      lon_center_name = "lon_centers"
      lat_center_name = "lat_centers"      
      
      call MAPL_grid_interior(grid, i_1, i_n, j_1, j_n)

      !- shared mem case in MPI
      Xdim=this%im_world
      Ydim=this%jm_world
      call MAPL_AllocateShared(centers,[Xdim,Ydim],transroot=.true.,_RC)
      call MAPL_SyncSharedMemory(_RC)

      ! do longitudes
       if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
          call get_v2d_netcdf(this%grid_file_name, lon_center_name, centers, Xdim, Ydim)
           centers=centers*MAPL_DEGREES_TO_RADIANS_R8
       end if
       call MAPL_SyncSharedMemory(_RC)

       call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
          staggerloc=ESMF_STAGGERLOC_CENTER, &
          farrayPtr=fptr, rc=status)
       fptr=real(centers(i_1:i_n,j_1:j_n), kind=ESMF_KIND_R8)

       ! do latitudes
       if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
          call get_v2d_netcdf(this%grid_file_name, lat_center_name, centers, Xdim, Ydim)
           centers=centers*MAPL_DEGREES_TO_RADIANS_R8
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

      STOP 'not tested:  subroutine initialize_from_file_metadata'
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

         ! TODO: check radians vs degrees.  Assume degrees for now.

!        28-Feb-2023 disable array retrieval from metadata in Swath
!         
!        ! TODO: modify CoordinateVariable so that get_coordinate_data() is overloaded
!        ! for different types (as subroutine) to avoid casting here.
!        ! TODO: add get_coordinate_variable() interface to avoid the need to cast
!        v => file_metadata%get_coordinate_variable(lon_name,_RC)
!        ptr => v%get_coordinate_data()
!        _ASSERT(associated(ptr),'coordinate data not allocated')
!        select type (ptr)
!        type is (real(kind=REAL64))
!           this%lon_centers = ptr
!        type is (real(kind=REAL32))
!           this%lon_centers = ptr
!        class default
!           _FAIL('unsuppoted type of data; must be REAL32 or REAL64')
!        end select
!
!        if (any((this%lon_centers(2:im)-this%lon_centers(1:im-1))<0)) then
!           where(this%lon_centers > 180) this%lon_centers=this%lon_centers-360
!        end if
!
!
!        v => file_metadata%get_coordinate_variable(lat_name,_RC)
!        ptr => v%get_coordinate_data()
!        _ASSERT(associated(ptr),'coordinate data not allocated')
!        select type (ptr)
!        type is (real(kind=REAL64))
!           this%lat_centers = ptr
!        type is (real(kind=REAL32))
!           this%lat_centers = ptr
!        class default
!           _FAIL('unsupported type of data; must be REAL32 or REAL64')
!        end select
!
!
!        ! Check: is this a "mis-specified" pole-centered grid?
!        if (size(this%lat_centers) >= 4) then
!           ! Assume lbound=1 and ubound=size for now
!           i_min = 1 !lbound(this%lat_centers)
!           i_max = size(this%lat_centers) !ubound(this%lat_centers)
!           d_lat = (this%lat_centers(i_max-1) - this%lat_centers(i_min+1))/&
!                    (size(this%lat_centers)-3)
!           is_valid = .True.
!           ! Check: is this a regular grid (i.e. constant spacing away from the poles)?
!           do i=(i_min+1),(i_max-2)
!              d_lat_temp = this%lat_centers(i+1) - this%lat_centers(i)
!              is_valid = (is_valid.and.(abs((d_lat_temp/d_lat)-1.0) < 1.0e-5))
!              if (.not. is_valid) then
!                 exit
!              end if
!           end do
!           if (is_valid) then
!              ! Should the southernmost point actually be at the pole?
!              extrap_lat = this%lat_centers(i_min+1) - d_lat
!              if (extrap_lat <= ((d_lat/20.0)-90.0)) then
!                 this%lat_centers(i_min) = -90.0
!              end if
!              ! Should the northernmost point actually be at the pole?
!              extrap_lat = this%lat_centers(i_max-1) + d_lat
!              if (extrap_lat >= (90.0-(d_lat/20.0))) then
!                 this%lat_centers(i_max) =  90.0
!              end if
!           end if
!        end if
!
!        if (use_file_coords) then
!           this%lon_centers = MAPL_DEGREES_TO_RADIANS_R8 * this%lon_centers
!           this%lat_centers = MAPL_DEGREES_TO_RADIANS_R8 * this%lat_centers
!        end if
!         
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
      class (SwathGridFactory), intent(inout) :: this
      type (ESMF_Config), intent(inout) :: config
      character(len=*), intent(in) :: prefix  ! effectively optional due to overload without this argument
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: tmp
      type(ESMF_VM) :: VM
      integer :: Xdim, Ydim, ntime

      _UNUSED_DUMMY(unusable)

      call ESMF_VmGetCurrent(VM, _RC)

      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'GRIDNAME:', default=MAPL_GRID_NAME_DEFAULT)
      this%grid_name = trim(tmp)
      call ESMF_ConfigGetAttribute(config, this%nx, label=prefix//'NX:', default=MAPL_UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%ny, label=prefix//'NY:', default=MAPL_UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%lm, label=prefix//'LM:', default=MAPL_UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'GRIDSPEC:', _RC)
      this%grid_file_name = trim(tmp)
      call get_ncfile_dimension(this%grid_file_name, Xdim, Ydim, ntime)   ! xdim, ydim, tdim
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
!            write(6,*) 'values(i)=', values(i) 
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

      stop 'not implemented:   subroutine initialize_from_esmf_distGrid(this, dist_grid, lon_array, lat_array, unusable, rc)'
      
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


      !if (abs(lat(1) + PI/2) < tiny) then
      !   pole = 'PC'
      !elseif (abs(lat(1) + PI/2 - 0.5*(lat(2)-lat(1))) < tiny) then
      !   pole = 'PE'
      !else
      !   pole = 'PC'
      !end if

      ! the code below is kluge to return DE/DC wheither or not the file lons are -180 to 180 or 0 360
      ! it detects whether the first longitudes which are cell centers
      ! If first longitude is 0 or -180 (DC) it is dateline center in that 0 or -180 is
      ! in the center of a grid cell.
      ! or shifted by half a grid box (DE) so 0 or -180 is the edge of a cell
      ! really should have 4 options dateline edge (DE), dateline center(DC)
      ! grenwich center (GC) and grenwich edge (GE) but the last 2 are not supported
      ! if it is GC or GE we will shift the data on the usage so that it is DE or DC for now
      !do i=0,1
      !   if (abs(lon(1) + PI*i) < tiny) then
      !      dateline = 'DC'
      !      exit
      !   elseif (abs(lon(1) + PI*i - 0.5*(lon(2)-lon(1))) < tiny) then
      !      dateline = 'DE'
      !      exit
      !   end if
      !end do
      !if (abs(lon(1) + PI) < tiny) then
      !dateline = 'DC'
      !elseif (abs(lon(1) + PI - 0.5*(lon(2)-lon(1))) < tiny) then
      !dateline = 'DE'
      !elseif (abs(lon(1)) < tiny) then
      !dateline = 'GC'
      !elseif (abs(lon(1) - 0.5*(lon(2)-lon(1))) < tiny) then
      !dateline = 'GE'
      !end if

      !call MAPL_ConfigSetAttribute(config, pole, 'POLE:')
      !call MAPL_ConfigSetAttribute(config, dateline, 'DATELINE:')

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

         !-- prepare to delete it
         !equal = &
         !     & all(a%lon_centers == this%lon_centers) .and. &
         !     & all(a%lat_centers == this%lat_centers)
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

      character(len=4) :: im_string, jm_string

      !@bena-nasa Is there a problem that this grid name may overlap our names for LatLon? Maybe we need something like "swath" as a prefix or suffix? And maybe even the filename that provided the geolocations?
      
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
      STOP 'Stop: subroutine init_halo is not needed for SwathGridFactory'
   end subroutine init_halo

   subroutine halo(this, array, unusable, halo_width, rc)
      use MAPL_CommsMod
      class (SwathGridFactory), intent(inout) :: this
      real(kind=REAL32), intent(inout) :: array(:,:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: halo_width
      integer, optional, intent(out) :: rc
      STOP 'Stop: subroutine halo is not needed for SwathGridFactory'
   end subroutine halo



   subroutine append_metadata(this, metadata)
      use MAPL_Constants
      class (SwathGridFactory), intent(inout) :: this
      type (FileMetadata), intent(inout) :: metadata

      type (Variable) :: v
      real(kind=REAL64), allocatable :: temp_coords(:)

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
      _UNUSED_DUMMY(this)

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

  subroutine get_ncfile_dimension(filename, nlon, nlat, tdim)
    use netcdf
    implicit none
    character(len=*), intent(in) :: filename
    integer, intent(out) :: nlat, nlon, tdim
    integer :: ncid , dimid
    integer :: rc, status

    call check_nc_status(nf90_open(trim(fileName), NF90_NOWRITE, ncid), _RC)
    call check_nc_status(nf90_inq_dimid(ncid, "time", dimid), _RC)
    call check_nc_status(nf90_inquire_dimension(ncid, dimid, len=tdim), _RC)
    !
    call check_nc_status(nf90_inq_dimid(ncid, "lon", dimid), _RC)
    call check_nc_status(nf90_inquire_dimension(ncid, dimid, len=nlon), _RC)
    !
    call check_nc_status(nf90_inq_dimid(ncid, "lat", dimid), _RC)
    call check_nc_status(nf90_inquire_dimension(ncid, dimid, len=nlat), _RC)
    call check_nc_status(nf90_close(ncid), _RC)
    !! debug summary
    !! write(6,*) "get_ncfile_dimension:  nlat, nlon, tdim = ", nlat, nlon, tdim
  end subroutine get_ncfile_dimension

  
  subroutine get_v2d_netcdf(filename, name, array, Xdim, Ydim)
    use netcdf
    implicit none
    character(len=*), intent(in) :: name, filename
    integer, intent(in) :: Xdim, Ydim
    real, dimension(Xdim,Ydim), intent(out) :: array
    integer :: ncid, varid
    real    :: scale_factor, add_offset
    integer :: rc, status, iret
    
    call check_nc_status (  nf90_open      (trim(fileName), NF90_NOWRITE, ncid), _RC )
    call check_nc_status (  nf90_inq_varid (ncid,  name,  varid), _RC )
    call check_nc_status (  nf90_get_var   (ncid, varid,  array), _RC )
    
    iret = nf90_get_att(ncid, varid, 'scale_factor', scale_factor)
    if(iret .eq. 0) array = array * scale_factor
    !
    iret = nf90_get_att(ncid, varid, 'add_offset', add_offset)
    if(iret .eq. 0) array = array + add_offset
    !
    iret = nf90_close(ncid)
  end subroutine get_v2d_netcdf


  subroutine check_nc_status(status, rc)
    use netcdf
    implicit none
    integer, intent (in) :: status
    integer, intent (out), optional :: rc
    if(status /= nf90_noerr) then
       print *, 'netCDF error: '//trim(nf90_strerror(status))
    endif
    if(present(rc))  rc=status-nf90_noerr
  end subroutine check_nc_status
  
end module MAPL_SwathGridFactoryMod
