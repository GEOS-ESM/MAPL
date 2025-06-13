#include "MAPL_ErrLog.h"

! overload set interfaces in legacy
! Document Pole :: XY 
!          Date :: DE

! This module generates Equal Area Scalable Earth (EASE) grids as ESMF_Grids.
! EASE grids have equal area across all grid cells.
!
! Only global cylindrical EASEv[x]_M[yy] grids are implemented:
! - Spacing is uniform for lons and non-uniform for lats.
! - Dateline is on the edge of the grid (dateline='DE'), min/max lon = -180:180.
! - Poles are outside of the grid (pole='XY'), min/max lat ~ -85:85 (exact bounds depend on resolution).
!
! Polar EASE grids are not implemented.
!
! References: Brodzik et al 2012, doi:10.3390/ijgi1010032
!   Brodzik et al 2012: doi:10.3390/ijgi1010032
!   Brodzik et al 2014: doi:10.3390/ijgi3031154 -- correction of M25 "map_scale_m" parameters!

module MAPL_EASEGridFactoryMod
   use MAPL_AbstractGridFactoryMod
   use MAPL_MinMaxMod
   use MAPL_KeywordEnforcerMod
   use mapl_ErrorHandlingMod
   use MAPL_Constants
   use ESMF
   use pFIO
   use MAPL_CommsMod
   use MAPL_EASEConversion
   use, intrinsic :: iso_fortran_env, only: REAL32
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none
   private

   public :: EASEGridFactory

   integer, parameter :: NUM_DIM = 2

   type, extends(AbstractGridFactory) :: EASEGridFactory
      private
      logical :: is_evenspaced = .false.
      character(len=:), allocatable :: grid_name
      ! Grid dimensions
      integer :: im_world = MAPL_UNDEFINED_INTEGER
      integer :: jm_world = MAPL_UNDEFINED_INTEGER
      integer :: lm = MAPL_UNDEFINED_INTEGER
      real(kind=REAL64), allocatable :: lon_centers(:)
      real(kind=REAL64), allocatable :: lat_centers(:)
      real(kind=REAL64), allocatable :: lon_centers_degrees(:)
      real(kind=REAL64), allocatable :: lat_centers_degrees(:)
      real(kind=REAL64), allocatable :: lon_corners(:)
      real(kind=REAL64), allocatable :: lat_corners(:)
      logical :: force_decomposition = .false.

      ! Domain decomposition:
      integer :: nx = MAPL_UNDEFINED_INTEGER
      integer :: ny = MAPL_UNDEFINED_INTEGER
      integer, allocatable :: ims(:)
      integer, allocatable :: jms(:)
      ! Grid conventions:
      character(len=:), allocatable :: pole
      character(len=:), allocatable :: dateline
      ! Regional vs global:
      type (RealMinMax) :: lon_range = RealMinMax(MAPL_UNDEFINED_REAL,MAPL_UNDEFINED_REAL)
      type (RealMinMax) :: lat_range = RealMinMax(MAPL_UNDEFINED_REAL,MAPL_UNDEFINED_REAL)
      ! Used for halo
      type (ESMF_DELayout) :: layout
      integer :: px, py
      logical :: is_halo_initialized = .false.
      logical :: periodic = .true.
      character(len=:), allocatable :: lon_bounds_name
      character(len=:), allocatable :: lat_bounds_name
   contains
      procedure :: make_new_grid
      procedure :: create_basic_grid
      procedure :: get_longitudes_degrees
      procedure :: get_longitudes
      procedure :: get_latitudes_degrees
      procedure :: get_latitudes
      procedure :: compute_lon_centers
      procedure :: compute_lat_centers
      procedure :: get_lon_corners
      procedure :: get_lat_corners
      procedure :: compute_lon_corners
      procedure :: compute_lat_corners
      procedure :: add_horz_coordinates
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
   end type EASEGridFactory

   character(len=*), parameter :: MOD_NAME = 'MAPL_EASEGridFactory::'

   interface EASEGridFactory
      module procedure EASEGridFactory_from_parameters
   end interface EASEGridFactory

   interface set_with_default
      module procedure set_with_default_integer
      module procedure set_with_default_real
      module procedure set_with_default_character
      module procedure set_with_default_range
      module procedure set_with_default_logical
   end interface set_with_default


contains


   function EASEGridFactory_from_parameters(unusable, grid_name, &
        & lm, nx, ny, ims, jms, &
        & force_decomposition, rc) result(factory)
      type (EASEGridFactory) :: factory
      class (KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: grid_name

      ! grid details  are from grid_name:
      integer, optional, intent(in) :: lm
      ! decomposition:
      integer, optional, intent(in) :: nx
      integer, optional, intent(in) :: ny
      integer, optional, intent(in) :: ims(:)
      integer, optional, intent(in) :: jms(:)
      logical, optional, intent(in) :: force_decomposition
      integer, optional, intent(out) :: rc

      integer :: status, cols, rows
      real    :: cell_area, ur_lat, ur_lon, ll_lat, ll_lon 

      _UNUSED_DUMMY(unusable)

      factory%is_evenspaced = .false.
      call set_with_default(factory%grid_name, grid_name, MAPL_GRID_NAME_DEFAULT)

      call set_with_default(factory%nx, nx, MAPL_UNDEFINED_INTEGER)
      call set_with_default(factory%ny, ny, MAPL_UNDEFINED_INTEGER)

      call ease_extent(grid_name, cols, rows, cell_area=cell_area, ll_lon=ll_lon, ll_lat=ll_lat, ur_lon=ur_lon, ur_lat=ur_lat)

      call set_with_default(factory%im_world, cols, MAPL_UNDEFINED_INTEGER)
      call set_with_default(factory%jm_world, rows, MAPL_UNDEFINED_INTEGER)
      call set_with_default(factory%lm, lm, MAPL_UNDEFINED_INTEGER)

      ! default is unallocated
      if (present(ims)) factory%ims = ims
      if (present(jms)) factory%jms = jms

      call set_with_default(factory%pole,     'XY', MAPL_UNDEFINED_CHAR)
      call set_with_default(factory%dateline, 'DE', MAPL_UNDEFINED_CHAR)

      factory%lat_range =  RealMinMax(ll_lat, ur_lat)

      call set_with_default(factory%force_decomposition, force_decomposition, .false.)

      call factory%check_and_fill_consistency(_RC)

      ! Compute the centers and corners
      factory%lon_centers = factory%compute_lon_centers(factory%dateline, _RC)
      factory%lat_centers = factory%compute_lat_centers(_RC)
      factory%lon_centers_degrees = factory%compute_lon_centers(factory%dateline, &
            convert_to_radians = .false.,  _RC)
      factory%lat_centers_degrees = factory%compute_lat_centers( &
            convert_to_radians = .false.,  _RC)
      factory%lon_corners = factory%compute_lon_corners(factory%dateline, _RC)
      factory%lat_corners = factory%compute_lat_corners(_RC)

      _RETURN(_SUCCESS)

   end function EASEGridFactory_from_parameters


   function make_new_grid(this, unusable, rc) result(grid)
      type (ESMF_Grid) :: grid
      class (EASEGridFactory), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)
      grid = this%create_basic_grid(_RC)

      call this%add_horz_coordinates(grid, _RC)

      _RETURN(_SUCCESS)

   end function make_new_grid



   function create_basic_grid(this, unusable, rc) result(grid)
      type (ESMF_Grid) :: grid
      class (EASEGridFactory), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_PoleKind_Flag) :: polekindflag(2)

      _UNUSED_DUMMY(unusable)

      if (this%periodic) then
         if (this%pole == "XY") then 
            polekindflag = ESMF_POLEKIND_NONE
         else
            polekindflag = ESMF_POLEKIND_MONOPOLE
         end if
         grid = ESMF_GridCreate1PeriDim( &
              & name = this%grid_name, &
              & countsPerDEDim1=this%ims, &
              & countsPerDEDim2=this%jms, &
              & indexFlag=ESMF_INDEX_DELOCAL, &
              & gridEdgeLWidth=[0,0], &
              & gridEdgeUWidth=[0,1], &
              & coordDep1=[1,2], &
              & coordDep2=[1,2], &
              & coordSys=ESMF_COORDSYS_SPH_RAD, &
              & polekindflag=polekindflag, &
              & _RC)
      else
         grid = ESMF_GridCreateNoPeriDim( &
              & name = this%grid_name, &
              & countsPerDEDim1=this%ims, &
              & countsPerDEDim2=this%jms, &
              & indexFlag=ESMF_INDEX_DELOCAL, &
              & gridEdgeLWidth=[0,0], &
              & gridEdgeUWidth=[1,1], &
              & coordDep1=[1,2], &
              & coordDep2=[1,2], &
              & coordSys=ESMF_COORDSYS_SPH_RAD, &
              & _RC)
      end if

      ! Allocate coords at default stagger location
      call ESMF_GridAddCoord(grid, _RC)
      call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CORNER, _RC)

      if (this%lm /= MAPL_UNDEFINED_INTEGER) then
         call ESMF_AttributeSet(grid, name='GRID_LM', value=this%lm, _RC)
      end if

      call ESMF_AttributeSet(grid, 'GridType',  'EASE', _RC)
      ! set to false. no pole in EASE
      call ESMF_AttributeSet(grid, 'Global', .false., _RC)

      _RETURN(_SUCCESS)
   end function create_basic_grid

   ! in radians
   function get_longitudes(this, unusable, rc) result(longitudes)
      use MAPL_BaseMod
      class (EASEGridFactory), intent(in) :: this
      real(kind=REAL64), allocatable :: longitudes(:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)

      longitudes = this%lon_centers
      _RETURN(_SUCCESS)
   end function get_longitudes

   function get_longitudes_degrees(this, unusable, rc) result(longitudes)
      use MAPL_BaseMod
      class (EASEGridFactory), intent(in) :: this
      real(kind=REAL64), allocatable :: longitudes(:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)

      longitudes = this%lon_centers_degrees
      _RETURN(_SUCCESS)
   end function get_longitudes_degrees

   ! in radians
   function get_latitudes(this, unusable, rc) result(latitudes)
      use MAPL_BaseMod
      class (EASEGridFactory), intent(in) :: this
      real(kind=REAL64), allocatable :: latitudes(:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)

      latitudes = this%lat_centers
      _RETURN(_SUCCESS)
   end function get_latitudes

   function get_latitudes_degrees(this, unusable, rc) result(latitudes)
      use MAPL_BaseMod
      class (EASEGridFactory), intent(in) :: this
      real(kind=REAL64), allocatable :: latitudes(:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)

      latitudes = this%lat_centers_degrees
      _RETURN(_SUCCESS)
   end function get_latitudes_degrees

   ! in radians
   function compute_lon_centers(this, dateline, unusable, convert_to_radians, rc) result(lon_centers)
      use MAPL_Constants, only:MAPL_DEGREES_TO_RADIANS_R8
      use MAPL_BaseMod
      real(kind=REAL64), allocatable :: lon_centers(:)
      class (EASEGridFactory), intent(in) :: this
      character(2), intent(in) :: dateline
      class (KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in)  :: convert_to_radians
      integer, optional, intent(out) :: rc

      real(kind=REAL64) :: delta, min_coord, max_coord
      logical :: local_convert_to_radians
      logical :: regional
      integer :: status

      _UNUSED_DUMMY(unusable)
      if (present(convert_to_radians)) then
         local_convert_to_radians = convert_to_radians
      else
         local_convert_to_radians = .true.
      end if

      allocate(lon_centers(this%im_world))

      regional  = (dateline == 'XY')
      if (regional) then
         _FAIL('Not supported reginal lons')
        ! delta = (this%lon_range%max - this%lon_range%min) / this%im_world
        ! min_coord = this%lon_range%min + delta/2
        ! max_coord = this%lon_range%max - delta/2
      else
         delta = 360.d0 / this%im_world
         min_coord = -180.d0 + delta/2
         max_coord = +180.d0 - delta/2
      end if

      if (local_convert_to_radians) then
         lon_centers = MAPL_Range(min_coord, max_coord, this%im_world, &
              & conversion_factor=MAPL_DEGREES_TO_RADIANS_R8, _RC)
      else
         lon_centers = MAPL_Range(min_coord, max_coord, this%im_world, _RC)
      end if

      _RETURN(_SUCCESS)
   end function compute_lon_centers

   function compute_lon_corners(this, dateline, unusable, rc) result(lon_corners)
      use MAPL_Constants, only:MAPL_DEGREES_TO_RADIANS_R8
      use MAPL_BaseMod
      real(kind=REAL64), allocatable :: lon_corners(:)
      class (EASEGridFactory), intent(in) :: this
      character(2), intent(in) :: dateline
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      real(kind=REAL64) :: delta, min_coord, max_coord
      logical :: regional
      integer :: status

      _UNUSED_DUMMY(unusable)

      allocate(lon_corners(this%im_world+1))

      regional  = (dateline == 'XY')
      if (regional) then
         _FAIL('Not supported regional lons')
         !delta = (this%lon_range%max - this%lon_range%min) / this%im_world
         !min_coord = this%lon_range%min
         !max_coord = this%lon_range%max
      else
         delta = 360.d0 / this%im_world
         min_coord = -180.d0
         max_coord = +180.d0
      end if

      lon_corners = MAPL_Range(min_coord, max_coord, this%im_world+1, &
           & conversion_factor=MAPL_DEGREES_TO_RADIANS_R8, _RC)

      _RETURN(_SUCCESS)
   end function compute_lon_corners


   ! in radians
   function get_lon_corners(this, unusable, rc) result(lon_corners)
      use MAPL_BaseMod
      class (EASEGridFactory), intent(in) :: this
      real(kind=REAL64), allocatable :: lon_corners(:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)

      lon_corners = this%lon_corners
      _RETURN(_SUCCESS)

   end function get_lon_corners


   ! in radians
   function get_lat_corners(this, unusable, rc) result(lat_corners)
      use MAPL_BaseMod
      class (EASEGridFactory), intent(in) :: this
      real(kind=REAL64), allocatable :: lat_corners(:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)

      lat_corners = this%lat_corners
      _RETURN(_SUCCESS)

   end function get_lat_corners


   function compute_lat_centers(this, unusable, convert_to_radians, rc) result(lat_centers)
      use MAPL_Constants, only: MAPL_DEGREES_TO_RADIANS_R8
      use MAPL_BaseMod
      real(kind=REAL64), allocatable :: lat_centers(:)
      class (EASEGridFactory), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in)  :: convert_to_radians
      integer, optional, intent(out) :: rc

      real(kind=REAL32) :: lat, tmplon, s
      logical :: local_convert_to_radians
      integer :: status, row

      _UNUSED_DUMMY(unusable)
      if (present(convert_to_radians)) then
         local_convert_to_radians = convert_to_radians
      else
         local_convert_to_radians = .true.
      end if

      allocate(lat_centers(this%jm_world))

      ! 
      ! EASE grid counting from North to South, and the index is based on 0
      ! 
      do row = 0, this%jm_world-1
         s = row*1.0
         call ease_inverse(this%grid_name, 0., s, lat, tmplon) 
         lat_centers(this%jm_world - row) = lat ! use lat-lon grid index to avoid confusion
      enddo

      if (local_convert_to_radians) then
         lat_centers = lat_centers * MAPL_DEGREES_TO_RADIANS_R8
      endif

      _RETURN(_SUCCESS)

   end function compute_lat_centers

   function compute_lat_corners(this, unusable, rc) result(lat_corners)
      use MAPL_Constants, only: MAPL_DEGREES_TO_RADIANS_R8
      use MAPL_BaseMod
      real(kind=REAL64), allocatable :: lat_corners(:)
      class (EASEGridFactory), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      real(kind=REAL32) :: s, lat, tmplon 

      integer :: status, row

      _UNUSED_DUMMY(unusable)

      allocate(lat_corners(this%jm_world+1))

     
      do row = 0, this%jm_world
         s = row - 0.5
         call ease_inverse(this%grid_name, 0., s, lat, tmplon) 
         lat_corners(this%jm_world +1 -row) = lat
      enddo

      lat_corners = lat_corners * MAPL_DEGREES_TO_RADIANS_R8

      _RETURN(_SUCCESS)

   end function compute_lat_corners

   subroutine add_horz_coordinates(this, grid, unusable, rc)
      use MAPL_BaseMod, only: MAPL_grid_interior
      class (EASEGridFactory), intent(in) :: this
      type (ESMF_Grid), intent(inout) :: grid
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: i_1, i_n, j_1, j_n ! regional array bounds
      integer :: ic_1,ic_n,jc_1,jc_n ! regional corner bounds
      real(kind=ESMF_KIND_R8), pointer :: centers(:,:)
      real(kind=ESMF_KIND_R8), pointer :: corners(:,:)
      integer :: status
      integer :: i, j, ij(4)

      _UNUSED_DUMMY(unusable)

      call MAPL_grid_interior(grid, i_1, i_n, j_1, j_n)
      ij(1)=i_1
      ij(2)=i_n
      ij(3)=j_1
      ij(4)=j_n
      if (.not. any(ij == -1)) then
         if (this%periodic) then
            ic_1=i_1
            ic_n=i_n
         else
            ic_1=i_1
            if (i_n == this%im_world) then
               ic_n=i_n+1
            else
               ic_n=i_n
            end if
         end if

         jc_1=j_1
         if (j_n == this%jm_world) then
            jc_n=j_n+1
         else
            jc_n=j_n
         end if

         ! First we handle longitudes:
         call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
              staggerloc=ESMF_STAGGERLOC_CENTER, &
              farrayPtr=centers, _RC)
         call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
              staggerloc=ESMF_STAGGERLOC_CORNER, &
              farrayPtr=corners, _RC)
         do j = 1, size(centers,2)
            centers(:,j) = this%lon_centers(i_1:i_n)
         end do
         do j = 1, size(corners,2)
            corners(:,j) = this%lon_corners(ic_1:ic_n)
         end do

         ! Now latitudes
         call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
              staggerloc=ESMF_STAGGERLOC_CENTER, &
              farrayPtr=centers, _RC)
         call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
              staggerloc=ESMF_STAGGERLOC_CORNER, &
              farrayPtr=corners, _RC)

         do i = 1, size(centers,1)
            centers(i,:) = this%lat_centers(j_1:j_n)
         end do
         do i = 1, size(corners,1)
            corners(i,:) = this%lat_corners(jc_1:jc_n)
         end do
      end if

      _RETURN(_SUCCESS)

   end subroutine add_horz_coordinates


   subroutine initialize_from_file_metadata(this, file_metadata, unusable, force_file_coordinates, rc)
      use MAPL_KeywordEnforcerMod
      use MAPL_BaseMod, only: MAPL_DecomposeDim

      class (EASEGridFactory), intent(inout)  :: this
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
      character(:), allocatable :: grid_name
      integer :: i
      logical :: hasLon, hasLat, hasLongitude, hasLatitude, hasLev,hasLevel,regLat,regLon
      real(kind=REAL64) :: del12,delij

      integer :: i_min, i_max
      real(kind=REAL64) :: d_lat, d_lat_temp, extrap_lat
      logical :: is_valid, use_file_coords, compute_lons, compute_lats, has_bnds

      _UNUSED_DUMMY(unusable)

      if (present(force_file_coordinates)) then
         use_file_coords = force_file_Coordinates
      else
         use_file_coords = .false.
      end if

      ! Cannot assume that lats and lons are evenly spaced
      this%is_evenspaced = .false.

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

         grid_name = get_ease_gridname_by_cols(im)
         this%grid_name = grid_name

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

         v => file_metadata%get_coordinate_variable(lon_name, _RC)
         ptr => v%get_coordinate_data()
         _ASSERT(associated(ptr),'coordinate data not allocated')
         select type (ptr)
         type is (real(kind=REAL64))
            this%lon_centers = ptr
         type is (real(kind=REAL32))
            this%lon_centers = ptr
         class default
            _FAIL('unsuppoted type of data; must be REAL32 or REAL64')
         end select

         if (any((this%lon_centers(2:im)-this%lon_centers(1:im-1))<0)) then
            where(this%lon_centers > 180) this%lon_centers=this%lon_centers-360
         end if

         has_bnds = coordinate_has_bounds(file_metadata, lon_name, _RC)
         if (has_bnds) then
            this%lon_bounds_name = get_coordinate_bounds_name(file_metadata, lon_name, _RC)
            this%lon_corners = get_coordinate_bounds(file_metadata, lon_name, _RC)
         end if

         v => file_metadata%get_coordinate_variable(lat_name, _RC)
         ptr => v%get_coordinate_data()
         _ASSERT(associated(ptr),'coordinate data not allocated')
         select type (ptr)
         type is (real(kind=REAL64))
            this%lat_centers = ptr
         type is (real(kind=REAL32))
            this%lat_centers = ptr
         class default
            _FAIL('unsupported type of data; must be REAL32 or REAL64')
         end select

         has_bnds = coordinate_has_bounds(file_metadata, lat_name, _RC)
         if (has_bnds) then
            this%lat_bounds_name = get_coordinate_bounds_name(file_metadata, lat_name, _RC)
            this%lat_corners = get_coordinate_bounds(file_metadata, lat_name, _RC)
         end if

         ! lon Corners are the midpoints of lon centers
         if (.not. allocated(this%lon_corners)) then 
            allocate(this%lon_corners(im+1))
            this%lon_corners(1) = (this%lon_centers(im) + this%lon_centers(1))/2 - 180
            this%lon_corners(2:im) = (this%lon_centers(1:im-1) + this%lon_centers(2:im))/2
            this%lon_corners(im+1) = (this%lon_centers(im) + this%lon_centers(1))/2 + 180
         end if

         ! This section about pole/dateline is probably not needed in file data case.
         if (abs(this%lon_corners(1) + 180) < 1000*epsilon(1.0)) then
            this%dateline = 'DE'
         else ! assume 'XY'
            this%dateline = 'XY' 
            this%lon_range = RealMinMax(this%lon_centers(1), this%lon_centers(jm))
         end if
         
         if (.not. allocated(this%lat_corners)) then
            this%lat_corners = this%compute_lat_corners(_RC)
         end if

         this%pole = 'XY'
         this%lat_range = RealMinMax(this%lat_centers(1), this%lat_centers(jm))

         regLon=.true.
         regLat=.false.

         this%is_evenspaced = (regLat .and. regLon)

         if (use_file_coords) then
            this%is_evenspaced = .false.
            this%lon_centers = MAPL_DEGREES_TO_RADIANS_R8 * this%lon_centers
            this%lat_centers = MAPL_DEGREES_TO_RADIANS_R8 * this%lat_centers
            this%lon_corners = MAPL_DEGREES_TO_RADIANS_R8 * this%lon_corners
            this%lat_corners = MAPL_DEGREES_TO_RADIANS_R8 * this%lat_corners
         else
            compute_lons=.false.
            compute_lats=.false.
            if (regLon .and. (this%dateline.ne.'XY')) then
               compute_lons=.true.
            end if
            if (regLat .and. (this%pole.ne.'XY')) then
               compute_lats=.true.
            end if
            if (compute_lons .and. compute_lats) then
               this%lon_centers = this%compute_lon_centers(this%dateline, _RC)
               this%lon_centers_degrees = this%compute_lon_centers(this%dateline, &
                      convert_to_radians=.false., _RC)
               this%lon_corners = this%compute_lon_corners(this%dateline, _RC)
               this%lat_centers_degrees = this%compute_lat_centers(&
                      convert_to_radians=.false., _RC)
               this%lat_centers = this%compute_lat_centers( _RC)
               this%lat_corners = this%compute_lat_corners( _RC)
            else
               this%lon_centers_degrees = this%lon_centers
               this%lat_centers_degrees = this%lat_centers
               this%lon_centers = MAPL_DEGREES_TO_RADIANS_R8 * this%lon_centers
               this%lat_centers = MAPL_DEGREES_TO_RADIANS_R8 * this%lat_centers
               this%lon_corners = MAPL_DEGREES_TO_RADIANS_R8 * this%lon_corners
               this%lat_corners = MAPL_DEGREES_TO_RADIANS_R8 * this%lat_corners
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
      class (EASEGridFactory), intent(inout) :: this
      type (ESMF_Config), intent(inout) :: config
      character(len=*), intent(in) :: prefix  ! effectively optional due to overload without this argument
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: tmp
      type(ESMF_VM) :: VM
      real :: ur_lat, ll_lat

      _UNUSED_DUMMY(unusable)

      call ESMF_VmGetCurrent(VM, _RC)

      this%is_evenspaced = .false.
      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'GRIDNAME:', default=MAPL_GRID_NAME_DEFAULT)
      this%grid_name = trim(tmp)

      call ESMF_ConfigGetAttribute(config, this%nx, label=prefix//'NX:', default=MAPL_UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%ny, label=prefix//'NY:', default=MAPL_UNDEFINED_INTEGER)

      ! given grid_name, im_world and jm_world are comupted

      call ease_extent(this%grid_name, this%im_world, this%jm_world, ll_lat=ll_lat, ur_lat=ur_lat)

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

      call ESMF_ConfigGetAttribute(config, this%lm, label=prefix//'LM:', default=MAPL_UNDEFINED_INTEGER)

      this%pole     = "XY"
      this%dateline = "DE"

      call get_range(this%lon_range, 'LON_RANGE:', _RC)
      this%lat_range = RealMinMax(ll_lat, ur_lat)
      call this%check_and_fill_consistency(_RC)

      ! Compute the centers and corners
      this%lon_centers = this%compute_lon_centers(this%dateline, _RC)
      this%lon_centers_degrees = this%compute_lon_centers(this%dateline, &
               convert_to_radians = .false., _RC)
      this%lat_centers = this%compute_lat_centers( _RC)
      this%lat_centers_degrees = this%compute_lat_centers(&
               convert_to_radians = .false., _RC)
      this%lon_corners = this%compute_lon_corners(this%dateline, _RC)
      this%lat_corners = this%compute_lat_corners( _RC)

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
         call ESMF_ConfigFindLabel(config, label=prefix//label, _RC)
         do i = 1, n
            call ESMF_ConfigGetAttribute(config, values(i), _RC)
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

         if ( .not. FileExists) then
             print*, file_name // "   not found"
             _RETURN(_FAILURE)

         elseif (MAPL_AM_I_Root(VM)) then

            open(newunit=UNIT, file=trim(file_name), form="formatted", iostat=status )
            _VERIFY(STATUS)
            read(UNIT,*) total
            if (total /= n) then
                print*, file_name // " n is different from ", total
                _RETURN(_FAILURE)
            endif
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

         integer :: status
         logical :: isPresent

         call ESMF_ConfigFindLabel(config, label=prefix//label,isPresent=isPresent, _RC)
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
      class (EASEGridFactory), intent(in) :: this

      _UNUSED_DUMMY(this)
      string = 'EASEGridFactory'

   end function to_string



   subroutine check_and_fill_consistency(this, unusable, rc)
      use MAPL_BaseMod, only: MAPL_DecomposeDim
      class (EASEGridFactory), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: verify_decomp

      _UNUSED_DUMMY(unusable)

      _ASSERT( index(this%grid_name, 'EASE') /=0, "grid_name is important to EASE Grid")

      ! Check decomposition/bounds
      ! WY notes: should not have this assert
      !_ASSERT(allocated(this%ims) .eqv. allocated(this%jms), 'inconsistent options')
      call verify(this%nx, this%im_world, this%ims, _RC)
      call verify(this%ny, this%jm_world, this%jms, _RC)

      ! Check regional vs global, EASE grid doesnot include poles
      _ASSERT(this%lat_range%min /= MAPL_UNDEFINED_REAL, 'uninitialized min for lat_range')
      _ASSERT(this%lat_range%max /= MAPL_UNDEFINED_REAL, 'uninitialized max for lat_range')
      _ASSERT(this%lon_range%min == MAPL_UNDEFINED_REAL, 'inconsistent min for lon_range')
      _ASSERT(this%lon_range%max == MAPL_UNDEFINED_REAL, 'inconsistent max for lon_range')

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


   elemental subroutine set_with_default_range(to, from, default)
      type (RealMinMax), intent(out) :: to
      type (RealMinMax), optional, intent(in) :: from
      type (RealMinMax), intent(in) :: default

      if (present(from)) then
         to = from
      else
         to = default
      end if

   end subroutine set_with_default_range

   subroutine set_with_default_logical(to, from, default)
      logical, intent(out) :: to
      logical, optional, intent(in) :: from
      logical, intent(in) :: default

      if (present(from)) then
         to = from
      else
         to = default
      end if

   end subroutine set_with_default_logical

   ! MAPL uses values in lon_array and lat_array only to determine the
   ! general positioning.  Actual coordinates are then recomputed.
   ! This helps to avoid roundoff differences from slightly different
   ! input files.
   subroutine initialize_from_esmf_distGrid(this, dist_grid, lon_array, lat_array, unusable, rc)
      use MAPL_ConfigMod
      use MAPL_Constants, only: PI => MAPL_PI_R8
      class (EASEGridFactory), intent(inout)  :: this
      type (ESMF_DistGrid), intent(in) :: dist_grid
      type (ESMF_LocalArray), intent(in) :: lon_array
      type (ESMF_LocalArray), intent(in) :: lat_array
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: dim_count, tile_count
      integer, allocatable :: max_index(:,:)
      integer :: status
      character(len=2) :: pole ,dateline
      character(len=:), allocatable :: grid_name
     
      type (ESMF_Config) :: config
      type (ESMF_VM) :: vm
      integer :: nPet
      real(kind=REAL32), pointer :: lon(:)
      real(kind=REAL32), pointer :: lat(:)
      integer :: nx_guess,nx,ny
      integer :: i

      real, parameter :: tiny = 1.e-4

      _UNUSED_DUMMY(unusable)

      this%is_evenspaced = .false.
      call ESMF_DistGridGet(dist_grid, dimCount=dim_count, tileCount=tile_count)
      allocate(max_index(dim_count, tile_count))
      call ESMF_DistGridGet(dist_grid, maxindexPTile=max_index)

      config = MAPL_ConfigCreate(_RC)
      call MAPL_ConfigSetAttribute(config, max_index(1,1), 'IM_WORLD:', _RC)
      call MAPL_ConfigSetAttribute(config, max_index(2,1), 'JM_WORLD:', _RC)
      call MAPL_ConfigSetAttribute(config, max_index(3,1), 'LM:', _RC)

      grid_name = get_ease_gridname_by_cols(max_index(1,1))
      call MAPL_ConfigSetAttribute(config, grid_name, 'GRIDNAME:', _RC)

      lon => null()
      lat => null()
      call ESMF_LocalArrayGet(lon_array, farrayPtr=lon, _RC)
      call ESMF_LocalArrayGet(lat_array, farrayPtr=lat, _RC)

      pole = 'XY'
      dateline = 'DE'

      call MAPL_ConfigSetAttribute(config, pole, 'POLE:')
      call MAPL_ConfigSetAttribute(config, dateline, 'DATELINE:')

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
      class (EASEGridFactory), intent(in) :: this
      class (AbstractGridFactory), intent(in) :: a
      logical :: equal

      select type (a)
         class default
         equal = .false.
         return
      class is (EASEGridFactory)
         equal = .true.


         equal = size(a%ims)==size(this%ims) .and. size(a%jms)==size(this%jms)
         if (.not. equal) return

         ! same decomposition
         equal = all(a%ims == this%ims) .and. all(a%jms == this%jms)
         if (.not. equal) return

      end select

   end function decomps_are_equal


   function physical_params_are_equal(this, a) result(equal)
      class (EASEGridFactory), intent(in) :: this
      class (AbstractGridFactory), intent(in) :: a
      logical :: equal

      select type (a)
         class default
         equal = .false.
         return
      class is (EASEGridFactory)
         equal = .true.

         equal = (a%im_world == this%im_world) .and. (a%jm_world == this%jm_world)
         if (.not. equal) return

         equal = (a%is_evenspaced .eqv. this%is_evenspaced)
         if (.not. equal) return

         if (a%is_evenspaced) then
            equal = (a%pole == this%pole)
            if (.not. equal) return

            equal = (a%dateline == this%dateline)
            if (.not. equal) return

            if (a%pole == 'XY') then
               equal = (a%lat_range == this%lat_range)
               if (.not. equal) return
            end if

            if (a%dateline == 'XY') then
               equal = (a%lon_range == this%lon_range)
               if (.not. equal) return
            end if
         else
            equal = &
                 & all(a%lon_centers == this%lon_centers) .and. &
                 & all(a%lon_corners == this%lon_corners) .and. &
                 & all(a%lat_centers == this%lat_centers) .and. &
                 & all(a%lat_corners == this%lat_corners)
         end if
      end select

   end function physical_params_are_equal

   logical function equals(a, b)
      class (EASEGridFactory), intent(in) :: a
      class (AbstractGridFactory), intent(in) :: b

      select type (b)
         class default
         equals = .false.
         return
      class is (EASEGridFactory)
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
      class (EASEGridFactory), intent(in) :: this
      
      name = get_ease_gridname_by_cols(this%im_world)
   
   end function generate_grid_name

   function check_decomposition(this,unusable,rc) result(can_decomp)
      class (EASEGridFactory), target, intent(inout) :: this
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
      class (EASEGridFactory), target, intent(inout) :: this
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
      class (EASEGridFactory), target, intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type (ESMF_Grid), pointer :: grid
      integer :: dim_count
      integer :: pet
      integer :: ndes
      type (ESMF_DistGrid) :: dist_grid
      type (ESMF_VM) :: vm

      integer :: status

      _UNUSED_DUMMY(unusable)

      if (this%is_halo_initialized) return

      grid => this%get_grid(_RC)

      call ESMF_GridGet(grid,   distGrid=dist_grid, dimCount=dim_count, _RC)
      call ESMF_DistGridGet(dist_grid, delayout=this%layout, _RC)
      call ESMF_DELayoutGet (this%layout, vm=vm, _RC)

      call ESMF_VMGet(vm, localPet=pet, petCount=ndes, _RC)

      this%px = mod(pet, this%nx)
      this%py = pet / this%nx

      this%is_halo_initialized = .true.

      _RETURN(_SUCCESS)

   end subroutine init_halo


   subroutine halo(this, array, unusable, halo_width, rc)
      use MAPL_CommsMod
      class (EASEGridFactory), intent(inout) :: this
      real(kind=REAL32), intent(inout) :: array(:,:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: halo_width
      integer, optional, intent(out) :: rc

      integer :: status

      integer :: pet_north
      integer :: pet_south
      integer :: pet_east
      integer :: pet_west
      integer :: pet_N_E, pet_N_W, pet_S_E, pet_S_W
      integer :: last_lon, last_lat

      _UNUSED_DUMMY(unusable)
      ! not yet implmented, default is 1
      _UNUSED_DUMMY(halo_width)

      if (.not. this%is_halo_initialized) then
         call this%init_halo(_RC)
      end if

      last_lon = size(array,1)
      last_lat = size(array,2)

      associate (nx => this%nx, ny => this% ny, px => this%px, py => this%py)
        ! Nearest neighbors processor' ids
        pet_north = get_pet(px, py+1, nx, ny)
        pet_south = get_pet(px, py-1, nx, ny)
        pet_east  = get_pet(px+1, py, nx, ny)
        pet_west  = get_pet(px-1, py, nx, ny)

        call fill_north(array, _RC)
        call fill_south(array, _RC)

        call fill_east(array, _RC)
        call fill_west(array, _RC)

        pet_N_E   = get_pet(px+1, py+1, nx, ny)
        pet_N_W   = get_pet(px-1, py+1, nx, ny)
        pet_S_E   = get_pet(px+1, py-1, nx, ny)
        pet_S_W   = get_pet(px-1, py-1, nx, ny)

        !fill north east
        call MAPL_CommsSendRecv(this%layout,              &
              array(2,      2         ), 1,  pet_S_W,  &
              array(last_lon,last_lat ), 1,  pet_N_E,  &
              _RC)

        !fill north west
        call MAPL_CommsSendRecv(this%layout,              &
              array(last_lon-1, 2), 1,  pet_S_E,  &
              array(1,   last_lat), 1,  pet_N_W,  &
              _RC)

        ! north pol corner
        if(this%py== this%ny-1) then
           array(last_lon,last_lat) = array(last_lon-1,last_lat-1)
           array(1,last_lat)        = array(2,last_lat-1)
        endif

        !fill south east
        call MAPL_CommsSendRecv(this%layout,              &
              array(2, last_lat-1), 1,  pet_N_W,  &
              array(last_lon,1),    1,  pet_S_E,  &
              _RC)

        !fill south west
        call MAPL_CommsSendRecv(this%layout,              &
              array(last_lon-1,last_lat-1), 1,  pet_N_E,  &
              array(1,1                  ), 1,  pet_S_W,  &
              _RC)

        ! south pole corner
        if(this%py==0) then
           array(last_lon,1   ) = array(last_lon-1,2 )
           array(1,1   )        = array(2,2 )
        endif

      end associate

      _RETURN(ESMF_SUCCESS)

   contains

      integer function get_pet(px, py, nx, ny) result(pet)
         integer, intent(in) :: px, py  ! rank in x/y directions
         integer, intent(in) :: nx, ny  ! npets in x/y directions

         pet = mod(px+nx,nx) + nx*mod(py+ny,ny)

      end function get_pet


      subroutine fill_north(array, rc)
         real(kind=REAL32), intent(inout) :: array(:,:)
         integer, optional, intent(out) :: rc

         integer :: status

         integer :: len, last

         last = size(array,2)-1
         len = size(array,1)

         call MAPL_CommsSendRecv(this%layout,        &
              array(:,2        ),  len,  pet_south,  &
              array(:,last+1   ),  len,  pet_north,  &
              _RC)
         if(this%py==this%ny-1) then
            array(:,last+1   ) = array(:,last )
         end if

         _RETURN(_SUCCESS)

      end subroutine fill_north


      subroutine fill_south(array, rc)
         real(kind=REAL32), intent(inout) :: array(:,:)
         integer, optional, intent(out) :: rc

         integer :: status

         integer :: len, last

         last = size(array,2)-1
         len = size(array,1)

         call MAPL_CommsSendRecv(this%layout,     &
              array(:,last     ),  len,  pet_north,  &
              array(:,1        ),  len,  pet_south,  &
              _RC)

         if(this%py==0) then
            array(:,1   ) = array(:,2 )
         endif

         _RETURN(_SUCCESS)

      end subroutine fill_south


      subroutine fill_east(array, rc)
         real(kind=REAL32), intent(inout) :: array(:,:)
         integer, optional, intent(out) :: rc

         integer :: status

         integer :: len, last

         last = size(array,1)-1
         len = size(array,2)

         call MAPL_CommsSendRecv(this%layout,      &
              array(2     , : ),  len,  pet_west,  &
              array(last+1, : ),  len,  pet_east,  &
              _RC)

         _RETURN(_SUCCESS)

      end subroutine fill_east


      subroutine fill_west(array, rc)
         real(kind=REAL32), intent(inout) :: array(:,:)
         integer, optional, intent(out) :: rc

         integer :: status

         integer :: len, last

         last = size(array,1)-1
         len = size(array,2)

         call MAPL_CommsSendRecv(this%layout,   &
              array(last  , : ),  len,  pet_east,  &
              array(1     , : ),  len,  pet_west,  &
              _RC)

         _RETURN(_SUCCESS)

      end subroutine fill_west


   end subroutine halo


   subroutine append_metadata(this, metadata)
      use MAPL_Constants
      class (EASEGridFactory), intent(inout) :: this
      type (FileMetadata), intent(inout) :: metadata

      type (Variable) :: v
      real(kind=REAL64), allocatable :: temp_coords(:)

      ! Horizontal grid dimensions
      call metadata%add_dimension('lon', this%im_world)
      call metadata%add_dimension('lat', this%jm_world)

      ! Coordinate variables
      v = Variable(type=PFIO_REAL64, dimensions='lon')
      call v%add_attribute('long_name', 'longitude')
      call v%add_attribute('units', 'degrees_east')
      temp_coords = this%get_longitudes_degrees()
      call v%add_const_value(UnlimitedEntity(temp_coords))
      call metadata%add_variable('lon', v)
      deallocate(temp_coords)

      v = Variable(type=PFIO_REAL64, dimensions='lat')
      call v%add_attribute('long_name', 'latitude')
      call v%add_attribute('units', 'degrees_north')
      temp_coords=this%get_latitudes_degrees()
      call v%add_const_value(UnlimitedEntity(temp_coords))
      call metadata%add_variable('lat', v)

   end subroutine append_metadata

   function get_grid_vars(this) result(vars)
      class (EASEGridFactory), intent(inout) :: this

      character(len=:), allocatable :: vars
      _UNUSED_DUMMY(this)

      vars = 'lon,lat'

   end function get_grid_vars

   function get_file_format_vars(this) result(vars)
      class (EASEGridFactory), intent(inout) :: this

      character(len=:), allocatable :: vars
      integer :: i
      _UNUSED_DUMMY(this)

      vars = 'lon,lat'
      if (allocated(this%lon_bounds_name)) then
         vars = vars // ',' // this%lon_bounds_name
      end if
      if (allocated(this%lat_bounds_name)) then
         vars = vars // ',' // this%lat_bounds_name
      end if

   end function get_file_format_vars

   subroutine append_variable_metadata(this,var)
      class (EASEGridFactory), intent(inout) :: this
      type(Variable), intent(inout) :: var
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(var)
   end subroutine append_variable_metadata

   subroutine generate_file_bounds(this,grid,local_start,global_start,global_count,metadata,rc)
      use MAPL_BaseMod
      class(EASEGridFactory), intent(inout) :: this
      type(ESMF_Grid),      intent(inout) :: grid
      integer, allocatable, intent(out) :: local_start(:)
      integer, allocatable, intent(out) :: global_start(:)
      integer, allocatable, intent(out) :: global_count(:)
      type(FileMetaData), intent(in), optional :: metaData
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: global_dim(3), i1,j1,in,jn

      call MAPL_GridGet(grid,globalCellCountPerDim=global_dim,_RC)
      call MAPL_GridGetInterior(grid,i1,in,j1,jn)
      allocate(local_start,source=[i1,j1])
      allocate(global_start,source=[1,1])
      allocate(global_count,source=[global_dim(1),global_dim(2)])

      _RETURN(_SUCCESS)

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(metadata)
   end subroutine generate_file_bounds

   subroutine generate_file_corner_bounds(this,grid,local_start,global_start,global_count,rc)
      use esmf
      class (EASEGridFactory), intent(inout) :: this
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
      class(EASEGridFactory), intent(inout) :: this
      real, pointer, intent(in) :: fpointer(:,:)
      ref = ArrayReference(fpointer)
      _UNUSED_DUMMY(this)
   end function generate_file_reference2D

   function generate_file_reference3D(this,fpointer,metaData) result(ref)
      use pFIO
      type(ArrayReference) :: ref
      class(EASEGridFactory), intent(inout) :: this
      real, pointer, intent(in) :: fpointer(:,:,:)
      type(FileMetaData), intent(in), optional :: metaData
      ref = ArrayReference(fpointer)

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(metaData)
   end function generate_file_reference3D

   function coordinate_has_bounds(metadata, coord_name, rc) result(has_bounds)
      logical :: has_bounds
      type(FileMetadata), intent(in) :: metadata
      character(len=*), intent(in) :: coord_name
      integer, optional, intent(out) :: rc
      
      type(Variable), pointer :: var
      integer :: status

      var => metadata%get_variable(coord_name, _RC)
      has_bounds = var%is_attribute_present("bounds")

      _RETURN(_SUCCESS)
   end function

   function get_coordinate_bounds_name(metadata, coord_name, rc) result(coord_bounds_name)
      character(len=:), allocatable :: coord_bounds_name
      type(FileMetadata), intent(in) :: metadata
      character(len=*), intent(in) :: coord_name
      integer, optional, intent(out) :: rc
      
      type(Variable), pointer :: var
      type(Attribute), pointer :: attr
      integer :: status
      class(*), pointer :: attr_val

      var => metadata%get_variable(coord_name, _RC)
      attr => var%get_attribute("bounds", _RC)
      attr_val => attr%get_value()
      select type(attr_val)
      type is(character(*))
         coord_bounds_name = attr_val
      class default
         _FAIL('coordinate bounds must be a string')
      end select
      _RETURN(_SUCCESS)
   end function

   function get_coordinate_bounds(metadata, coord_name, rc) result(coord_bounds)
      real(kind=REAL64), allocatable :: coord_bounds(:)
      type(FileMetadata), intent(in) :: metadata
      character(len=*), intent(in) :: coord_name
      integer, optional, intent(out) :: rc
      
      type(Variable), pointer :: var
      type(Attribute), pointer :: attr
      integer :: status, im, i
      class(*), pointer :: attr_val
      character(len=:), allocatable :: bnds_name, source_file
      real(kind=REAL64), allocatable :: file_bounds(:,:)
      type(NetCDF4_FileFormatter) :: file_formatter
      

      var => metadata%get_variable(coord_name, _RC)
      attr => var%get_attribute("bounds", _RC)
      attr_val => attr%get_value()
      select type(attr_val)
      type is(character(*))
         bnds_name = attr_val
      class default
         _FAIL('coordinate bounds must be a string')
      end select
      im = metadata%get_dimension(coord_name, _RC)
      allocate(coord_bounds(im+1), _STAT)
      allocate(file_bounds(2,im), _STAT)
      source_file = metadata%get_source_file() 

      call file_formatter%open(source_file, PFIO_READ, _RC)
      call file_formatter%get_var(bnds_name, file_bounds, _RC)
      call file_formatter%close(_RC)
      do i=1,im-1
         _ASSERT(file_bounds(2,i)==file_bounds(1,i+1), "Bounds are not contiguous in file")
      enddo
      do i=1,im
         coord_bounds(i) = file_bounds(1,i)
         coord_bounds(i+1) = file_bounds(2,i)
      enddo

      _RETURN(_SUCCESS)
   end function

end module MAPL_EASEGridFactoryMod
