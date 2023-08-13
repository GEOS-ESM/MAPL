#include "MAPL_ErrLog.h"

! overload set interfaces in legacy
! Document PE, PC, DC, DE, GC

! This module generates ESMF_Grids corresponding to _regular_ lat-lon coordinate grids.
! I.e., spacing between lats (lons) is constant.

module mapl3g_LatLonGeomFactory
   use mapl3g_GeomFactory
   use mapl_MinMaxMod
   use mapl_KeywordEnforcerMod
   use mapl_ErrorHandlingMod
   use mapl_Constants

   use mapl3g_GeomCoordinates1D
   use mapl3g_GeomDecomposition2D
   
   use esmf
   use pFIO
!#   use MAPL_CommsMod
   use, intrinsic :: iso_fortran_env, only: REAL32
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none
   private

   public :: LatLonGeomSpec

   integer, parameter :: NUM_DIM = 2

!   Note that LatLonGeomSpec (type and type constructor) are _private_.
!   This may be relaxed if we want for testing.
   type, extends(GeomSpec) :: LatLonGeomSpec
      private
      character(len=:), allocatable :: name

      logical :: force_decomposition = .false.
      type(GeomResolution2D) :: resolution
      type(GeomCoordinates1D) :: coordinates
      type(GeomDecomposition2D) :: decomposition

      ! Grid conventions:
      character(len=:), allocatable :: pole
      character(len=:), allocatable :: dateline
      ! Regional vs global:
      type (RealMinMax) :: lon_range = RealMinMax(MAPL_UNDEFINED_REAL,MAPL_UNDEFINED_REAL)
      type (RealMinMax) :: lat_range = RealMinMax(MAPL_UNDEFINED_REAL,MAPL_UNDEFINED_REAL)
   contains
      procedure :: equal_to
   end type LatLonGeomSpec


   interface LatLonGeomSpec
      module procedure new_LatLonGeomSpec_from_hconfig
      module procedure new_LatLonGeomSpec_from_metadata
   end interface LatLonGeomSpec

   interface get
      procedure get_integer
      procedure get_string
   end interface get


  interface set_with_default
      module procedure set_with_default_integer
      module procedure set_with_default_real
      module procedure set_with_default_character
      module procedure set_with_default_range
      module procedure set_with_default_logical
   end interface set_with_default


contains

   subroutine new_LatLonGeomSpec_from_hconfig(this, hconfig, unusable, rc)
      use esmf
      class (LatLonGridFactory), intent(inout) :: this
      type (ESMF_HConfig), intent(inout) :: hconfig
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_VM) :: VM


      call ESMF_VmGetCurrent(VM, _RC)

      this%is_regular = .true.

      spec%name = get(hconfig, 'name', default=MAPL_GRID_NAME_DEFAULT, _RC)

      spec%decomposition = GeomDecomposition2D(hconfig, _RC)
      
      
      call ESMF_ConfigGetAttribute(config, this%nx, label=prefix//'NX:', default=MAPL_UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%ny, label=prefix//'NY:', default=MAPL_UNDEFINED_INTEGER)

      call ESMF_ConfigGetAttribute(config, this%im_world, label=prefix//'IM_WORLD:', default=MAPL_UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%jm_world, label=prefix//'JM_WORLD:', default=MAPL_UNDEFINED_INTEGER)

      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'IMS_FILE:', rc=status)
      if ( status == _SUCCESS ) then
         call get_ims_from_file(this%ims, trim(tmp),this%nx, _RC)
      else
         call get_multi_integer(this%ims, 'IMS:', rc=status)
         _VERIFY(status)
      endif
      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'JMS_FILE:', rc=status)
      if ( status == _SUCCESS ) then
         call get_ims_from_file(this%jms, trim(tmp),this%ny, rc=status)
         _VERIFY(status)
      else
         call get_multi_integer(this%jms, 'JMS:', rc=status)
         _VERIFY(status)
      endif

      call ESMF_ConfigGetAttribute(config, this%lm, label=prefix//'LM:', default=MAPL_UNDEFINED_INTEGER)

      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'POLE:', default=MAPL_UNDEFINED_CHAR, rc=status)
      if (status == _SUCCESS) then
         this%pole = trim(tmp)
      end if
      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'DATELINE:', default=MAPL_UNDEFINED_CHAR, rc=status)
      if (status == _SUCCESS) then
         this%dateline = trim(tmp)
      end if

      call get_range(this%lon_range, 'LON_RANGE:', _RC)
      call get_range(this%lat_range, 'LAT_RANGE:', _RC)
      call this%check_and_fill_consistency(_RC)

      ! Compute the centers and corners
      this%lon_centers = this%compute_lon_centers(this%dateline, _RC)
      this%lon_centers_degrees = this%compute_lon_centers(this%dateline, convert_to_radians = .false., _RC)
      this%lat_centers = this%compute_lat_centers(this%pole, _RC)
      this%lat_centers_degrees = this%compute_lat_centers(this%pole, &
               convert_to_radians = .false., _RC)
      this%lon_corners = this%compute_lon_corners(this%dateline, _RC)
      this%lat_corners = this%compute_lat_corners(this%pole, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine new_LatLonGeomSpec_from_hconfig

 


   function LatLonGeomFactory_from_parameters(unusable, grid_name, &
        & im_world, jm_world, lm, nx, ny, ims, jms, &
        & pole, dateline, lon_range, lat_range, force_decomposition, rc) result(factory)
      type (LatLonGeomFactory) :: factory
      class (KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: grid_name

      ! grid details:
      integer, optional, intent(in) :: im_world
      integer, optional, intent(in) :: jm_world
      integer, optional, intent(in) :: lm
      character(len=2), optional, intent(in) :: pole
      character(len=2), optional, intent(in) :: dateline
      type (RealMinMax), optional, intent(in) :: lon_range
      type (RealMinMax), optional, intent(in) :: lat_range

      ! decomposition:
      integer, optional, intent(in) :: nx
      integer, optional, intent(in) :: ny
      integer, optional, intent(in) :: ims(:)
      integer, optional, intent(in) :: jms(:)
      logical, optional, intent(in) :: force_decomposition

      integer, optional, intent(out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      factory%is_regular = .true.
      call set_with_default(factory%grid_name, grid_name, MAPL_GRID_NAME_DEFAULT)

      call set_with_default(factory%nx, nx, MAPL_UNDEFINED_INTEGER)
      call set_with_default(factory%ny, ny, MAPL_UNDEFINED_INTEGER)

      call set_with_default(factory%im_world, im_world, MAPL_UNDEFINED_INTEGER)
      call set_with_default(factory%jm_world, jm_world, MAPL_UNDEFINED_INTEGER)
      call set_with_default(factory%lm, lm, MAPL_UNDEFINED_INTEGER)

      ! default is unallocated
      if (present(ims)) factory%ims = ims
      if (present(jms)) factory%jms = jms

      call set_with_default(factory%pole, pole, MAPL_UNDEFINED_CHAR)
      call set_with_default(factory%dateline, dateline, MAPL_UNDEFINED_CHAR)

      call set_with_default(factory%lon_range, lon_range, RealMinMax(MAPL_UNDEFINED_REAL,MAPL_UNDEFINED_REAL))
      call set_with_default(factory%lat_range, lat_range, RealMinMax(MAPL_UNDEFINED_REAL,MAPL_UNDEFINED_REAL))
      call set_with_default(factory%force_decomposition, force_decomposition, .false.)

      call factory%check_and_fill_consistency(rc=status)
      _VERIFY(status)

      ! Compute the centers and corners
      factory%lon_centers = factory%compute_lon_centers(factory%dateline, rc=status)
      _VERIFY(status)
      factory%lat_centers = factory%compute_lat_centers(factory%pole, rc=status)
      _VERIFY(status)
      factory%lon_centers_degrees = factory%compute_lon_centers(factory%dateline, &
            convert_to_radians = .false.,  rc=status)
      _VERIFY(status)
      factory%lat_centers_degrees = factory%compute_lat_centers(factory%pole, &
            convert_to_radians = .false.,  rc=status)
      _VERIFY(status)
      factory%lon_corners = factory%compute_lon_corners(factory%dateline, rc=status)
      _VERIFY(status)
      factory%lat_corners = factory%compute_lat_corners(factory%pole, rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)

   end function LatLonGeomFactory_from_parameters


   function make_new_grid(this, unusable, rc) result(grid)
      type (ESMF_Grid) :: grid
      class (LatLonGeomFactory), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)
      grid = this%create_basic_grid(rc=status)
      _VERIFY(status)

      call this%add_horz_coordinates(grid, rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)

   end function make_new_grid



   function create_basic_grid(this, unusable, rc) result(grid)
      type (ESMF_Grid) :: grid
      class (LatLonGeomFactory), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ESMF_Info) :: infoh
      integer :: status

      _UNUSED_DUMMY(unusable)

      if (this%periodic) then
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
              & rc=status)
         _VERIFY(status)
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
              & rc=status)
         _VERIFY(status)
      end if

      ! Allocate coords at default stagger location
      call ESMF_GridAddCoord(grid, rc=status)
      _VERIFY(status)
      call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=status)
      _VERIFY(status)

      call ESMF_InfoGetFromHost(grid,infoh,rc=status)
      _VERIFY(status)
      if (this%lm /= MAPL_UNDEFINED_INTEGER) then
         call ESMF_InfoSet(infoh,'GRID_LM',this%lm,rc=status)
         _VERIFY(status)
      end if

      call ESMF_InfoSet(infoh,'GridType','LatLon',rc=status)
      _VERIFY(status)
      if (.not.this%periodic) then
         call ESMF_InfoSet(infoh,key='Global',value=.false.,rc=status)
         _VERIFY(status)
      end if

      _RETURN(_SUCCESS)
   end function create_basic_grid

   ! in radians
   function get_longitudes(this, unusable, rc) result(longitudes)
      use MAPL_BaseMod
      class (LatLonGeomFactory), intent(in) :: this
      real(kind=REAL64), allocatable :: longitudes(:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)

      longitudes = this%lon_centers
      _RETURN(_SUCCESS)
   end function get_longitudes

   function get_longitudes_degrees(this, unusable, rc) result(longitudes)
      use MAPL_BaseMod
      class (LatLonGeomFactory), intent(in) :: this
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
      class (LatLonGeomFactory), intent(in) :: this
      real(kind=REAL64), allocatable :: latitudes(:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)

      latitudes = this%lat_centers
      _RETURN(_SUCCESS)
   end function get_latitudes

   function get_latitudes_degrees(this, unusable, rc) result(latitudes)
      use MAPL_BaseMod
      class (LatLonGeomFactory), intent(in) :: this
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
      class (LatLonGeomFactory), intent(in) :: this
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
         delta = (this%lon_range%max - this%lon_range%min) / this%im_world
         min_coord = this%lon_range%min + delta/2
         max_coord = this%lon_range%max - delta/2
      else
         delta = 360.d0 / this%im_world
         select case (dateline)
         case ('DC')
            min_coord = -180.d0
            max_coord = +180.d0 - delta
         case ('DE')
            min_coord = -180.d0 + delta/2
            max_coord = +180.d0 - delta/2
         case ('GC')
            min_coord = 0.d0
            max_coord = 360.d0 - delta
         case ('GE')
            min_coord = delta/2
            max_coord = 360.d0 - delta/2
         end select
      end if

      if (local_convert_to_radians) then
         lon_centers = MAPL_Range(min_coord, max_coord, this%im_world, &
              & conversion_factor=MAPL_DEGREES_TO_RADIANS_R8, rc=status)
         _VERIFY(status)
      else
         lon_centers = MAPL_Range(min_coord, max_coord, this%im_world, rc=status)
         _VERIFY(status)
      end if

      _RETURN(_SUCCESS)
   end function compute_lon_centers

   function compute_lon_corners(this, dateline, unusable, rc) result(lon_corners)
      use MAPL_Constants, only:MAPL_DEGREES_TO_RADIANS_R8
      use MAPL_BaseMod
      real(kind=REAL64), allocatable :: lon_corners(:)
      class (LatLonGeomFactory), intent(in) :: this
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
         delta = (this%lon_range%max - this%lon_range%min) / this%im_world
         min_coord = this%lon_range%min
         max_coord = this%lon_range%max
      else
         delta = 360.d0 / this%im_world
         select case (dateline)
         case ('DC')
            min_coord = -180.d0 - delta/2
            max_coord = +180.d0 - delta/2
         case ('DE')
            min_coord = -180.d0
            max_coord = +180.d0
         case ('GC')
            min_coord = 0.d0-delta/2
            max_coord = 360.d0-delta/2
         case ('GE')
            min_coord = 0.d0
            max_coord = 360.d0 - delta
         end select
      end if

      lon_corners = MAPL_Range(min_coord, max_coord, this%im_world+1, &
           & conversion_factor=MAPL_DEGREES_TO_RADIANS_R8, rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end function compute_lon_corners


   ! in radians
   function get_lon_corners(this, unusable, rc) result(lon_corners)
      use MAPL_BaseMod
      class (LatLonGeomFactory), intent(in) :: this
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
      class (LatLonGeomFactory), intent(in) :: this
      real(kind=REAL64), allocatable :: lat_corners(:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)

      lat_corners = this%lat_corners
      _RETURN(_SUCCESS)

   end function get_lat_corners


   function compute_lat_centers(this, pole, unusable, convert_to_radians, rc) result(lat_centers)
      use MAPL_Constants, only: MAPL_DEGREES_TO_RADIANS_R8
      use MAPL_BaseMod
      real(kind=REAL64), allocatable :: lat_centers(:)
      class (LatLonGeomFactory), intent(in) :: this
      character(2), intent(in) :: pole
      class (KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in)  :: convert_to_radians
      integer, optional, intent(out) :: rc

      real(kind=REAL64) :: delta, min_coord, max_coord
      logical :: regional
      logical :: local_convert_to_radians
      integer :: status

      _UNUSED_DUMMY(unusable)
      if (present(convert_to_radians)) then
         local_convert_to_radians = convert_to_radians
      else
         local_convert_to_radians = .true.
      end if

      allocate(lat_centers(this%jm_world))

      regional  = (pole == 'XY')
      if (regional) then
         delta = (this%lat_range%max - this%lat_range%min) / this%jm_world
         min_coord = this%lat_range%min + delta/2
         max_coord = this%lat_range%max - delta/2
      else ! global grid

         select case (pole)
         case ('PE')
            delta = 180.d0 / this%jm_world
            min_coord = -90.d0 + delta/2
            max_coord = +90.d0 - delta/2
         case ('PC')
            _ASSERT(this%jm_world > 1,'degenerate grid')
            min_coord = -90.d0
            max_coord = +90.d0
         end select
      end if

      if (local_convert_to_radians) then
         lat_centers = MAPL_Range(min_coord, max_coord, this%jm_world, &
              & conversion_factor=MAPL_DEGREES_TO_RADIANS_R8, rc=status)
      else
         lat_centers = MAPL_Range(min_coord, max_coord, this%jm_world, rc=status)
      end if

      _RETURN(_SUCCESS)

   end function compute_lat_centers

   function compute_lat_corners(this, pole, unusable, rc) result(lat_corners)
      use MAPL_Constants, only: MAPL_DEGREES_TO_RADIANS_R8
      use MAPL_BaseMod
      real(kind=REAL64), allocatable :: lat_corners(:)
      class (LatLonGeomFactory), intent(in) :: this
      character(2), intent(in) :: pole
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      real(kind=REAL64) :: delta, min_coord, max_coord
      logical :: regional

      integer :: status

      _UNUSED_DUMMY(unusable)

      allocate(lat_corners(this%jm_world+1))

      regional  = (pole == 'XY')
      if (regional) then
         delta = (this%lat_range%max - this%lat_range%min) / this%jm_world
         min_coord = this%lat_range%min
         max_coord = this%lat_range%max
      else ! global grid

         select case (pole)
         case ('PE')
            delta = 180.d0 / this%jm_world
            min_coord = -90.d0
            max_coord = +90.d0
         case ('PC')
            _ASSERT(this%jm_world > 1, 'degenerate grid')
            delta = 180.d0 / (this%jm_world-1)
            min_coord = -90.d0-delta/2
            max_coord = +90.d0+delta/2
         end select
      end if

      lat_corners = MAPL_Range(min_coord, max_coord, this%jm_world+1, &
           & conversion_factor=MAPL_DEGREES_TO_RADIANS_R8, rc=status)
      if (pole == 'PC') then
         lat_corners(1)=-90.d0*MAPL_DEGREES_TO_RADIANS_R8
         lat_corners(this%jm_world+1)=90.d0*MAPL_DEGREES_TO_RADIANS_R8
      end if

      _RETURN(_SUCCESS)

   end function compute_lat_corners


   subroutine add_horz_coordinates(this, grid, unusable, rc)
      use MAPL_BaseMod, only: MAPL_grid_interior
      class (LatLonGeomFactory), intent(in) :: this
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
              farrayPtr=centers, rc=status)
         _VERIFY(status)
         call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
              staggerloc=ESMF_STAGGERLOC_CORNER, &
              farrayPtr=corners, rc=status)
         _VERIFY(status)
         do j = 1, size(centers,2)
            centers(:,j) = this%lon_centers(i_1:i_n)
         end do
         do j = 1, size(corners,2)
            corners(:,j) = this%lon_corners(ic_1:ic_n)
         end do

         ! Now latitudes
         call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
              staggerloc=ESMF_STAGGERLOC_CENTER, &
              farrayPtr=centers, rc=status)
         _VERIFY(status)
         call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
              staggerloc=ESMF_STAGGERLOC_CORNER, &
              farrayPtr=corners, rc=status)
         _VERIFY(status)

         do i = 1, size(centers,1)
            centers(i,:) = this%lat_centers(j_1:j_n)
         end do
         do i = 1, size(corners,1)
            corners(i,:) = this%lat_corners(jc_1:jc_n)
         end do
      end if

      _RETURN(_SUCCESS)

   end subroutine add_horz_coordinates
   
   ! TODO: check radians vs degrees.  Assume degrees for now.

   function new_LatLonGeomSpec_from_metadata(file_metadata, supports, rc) result(spec)
      use mapl_KeywordEnforcerMod
      use mapl_BaseMod, only: MAPL_DecomposeDim
      class(GeomSpec), allocatable :: spec
      type (FileMetadata), target, intent(in) :: file_metadata
      logical, optional, intent(in) :: supports
      integer, optional, intent(out) :: rc

      integer :: status


      integer :: i
      logical :: hasLon, hasLat, hasLongitude, hasLatitude, hasLev,hasLevel,regLat,regLon
      real(kind=REAL64) :: del12,delij

      integer :: i_min, i_max
      real(kind=REAL64) :: d_lat, d_lat_temp, extrap_lat
      logical :: is_valid, use_file_coords, compute_lons, compute_lats

      character(:), allocatable :: lon_name, lat_name


      ! Cannot assume that lats and lons are evenly spaced
      spec%is_regular = .false.

      associate (im => spec%im_world, jm => spec%jm_world, lm => spec%lm)
        lon_name = find_dim_name(file_metadata, 'lon', 'longitude', _RC)
        lat_name = find_dim_name(file_metadata, 'lat', 'latitude', _RC)

        im = file_metadata%get_dimension(lon_name, _RC)
        jm = file_metadata%get_dimension(lat_name, _RC)

        spec%lon_centers = get_coordinates(file_metadata, lon_name, _RC)
        spec%lat_centers = get_coordinates(file_metadata, lat_name, _RC)

        ! Enforce lon range (-180,180)
        if (any((spec%lon_centers(2:im)-spec%lon_centers(1:im-1))<0)) then
           where(spec%lon_centers > 180) spec%lon_centers=spec%lon_centers-360
        end if
      end associate

      ! Check: is spec a "mis-specified" pole-centered grid?
      if (size(spec%lat_centers) >= 4) then
         ! Assume lbound=1 and ubound=size for now
         i_min = 1 !lbound(spec%lat_centers)
         i_max = size(spec%lat_centers) !ubound(spec%lat_centers)
         d_lat = (spec%lat_centers(i_max-1) - spec%lat_centers(i_min+1))/&
              (size(spec%lat_centers)-3)
         is_valid = .True.
         ! Check: is spec a regular grid (i.e. constant spacing away from the poles)?
         do i=(i_min+1),(i_max-2)
            d_lat_temp = spec%lat_centers(i+1) - spec%lat_centers(i)
            is_valid = (is_valid.and.(abs((d_lat_temp/d_lat)-1.0) < 1.0e-5))
            if (.not. is_valid) then
               exit
            end if
         end do
         if (is_valid) then
            ! Should the southernmost point actually be at the pole?
            extrap_lat = spec%lat_centers(i_min+1) - d_lat
            if (extrap_lat <= ((d_lat/20.0)-90.0)) then
               spec%lat_centers(i_min) = -90.0
            end if
            ! Should the northernmost point actually be at the pole?
            extrap_lat = spec%lat_centers(i_max-1) + d_lat
            if (extrap_lat >= (90.0-(d_lat/20.0))) then
               spec%lat_centers(i_max) =  90.0
            end if
         end if
      end if


      call derive_corners_and_staggering(spec, _RC)

      ! check if evenly spaced
      regLon = .true.
      do i = 2, size(spec%lon_centers)
         del12=spec%lon_centers(2)-spec%lon_centers(1)
         delij=spec%lon_centers(i)-spec%lon_centers(i-1)
         if ((del12-delij)>epsilon(1.0)) regLon=.false.
      end do
      regLat=.true.
      do i = 2, size(spec%lat_centers)
         del12=spec%lat_centers(2)-spec%lat_centers(1)
         delij=spec%lat_centers(i)-spec%lat_centers(i-1)
         if ((del12-delij) > epsilon(1.0)) regLat = .false.
      end do
      spec%is_regular = (regLat .and. regLon)

      if (use_file_coords) then
         spec%is_regular = .false.
         spec%lon_centers = MAPL_DEGREES_TO_RADIANS_R8 * spec%lon_centers
         spec%lat_centers = MAPL_DEGREES_TO_RADIANS_R8 * spec%lat_centers
         spec%lon_corners = MAPL_DEGREES_TO_RADIANS_R8 * spec%lon_corners
         spec%lat_corners = MAPL_DEGREES_TO_RADIANS_R8 * spec%lat_corners
      else
         compute_lons=.false.
         compute_lats=.false.
         if (regLon .and. (spec%dateline.ne.'XY')) then
            compute_lons=.true.
         end if
         if (regLat .and. (spec%pole.ne.'XY')) then
            compute_lats=.true.
         end if
         if (compute_lons .and. compute_lats) then
            spec%lon_centers = spec%compute_lon_centers(spec%dateline, _RC)
            spec%lon_centers_degrees = spec%compute_lon_centers(spec%dateline, &
                 convert_to_radians=.false., _RC)
            spec%lon_corners = spec%compute_lon_corners(spec%dateline, _RC)
            spec%lat_centers_degrees = spec%compute_lat_centers(spec%pole, &
                 convert_to_radians=.false., _RC)
            spec%lat_centers = spec%compute_lat_centers(spec%pole, _RC)
            spec%lat_corners = spec%compute_lat_corners(spec%pole, _RC)
         else
            spec%lon_centers_degrees = spec%lon_centers
            spec%lat_centers_degrees = spec%lat_centers
            spec%lon_centers = MAPL_DEGREES_TO_RADIANS_R8 * spec%lon_centers
            spec%lat_centers = MAPL_DEGREES_TO_RADIANS_R8 * spec%lat_centers
            spec%lon_corners = MAPL_DEGREES_TO_RADIANS_R8 * spec%lon_corners
            spec%lat_corners = MAPL_DEGREES_TO_RADIANS_R8 * spec%lat_corners
         end if
      end if

      call spec%make_arbitrary_decomposition(spec%nx, spec%ny, _RC)

      ! Determine IMS and JMS with constraint for ESMF that each DE has at least an extent
      ! of 2.  Required for ESMF_FieldRegrid().
      allocate(spec%ims(0:spec%nx-1))
      allocate(spec%jms(0:spec%ny-1))
      call MAPL_DecomposeDim(spec%im_world, spec%ims, spec%nx, min_DE_extent=2)
      call MAPL_DecomposeDim(spec%jm_world, spec%jms, spec%ny, min_DE_extent=2)
      
      call spec%check_and_fill_consistency(rc=status)
      _VERIFY(status)
      
      _RETURN(_SUCCESS)
      
      _UNUSED_DUMMY(unusable)
 
   contains

      subroutine derive_corners_and_staggering(spec, rc)
         type(LatLonGeomSpec), intent(inout) :: spec
         integer, optional, intent(out) :: rc

         integer :: status

         ! Corners are the midpoints of centers (and extrapolated at the
         ! poles for lats.)
         allocate(spec%lon_corners(im+1), spec%lat_corners(jm+1))

         spec%lon_corners(1) = (spec%lon_centers(im) + spec%lon_centers(1))/2 - 180
         spec%lon_corners(2:im) = (spec%lon_centers(1:im-1) + spec%lon_centers(2:im))/2
         spec%lon_corners(im+1) = (spec%lon_centers(im) + spec%lon_centers(1))/2 + 180

         ! Spec section about pole/dateline is probably not needed in file data case.
         if (abs(spec%lon_centers(1) + 180) < 1000*epsilon(1.0)) then
            spec%dateline = 'DC'
         else if (abs(spec%lon_centers(1)) < 1000*epsilon(1.0)) then
            spec%dateline = 'GC'
         else if (abs(spec%lon_corners(1) + 180) < 1000*epsilon(1.0)) then
            spec%dateline = 'DE'
         else if (abs(spec%lon_corners(1)) < 1000*epsilon(1.0)) then
            spec%dateline = 'GE'
         else ! assume 'XY'
            spec%dateline = 'XY'
            spec%lon_range = RealMinMax(spec%lon_centers(1), spec%lon_centers(jm))
         end if

         spec%lat_corners(1) = spec%lat_centers(1) - (spec%lat_centers(2)-spec%lat_centers(1))/2
         spec%lat_corners(2:jm) = (spec%lat_centers(1:jm-1) + spec%lat_centers(2:jm))/2
         spec%lat_corners(jm+1) = spec%lat_centers(jm) - (spec%lat_centers(jm-1)-spec%lat_centers(jm))/2
      end subroutine derive_corners_and_staggering


   end function make_geom_spec_from_metadata



   subroutine initialize_from_config_with_prefix(this, config, prefix, unusable, rc)
      use esmf
      class (LatLonGeomFactory), intent(inout) :: this
      type (ESMF_Config), intent(inout) :: config
      character(len=*), intent(in) :: prefix  ! effectively optional due to overload without this argument
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: tmp
      type(ESMF_VM) :: VM

      _UNUSED_DUMMY(unusable)

      call ESMF_VmGetCurrent(VM, rc=status)
      _VERIFY(status)

      this%is_regular = .true.
      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'GRIDNAME:', default=MAPL_GRID_NAME_DEFAULT)
      this%grid_name = trim(tmp)

      call ESMF_ConfigGetAttribute(config, this%nx, label=prefix//'NX:', default=MAPL_UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%ny, label=prefix//'NY:', default=MAPL_UNDEFINED_INTEGER)

      call ESMF_ConfigGetAttribute(config, this%im_world, label=prefix//'IM_WORLD:', default=MAPL_UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%jm_world, label=prefix//'JM_WORLD:', default=MAPL_UNDEFINED_INTEGER)

      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'IMS_FILE:', rc=status)
      if ( status == _SUCCESS ) then
         call get_ims_from_file(this%ims, trim(tmp),this%nx, rc=status)
         _VERIFY(status)
      else
         call get_multi_integer(this%ims, 'IMS:', rc=status)
         _VERIFY(status)
      endif
      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'JMS_FILE:', rc=status)
      if ( status == _SUCCESS ) then
         call get_ims_from_file(this%jms, trim(tmp),this%ny, rc=status)
         _VERIFY(status)
      else
         call get_multi_integer(this%jms, 'JMS:', rc=status)
         _VERIFY(status)
      endif

      call ESMF_ConfigGetAttribute(config, this%lm, label=prefix//'LM:', default=MAPL_UNDEFINED_INTEGER)

      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'POLE:', default=MAPL_UNDEFINED_CHAR, rc=status)
      if (status == _SUCCESS) then
         this%pole = trim(tmp)
      end if
      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'DATELINE:', default=MAPL_UNDEFINED_CHAR, rc=status)
      if (status == _SUCCESS) then
         this%dateline = trim(tmp)
      end if

      call get_range(this%lon_range, 'LON_RANGE:', rc=status); _VERIFY(status)
      call get_range(this%lat_range, 'LAT_RANGE:', rc=status); _VERIFY(status)
      call this%check_and_fill_consistency(rc=status); _VERIFY(status)

      ! Compute the centers and corners
      this%lon_centers = this%compute_lon_centers(this%dateline, rc=status)
      _VERIFY(status)
      this%lon_centers_degrees = this%compute_lon_centers(this%dateline, &
               convert_to_radians = .false., rc=status)
      _VERIFY(status)
      this%lat_centers = this%compute_lat_centers(this%pole, rc=status)
      _VERIFY(status)
      this%lat_centers_degrees = this%compute_lat_centers(this%pole, &
               convert_to_radians = .false., rc=status)
      this%lon_corners = this%compute_lon_corners(this%dateline, rc=status)
      _VERIFY(status)
      this%lat_corners = this%compute_lat_corners(this%pole, rc=status)
      _VERIFY(status)

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

         call ESMF_ConfigFindLabel(config, label=prefix//label, isPresent=isPresent, rc=status)
         _VERIFY(status)
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
         call ESMF_ConfigFindLabel(config, label=prefix//label,rc=status)
         _VERIFY(status)
         do i = 1, n
            call ESMF_ConfigGetAttribute(config, values(i), rc=status)
            _VERIFY(status)
         end do

         _RETURN(_SUCCESS)

      end subroutine get_multi_integer

      subroutine get_range(range, label, rc)
         type(RealMinMax), intent(out) :: range
         character(len=*) :: label
         integer, optional, intent(out) :: rc

         integer :: i
         integer :: n
         integer :: status
         logical :: isPresent

         call ESMF_ConfigFindLabel(config, label=prefix//label,isPresent=isPresent,rc=status)
         _VERIFY(status)
         if (.not. isPresent) then
            _RETURN(_SUCCESS)
         end if

         ! Must be 2 values: min and max
         call ESMF_ConfigGetAttribute(config, range%min, rc=status)
         _VERIFY(status)
         call ESMF_ConfigGetAttribute(config, range%max, rc=status)
         _VERIFY(status)

         _RETURN(_SUCCESS)

      end subroutine get_range

        subroutine derive_corners(this, rc)
           class(LatLonGeomFactory), intent(inout) :: this
           integer, optional, intent(out) :: rc

           integer :: status
           
         ! Corners are the midpoints of centers (and extrapolated at the
         ! poles for lats.)
         allocate(this%lon_corners(im+1), this%lat_corners(jm+1))

         this%lon_corners(1) = (this%lon_centers(im) + this%lon_centers(1))/2 - 180
         this%lon_corners(2:im) = (this%lon_centers(1:im-1) + this%lon_centers(2:im))/2
         this%lon_corners(im+1) = (this%lon_centers(im) + this%lon_centers(1))/2 + 180

         ! This section about pole/dateline is probably not needed in file data case.
         if (abs(this%lon_centers(1) + 180) < 1000*epsilon(1.0)) then
            this%dateline = 'DC'
         else if (abs(this%lon_centers(1)) < 1000*epsilon(1.0)) then
            this%dateline = 'GC'
         else if (abs(this%lon_corners(1) + 180) < 1000*epsilon(1.0)) then
            this%dateline = 'DE'
         else if (abs(this%lon_corners(1)) < 1000*epsilon(1.0)) then
            this%dateline = 'GE'
         else ! assume 'XY'
            this%dateline = 'XY'
            this%lon_range = RealMinMax(this%lon_centers(1), this%lon_centers(jm))
         end if

         this%lat_corners(1) = this%lat_centers(1) - (this%lat_centers(2)-this%lat_centers(1))/2
         this%lat_corners(2:jm) = (this%lat_centers(1:jm-1) + this%lat_centers(2:jm))/2
         this%lat_corners(jm+1) = this%lat_centers(jm) - (this%lat_centers(jm-1)-this%lat_centers(jm))/2
      end subroutine derive_corners

   end subroutine initialize_from_config_with_prefix



   function to_string(this) result(string)
      character(len=:), allocatable :: string
      class (LatLonGeomFactory), intent(in) :: this

      _UNUSED_DUMMY(this)
      string = 'LatLonGeomFactory'

   end function to_string



   subroutine check_and_fill_consistency(this, unusable, rc)
      use MAPL_BaseMod, only: MAPL_DecomposeDim
      class (LatLonGeomFactory), intent(inout) :: this
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

      ! Check regional vs global
      if (this%pole == 'XY') then ! regional
         this%periodic = .false.
         _ASSERT(this%lat_range%min /= MAPL_UNDEFINED_REAL, 'uninitialized min for lat_range')
         _ASSERT(this%lat_range%max /= MAPL_UNDEFINED_REAL, 'uninitialized min for lat_range')
      else ! global
         _ASSERT(any(this%pole == ['PE', 'PC']), 'unsupported option for pole:'//this%pole)
         _ASSERT(this%lat_range%min == MAPL_UNDEFINED_REAL, 'inconsistent min for lat_range')
         _ASSERT(this%lat_range%max == MAPL_UNDEFINED_REAL, 'inconsistent max for lat_range')
      end if
      if (this%dateline == 'XY') then
         this%periodic = .false.
         _ASSERT(this%lon_range%min /= MAPL_UNDEFINED_REAL, 'uninitialized min for lon_range')
         _ASSERT(this%lon_range%max /= MAPL_UNDEFINED_REAL, 'uninitialized max for lon_range')
      else
         _ASSERT(any(this%dateline == ['DC', 'DE', 'GC', 'GE']), 'unsupported option for dateline')
         _ASSERT(this%lon_range%min == MAPL_UNDEFINED_REAL, 'inconsistent min for lon_range')
         _ASSERT(this%lon_range%max == MAPL_UNDEFINED_REAL, 'inconsistent max for lon_range')
      end if
      if (.not.this%force_decomposition) then
         verify_decomp = this%check_decomposition(rc=status)
         _VERIFY(status)
         if ( (.not.verify_decomp) ) then
            call this%generate_newnxy(rc=status)
            _VERIFY(status)
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
      class (LatLonGeomFactory), intent(inout)  :: this
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

      _UNUSED_DUMMY(unusable)

      this%is_regular = .true.
      call ESMF_DistGridGet(dist_grid, dimCount=dim_count, tileCount=tile_count)
      allocate(max_index(dim_count, tile_count))
      call ESMF_DistGridGet(dist_grid, maxindexPTile=max_index)

      config = MAPL_ConfigCreate(rc=status)
      _VERIFY(status)
      call MAPL_ConfigSetAttribute(config, max_index(1,1), 'IM_WORLD:', rc=status)
      _VERIFY(status)
      call MAPL_ConfigSetAttribute(config, max_index(2,1), 'JM_WORLD:', rc=status)
      _VERIFY(status)
      call MAPL_ConfigSetAttribute(config, max_index(3,1), 'LM:', rc=status)
      _VERIFY(status)

      lon => null()
      lat => null()
      call ESMF_LocalArrayGet(lon_array, farrayPtr=lon, rc=status)
      _VERIFY(status)
      call ESMF_LocalArrayGet(lat_array, farrayPtr=lat, rc=status)
      _VERIFY(status)


      if (abs(lat(1) + PI/2) < tiny) then
         pole = 'PC'
      elseif (abs(lat(1) + PI/2 - 0.5*(lat(2)-lat(1))) < tiny) then
         pole = 'PE'
      else
         pole = 'PC'
      end if

      ! the code below is kluge to return DE/DC wheither or not the file lons are -180 to 180 or 0 360
      ! it detects whether the first longitudes which are cell centers
      ! If first longitude is 0 or -180 (DC) it is dateline center in that 0 or -180 is
      ! in the center of a grid cell.
      ! or shifted by half a grid box (DE) so 0 or -180 is the edge of a cell
      ! really should have 4 options dateline edge (DE), dateline center(DC)
      ! grenwich center (GC) and grenwich edge (GE) but the last 2 are not supported
      ! if it is GC or GE we will shift the data on the usage so that it is DE or DC for now
      do i=0,1
         if (abs(lon(1) + PI*i) < tiny) then
            dateline = 'DC'
            exit
         elseif (abs(lon(1) + PI*i - 0.5*(lon(2)-lon(1))) < tiny) then
            dateline = 'DE'
            exit
         end if
      end do
      !if (abs(lon(1) + PI) < tiny) then
      !dateline = 'DC'
      !elseif (abs(lon(1) + PI - 0.5*(lon(2)-lon(1))) < tiny) then
      !dateline = 'DE'
      !elseif (abs(lon(1)) < tiny) then
      !dateline = 'GC'
      !elseif (abs(lon(1) - 0.5*(lon(2)-lon(1))) < tiny) then
      !dateline = 'GE'
      !end if

      call MAPL_ConfigSetAttribute(config, pole, 'POLE:')
      call MAPL_ConfigSetAttribute(config, dateline, 'DATELINE:')

      call ESMF_VMGetCurrent(vm, rc=status)
      _VERIFY(status)
      call ESMF_VMGet(vm, PETcount=nPet, rc=status)
      _VERIFY(status)

      nx_guess = nint(sqrt(real(nPet)))
      do nx = nx_guess,1,-1
         ny=nPet/nx
         if (nx*ny==nPet) then
            call MAPL_ConfigSetAttribute(config, nx, 'NX:')
            call MAPL_ConfigSetAttribute(config, ny, 'NY:')
            exit
         end if
      enddo

      call this%initialize(config, rc=status)
      _VERIFY(status)


   end subroutine initialize_from_esmf_distGrid

   function decomps_are_equal(this,a) result(equal)
      class (LatLonGeomFactory), intent(in) :: this
      class (AbstractGeomFactory), intent(in) :: a
      logical :: equal

      select type (a)
         class default
         equal = .false.
         return
      class is (LatLonGeomFactory)
         equal = .true.


         equal = size(a%ims)==size(this%ims) .and. size(a%jms)==size(this%jms)
         if (.not. equal) return

         ! same decomposition
         equal = all(a%ims == this%ims) .and. all(a%jms == this%jms)
         if (.not. equal) return

      end select

   end function decomps_are_equal


   function physical_params_are_equal(this, a) result(equal)
      class (LatLonGeomFactory), intent(in) :: this
      class (AbstractGeomFactory), intent(in) :: a
      logical :: equal

      select type (a)
         class default
         equal = .false.
         return
      class is (LatLonGeomFactory)
         equal = .true.

         equal = (a%im_world == this%im_world) .and. (a%jm_world == this%jm_world)
         if (.not. equal) return

         equal = (a%is_regular .eqv. this%is_regular)
         if (.not. equal) return

         if (a%is_regular) then
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
      class (LatLonGeomFactory), intent(in) :: a
      class (AbstractGeomFactory), intent(in) :: b

      select type (b)
         class default
         equals = .false.
         return
      class is (LatLonGeomFactory)
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
      class (LatLonGeomFactory), intent(in) :: this

      character(len=4) :: im_string, jm_string

      write(im_string,'(i4.4)') this%im_world
      write(jm_string,'(i4.4)') this%jm_world

      name = this%dateline // im_string // 'x' // this%pole // jm_string

   end function generate_grid_name

   function check_decomposition(this,unusable,rc) result(can_decomp)
      class (LatLonGeomFactory), target, intent(inout) :: this
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
      class (LatLonGeomFactory), target, intent(inout) :: this
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


   subroutine append_metadata(this, metadata)
      use MAPL_Constants
      class (LatLonGeomFactory), intent(inout) :: this
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
      class (LatLonGeomFactory), intent(inout) :: this

      character(len=:), allocatable :: vars
      _UNUSED_DUMMY(this)

      vars = 'lon,lat'

   end function get_grid_vars

   function get_file_format_vars(this) result(vars)
      class (LatLonGeomFactory), intent(inout) :: this

      character(len=:), allocatable :: vars
      _UNUSED_DUMMY(this)

      vars = 'lon,lat'

   end function get_file_format_vars

   subroutine append_variable_metadata(this,var)
      class (LatLonGeomFactory), intent(inout) :: this
      type(Variable), intent(inout) :: var
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(var)
   end subroutine append_variable_metadata

   subroutine generate_file_bounds(this,grid,local_start,global_start,global_count,metadata,rc)
      use MAPL_BaseMod
      class(LatLonGeomFactory), intent(inout) :: this
      type(ESMF_Grid),      intent(inout) :: grid
      integer, allocatable, intent(out) :: local_start(:)
      integer, allocatable, intent(out) :: global_start(:)
      integer, allocatable, intent(out) :: global_count(:)
      type(FileMetaData), intent(in), optional :: metaData
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: global_dim(3), i1,j1,in,jn

      _UNUSED_DUMMY(this)

      call MAPL_GridGet(grid,globalCellCountPerDim=global_dim,rc=status)
      _VERIFY(status)
      call MAPL_GridGetInterior(grid,i1,in,j1,jn)
      allocate(local_start,source=[i1,j1])
      allocate(global_start,source=[1,1])
      allocate(global_count,source=[global_dim(1),global_dim(2)])

      _RETURN(_SUCCESS)

   end subroutine generate_file_bounds

   subroutine generate_file_corner_bounds(this,grid,local_start,global_start,global_count,rc)
      use esmf
      class (LatLonGeomFactory), intent(inout) :: this
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
      class(LatLonGeomFactory), intent(inout) :: this
      real, pointer, intent(in) :: fpointer(:,:)
      _UNUSED_DUMMY(this)
      ref = ArrayReference(fpointer)
   end function generate_file_reference2D

   function generate_file_reference3D(this,fpointer,metaData) result(ref)
      use pFIO
      type(ArrayReference) :: ref
      class(LatLonGeomFactory), intent(inout) :: this
      real, pointer, intent(in) :: fpointer(:,:,:)
      type(FileMetaData), intent(in), optional :: metaData
      _UNUSED_DUMMY(this)
      ref = ArrayReference(fpointer)
   end function generate_file_reference3D

   ! helper functions

   function find_dim_name(file_metadata, name, varname, rc) result(dim_name)
      character(:), allocatable :: extent
      type(FileMetadata), intent(in) :: filemetadata
      character(*), intent(in) :: name
      character(*), intent(in) :: varname
      integer, optional, intent(out) :: rc
      
      integer :: status

      if (file_metadata%has_dimension(name)) then
         dim_name = name
         _RETURN(_SUCCESS)
      end if
      
      if (file_metadata%has_dimension(varname)) then
         dim_name = varname
         _RETURN(_SUCCESS)
      end if

      dim_name = ''
      _FAIL('Neither '//name//' nor '//varname//' found in metadata.')

   end function find_dim_name

   function get_coordinates(file_metatada, dim_name, rc) result(coordinates)
      real(kind=REAL64), allocatable :: coordinates
      type(FileMetadata), intent(in) :: file_metadata
      character(*), intent(in) :: dim_name
      integer, optional, intent(out) :: rc

      integer :: status
      class (CoordinateVariable), pointer :: v
      class (*), pointer :: ptr(:)

      v => file_metadata%get_coordinate_variable(dim_name, _RC)
      ptr => v%get_coordinate_data()
      _ASSERT(associated(ptr),'coordinate data not allocated')

      select type (ptr)
      type is (real(kind=REAL64))
         coordinates = ptr
      type is (real(kind=REAL32))
         coordinates = ptr
      class default
         _FAIL('unsuppoted type of data; must be REAL32 or REAL64')
      end select
 
     _RETURN(_SUCCESS)
   end function get_coordinates

end module mapl3g_LatLonGeomFactory






!##include "MAPL_Generic.h"
!#
!#module mapl3g_LatLonGeomFactory
!#   use mapl3g_GeomFactory
!#   use mapl3g_GeomSpec
!#   use mapl3g_NullGeomSpec
!#   use esmf, only: ESMF_HConfig
!#   implicit none
!#
!#   public :: LatLonGeomFactory
!#   public :: LatLonGeomSpec
!#
!#   ! Note that LatLonGeomSpec (type and type constructor) are PRIVATE.
!#   ! This may be relaxed if we want for testing.
!#   type, extends(GeomSpec) :: LatLonGeomSpec
!#      private
!#      integer :: im_world ! cells per face x-edge
!#      integer :: jm_world ! cells per face y-edge
!#      integer :: lm       ! number of levels
!#      integer :: nx       ! decomposition in x direction
!#      integer :: ny       ! decomposition in y direction
!#      integer, allocatable :: ims(:)   ! decomposition in x direction
!#      integer, allocatable :: jms(:)   ! decomposition in y direction
!#      character(2) :: pole ! grid staggering relative to pole ("PC", "PE", "XY")
!#      character(2) :: dateline ! grid staggering relative to dateline ("DC", "DE", "GC", "GE")
!#   contains
!#      procedure :: equal_to
!#   end type LatLonGeomSpec
!#
!#
!#contains
!#
!#  ! Process hconfig to determine all necessary spec components. Some
!#   ! spec components (e.g. nx, ny) may be determined from default
!#   ! heuristics.
!#   function new_LatLonGeomSpec_from_hconfig(hconfig, supports, rc) result(spec)
!#      type(LatLonGeomSpec) :: spec
!#      type(ESMF_HConfig), intent(in) :: hconfig
!#      integer, optional, intent(out) :: supports
!#      integer, optional, intent(out) :: rc
!#
!#      integer :: status
!#      logical :: has_name
!#      
!#      this%name = MAPL_GRID_NAME_DEFAULT
!#      has_name = ESMF_HConfigIsDefined(hconfig, keystring='name', _RC)
!#      if (has_name) then
!#         this%name = ESMF_HConfigAsString(hconfig, keystring = 'name', _RC)
!#      end if
!#
!#      call get(this%nx, hconfig, key='nx', MAPL_UNDEFINED_INTEGER, _RC)
!#      call get(this%ny, hconfig, key='ny', MAPL_UNDEFINED_INTEGER, _RC)
!#
!#
!#
!#      _RETURN(_SUCCESS)
!#   end function new_LatLonGeomSpec_from_hconfig
!#
!#   ! Process metadata to determine all necessary spec components.  Some
!#   ! spec components (e.g. nx, ny) may be determined from default
!#   ! heuristics.
!#   function new_LatLonGeomSpec_from_metadata(metadata, supports, rc) result(spec)
!#      type(LatLonGeom_spec) :: spec
!#      type(FileMetadata), intent(in) :: metadata
!#      integer, optional, intent(out) :: supports
!#      integer, optional, intent(out) :: rc
!#
!#      integer :: status
!#      ...
!#
!#      _RETURN(_SUCCESS)
!#   end function new_LatLonGeomSpec_from_metadata
!#
!# 
!#   function make_mapl_geom_from_spec(this, geom_spec, supports, rc) result(mapl_geom)
!#      type(MaplGeom) :: mapl_geom
!#      class(LatLonGeomFactory), intent(in) :: this
!#      class(GeomSpec), intent(in) :: geom_spec
!#      integer, optional, intent(out) :: supports
!#      integer, optional, intent(out) :: rc
!#
!#      select type(q => geom_spec)
!#      type is (LatLonGeomSpec)
!#         if (present(supports)) supports = .true.
!#         mapl_geom = type_safe_make_mapl_geom_from_spec(q, _RC)
!#      class default
!#         mapl_geom = NullGeomSpec()
!#         if (present(supports)) supports = .false.
!#      end select
!#
!#      _RETURN(_SUCCESS)
!#   end function make_mapl_geom_from_spec
!#
!#
!#   function type_safe_make_mapl_geom_from_spec(spec, rc) result(mapl_geom)
!#      type(MaplGeom) :: mapl_geom
!#      type(LatLonGeomSpec), intent(in) :: spec
!#      integer, optional, intent(out) :: rc
!#
!#      type(ESMF_Geom) :: geom
!#
!#      geom = make_esmf_geom(spec, _RC)
!#      file_metadata = make_file_metadata(spec, _RC)
!#      gridded_dimensions = make_gridded_dimensions(spec, _RC)
!#
!#      mapl_geom = MaplGeom(geom, file_metadata, gridded_dimensions)
!#
!#   end function type_safe_make_mapl_geom_from_spec
!#
!#
!#   ! Helper procedures
!#   function make_esmf_geom(geom_spec, rc) result(geom)
!#      type(ESMF_Geom) :: geom
!#      type(LatLonGeomSpec), intent(in) :: geom_spec
!#      
!#      grid = ESMF_GridCreate(...)
!#      ...
!#      geom = ESMF_GeomCreate(geom)
!#
!#   end function make_esmf_geom
!#
!#   function make_file_metadata(geom_spec, rc) result(file_metadata)
!#      type(FileMetadata) :: file_metadata
!#      type(LatLonGeomSpec), intent(in) :: geom_spec
!#      integer, optional, intent(out) ::: rc
!#
!#      metdata = FileMetadata()
!#      call add_dimensions(param, metadata, _RC)
!#      call add_coordinate_variables(param, metadata, _RC)
!#
!#      _RETURN(_SUCCESS)
!#   end function make_file_metadata
!#
!#
!#   subroutine add_coordinates(this, metadata, rc)
!#      class(LatLonGeomSpec), intent(in) :: this
!#      type(FileMetadata), intent(inout) :: metadata
!#      integer, optional, intent(out) :: rc
!#
!#      integer :: status
!#      type(Variable) :: v
!#
!#      ! Coordinate variables
!#      v = coordinate('lon', 'longitude', 'degrees_east', this%get_longitudes_degrees())
!#      call metadata%add_variable(v)
!#      v = coordinate('lat', 'latitude', 'degrees_northt', this%get_latitude_degrees())
!#      call metadata%add_variable(v)
!#
!#      if (this%has_vertical_dimension()) then
!#         v = VerticalCoordinate(...)
!#         call metadata%add_variable('lev', v)
!#      end if
!#
!#      _RETURN(_SUCCESS)
!#
!#   contains      
!#
!#      function coordinate(dimensions, long_name, units, coords) result(v)
!#         type(Variable) :: v
!#         character(*), intent(in) :: dimensions
!#         character(*), intent(in) :: long_name
!#         character(*), intent(in) :: units
!#         real(kind=REAL64), intent(in) :: coords(:)
!#
!#         v = Variable(type=PFIO_REAL64, dimensions=dimensions)
!#         call v%add_attribute('long_name', long_name)
!#         call v%add_attribute('units', units)
!#         call v%add_const_value(UnlimitedEntity(coords))
!#
!#      end function coordinate
!#
!#   end subroutine add_coordinates
!#
!#   
!#   pure logical function equal_to(a, b)
!#      class(LatLonGeomSpec), intent(in) :: a
!#      class(GeomSpec), intent(in) :: b
!#
!#      select type (b)
!#      type is (LatLonGeomSpec)
!#         equal_to = a%im_world == b%im_world .and. a%jm_world == b%jm_world &
!#              .and. a%lm == b%lm &
!#              .and. a%nx == b%nx .and. a%ny == b%ny &
!#              .and. a%ims == b%ims .and. a%jms == b%jms &
!#              .and. a%pole == b%pole .and. a%dateline == b%dateline
!#      class default
!#         equal_to = .false.
!#      end select
!#
!#   end function equal_to
!#
!#
!#   subroutine get_integer(value, hconfig, key, unusable, default, rc)
!#      integer, intent(out) :: value
!#      type(ESMF_HConfig), intent(inout) :: hconfig
!#      character(*), intent(in) :: key
!#      integer, optional, intent(in) :: default
!#      class(KeywordEnforcer), intent(in) :: unusable
!#      integer, optional, intent(out) :: rc
!#
!#      integer :: status
!#      logical :: has_key
!#
!#      if (present(default)) value = default
!#      has_key = ESMF_HConfigIsDefined(hconfig, keystring=key, _RC)
!#      _RETURN_UNLESS(has_key)
!#
!#      value = ESMF_HConfigAsI4(hconfig, keystring=key, _RC)
!#
!#   end subroutine get_integer
!#
!#
!#
!#   subroutine get_string(value, hconfig, key, unusable, default, rc)
!#      character(:), allocatable :: value
!#      type(ESMF_HConfig), intent(inout) :: hconfig
!#      character(*), intent(in) :: key
!#      integer, optional, intent(in) :: default
!#      class(KeywordEnforcer), intent(in) :: unusable
!#      integer, optional, intent(out) :: rc
!#
!#      integer :: status
!#      logical :: has_key
!#
!#      if (present(default)) value = default
!#      has_key = ESMF_HConfigIsDefined(hconfig, keystring=key, _RC)
!#      _RETURN_UNLESS(has_key)
!#
!#      value = ESMF_HConfigAsString(hconfig, keystring=key, _RC)
!#
!#   end subroutine get_string
!#
!#
!#end module mapl3g_LatLonGeomFactory


        
