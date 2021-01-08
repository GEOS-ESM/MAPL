#include "MAPL_ErrLog.h"

! overload set interfaces in legacy
! Document PE, PC, DC, DE, GC

! This module generates ESMF_Grids corresponding to _regular_ lat-lon coordinate grids.
! I.e., spacing between lats (lons) is constant.

module MAPL_LatLonGridFactoryMod
   use MAPL_AbstractGridFactoryMod
   use MAPL_MinMaxMod
   use MAPL_KeywordEnforcerMod
   use mapl_ErrorHandlingMod
   use MAPL_ConstantsMod
   use ESMF
   use pFIO
   use MAPL_CommsMod
   use MAPL_IOMod, only : GETFILE, FREE_FILE
   use, intrinsic :: iso_fortran_env, only: REAL32
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none
   private

   public :: LatLonGridFactory

   integer, parameter :: NUM_DIM = 2
   integer, parameter :: UNDEFINED_INTEGER = 1-huge(1)
   real, parameter :: UNDEFINED_REAL = huge(1.)
   character(len=*), parameter :: UNDEFINED_CHAR = '**'

   character(len=*), parameter :: GRID_NAME_DEFAULT = 'UNKNOWN'

   type, extends(AbstractGridFactory) :: LatLonGridFactory
      private
      logical :: is_regular = .false.
      character(len=:), allocatable :: grid_name
      ! Grid dimensions
      integer :: im_world = UNDEFINED_INTEGER
      integer :: jm_world = UNDEFINED_INTEGER
      integer :: lm = UNDEFINED_INTEGER
      real(kind=REAL64), allocatable :: lon_centers(:)
      real(kind=REAL64), allocatable :: lat_centers(:)
      real(kind=REAL64), allocatable :: lon_corners(:)
      real(kind=REAL64), allocatable :: lat_corners(:)
      
      ! Domain decomposition:
      integer :: nx = UNDEFINED_INTEGER
      integer :: ny = UNDEFINED_INTEGER
      integer, allocatable :: ims(:)
      integer, allocatable :: jms(:)
      ! Grid conventions:
      character(len=:), allocatable :: pole
      character(len=:), allocatable :: dateline
      ! Regional vs global:
      type (RealMinMax) :: lon_range = RealMinMax(UNDEFINED_REAL,UNDEFINED_REAL)
      type (RealMinMax) :: lat_range = RealMinMax(UNDEFINED_REAL,UNDEFINED_REAL)
      ! Used for halo
      type (ESMF_DELayout) :: layout
      integer :: px, py
      logical :: is_halo_initialized = .false.
      logical :: periodic = .true.
   contains
      procedure :: make_new_grid
      procedure :: create_basic_grid
      procedure :: get_longitudes
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
      procedure :: append_variable_metadata
      procedure :: check_decomposition
      procedure :: generate_newnxy
      procedure :: generate_file_bounds
      procedure :: generate_file_corner_bounds
      procedure :: generate_file_reference2D
      procedure :: generate_file_reference3D
   end type LatLonGridFactory

   character(len=*), parameter :: MOD_NAME = 'MAPL_LatLonGridFactory::'

   interface LatLonGridFactory
      module procedure LatLonGridFactory_basic
      module procedure LatLonGridFactory_from_parameters
   end interface LatLonGridFactory

   interface set_with_default
      module procedure set_with_default_integer
      module procedure set_with_default_real
      module procedure set_with_default_character
      module procedure set_with_default_range
   end interface set_with_default


contains

   ! Note: lats and lons must be in _radians_, as the ESMF_Grid
   ! constructor is currently assuming that choice.
   function Latlongridfactory_basic(grid_name, &
        & lon_centers, lat_centers, lon_corners, lat_corners, &
        & ims, jms, lm, unusable, rc) result(factory)
      type (LatLonGridFactory) :: factory
      character(len=*), intent(in) :: grid_name
      real(kind=REAL64), intent(in) :: lon_centers(:)
      real(kind=REAL64), intent(in) :: lat_centers(:)
      real(kind=REAL64), intent(in) :: lon_corners(:)
      real(kind=REAL64), intent(in) :: lat_corners(:)
      integer, intent(in) :: ims(:)
      integer, intent(in) :: jms(:)
      integer, intent(in) :: lm
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type (ESMF_VM) :: vm
      integer :: nPet

      integer :: status
      character(*), parameter :: IAM = __FILE__

      _UNUSED_DUMMY(unusable)

      factory%is_regular = .false.
      
      factory%grid_name = grid_name
      factory%lon_centers = lon_centers
      factory%lat_centers = lat_centers
      factory%lon_corners = lon_corners
      factory%lat_corners = lat_corners

      factory%im_world = size(lon_centers)
      factory%jm_world = size(lon_centers)
      factory%lm = lm

      ! Decomposition
      factory%ims = ims
      factory%jms = jms
      factory%nx = size(ims)
      factory%ny = size(jms)

      ! Check consistency

      _ASSERT(size(lon_corners) == size(lon_centers)+1, 'inconsistent shape')
      _ASSERT(size(lat_corners) == size(lat_centers)+1, 'inconsistent shape')

      _ASSERT(sum(ims) == size(lon_centers),'inconcistent decomposition')
      _ASSERT(sum(jms) == size(lat_centers),'inconcistent decomposition')

      call ESMF_VMGetCurrent(vm, rc=status)
      _VERIFY(status)
      call ESMF_VMGet(vm, PETcount=nPet, rc=status)
      _VERIFY(status)
      _ASSERT(factory%nx*factory%ny == nPet,'inconsistent process topology')

      _RETURN(_SUCCESS)
      
   end function LatLonGridFactory_basic


   function LatLonGridFactory_from_parameters(unusable, grid_name, &
        & im_world, jm_world, lm, nx, ny, ims, jms, &
        & pole, dateline, lon_range, lat_range, rc) result(factory)
      type (LatLonGridFactory) :: factory
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

      integer, optional, intent(out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      factory%is_regular = .true.
      call set_with_default(factory%grid_name, grid_name, GRID_NAME_DEFAULT)

      call set_with_default(factory%nx, nx, UNDEFINED_INTEGER)
      call set_with_default(factory%ny, ny, UNDEFINED_INTEGER)

      call set_with_default(factory%im_world, im_world, UNDEFINED_INTEGER)
      call set_with_default(factory%jm_world, jm_world, UNDEFINED_INTEGER)
      call set_with_default(factory%lm, lm, UNDEFINED_INTEGER)

      ! default is unallocated
      if (present(ims)) factory%ims = ims
      if (present(jms)) factory%jms = jms

      call set_with_default(factory%pole, pole, UNDEFINED_CHAR)
      call set_with_default(factory%dateline, dateline, UNDEFINED_CHAR)

      call set_with_default(factory%lon_range, lon_range, RealMinMax(UNDEFINED_REAL,UNDEFINED_REAL))
      call set_with_default(factory%lat_range, lat_range, RealMinMax(UNDEFINED_REAL,UNDEFINED_REAL))

      call factory%check_and_fill_consistency(rc=status)
      _VERIFY(status)

      ! Compute the centers and corners
      factory%lon_centers = factory%compute_lon_centers(factory%dateline, rc=status)
      _VERIFY(status)
      factory%lat_centers = factory%compute_lat_centers(factory%pole, rc=status)
      _VERIFY(status)
      factory%lon_corners = factory%compute_lon_corners(factory%dateline, rc=status)
      _VERIFY(status)
      factory%lat_corners = factory%compute_lat_corners(factory%pole, rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)

   end function LatLonGridFactory_from_parameters


   function make_new_grid(this, unusable, rc) result(grid)
      type (ESMF_Grid) :: grid
      class (LatLonGridFactory), intent(in) :: this
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
      class (LatLonGridFactory), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

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


      if (this%lm /= UNDEFINED_INTEGER) then
         call ESMF_AttributeSet(grid, name='GRID_LM', value=this%lm, rc=status)
         _VERIFY(status)
      end if

      call ESMF_AttributeSet(grid, 'GridType', 'LatLon', rc=status)
      _VERIFY(status)
      if (.not.this%periodic) then
         call ESMF_AttributeSet(grid, 'Global', .false., rc=status)
         _VERIFY(status)
      end if

      _RETURN(_SUCCESS)
   end function create_basic_grid

   ! in radians
   function get_longitudes(this, unusable, rc) result(longitudes)
      use MAPL_BaseMod
      class (LatLonGridFactory), intent(in) :: this
      real(kind=REAL64), allocatable :: longitudes(:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)

      longitudes = this%lon_centers
      _RETURN(_SUCCESS)
   end function get_longitudes


   ! in radians
   function get_latitudes(this, unusable, rc) result(latitudes)
      use MAPL_BaseMod
      class (LatLonGridFactory), intent(in) :: this
      real(kind=REAL64), allocatable :: latitudes(:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)

      latitudes = this%lat_centers
      _RETURN(_SUCCESS)
   end function get_latitudes

   ! in radians
   function compute_lon_centers(this, dateline, unusable, rc) result(lon_centers)
      use MAPL_ConstantsMod, only:MAPL_DEGREES_TO_RADIANS
      use MAPL_BaseMod
      real(kind=REAL64), allocatable :: lon_centers(:)
      class (LatLonGridFactory), intent(in) :: this
      character(2), intent(in) :: dateline
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      real(kind=REAL64) :: delta, min_coord, max_coord
      logical :: regional
      integer :: status

      _UNUSED_DUMMY(unusable)

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

      lon_centers = MAPL_Range(min_coord, max_coord, this%im_world, &
           & conversion_factor=MAPL_DEGREES_TO_RADIANS, rc=status)

      _RETURN(_SUCCESS)
   end function compute_lon_centers

   function compute_lon_corners(this, dateline, unusable, rc) result(lon_corners)
      use MAPL_ConstantsMod, only:MAPL_DEGREES_TO_RADIANS
      use MAPL_BaseMod
      real(kind=REAL64), allocatable :: lon_corners(:)
      class (LatLonGridFactory), intent(in) :: this
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
           & conversion_factor=MAPL_DEGREES_TO_RADIANS, rc=status)

      _RETURN(_SUCCESS)
   end function compute_lon_corners


   ! in radians
   function get_lon_corners(this, unusable, rc) result(lon_corners)
      use MAPL_BaseMod
      class (LatLonGridFactory), intent(in) :: this
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
      class (LatLonGridFactory), intent(in) :: this
      real(kind=REAL64), allocatable :: lat_corners(:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)

      lat_corners = this%lat_corners
      _RETURN(_SUCCESS)

   end function get_lat_corners


   function compute_lat_centers(this, pole, unusable, rc) result(lat_centers)
      use MAPL_ConstantsMod, only: MAPL_DEGREES_TO_RADIANS
      use MAPL_BaseMod
      real(kind=REAL64), allocatable :: lat_centers(:)
      class (LatLonGridFactory), intent(in) :: this
      character(2), intent(in) :: pole
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      real(kind=REAL64) :: delta, min_coord, max_coord
      logical :: regional
      integer :: status

      _UNUSED_DUMMY(unusable)

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

      lat_centers = MAPL_Range(min_coord, max_coord, this%jm_world, &
           & conversion_factor=MAPL_DEGREES_TO_RADIANS, rc=status)

      _RETURN(_SUCCESS)

   end function compute_lat_centers

   function compute_lat_corners(this, pole, unusable, rc) result(lat_corners)
      use MAPL_ConstantsMod, only: MAPL_DEGREES_TO_RADIANS
      use MAPL_BaseMod
      real(kind=REAL64), allocatable :: lat_corners(:)
      class (LatLonGridFactory), intent(in) :: this
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
           & conversion_factor=MAPL_DEGREES_TO_RADIANS, rc=status)
      if (pole == 'PC') then
         lat_corners(1)=-90.d0*MAPL_DEGREES_TO_RADIANS
         lat_corners(this%jm_world+1)=90.d0*MAPL_DEGREES_TO_RADIANS
      end if

      _RETURN(_SUCCESS)

   end function compute_lat_corners


   subroutine add_horz_coordinates(this, grid, unusable, rc)
      use MAPL_BaseMod, only: MAPL_grid_interior
      class (LatLonGridFactory), intent(in) :: this
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


   subroutine initialize_from_file_metadata(this, file_metadata, unusable, rc)
      use MAPL_KeywordEnforcerMod
      use MAPL_BaseMod, only: MAPL_DecomposeDim

      class (LatLonGridFactory), intent(inout)  :: this
      type (FileMetadata), target, intent(in) :: file_metadata
      class (KeywordEnforcer), optional, intent(in) :: unusable
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
      logical :: is_valid
      
      _UNUSED_DUMMY(unusable)

      ! Cannot assume that lats and lons are evenly spaced
      this%is_regular = .false.
      
      associate (im => this%im_world, jm => this%jm_world, lm => this%lm)
         lon_name = 'lon'
         hasLon = file_metadata%has_dimension(lon_name)
         if (hasLon) then
            im = file_metadata%get_dimension(lon_name, rc=status)
            _VERIFY(status)
         else
            lon_name = 'longitude'
            hasLongitude = file_metadata%has_dimension(lon_name)
            if (hasLongitude) then            
               im = file_metadata%get_dimension(lon_name, rc=status)
               _VERIFY(status)
            else
               _FAIL('no longitude coordinate')
            end if
         end if
         lat_name = 'lat'
         hasLat = file_metadata%has_dimension(lat_name)
         if (hasLat) then
            jm = file_metadata%get_dimension(lat_name, rc=status)
            _VERIFY(status)
         else
            lat_name = 'latitude'
            hasLatitude = file_metadata%has_dimension(lat_name)
            if (hasLatitude) then            
               jm = file_metadata%get_dimension(lat_name, rc=status)
               _VERIFY(status)
            else
               _FAIL('no latitude coordinate')
            end if
         end if
         hasLev=.false.
         hasLevel=.false.
         lev_name = 'lev'
         hasLev = file_metadata%has_dimension(lev_name)
         if (hasLev) then
            lm = file_metadata%get_dimension(lev_name,rc=status)
            _VERIFY(status)
         else
            lev_name = 'levels'
            hasLevel = file_metadata%has_dimension(lev_name)
            if (hasLevel) then
               lm = file_metadata%get_dimension(lev_name,rc=status)
               _VERIFY(status)
            end if
         end if   
         
        ! TODO: if 'lat' and 'lon' are not present then
        ! assume ... pole/dateline are ?
        
        ! TODO: check radians vs degrees.  Assume degrees for now.


        ! TODO: modify CoordinateVariable so that get_coordinate_data() is overloaded
        ! for different types (as subroutine) to avoid casting here.
        ! TODO: add get_coordinate_variable() interface to avoid the need to cast
        v => file_metadata%get_coordinate_variable(lon_name, rc=status)
        _VERIFY(status)
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


        v => file_metadata%get_coordinate_variable(lat_name, rc=status)
        _VERIFY(status)
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


        ! Check: is this a "mis-specified" pole-centered grid?
        if (size(this%lat_centers) >= 4) then
           ! Assume lbound=1 and ubound=size for now
           i_min = 1 !lbound(this%lat_centers)
           i_max = size(this%lat_centers) !ubound(this%lat_centers)
           d_lat = (this%lat_centers(i_max-1) - this%lat_centers(i_min+1))/&
                    (size(this%lat_centers)-3)
           is_valid = .True.
           ! Check: is this a regular grid (i.e. constant spacing away from the poles)?
           do i=(i_min+1),(i_max-2)
              d_lat_temp = this%lat_centers(i+1) - this%lat_centers(i)
              is_valid = (is_valid.and.(abs((d_lat_temp/d_lat)-1.0) < 1.0e-5))
              if (.not. is_valid) then
                 exit
              end if
           end do
           if (is_valid) then
              ! Should the southernmost point actually be at the pole?
              extrap_lat = this%lat_centers(i_min+1) - d_lat
              if (extrap_lat <= ((d_lat/20.0)-90.0)) then
                 this%lat_centers(i_min) = -90.0
              end if
              ! Should the northernmost point actually be at the pole?
              extrap_lat = this%lat_centers(i_max-1) + d_lat
              if (extrap_lat >= (90.0-(d_lat/20.0))) then
                 this%lat_centers(i_max) =  90.0
              end if
           end if
        end if
        

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
         
         if (abs(this%lat_centers(1) + 90) < 1000*epsilon(1.0)) then
            this%pole = 'PC'
         else if (abs(this%lat_corners(1) + 90) < 1000*epsilon(1.0)) then
            this%pole = 'PE'
         else ! assume XY
            this%pole = 'XY'
            this%lat_range = RealMinMax(this%lat_centers(1), this%lat_centers(jm))
         end if
         if (this%lat_corners(1) < -90) this%lat_corners(1)=-90
         if (this%lat_corners(jm+1) > 90) this%lat_corners(jm+1)=90

         ! check if evenly spaced
         regLon=.true.
         do i=2,size(this%lon_centers)
            del12=this%lon_centers(2)-this%lon_centers(1)
            delij=this%lon_centers(i)-this%lon_centers(i-1)
            if ((del12-delij)>epsilon(1.0)) regLon=.false.
         end do
         regLat=.true.
         do i=2,size(this%lat_centers)
            del12=this%lat_centers(2)-this%lat_centers(1)
            delij=this%lat_centers(i)-this%lat_centers(i-1)
            if ((del12-delij)>epsilon(1.0)) regLat=.false.
         end do
         this%is_regular = (regLat .and. regLon) 

         ! Convert to radians
         this%lon_centers = MAPL_DEGREES_TO_RADIANS * this%lon_centers
         this%lat_centers = MAPL_DEGREES_TO_RADIANS * this%lat_centers
         this%lon_corners = MAPL_DEGREES_TO_RADIANS * this%lon_corners
         this%lat_corners = MAPL_DEGREES_TO_RADIANS * this%lat_corners

    end associate
    
    call this%make_arbitrary_decomposition(this%nx, this%ny, rc=status)
    _VERIFY(status)

    ! Determine IMS and JMS with constraint for ESMF that each DE has at least an extent
    ! of 2.  Required for ESMF_FieldRegrid().
    allocate(this%ims(0:this%nx-1))
    allocate(this%jms(0:this%ny-1))
    call MAPL_DecomposeDim(this%im_world, this%ims, this%nx, min_DE_extent=2)
    call MAPL_DecomposeDim(this%jm_world, this%jms, this%ny, min_DE_extent=2)
    
    call this%check_and_fill_consistency(rc=status)
    _VERIFY(status)

    _RETURN(_SUCCESS)

   end subroutine initialize_from_file_metadata



   subroutine initialize_from_config_with_prefix(this, config, prefix, unusable, rc)
      use esmf
      class (LatLonGridFactory), intent(inout) :: this
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
      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'GRIDNAME:', default=GRID_NAME_DEFAULT)
      this%grid_name = trim(tmp)

      call ESMF_ConfigGetAttribute(config, this%nx, label=prefix//'NX:', default=UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%ny, label=prefix//'NY:', default=UNDEFINED_INTEGER)

      call ESMF_ConfigGetAttribute(config, this%im_world, label=prefix//'IM_WORLD:', default=UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%jm_world, label=prefix//'JM_WORLD:', default=UNDEFINED_INTEGER)

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

      call ESMF_ConfigGetAttribute(config, this%lm, label=prefix//'LM:', default=UNDEFINED_INTEGER)

      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'POLE:', default=UNDEFINED_CHAR, rc=status)
      if (status == _SUCCESS) then
         this%pole = trim(tmp)
      end if
      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'DATELINE:', default=UNDEFINED_CHAR, rc=status)
      if (status == _SUCCESS) then
         this%dateline = trim(tmp)
      end if

      call get_range(this%lon_range, 'LON_RANGE:', rc=status); _VERIFY(status)
      call get_range(this%lat_range, 'LAT_RANGE:', rc=status); _VERIFY(status)
      call this%check_and_fill_consistency(rc=status); _VERIFY(status)

      ! Compute the centers and corners
      this%lon_centers = this%compute_lon_centers(this%dateline, rc=status)
      _VERIFY(status)
      this%lat_centers = this%compute_lat_centers(this%pole, rc=status)
      _VERIFY(status)
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

            UNIT = GETFILE ( trim(file_name), form="formatted", rc=status )
            _VERIFY(STATUS)
            read(UNIT,*) total
            if (total /= n) then
                print*, file_name // " n is different from ", total
                _RETURN(_FAILURE)
            endif
            do i = 1,total
                read(UNIT,*) values(i)
            enddo
            call FREE_FILE(UNIT)
         endif

         call MAPL_CommsBcast(VM, values, n=N, ROOT=MAPL_Root, rc=status)
         _VERIFY(STATUS)
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


   end subroutine initialize_from_config_with_prefix



   function to_string(this) result(string)
      character(len=:), allocatable :: string
      class (LatLonGridFactory), intent(in) :: this

      _UNUSED_DUMMY(this)
      string = 'LatLonGridFactory'

   end function to_string



   subroutine check_and_fill_consistency(this, unusable, rc)
      use MAPL_BaseMod, only: MAPL_DecomposeDim
      class (LatLonGridFactory), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: verify_decomp

      _UNUSED_DUMMY(unusable)

      if (.not. allocated(this%grid_name)) then
         this%grid_name = GRID_NAME_DEFAULT
      end if

      ! Check decomposition/bounds
      ! WY notes: should not have this assert
      !_ASSERT(allocated(this%ims) .eqv. allocated(this%jms), 'inconsistent options')
      call verify(this%nx, this%im_world, this%ims, rc=status)
      call verify(this%ny, this%jm_world, this%jms, rc=status)

      ! Check regional vs global
      if (this%pole == 'XY') then ! regional
         this%periodic = .false.
         _ASSERT(this%lat_range%min /= UNDEFINED_REAL, 'uninitialized min for lat_range')
         _ASSERT(this%lat_range%max /= UNDEFINED_REAL, 'uninitialized min for lat_range')
      else ! global
         _ASSERT(any(this%pole == ['PE', 'PC']), 'unsupported option for pole:'//this%pole)
         _ASSERT(this%lat_range%min == UNDEFINED_REAL, 'inconsistent min for lat_range')
         _ASSERT(this%lat_range%max == UNDEFINED_REAL, 'inconsistent max for lat_range')
      end if
      if (this%dateline == 'XY') then
         this%periodic = .false.
         _ASSERT(this%lon_range%min /= UNDEFINED_REAL, 'uninitialized min for lon_range')
         _ASSERT(this%lon_range%max /= UNDEFINED_REAL, 'uninitialized max for lon_range')
      else
         _ASSERT(any(this%dateline == ['DC', 'DE', 'GC', 'GE']), 'unsupported option for dateline')
         _ASSERT(this%lon_range%min == UNDEFINED_REAL, 'inconsistent min for lon_range')
         _ASSERT(this%lon_range%max == UNDEFINED_REAL, 'inconsistent max for lon_range')
      end if
      verify_decomp = this%check_decomposition(rc=status)
      _VERIFY(status)
      if ( (.not.verify_decomp) ) then
         call this%generate_newnxy(rc=status)
         _VERIFY(status)
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

            if (n == UNDEFINED_INTEGER) then
               n = size(ms)
            else
               _ASSERT(n == size(ms), 'inconsistent topology')
            end if

            if (m_world == UNDEFINED_INTEGER) then
               m_world = sum(ms)
            else
               _ASSERT(m_world == sum(ms), 'inconsistent decomponsition')
            end if

         else

            _ASSERT(n /= UNDEFINED_INTEGER, 'uninitialized topology')
            _ASSERT(m_world /= UNDEFINED_INTEGER,'uninitialized dimension')
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


   ! MAPL uses values in lon_array and lat_array only to determine the
   ! general positioning.  Actual coordinates are then recomputed.
   ! This helps to avoid roundoff differences from slightly different
   ! input files.
   subroutine initialize_from_esmf_distGrid(this, dist_grid, lon_array, lat_array, unusable, rc)
      use MAPL_ConfigMod
      use MAPL_ConstantsMod, only: PI => MAPL_PI_R8
      class (LatLonGridFactory), intent(inout)  :: this
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



   logical function equals(a, b)
      class (LatLonGridFactory), intent(in) :: a
      class (AbstractGridFactory), intent(in) :: b

     
      select type (b)
         class default
         equals = .false.
         return
      class is (LatLonGridFactory)
         equals = .true.

         equals = (a%im_world == b%im_world) .and. (a%jm_world == b%jm_world)
         if (.not. equals) return

         equals = (a%lm == b%lm)
         if (.not. equals) return

         equals = size(a%ims)==size(b%ims) .and. size(a%jms)==size(b%jms)
         if (.not. equals) return

         ! same decomposition
         equals = all(a%ims == b%ims) .and. all(a%jms == b%jms)
         if (.not. equals) return

         equals = (a%is_regular .eqv. b%is_regular)
         if (.not. equals) return

         if (a%is_regular) then
            equals = (a%pole == b%pole)
            if (.not. equals) return

            equals = (a%dateline == b%dateline)
            if (.not. equals) return

            if (a%pole == 'XY') then
               equals = (a%lat_range == b%lat_range)
               if (.not. equals) return
            end if

            if (a%dateline == 'XY') then
               equals = (a%lon_range == b%lon_range)
               if (.not. equals) return
            end if
         else
            equals = &
                 & all(a%lon_centers == b%lon_centers) .and. & 
                 & all(a%lon_corners == b%lon_corners) .and. &
                 & all(a%lat_centers == b%lat_centers) .and. &
                 & all(a%lat_corners == b%lat_corners)
         end if
      end select

   end function equals


   function generate_grid_name(this) result(name)
      character(len=:), allocatable :: name
      class (LatLonGridFactory), intent(in) :: this

      character(len=4) :: im_string, jm_string

      write(im_string,'(i4.4)') this%im_world
      write(jm_string,'(i4.4)') this%jm_world

      name = this%dateline // im_string // 'x' // this%pole // jm_string

   end function generate_grid_name

   function check_decomposition(this,unusable,rc) result(can_decomp)
      class (LatLonGridFactory), target, intent(inout) :: this
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
      class (LatLonGridFactory), target, intent(inout) :: this
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
      class (LatLonGridFactory), target, intent(inout) :: this
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

      grid => this%get_grid(rc=status)
      _VERIFY(status)

      call ESMF_GridGet(grid,   distGrid=dist_grid, dimCount=dim_count, rc=status)
      _VERIFY(status)
      call ESMF_DistGridGet(dist_grid, delayout=this%layout, rc=status)
      _VERIFY(status)
      call ESMF_DELayoutGet (this%layout, vm=vm, rc=status)
      _VERIFY(status)

      call ESMF_VMGet(vm, localPet=pet, petCount=ndes, rc=status)
      _VERIFY(status)

      this%px = mod(pet, this%nx)
      this%py = pet / this%nx

      this%is_halo_initialized = .true.
      
      _RETURN(_SUCCESS)

   end subroutine init_halo


   subroutine halo(this, array, unusable, halo_width, rc)
      use MAPL_CommsMod
      class (LatLonGridFactory), intent(inout) :: this
      real(kind=REAL32), intent(inout) :: array(:,:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: halo_width
      integer, optional, intent(out) :: rc

      integer :: status

      integer :: pet_north
      integer :: pet_south
      integer :: pet_east
      integer :: pet_west

      _UNUSED_DUMMY(unusable)
      ! not yet implmented, default is 1
      _UNUSED_DUMMY(halo_width)

      if (.not. this%is_halo_initialized) then
         call this%init_halo(rc=status)
         _VERIFY(status)
      end if
         
      associate (nx => this%nx, ny => this% ny, px => this%px, py => this%py)
        ! Nearest neighbors processor' ids
        pet_north = get_pet(px, py+1, nx, ny)
        pet_south = get_pet(px, py-1, nx, ny)
        pet_east  = get_pet(px+1, py, nx, ny)
        pet_west  = get_pet(px-1, py, nx, ny)

        call fill_north(array, rc=status)
        _VERIFY(status)
        call fill_south(array, rc=status)
        _VERIFY(status)

        call fill_east(array, rc=status)
        _VERIFY(status)
        call fill_west(array, rc=status)
        _VERIFY(status)

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
              rc=status)
         _VERIFY(status)
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
              rc=status)
         _VERIFY(status)

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

         last = size(array,2)-1 
         len = size(array,1)

         call MAPL_CommsSendRecv(this%layout,      &
              array(2     , : ),  len,  pet_west,  &
              array(last+1, : ),  len,  pet_east,  &
              rc=status)
         _VERIFY(status)

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
              array(last  , : ),  len,  pet_west,  &
              array(1     , : ),  len,  pet_east,  &
              rc=status)
         _VERIFY(status)

         _RETURN(_SUCCESS)

      end subroutine fill_west


   end subroutine halo


   subroutine append_metadata(this, metadata)
      use MAPL_ConstantsMod
      class (LatLonGridFactory), intent(inout) :: this
      type (FileMetadata), intent(inout) :: metadata

      type (Variable) :: v
      
      ! Horizontal grid dimensions
      call metadata%add_dimension('lon', this%im_world)
      call metadata%add_dimension('lat', this%jm_world)

      ! Coordinate variables
      v = Variable(type=PFIO_REAL32, dimensions='lon')
      call v%add_attribute('long_name', 'longitude')
      call v%add_attribute('units', 'degrees_east')
      call v%add_const_value(UnlimitedEntity(MAPL_RADIANS_TO_DEGREES*this%get_longitudes()))
      call metadata%add_variable('lon', v)

      v = Variable(type=PFIO_REAL32, dimensions='lat')
      call v%add_attribute('long_name', 'latitude')
      call v%add_attribute('units', 'degrees_north')
      call v%add_const_value(UnlimitedEntity(MAPL_RADIANS_TO_DEGREES*this%get_latitudes()))
      call metadata%add_variable('lat', v)

   end subroutine append_metadata

   function get_grid_vars(this) result(vars)
      class (LatLonGridFactory), intent(inout) :: this

      character(len=:), allocatable :: vars
      _UNUSED_DUMMY(this)

      vars = 'lon,lat'

   end function get_grid_vars

   subroutine append_variable_metadata(this,var)
      class (LatLonGridFactory), intent(inout) :: this
      type(Variable), intent(inout) :: var
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(var)
   end subroutine append_variable_metadata

   subroutine generate_file_bounds(this,grid,local_start,global_start,global_count,rc)
      use MAPL_BaseMod
      class(LatLonGridFactory), intent(inout) :: this
      type(ESMF_Grid),      intent(inout) :: grid
      integer, allocatable, intent(out) :: local_start(:)
      integer, allocatable, intent(out) :: global_start(:)
      integer, allocatable, intent(out) :: global_count(:)
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
      class (LatLonGridFactory), intent(inout) :: this
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
      class(LatLonGridFactory), intent(inout) :: this
      real, pointer, intent(in) :: fpointer(:,:)
      _UNUSED_DUMMY(this)
      ref = ArrayReference(fpointer)
   end function generate_file_reference2D
      
   function generate_file_reference3D(this,fpointer) result(ref)
      use pFIO
      type(ArrayReference) :: ref
      class(LatLonGridFactory), intent(inout) :: this
      real, pointer, intent(in) :: fpointer(:,:,:)
      _UNUSED_DUMMY(this)
      ref = ArrayReference(fpointer)
   end function generate_file_reference3D
      

end module MAPL_LatLonGridFactoryMod
