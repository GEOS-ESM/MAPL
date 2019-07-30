!NOTES

! overload set interfaces in legacy
! Document PE, PC, DC, DE, GC


#define _SUCCESS      0
#define _FAILURE     1
#define _VERIFY(A)   if(  A/=0) then; if(present(rc)) rc=A; PRINT *, Iam, __LINE__; return; endif
#define _ASSERT(A)   if(.not.(A)) then; if(present(rc)) rc=_FAILURE; PRINT *, Iam, __LINE__; return; endif
#define _RETURN(A)   if(present(rc)) rc=A; return
#include "unused_dummy.H"


! This module generates ESMF_Grids corresponding to _regular_ lat-lon coordinate grids.
! I.e., spacing between lats (lons) is constant.

module MAPL_TripolarGridFactoryMod
   use MAPL_AbstractGridFactoryMod
   use MAPL_KeywordEnforcerMod
   use ESMF
   use, intrinsic :: iso_fortran_env, only: REAL32
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none
   private

   public :: TripolarGridFactory

   integer, parameter :: NUM_DIM = 2
   integer, parameter :: UNDEFINED_INTEGER = 1-huge(1)
   character(len=*), parameter :: UNDEFINED_CHAR = '**'

   character(len=*), parameter :: GRID_NAME_DEFAULT = 'UNKNOWN'

   type, extends(AbstractGridFactory) :: TripolarGridFactory
      private
      character(len=:), allocatable :: grid_file_name
      character(len=:), allocatable :: grid_name
      ! Grid dimensions
      integer :: im_world = UNDEFINED_INTEGER
      integer :: jm_world = UNDEFINED_INTEGER
      integer :: lm
      ! Domain decomposition:
      integer :: nx = UNDEFINED_INTEGER
      integer :: ny = UNDEFINED_INTEGER
      integer, allocatable :: ims(:)
      integer, allocatable :: jms(:)
      ! Used for halo
      type (ESMF_DELayout) :: layout
      integer :: px, py
      ! global coords
   contains
      procedure :: make_new_grid
      procedure :: create_basic_grid
      procedure :: add_horz_coordinates
      procedure :: init_halo
      procedure :: halo
      

      procedure :: initialize_from_file_metadata
      procedure :: initialize_from_config_with_prefix
      procedure :: initialize_from_esmf_distGrid

      procedure :: equals

      procedure :: read_grid_dimensions
      procedure :: read_grid_coordinates
      procedure :: check_and_fill_consistency
      procedure :: generate_grid_name
      procedure :: to_string

      procedure :: append_metadata
      procedure :: get_grid_vars
      procedure :: append_variable_metadata
   end type TripolarGridFactory
   
   character(len=*), parameter :: MOD_NAME = 'MAPL_TripolarGridFactory::'
   
   interface TripolarGridFactory
      module procedure TripolarGridFactory_from_parameters
   end interface TripolarGridFactory

   interface set_with_default
      module procedure set_with_default_integer
      module procedure set_with_default_character
   end interface set_with_default


contains


   function TripolarGridFactory_from_parameters(unusable, grid_file_name, grid_name, &
        & nx, ny, rc) result(factory)
      type (TripolarGridFactory) :: factory
      class (KeywordEnforcer), optional, intent(in) :: unusable

      ! grid details:
      character(len=*), intent(in) :: grid_file_name ! required
      character(len=*), optional, intent(in) :: grid_name

      ! decomposition:
      integer, optional, intent(in) :: nx
      integer, optional, intent(in) :: ny
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'TripolarGridFactory_from_parameters'
      logical :: exists
      
      if (present(unusable)) print*,shape(unusable)

      call set_with_default(factory%grid_name, grid_name, GRID_NAME_DEFAULT)

      call set_with_default(factory%ny, nx, UNDEFINED_INTEGER)
      call set_with_default(factory%nx, ny, UNDEFINED_INTEGER)

      factory%grid_file_name = grid_file_name
      inquire(file=grid_file_name, exist=exists)
      _ASSERT(exists)

      call factory%read_grid_dimensions()

      call factory%check_and_fill_consistency(rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)

   end function TripolarGridFactory_from_parameters


   function make_new_grid(this, unusable, rc) result(grid)
      type (ESMF_Grid) :: grid
      class (TripolarGridFactory), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'make_geos_grid'

      _UNUSED_DUMMY(unusable)

      grid = this%create_basic_grid(rc=status)
      _VERIFY(status)

      call this%add_horz_coordinates(grid, rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)

   end function make_new_grid


   
   function create_basic_grid(this, unusable, rc) result(grid)
      type (ESMF_Grid) :: grid
      class (TripolarGridFactory), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      
      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'create_basic_grid'

      _UNUSED_DUMMY(unusable)

      grid = ESMF_GridCreate( &
           & name = this%grid_name, &
           & countsPerDEDim1=this%ims, &
           & countsPerDEDim2=this%jms, &
           & indexFlag=ESMF_INDEX_DELOCAL, &
           & gridEdgeLWidth=[0,0], &
           & gridEdgeUWidth=[0,0], &
           & coordDep1=[1,2], &
           & coordDep2=[1,2], &
           & rc=status)
      _VERIFY(status)
      
      ! Allocate coords at default stagger location
      call ESMF_GridAddCoord(grid, rc=status)
      _VERIFY(status)
      
      if (this%lm /= UNDEFINED_INTEGER) then
         call ESMF_AttributeSet(grid, name='GRID_LM', value=this%lm, rc=status)
         _VERIFY(status)
      end if
      
      call ESMF_AttributeSet(grid, 'GridType', 'Tripolar', rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end function create_basic_grid

   subroutine add_horz_coordinates(this, grid, unusable, rc)
      use MAPL_BaseMod, only: MAPL_grid_interior
      use MAPL_CommsMod
      class (TripolarGridFactory), intent(in) :: this
      type (ESMF_Grid), intent(inout) :: grid
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      
      integer :: i_1, i_n, j_1, j_n ! regional array bounds
      real(kind=ESMF_KIND_R8), pointer :: centers(:,:)
      real(kind=ESMF_KIND_R8), allocatable :: longitudes(:,:)
      real(kind=ESMF_KIND_R8), allocatable :: latitudes(:,:)
      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'add_horz_coordinates'
      
      _UNUSED_DUMMY(unusable)

      call this%read_grid_coordinates(longitudes, latitudes)

      call MAPL_grid_interior(grid, i_1, i_n, j_1, j_n)

      ! First we handle longitudes:
      call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, &
           farrayPtr=centers, rc=status)
      _VERIFY(status)

      call ArrayScatter(centers, longitudes, grid, rc=status)
      _VERIFY(status)

      ! Now latitudes
      call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, &
           farrayPtr=centers, rc=status)
      _VERIFY(status)
      call ArrayScatter(centers, latitudes, grid, rc=status)
      _VERIFY(status)

      deallocate(longitudes, latitudes)
      _RETURN(_SUCCESS)

   end subroutine add_horz_coordinates


   subroutine initialize_from_file_metadata(this, file_metadata, unusable, rc)
      use pFIO_FileMetadataMod
      use pFIO_NetCDF4_FileFormatterMod
      use MAPL_KeywordEnforcerMod

      class (TripolarGridFactory), intent(inout)  :: this
      type (FileMetadata), target, intent(in) :: file_metadata
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam= MOD_NAME // 'initialize_from_file_metadata()'
      integer :: status

      _UNUSED_DUMMY(unusable)

   end subroutine initialize_from_file_metadata


   subroutine initialize_from_config_with_prefix(this, config, prefix, unusable, rc)
      use esmf
      class (TripolarGridFactory), intent(inout) :: this
      type (ESMF_Config), intent(inout) :: config
      character(len=*), intent(in) :: prefix  ! effectively optional due to overload without this argument
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME//'make_geos_grid_from_config'
      character(len=ESMF_MAXSTR) :: tmp

      if (present(unusable)) print*,shape(unusable)

      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'GRIDNAME:', default=GRID_NAME_DEFAULT)
      this%grid_name = trim(tmp)

      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'GRID_FILE_NAME:', rc=status)
      _VERIFY(status)
      this%grid_file_name = trim(tmp)
      call this%read_grid_dimensions()

      call ESMF_ConfigGetAttribute(config, this%nx, label=prefix//'NX:', default=UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%ny, label=prefix//'NY:', default=UNDEFINED_INTEGER)

      call ESMF_ConfigGetAttribute(config, this%lm, label=prefix//'LM:', default=UNDEFINED_INTEGER)

      call this%check_and_fill_consistency(rc=status)

      call this%init_halo()

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
         
         call ESMF_ConfigFindLabel(config, label=prefix//label,isPresent=isPresent,rc=status)
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

   end subroutine initialize_from_config_with_prefix
   


   function to_string(this) result(string)
      character(len=:), allocatable :: string
      class (TripolarGridFactory), intent(in) :: this

      _UNUSED_DUMMY(this)
      string = 'TripolarGridFactory'

   end function to_string



   subroutine check_and_fill_consistency(this, unusable, rc)
      use MAPL_BaseMod, only: MAPL_DecomposeDim
      class (TripolarGridFactory), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME // 'check_and_fill_consistency'

      _UNUSED_DUMMY(unusable)

      if (.not. allocated(this%grid_name)) then
         this%grid_name = GRID_NAME_DEFAULT
      end if

      ! Check decomposition/bounds
      ! Tripolar requires even divisibility
      _ASSERT(mod(this%im_world, this%nx) == 0)
      _ASSERT(mod(this%jm_world, this%ny) == 0)

      ! local extents
      this%ims = spread(this%im_world / this%nx, 1, this%nx)
      this%jms = spread(this%jm_world / this%ny, 1, this%ny)
      
      _RETURN(_SUCCESS)
         
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

   ! MAPL uses values in lon_array and lat_array only to determine the
   ! general positioning.  Actual coordinates are then recomputed.
   ! This helps to avoid roundoff differences from slightly different
   ! input files.
   subroutine initialize_from_esmf_distGrid(this, dist_grid, lon_array, lat_array, unusable, rc)
      use MAPL_ConfigMod
      use MAPL_ConstantsMod, only: PI => MAPL_PI_R8
      class (TripolarGridFactory), intent(inout)  :: this
      type (ESMF_DistGrid), intent(in) :: dist_grid
      type (ESMF_LocalArray), intent(in) :: lon_array
      type (ESMF_LocalArray), intent(in) :: lat_array
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME // 'initialize_from_esmf_distGrid'

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(dist_grid)
      _UNUSED_DUMMY(lon_array)
      _UNUSED_DUMMY(lat_array)

      
      ! not supported
      _ASSERT(.false.) 

   end subroutine initialize_from_esmf_distGrid

   

   logical function equals(a, b)
      class (TripolarGridFactory), intent(in) :: a
      class (AbstractGridFactory), intent(in) :: b

      select type (b)
      class default
         equals = .false.
         return
      class is (TripolarGridFactory)
         equals = .true.

         equals = (a%grid_file_name == b%grid_file_name)
         if (.not. equals) return

         equals = (a%im_world == b%im_world) .and. (a%jm_world == b%jm_world)
         if (.not. equals) return
         
         equals = (a%lm == b%lm)
         if (.not. equals) return
         
         ! same decomposition
         equals = a%nx == b%nx .and. a%ny == b%ny
         if (.not. equals) return
         
      end select
         
   end function equals


   function generate_grid_name(this) result(name)
      character(len=:), allocatable :: name
      class (TripolarGridFactory), intent(in) :: this

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(name)

      ! needs to be implemented
      error stop -1

   end function generate_grid_name


   subroutine read_grid_coordinates(this, longitudes, latitudes, unusable, rc)
      class (TripolarGridFactory), intent(in) :: this
      real(kind=REAL64), allocatable, intent(out) :: longitudes(:,:)
      real(kind=REAL64), allocatable, intent(out) :: latitudes(:,:)
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc

      include 'netcdf.inc'
      
      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'read_grid_coordinates()'

      integer :: xid, yid
      integer :: start(2), counts(2)
      integer :: pet, ndes
      logical :: i_am_root
      integer :: ncid
      type (ESMF_VM) :: vm

      real(kind=REAL64), allocatable :: lons(:,:), lats(:,:)

      _UNUSED_DUMMY(unusable)

      call ESMF_VMGetCurrent(vm, rc=status)
      _VERIFY(status)

      call ESMF_VMGet(vm, localpet=pet, petCount=ndes, rc=status)
      _VERIFY(status)

      i_am_root = (pet == 0)

      if (i_am_root) then
         allocate(longitudes(this%im_world, this%jm_world), stat=status)
         _VERIFY(status)
         allocate(latitudes(this%im_world, this%jm_world), stat=status)
         _VERIFY(status)

         ncid = ncopn(this%grid_file_name, NCNOWRIT, status)
         _VERIFY(status)

         xid = ncvid(ncid, 'x_T', status)
         _VERIFY(status)
         
         yid = ncvid(ncid, 'y_T', status)
         _VERIFY(status)
         
         call ncvgt(ncid, xid, start, counts, lons, status)
         _VERIFY(status)
         call ncvgt(ncid, yid, start, counts, lats, status)
         _VERIFY(status)

         call ncclos(ncid, status)
         _VERIFY(status)
      else
         allocate(longitudes(0,0))
         allocate(latitudes(0,0))
      end if

   end subroutine read_grid_coordinates

   subroutine read_grid_dimensions(this, unusable, rc)
      use MAPL_CommsMod
      class (TripolarGridFactory), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(out) :: unusable
      integer, optional, intent(out) :: rc

      include 'netcdf.inc'
      
      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'read_grid_dimensions()'

      integer :: xid
      character(len=128) :: name
      integer :: type
      integer :: n
      integer :: dims(MAXVDIMS)
      integer :: natt
      integer :: pet, ndes
      logical :: i_am_root
      integer :: ncid
      type (ESMF_VM) :: vm

      _UNUSED_DUMMY(unusable)

      call ESMF_VMGetCurrent(vm, rc=status)
      _VERIFY(status)

      call ESMF_VMGet(vm, localpet=pet, petCount=ndes, rc=status)
      _VERIFY(status)

      i_am_root = (pet == 0)

      if (i_am_root) then
         ncid = ncopn(this%grid_file_name, NCNOWRIT, status)
         _VERIFY(status)

         xid = ncvid(ncid, 'x_T', status)
         _VERIFY(status)
         
         call ncvinq (ncid, xid, name, type, n, dims, natt, status)
         _VERIFY(status)
         
         associate (im => this%im_world, jm => this%jm_world)
           call ncdinq(ncid, dims(1), name, im, status)
           _VERIFY(status)
           call ncdinq(ncid, dims(2), name, jm, status)
           _VERIFY(status)
         end associate
      end if

      call MAPL_CommsBCast(vm, this%im_world, 1, root=0, rc=status)
      _VERIFY(status)
      call MAPL_CommsBCast(vm, this%jm_world, 1, root=0, rc=status)
      _VERIFY(status)


   end subroutine read_grid_dimensions

   subroutine init_halo(this, unusable, rc)
      class (TripolarGridFactory), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type (ESMF_Grid) :: grid
      type (ESMF_DistGrid) :: dist_grid
      integer :: dim_count
      integer :: pet
      integer :: ndes
      type (ESMF_VM) :: vm

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'init_halo'
      
      _UNUSED_DUMMY(unusable)

      grid = this%make_grid(rc=status)
      _VERIFY(status)
      
      call ESMF_GridGet(grid,   distGrid=dist_grid, dimCount=dim_count, rc=status)
      _VERIFY(status)
      call ESMF_DistGridGet(dist_grid, delayout=this%layout, rc=status)
      _VERIFY(status)

      call ESMF_DELayoutGet (this%layout, vm=vm, rc=status)
      _VERIFY(status)

      call ESMF_VmGet(vm, localPet=pet, petCount=ndes, rc=status)
      _VERIFY(status)
      
      this%px = mod(pet, this%nx)
      this%py = pet / this%nx

      _RETURN(_SUCCESS)

   end subroutine init_halo


   subroutine halo(this, array, unusable,  halo_width, rc)
      use MAPL_CommsMod
      class (TripolarGridFactory), intent(inout) :: this
      real(kind=REAL32), intent(inout) :: array(:,:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: halo_width
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'halo'
      include 'mpif.h'

      integer :: pet_north
      integer :: pet_south
      integer :: pet_east
      integer :: pet_west

      _UNUSED_DUMMY(unusable)
      ! not yet implmented, default halo_width=1
      _UNUSED_DUMMY(halo_width)
      associate (nx => this%nx, ny => this%ny, px => this%px, py => this%py)
        ! Nearest neighbors processor' ids
        pet_north = get_pet(px, py+1, nx, ny)
        pet_south = get_pet(px, py-1, nx, ny)
        pet_east  = get_pet(px+1, py, nx, ny)
        pet_west  = get_pet(px-1, py, nx, ny)

        call fill_south(array, rc=status)
        _VERIFY(status)

        call fill_north(array, rc=status)
        _VERIFY(status)

        call fill_east(array, rc=status)
        _VERIFY(status)

        call fill_west(array, rc=status)
        _VERIFY(status)

      end associate

      _RETURN(ESMF_SUCCESS)

   contains

      ! Neighbor pet is more subtle here than for latlon.
      ! At the southern edge of the grid, there is no southern neighbor.
      ! At the northern edge, there grid folds on itself. TLC

      integer function get_pet(px, py, nx, ny) result(pet)
         integer, intent(in) :: px, py  ! rank in x/y directions
         integer, intent(in) :: nx, ny  ! npets in x/y directions

         if (py < 0) then ! off the south pole
            pet = MPI_PROC_NULL
         elseif (py == nx) then ! reflect on north pole
            pet = mod(nx + 1 - px,nx) + nx*(py-1)
         else
            pet = mod(px+nx,nx) + nx*mod(py+ny,ny)
         end if
         
      end function get_pet


      subroutine fill_north(array, rc)
         real(kind=REAL32), intent(inout) :: array(:,:)
         integer, optional, intent(out) :: rc

         integer :: status
         character(len=*), parameter :: Iam = MOD_NAME // 'fill_north'

         integer :: len, last

         last = size(array,2)-1 
         len = size(array,1)
         
         if(this%py==this%ny-1) then
            call MAPL_CommsSendRecv(this%layout,        &
                 array(:,2        ),  len,  pet_south,  &
                 array(:,last+1   ),  len,  pet_north,  &
                 rc=status)
            _VERIFY(status)
         else
            call MAPL_CommsSendRecv(this%layout,        &
                 array(:,last     ),  len,  pet_north,  &
                 array(:,last+1   ),  len,  pet_north,  &
                 rc=status)
            _VERIFY(status)
            ! reflect results
            block
              integer :: n, i, ii
              real(kind=REAL32) :: tmp
              n = size(array,1)
              do i = 1, n/2
                 ii = n + 1 - i
                 tmp = array(i,last+1)
                 array(i,last+1) = array(ii,last+1)
                 array(ii,last+1) = tmp
              end do
            end block
         end if
         
         _RETURN(_SUCCESS)

      end subroutine fill_north

     
      subroutine fill_south(array, rc)
         use MAPL_BaseMod, only: MAPL_UNDEF
         real(kind=REAL32), intent(inout) :: array(:,:)
         integer, optional, intent(out) :: rc

         integer :: status
         character(len=*), parameter :: Iam = MOD_NAME // 'fill_south'

         integer :: len, last

         last = size(array,2)-1 
         len = size(array,1)

         call MAPL_CommsSendRecv(this%layout,     &
              array(:,last     ),  len,  pet_north,  &
              array(:,1        ),  len,  pet_south,  &
              rc=status)
         _VERIFY(status)

         if(this%py==0) then
            array(:,1   ) = MAPL_UNDEF
         endif

         _RETURN(_SUCCESS)

      end subroutine fill_south


      subroutine fill_east(array, rc)
         real(kind=REAL32), intent(inout) :: array(:,:)
         integer, optional, intent(out) :: rc

         integer :: status
         character(len=*), parameter :: Iam = MOD_NAME // 'fill_east'
         
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
         character(len=*), parameter :: Iam = MOD_NAME // 'fill_west'

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
      use pFIO
      class (TripolarGridFactory), intent(inout) :: this
      type (FileMetadata), intent(inout) :: metadata

      call metadata%add_dimension('lon', this%im_world)
      call metadata%add_dimension('lat', this%jm_world)


   end subroutine append_metadata

   function get_grid_vars(this) result(vars)
      use pFIO
      class (TripolarGridFactory), intent(inout) :: this

      character(len=:), allocatable :: vars

      vars = 'lon,lat'

   end function get_grid_vars

   subroutine append_variable_metadata(this,var)
      use pFIO_VariableMod
      class (TripolarGridFactory), intent(inout) :: this
      type(Variable), intent(inout) :: var
   end subroutine append_variable_metadata


end module MAPL_TripolarGridFactoryMod
