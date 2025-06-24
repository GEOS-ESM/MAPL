#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_TripolarGridFactoryMod
   use MAPL_AbstractGridFactoryMod
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_ShmemMod
   use MAPL_Constants
   use ESMF
   use pFIO
   use NetCDF
   use, intrinsic :: iso_fortran_env, only: REAL32
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none
   private

   public :: TripolarGridFactory

   integer, parameter :: NUM_DIM = 2

   type, extends(AbstractGridFactory) :: TripolarGridFactory
      private
      character(len=:), allocatable :: grid_file_name
      character(len=:), allocatable :: grid_name
      ! Grid dimensions
      integer :: im_world = MAPL_UNDEFINED_INTEGER
      integer :: jm_world = MAPL_UNDEFINED_INTEGER
      integer :: lm
      ! Domain decomposition:
      integer :: nx = MAPL_UNDEFINED_INTEGER
      integer :: ny = MAPL_UNDEFINED_INTEGER
      integer, allocatable :: ims(:)
      integer, allocatable :: jms(:)

      ! Used for halo
      type (ESMF_DELayout) :: layout
      integer :: px, py
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
      procedure :: generate_file_bounds
      procedure :: generate_file_corner_bounds
      procedure :: generate_file_reference2D
      procedure :: generate_file_reference3D
      procedure :: decomps_are_equal
      procedure :: physical_params_are_equal
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
        & im_world,jm_world,lm,nx, ny, rc) result(factory)
      type (TripolarGridFactory) :: factory
      class (KeywordEnforcer), optional, intent(in) :: unusable

      ! grid details:
      character(len=*), intent(in) :: grid_file_name ! required
      character(len=*), optional, intent(in) :: grid_name
      integer, optional, intent(in) :: im_world
      integer, optional, intent(in) :: jm_world
      integer, optional, intent(in) :: lm

      ! decomposition:
      integer, optional, intent(in) :: nx
      integer, optional, intent(in) :: ny
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'TripolarGridFactory_from_parameters'


      if (present(unusable)) print*,shape(unusable)

      call set_with_default(factory%grid_name, grid_name, MAPL_GRID_NAME_DEFAULT)
      call set_with_default(factory%grid_file_name, grid_file_name, MAPL_GRID_FILE_NAME_DEFAULT)

      call set_with_default(factory%ny, nx, MAPL_UNDEFINED_INTEGER)
      call set_with_default(factory%nx, ny, MAPL_UNDEFINED_INTEGER)
      call set_with_default(factory%im_world, im_world, MAPL_UNDEFINED_INTEGER)
      call set_with_default(factory%jm_world, jm_world, MAPL_UNDEFINED_INTEGER)
      call set_with_default(factory%lm, lm, MAPL_UNDEFINED_INTEGER)




      call factory%check_and_fill_consistency(rc=status)
      _verify(status)

      _return(_success)

   end function TripolarGridFactory_from_parameters


   function make_new_grid(this, unusable, rc) result(grid)
      type (ESMF_Grid) :: grid
      class (TripolarGridFactory), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'make_geos_grid'

      _unused_dummy(unusable)

      grid = this%create_basic_grid(rc=status)
      _verify(status)

      call this%add_horz_coordinates_from_file(grid, rc=status)
      _verify(status)

      _return(_success)

   end function make_new_grid



   function create_basic_grid(this, unusable, rc) result(grid)
      type (ESMF_Grid) :: grid
      class (TripolarGridFactory), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'create_basic_grid'

      _unused_dummy(unusable)

       grid = ESMF_GridCreate1PeriDim( &
            name=trim(this%grid_name) ,&
            countsPerDEDim1=this%ims, &
            countsPerDEDim2=this%jms, &
            indexFlag=ESMF_INDEX_DELOCAL, &
            gridEdgeLWidth=[0,0], &
            gridEdgeUWidth=[0,1], &
            coordDep1=[1,2], &
            coordDep2=[1,2], &
            poleKindFlag=[ESMF_POLEKIND_MONOPOLE,ESMF_POLEKIND_BIPOLE], &
            coordSys=ESMF_COORDSYS_SPH_RAD, rc=status)
      _verify(status)

      ! Allocate coords at default stagger location
      call ESMF_GridAddCoord(grid, rc=status)
      _verify(status)
      call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=status)
      _verify(status)

      if (this%lm /= MAPL_UNDEFINED_INTEGER) then
         call ESMF_AttributeSet(grid, name='GRID_LM', value=this%lm, rc=status)
         _verify(status)
      end if

      call ESMF_AttributeSet(grid, 'GridType', 'Tripolar', rc=status)
      _verify(status)

      _return(_success)
   end function create_basic_grid

   subroutine add_horz_coordinates_from_file(this, grid, unusable, rc)
      use MAPL_BaseMod, only: MAPL_grid_interior, MAPL_gridget
      use MAPL_CommsMod
      use MAPL_IOMod
      use MAPL_Constants
      class (TripolarGridFactory), intent(in) :: this
      type (ESMF_Grid), intent(inout) :: grid
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'add_horz_coordinates'

      integer :: i_1,i_n,j_1,j_n, ncid, varid
      integer :: ic_1,ic_n,jc_1,jc_n ! regional corner bounds
      real, pointer :: centers(:,:), corners(:,:)
      real(ESMF_KIND_R8), pointer :: fptr(:,:)

      integer :: IM, JM
      integer :: IM_WORLD, JM_WORLD
      integer :: COUNTS(3), DIMS(3)
      character(len=:), allocatable :: lon_center_name, lat_center_name, lon_corner_name, lat_corner_name

      _unused_dummy(unusable)

       if (this%initialized_from_metadata) then
          lon_center_name = "lons"
          lat_center_name = "lats"
          lon_corner_name = "corner_lons"
          lat_corner_name = "corner_lats"
       else
          lon_center_name = "lon_centers"
          lat_center_name = "lat_centers"
          lon_corner_name = "lon_corners"
          lat_corner_name = "lat_corners"
       end if
       call MAPL_GridGet(GRID, localCellCountPerDim=COUNTS, globalCellCountPerDim=DIMS, RC=STATUS)
       _verify(STATUS)
       IM = COUNTS(1)
       JM = COUNTS(2)
       IM_WORLD = DIMS(1)
       JM_WORLD = DIMS(2)
       call MAPL_Grid_Interior(grid, i_1, i_n, j_1, j_n)

       ic_1=i_1
       ic_n=i_n

       jc_1=j_1
       if (j_n == this%jm_world) then
          jc_n=j_n+1
       else
          jc_n=j_n
       end if

       if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
          status = nf90_open(this%grid_file_name,NF90_NOWRITE,ncid)
          _verify(status)
       end if

       call MAPL_AllocateShared(centers,[im_world,jm_world],transroot=.true.,_rc)

       call MAPL_SyncSharedMemory(_rc)

       ! do longitudes
       if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
          status = nf90_inq_varid(ncid,lon_center_name,varid)
          _verify(status)
          status = nf90_get_var(ncid,varid,centers)
          _verify(status)
          centers=centers*MAPL_DEGREES_TO_RADIANS_R8
       end if
       call MAPL_SyncSharedMemory(_rc)

       call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
          staggerloc=ESMF_STAGGERLOC_CENTER, &
          farrayPtr=fptr, rc=status)
       fptr=centers(i_1:i_n,j_1:j_n)
       ! do latitudes

       call MAPL_SyncSharedMemory(_rc)
       if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
          status = nf90_inq_varid(ncid,lat_center_name,varid)
          _verify(status)
          status = nf90_get_var(ncid,varid,centers)
          _verify(status)
           centers=centers*MAPL_DEGREES_TO_RADIANS_R8
       end if
       call MAPL_SyncSharedMemory(_rc)

       call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
          staggerloc=ESMF_STAGGERLOC_CENTER, &
          farrayPtr=fptr, rc=status)
       fptr=centers(i_1:i_n,j_1:j_n)

       call MAPL_SyncSharedMemory(_rc)
       if(MAPL_ShmInitialized) then
          call MAPL_DeAllocNodeArray(centers,_rc)
       else
          deallocate(centers)
       end if
       ! now repeat for corners
       call MAPL_AllocateShared(corners,[im_world+1,jm_world+1],transroot=.true.,_rc)

       ! do longitudes

       call MAPL_SyncSharedMemory(_rc)
       if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
          status = nf90_inq_varid(ncid,lon_corner_name,varid)
          _verify(status)
          status = nf90_get_var(ncid,varid,corners)
          _verify(status)
          corners=corners*MAPL_DEGREES_TO_RADIANS_R8
       end if
       call MAPL_SyncSharedMemory(_rc)

       call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
          staggerloc=ESMF_STAGGERLOC_CORNER, &
          farrayPtr=fptr, _rc)
       fptr=corners(ic_1:ic_n,jc_1:jc_n)
       ! do latitudes

       call MAPL_SyncSharedMemory(_rc)
       if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
          status = nf90_inq_varid(ncid,lat_corner_name,varid)
          _verify(status)
          status = nf90_get_var(ncid,varid,corners)
          _verify(status)
          corners=corners*MAPL_DEGREES_TO_RADIANS_R8
       end if
       call MAPL_SyncSharedMemory(_rc)

       call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
          staggerloc=ESMF_STAGGERLOC_CORNER, &
          farrayPtr=fptr, _rc)
       fptr=corners(ic_1:ic_n,jc_1:jc_n)

       call MAPL_SyncSharedMemory(_rc)
       if(MAPL_ShmInitialized) then
          call MAPL_DeAllocNodeArray(corners,_rc)
       else
          deallocate(corners)
       end if

      if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
         status=nf90_close(ncid)
         _verify(status)
      end if

      _return(_success)

   end subroutine add_horz_coordinates_from_file

   subroutine initialize_from_file_metadata(this, file_metadata, unusable, force_file_coordinates, rc)
      use MAPL_KeywordEnforcerMod
      use MAPL_BaseMod, only: MAPL_DecomposeDim
      class (TripolarGridFactory), intent(inout)  :: this
      type (FileMetadata), target, intent(in) :: file_metadata
      class (KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in) :: force_file_coordinates
      integer, optional, intent(out) :: rc

      integer :: status

      this%im_world = file_metadata%get_dimension('Xdim',_rc)
      this%jm_world = file_Metadata%get_dimension('Ydim',_rc)
      if (file_metadata%has_dimension('lev')) then
         this%lm = file_metadata%get_dimension('lev',_rc)
      end if

      this%grid_file_name=file_metadata%get_source_file()

      this%initialized_from_metadata = .true.
      call this%make_arbitrary_decomposition(this%nx, this%ny, rc=status)
      _verify(status)

      ! Determine IMS and JMS with constraint for ESMF that each DE has at least an extent
      ! of 2.  Required for ESMF_FieldRegrid().
      allocate(this%ims(0:this%nx-1))
      allocate(this%jms(0:this%ny-1))
      call MAPL_DecomposeDim(this%im_world, this%ims, this%nx, min_DE_extent=2)
      call MAPL_DecomposeDim(this%jm_world, this%jms, this%ny, min_DE_extent=2)

      call this%check_and_fill_consistency(rc=status)
      _verify(status)

      _unused_dummy(this)
      _unused_dummy(unusable)
      _unused_dummy(rc)
      _unused_dummy(force_file_coordinates)

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

      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'GRIDNAME:', default=MAPL_GRID_NAME_DEFAULT)
      this%grid_name = trim(tmp)

      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'GRIDSPEC:', rc=status)
      _verify(status)
      this%grid_file_name = trim(tmp)

      call ESMF_ConfigGetAttribute(config, this%nx, label=prefix//'NX:', default=MAPL_UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%ny, label=prefix//'NY:', default=MAPL_UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%im_world, label=prefix//'IM_WORLD:', default=MAPL_UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%jm_world, label=prefix//'JM_WORLD:', default=MAPL_UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%lm, label=prefix//'LM:', default=MAPL_UNDEFINED_INTEGER)

      call this%check_and_fill_consistency(rc=status)

      call this%init_halo()

      _return(_success)

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
         _verify(status)
         if (.not. isPresent) then
            _return(_success)
         end if

         ! First pass:  count values
         n = 0
         do
            call ESMF_ConfigGetAttribute(config, tmp, rc=status)
            if (status /= _success) then
               exit
            else
               n = n + 1
            end if
         end do

         ! Second pass: allocate and fill
         allocate(values(n), stat=status) ! no point in checking status
         _verify(status)
         call ESMF_ConfigFindLabel(config, label=prefix//label,rc=status)
         _verify(status)
         do i = 1, n
            call ESMF_ConfigGetAttribute(config, values(i), rc=status)
            _verify(status)
         end do

         _return(_success)

      end subroutine get_multi_integer

   end subroutine initialize_from_config_with_prefix



   function to_string(this) result(string)
      character(len=:), allocatable :: string
      class (TripolarGridFactory), intent(in) :: this

      _unused_dummy(this)
      string = 'TripolarGridFactory'

   end function to_string



   subroutine check_and_fill_consistency(this, unusable, rc)
      use MAPL_BaseMod, only: MAPL_DecomposeDim
      class (TripolarGridFactory), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'check_and_fill_consistency'

      _unused_dummy(unusable)

      if (.not. allocated(this%grid_name)) then
         this%grid_name = MAPL_GRID_NAME_DEFAULT
      end if

      ! Check decomposition/bounds
      ! local extents
      call verify(this%nx, this%im_world, this%ims, rc=status)
      call verify(this%ny, this%jm_world, this%jms, rc=status)
      !this%ims = spread(this%im_world / this%nx, 1, this%nx)
      !this%jms = spread(this%jm_world / this%ny, 1, this%ny)

      _return(_success)

   contains

      subroutine verify(n, m_world, ms, rc)
         integer, intent(inout) :: n
         integer, intent(inout) :: m_world
         integer, allocatable, intent(inout) :: ms(:)
         integer, optional, intent(out) :: rc

         integer :: status

         if (allocated(ms)) then
            _assert(size(ms) > 0,"needs message")

            if (n == MAPL_UNDEFINED_INTEGER) then
               n = size(ms)
            else
               _assert(n == size(ms),"needs message")
            end if

            if (m_world == MAPL_UNDEFINED_INTEGER) then
               m_world = sum(ms)
            else
               _assert(m_world == sum(ms),"needs message")
            end if

         else

            _assert(n /= MAPL_UNDEFINED_INTEGER,"needs message")
            _assert(m_world /= MAPL_UNDEFINED_INTEGER,"needs message")
            allocate(ms(n), stat=status)
            _verify(status)
            call MAPL_DecomposeDim(m_world, ms, n)

         end if

         _return(_success)

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
      use MAPL_Constants, only: PI => MAPL_PI_R8
      class (TripolarGridFactory), intent(inout)  :: this
      type (ESMF_DistGrid), intent(in) :: dist_grid
      type (ESMF_LocalArray), intent(in) :: lon_array
      type (ESMF_LocalArray), intent(in) :: lat_array
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME // 'initialize_from_esmf_distGrid'

      _unused_dummy(this)
      _unused_dummy(unusable)
      _unused_dummy(dist_grid)
      _unused_dummy(lon_array)
      _unused_dummy(lat_array)


      ! not supported
      _fail("tripolar initialize from distgrid non supported")

   end subroutine initialize_from_esmf_distGrid

   function decomps_are_equal(this,a) result(equal)
      class (TripolarGridFactory), intent(in) :: this
      class (AbstractGridFactory), intent(in) :: a
      logical :: equal

      select type (a)
      class default
         equal = .false.
         return
      class is (TripolarGridFactory)
         equal = .true.

         ! same decomposition
         equal = a%nx == this%nx .and. a%ny == this%ny
         if (.not. equal) return

      end select

   end function decomps_are_equal


   function physical_params_are_equal(this, a) result(equal)
      class (TripolarGridFactory), intent(in) :: this
      class (AbstractGridFactory), intent(in) :: a
      logical :: equal

      select type (a)
      class default
         equal = .false.
         return
      class is (TripolarGridFactory)
         equal = .true.

         !equal = (a%grid_file_name == this%grid_file_name)
         !if (.not. equal) return

         equal = (a%im_world == this%im_world) .and. (a%jm_world == this%jm_world)
         if (.not. equal) return

      end select

   end function physical_params_are_equal


   logical function equals(a, b)
      class (TripolarGridFactory), intent(in) :: a
      class (AbstractGridFactory), intent(in) :: b

      select type (b)
      class default
         equals = .false.
         return
      class is (TripolarGridFactory)
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
      class (TripolarGridFactory), intent(in) :: this

      _unused_dummy(this)

      name = ''
      ! needs to be implemented
      error stop -1

   end function generate_grid_name

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

      _unused_dummy(unusable)

      grid = this%make_grid(rc=status)
      _verify(status)

      call ESMF_GridGet(grid,   distGrid=dist_grid, dimCount=dim_count, rc=status)
      _verify(status)
      call ESMF_DistGridGet(dist_grid, delayout=this%layout, rc=status)
      _verify(status)

      call ESMF_DELayoutGet (this%layout, vm=vm, rc=status)
      _verify(status)

      call ESMF_VmGet(vm, localPet=pet, petCount=ndes, rc=status)
      _verify(status)

      this%px = mod(pet, this%nx)
      this%py = pet / this%nx

      _return(_success)

   end subroutine init_halo


   subroutine halo(this, array, unusable, halo_width, rc)
      use MAPL_CommsMod
      use mpi
      class (TripolarGridFactory), intent(inout) :: this
      real(kind=REAL32), intent(inout) :: array(:,:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: halo_width
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'halo'

      integer :: pet_north
      integer :: pet_south
      integer :: pet_east
      integer :: pet_west

      _unused_dummy(unusable)
      ! not yet implmented, default halo_width=1
      _unused_dummy(halo_width)
      associate (nx => this%nx, ny => this%ny, px => this%px, py => this%py)
        ! Nearest neighbors processor' ids
        pet_north = get_pet(px, py+1, nx, ny)
        pet_south = get_pet(px, py-1, nx, ny)
        pet_east  = get_pet(px+1, py, nx, ny)
        pet_west  = get_pet(px-1, py, nx, ny)

        call fill_south(array, rc=status)
        _verify(status)

        call fill_north(array, rc=status)
        _verify(status)

        call fill_east(array, rc=status)
        _verify(status)

        call fill_west(array, rc=status)
        _verify(status)

      end associate

      _return(ESMF_SUCCESS)

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
            _verify(status)
         else
            call MAPL_CommsSendRecv(this%layout,        &
                 array(:,last     ),  len,  pet_north,  &
                 array(:,last+1   ),  len,  pet_north,  &
                 rc=status)
            _verify(status)
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

         _return(_success)

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
         _verify(status)

         if(this%py==0) then
            array(:,1   ) = MAPL_UNDEF
         endif

         _return(_success)

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
         _verify(status)

         _return(_success)

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
         _verify(status)

         _return(_success)

      end subroutine fill_west


   end subroutine halo

   subroutine append_metadata(this, metadata)
      class (TripolarGridFactory), intent(inout) :: this
      type (FileMetadata), intent(inout) :: metadata

      type (Variable) :: v
      real(kind=real64), allocatable :: fake_coord(:)
      integer :: i

      call metadata%add_dimension('Xdim', this%im_world)
      call metadata%add_dimension('Ydim', this%jm_world)
      call metadata%add_dimension('XCdim', this%im_world+1)
      call metadata%add_dimension('YCdim', this%jm_world+1)

      allocate(fake_coord(this%im_world))
      do i=1,this%im_world
         fake_coord(i)=dble(i)
      enddo

      ! Coordinate variables
      v = Variable(type=PFIO_REAL64, dimensions='Xdim')
      call v%add_attribute('long_name', 'Fake Longitude for GrADS Compatibility')
      call v%add_attribute('units', 'degrees_east')
      call v%add_const_value(UnlimitedEntity(fake_coord))
      call metadata%add_variable('Xdim', v)
      deallocate(fake_coord)

      allocate(fake_coord(this%jm_world))
      do i=1,this%jm_world
         fake_coord(i)=dble(i)
      enddo

      v = Variable(type=PFIO_REAL64, dimensions='Ydim')
      call v%add_attribute('long_name', 'Fake Latitude for GrADS Compatibility')
      call v%add_attribute('units', 'degrees_north')
      call v%add_const_value(UnlimitedEntity(fake_coord))
      call metadata%add_variable('Ydim', v)
      deallocate(fake_coord)

      v = Variable(type=PFIO_REAL64, dimensions='Xdim,Ydim')
      call v%add_attribute('long_name','longitude')
      call v%add_attribute('units','degrees_east')
      call metadata%add_variable('lons',v)

      v = Variable(type=PFIO_REAL64, dimensions='Xdim,Ydim')
      call v%add_attribute('long_name','latitude')
      call v%add_attribute('units','degrees_north')
      call metadata%add_variable('lats',v)

      v = Variable(type=PFIO_REAL64, dimensions='XCdim,YCdim')
      call v%add_attribute('long_name','longitude')
      call v%add_attribute('units','degrees_east')
      call metadata%add_variable('corner_lons',v)

      v = Variable(type=PFIO_REAL64, dimensions='XCdim,YCdim')
      call v%add_attribute('long_name','latitude')
      call v%add_attribute('units','degrees_north')
      call metadata%add_variable('corner_lats',v)

     call metadata%add_attribute('grid_type','Tripolar')

   end subroutine append_metadata

   function get_grid_vars(this) result(vars)
      class (TripolarGridFactory), intent(inout) :: this

      character(len=:), allocatable :: vars
      _unused_dummy(this)

      vars = 'Xdim,Ydim'

   end function get_grid_vars

   function get_file_format_vars(this) result(vars)
      class (TripolarGridFactory), intent(inout) :: this

      character(len=:), allocatable :: vars
      _unused_dummy(this)

      vars = 'Xdim,Ydim,lons,lats,corner_lons,corner_lats'

   end function get_file_format_vars

   subroutine append_variable_metadata(this,var)
      class (TripolarGridFactory), intent(inout) :: this
      type(Variable), intent(inout) :: var
      _unused_dummy(this)
      _unused_dummy(var)
   end subroutine append_variable_metadata

   subroutine generate_file_bounds(this,grid,local_start,global_start,global_count,metadata,rc)
      use MAPL_BaseMod
      class(TripolarGridFactory), intent(inout) :: this
      type(ESMF_Grid),      intent(inout) :: grid
      integer, allocatable, intent(out) :: local_start(:)
      integer, allocatable, intent(out) :: global_start(:)
      integer, allocatable, intent(out) :: global_count(:)
      type(FileMetaData), intent(in), optional :: metaData
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: global_dim(3), i1,j1,in,jn
      character(len=*), parameter :: Iam = MOD_NAME // 'generate_file_bounds'

      call MAPL_GridGet(grid,globalCellCountPerDim=global_dim,rc=status)
      _verify(status)
      call MAPL_GridGetInterior(grid,i1,in,j1,jn)
      allocate(local_start,source=[i1,j1])
      allocate(global_start,source=[1,1])
      allocate(global_count,source=[global_dim(1),global_dim(2)])

      _unused_dummy(this)
      _unused_dummy(metadata)
   end subroutine generate_file_bounds

   subroutine generate_file_corner_bounds(this,grid,local_start,global_start,global_count,rc)
      use MAPL_BaseMod
      class (TripolarGridFactory), intent(inout) :: this
      type(ESMF_Grid), intent(inout)      :: grid
      integer, allocatable, intent(out) :: local_start(:)
      integer, allocatable, intent(out) :: global_start(:)
      integer, allocatable, intent(out) :: global_count(:)
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME // 'generate_file_corner_bounds'

      integer :: status
      integer :: global_dim(3), i1,j1,in,jn

      call MAPL_GridGet(grid,globalCellCountPerDim=global_dim,rc=status)
      _verify(status)
      call MAPL_GridGetInterior(grid,i1,in,j1,jn)
      allocate(local_start,source=[i1,j1])
      allocate(global_start,source=[1,1])
      allocate(global_count,source=[global_dim(1)+1,global_dim(2)+1])

      _unused_dummy(this)

   end subroutine generate_file_corner_bounds

   function generate_file_reference2D(this,fpointer) result(ref)
      use pFIO
      type(ArrayReference) :: ref
      class(TripolarGridFactory), intent(inout) :: this
      real, pointer, intent(in) :: fpointer(:,:)
      ref = ArrayReference(fpointer)
      _unused_dummy(this)
   end function generate_file_reference2D

   function generate_file_reference3D(this,fpointer,metadata) result(ref)
      use pFIO
      type(ArrayReference) :: ref
      class(TripolarGridFactory), intent(inout) :: this
      real, pointer, intent(in) :: fpointer(:,:,:)
      type(FileMetaData), intent(in), optional :: metaData
      ref = ArrayReference(fpointer)
      _unused_dummy(this)
      _unused_dummy(metadata)
   end function generate_file_reference3D

end module MAPL_TripolarGridFactoryMod
