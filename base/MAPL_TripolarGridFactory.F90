#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_TripolarGridFactoryMod
   use MAPL_AbstractGridFactoryMod
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_ShmemMod
   use ESMF
   use pFIO
   use, intrinsic :: iso_fortran_env, only: REAL32
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none
   private

   public :: TripolarGridFactory

   integer, parameter :: NUM_DIM = 2
   integer, parameter :: UNDEFINED_INTEGER = 1-huge(1)
   character(len=*), parameter :: UNDEFINED_CHAR = '**'

   character(len=*), parameter :: GRID_NAME_DEFAULT = 'UNKNOWN'
   character(len=*), parameter :: GRID_FILE_NAME_DEFAULT = 'UNKNOWN'

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

      procedure :: check_and_fill_consistency
      procedure :: generate_grid_name
      procedure :: to_string

      procedure :: append_metadata
      procedure :: get_grid_vars
      procedure :: append_variable_metadata
      procedure :: generate_file_bounds
      procedure :: generate_file_corner_bounds
      procedure :: generate_file_reference2D
      procedure :: generate_file_reference3D
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

      call set_with_default(factory%grid_name, grid_name, GRID_NAME_DEFAULT)
      call set_with_default(factory%grid_file_name, grid_file_name, GRID_FILE_NAME_DEFAULT)

      call set_with_default(factory%ny, nx, UNDEFINED_INTEGER)
      call set_with_default(factory%nx, ny, UNDEFINED_INTEGER)
      call set_with_default(factory%im_world, im_world, UNDEFINED_INTEGER)
      call set_with_default(factory%jm_world, jm_world, UNDEFINED_INTEGER)
      call set_with_default(factory%lm, lm, UNDEFINED_INTEGER)




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
      _VERIFY(status)
      
      ! Allocate coords at default stagger location
      call ESMF_GridAddCoord(grid, rc=status)
      _VERIFY(status)
      call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=status)
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
      use MAPL_BaseMod, only: MAPL_grid_interior, MAPL_gridget
      use MAPL_CommsMod
      use MAPL_IOMod
      use NetCDF
      use MAPL_ConstantsMod
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

      _UNUSED_DUMMY(unusable)

       call MAPL_GridGet(GRID, localCellCountPerDim=COUNTS, globalCellCountPerDim=DIMS, RC=STATUS)
       _VERIFY(STATUS)
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
          _VERIFY(status)
       end if

       call MAPL_AllocateShared(centers,[im_world,jm_world],transroot=.true.,__RC__)

       !allocate(centers(im_world,jm_world),__STAT__)
       call MAPL_SyncSharedMemory(__RC__)

       ! do longitudes
       if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
          status = nf90_inq_varid(ncid,'lon_centers',varid)
          _VERIFY(status)
          status = nf90_get_var(ncid,varid,centers)
          _VERIFY(status)
          centers=centers*MAPL_PI_R8/180.d0
       end if
       call MAPL_SyncSharedMemory(__RC__)
 
       call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
          staggerloc=ESMF_STAGGERLOC_CENTER, &
          farrayPtr=fptr, rc=status)
       fptr=centers(i_1:i_n,j_1:j_n)
       ! do latitudes

       call MAPL_SyncSharedMemory(__RC__)
       if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
          status = nf90_inq_varid(ncid,'lat_centers',varid)
          _VERIFY(status)
          status = nf90_get_var(ncid,varid,centers)
          _VERIFY(status)
           centers=centers*MAPL_PI_R8/180.d0
       end if
       call MAPL_SyncSharedMemory(__RC__)

       call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
          staggerloc=ESMF_STAGGERLOC_CENTER, &
          farrayPtr=fptr, rc=status)
       fptr=centers(i_1:i_n,j_1:j_n)

       call MAPL_SyncSharedMemory(__RC__)
       if(MAPL_ShmInitialized) then
          call MAPL_DeAllocNodeArray(centers,__RC__)
       else
          deallocate(centers)
       end if
       ! now repeat for corners
       !allocate(corners(im_world+1,jm_world+1),__STAT__)
       call MAPL_AllocateShared(corners,[im_world+1,jm_world+1],transroot=.true.,__RC__)

       ! do longitudes

       call MAPL_SyncSharedMemory(__RC__)
       if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
          status = nf90_inq_varid(ncid,'lon_corners',varid)
          _VERIFY(status)
          status = nf90_get_var(ncid,varid,corners)
          _VERIFY(status)
          corners=corners*MAPL_PI_R8/180.d0
       end if
       call MAPL_SyncSharedMemory(__RC__)

       call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
          staggerloc=ESMF_STAGGERLOC_CORNER, &
          farrayPtr=fptr, __RC__)
       fptr=corners(ic_1:ic_n,jc_1:jc_n)
       ! do latitudes

       call MAPL_SyncSharedMemory(__RC__)
       if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
          status = nf90_inq_varid(ncid,'lat_corners',varid)
          _VERIFY(status)
          status = nf90_get_var(ncid,varid,corners)
          _VERIFY(status)
          corners=corners*MAPL_PI_R8/180.d0
       end if
       call MAPL_SyncSharedMemory(__RC__)

       call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
          staggerloc=ESMF_STAGGERLOC_CORNER, &
          farrayPtr=fptr, __RC__)
       fptr=corners(ic_1:ic_n,jc_1:jc_n)

       call MAPL_SyncSharedMemory(__RC__)
       if(MAPL_ShmInitialized) then
          call MAPL_DeAllocNodeArray(corners,__RC__)
       else
          deallocate(corners)
       end if

      if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
         status=nf90_close(ncid)
         _VERIFY(status)
      end if

      _RETURN(_SUCCESS)

   end subroutine add_horz_coordinates

   subroutine initialize_from_file_metadata(this, file_metadata, unusable, rc)
      use MAPL_KeywordEnforcerMod
      class (TripolarGridFactory), intent(inout)  :: this
      type (FileMetadata), target, intent(in) :: file_metadata
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam= MOD_NAME // 'initialize_from_file_metadata()'

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(rc)

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

      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'GRIDSPEC:', rc=status)
      _VERIFY(status)
      this%grid_file_name = trim(tmp)

      call ESMF_ConfigGetAttribute(config, this%nx, label=prefix//'NX:', default=UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%ny, label=prefix//'NY:', default=UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%im_world, label=prefix//'IM_WORLD:', default=UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%jm_world, label=prefix//'JM_WORLD:', default=UNDEFINED_INTEGER)
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

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'check_and_fill_consistency'

      _UNUSED_DUMMY(unusable)

      if (.not. allocated(this%grid_name)) then
         this%grid_name = GRID_NAME_DEFAULT
      end if

      ! Check decomposition/bounds
      ! Tripolar requires even divisibility
      _ASSERT(mod(this%im_world, this%nx) == 0,"needs message")
      _ASSERT(mod(this%jm_world, this%ny) == 0,"needs message")

      ! local extents
      call verify(this%nx, this%im_world, this%ims, rc=status)
      call verify(this%ny, this%jm_world, this%jms, rc=status)
      !this%ims = spread(this%im_world / this%nx, 1, this%nx)
      !this%jms = spread(this%jm_world / this%ny, 1, this%ny)
      
      _RETURN(_SUCCESS)

   contains

      subroutine verify(n, m_world, ms, rc)
         integer, intent(inout) :: n
         integer, intent(inout) :: m_world
         integer, allocatable, intent(inout) :: ms(:)
         integer, optional, intent(out) :: rc

         integer :: status

         if (allocated(ms)) then
            _ASSERT(size(ms) > 0,"needs message")

            if (n == UNDEFINED_INTEGER) then
               n = size(ms)
            else
               _ASSERT(n == size(ms),"needs message")
            end if

            if (m_world == UNDEFINED_INTEGER) then
               m_world = sum(ms)
            else
               _ASSERT(m_world == sum(ms),"needs message")
            end if

         else

            _ASSERT(n /= UNDEFINED_INTEGER,"needs message")
            _ASSERT(m_world /= UNDEFINED_INTEGER,"needs message")
            allocate(ms(n), stat=status)
            _VERIFY(status)
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
      _ASSERT(.false.,"tripolar initialize from distgrid non supported") 

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


   subroutine halo(this, array, unusable, halo_width, rc)
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
      class (TripolarGridFactory), intent(inout) :: this
      type (FileMetadata), intent(inout) :: metadata

      type (Variable) :: v
      real(kind=real64), allocatable :: fake_coord(:)
      integer :: i

      call metadata%add_dimension('Xdim', this%im_world)
      call metadata%add_dimension('Ydim', this%jm_world)

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


   end subroutine append_metadata

   function get_grid_vars(this) result(vars)
      class (TripolarGridFactory), intent(inout) :: this

      character(len=:), allocatable :: vars
      _UNUSED_DUMMY(this)

      vars = 'Xdim,Ydim'

   end function get_grid_vars

   subroutine append_variable_metadata(this,var)
      class (TripolarGridFactory), intent(inout) :: this
      type(Variable), intent(inout) :: var
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(var)
   end subroutine append_variable_metadata

   subroutine generate_file_bounds(this,grid,local_start,global_start,global_count,rc)
      use MAPL_BaseMod
      class(TripolarGridFactory), intent(inout) :: this
      type(ESMF_Grid),      intent(inout) :: grid
      integer, allocatable, intent(out) :: local_start(:)
      integer, allocatable, intent(out) :: global_start(:)
      integer, allocatable, intent(out) :: global_count(:)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: global_dim(3), i1,j1,in,jn
      character(len=*), parameter :: Iam = MOD_NAME // 'generate_file_bounds'
      _UNUSED_DUMMY(this)

      call MAPL_GridGet(grid,globalCellCountPerDim=global_dim,rc=status)
      _VERIFY(status)
      call MAPL_GridGetInterior(grid,i1,in,j1,jn)
      allocate(local_start,source=[i1,j1])
      allocate(global_start,source=[1,1])
      allocate(global_count,source=[global_dim(1),global_dim(2)])

   end subroutine generate_file_bounds

   subroutine generate_file_corner_bounds(this,grid,local_start,global_start,global_count,rc)
      use esmf
      class (TripolarGridFactory), intent(inout) :: this
      type(ESMF_Grid), intent(inout)      :: grid
      integer, allocatable, intent(out) :: local_start(:)
      integer, allocatable, intent(out) :: global_start(:)
      integer, allocatable, intent(out) :: global_count(:)
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME // 'generate_file_corner_bounds'
      integer :: status

      _ASSERT(.false.,"not yet implemented")

   end subroutine generate_file_corner_bounds

   function generate_file_reference2D(this,fpointer) result(ref)
      use pFIO
      type(ArrayReference) :: ref
      class(TripolarGridFactory), intent(inout) :: this
      real, pointer, intent(in) :: fpointer(:,:)
      _UNUSED_DUMMY(this)
      ref = ArrayReference(fpointer)
   end function generate_file_reference2D

   function generate_file_reference3D(this,fpointer) result(ref)
      use pFIO
      type(ArrayReference) :: ref
      class(TripolarGridFactory), intent(inout) :: this
      real, pointer, intent(in) :: fpointer(:,:,:)
      _UNUSED_DUMMY(this)
      ref = ArrayReference(fpointer)
   end function generate_file_reference3D

end module MAPL_TripolarGridFactoryMod
