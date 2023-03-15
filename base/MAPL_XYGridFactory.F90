#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_XYGridFactoryMod
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

   public :: XYGridFactory

   integer, parameter :: NUM_DIM = 2

   type, extends(AbstractGridFactory) :: XYGridFactory
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
      logical :: has_corners
      
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
      procedure :: file_has_corners
   end type XYGridFactory
   
   character(len=*), parameter :: MOD_NAME = 'MAPL_XYGridFactory::'
   
   interface XYGridFactory
      module procedure XYGridFactory_from_parameters
   end interface XYGridFactory

   interface set_with_default
      module procedure set_with_default_integer
      module procedure set_with_default_character
   end interface set_with_default


contains

   function XYGridFactory_from_parameters(unusable, grid_file_name, grid_name, &
        & im_world,jm_world,lm,nx, ny, rc) result(factory)
      type (XYGridFactory) :: factory
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
      character(len=*), parameter :: Iam = MOD_NAME // 'XYGridFactory_from_parameters'

      
      if (present(unusable)) print*,shape(unusable)

      call set_with_default(factory%grid_name, grid_name, MAPL_GRID_NAME_DEFAULT)
      call set_with_default(factory%grid_file_name, grid_file_name, MAPL_GRID_FILE_NAME_DEFAULT)

      call set_with_default(factory%ny, nx, MAPL_UNDEFINED_INTEGER)
      call set_with_default(factory%nx, ny, MAPL_UNDEFINED_INTEGER)
      call set_with_default(factory%im_world, im_world, MAPL_UNDEFINED_INTEGER)
      call set_with_default(factory%jm_world, jm_world, MAPL_UNDEFINED_INTEGER)
      call set_with_default(factory%lm, lm, MAPL_UNDEFINED_INTEGER)




      call factory%check_and_fill_consistency(rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)

   end function XYGridFactory_from_parameters


   function make_new_grid(this, unusable, rc) result(grid)
      type (ESMF_Grid) :: grid
      class (XYGridFactory), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'make_geos_grid'

      _UNUSED_DUMMY(unusable)

      grid = this%create_basic_grid(rc=status)
      _VERIFY(status)

      call this%add_horz_coordinates_from_file(grid, rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)

   end function make_new_grid


   
   function create_basic_grid(this, unusable, rc) result(grid)
      type (ESMF_Grid) :: grid
      class (XYGridFactory), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      
      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'create_basic_grid'

      _UNUSED_DUMMY(unusable)

       grid = ESMF_GridCreateNoPeriDim( &
            name=trim(this%grid_name) ,&
            countsPerDEDim1=this%ims, &
            countsPerDEDim2=this%jms, &
            indexFlag=ESMF_INDEX_DELOCAL, &
            gridEdgeLWidth=[0,0], &
            gridEdgeUWidth=[0,1], &
            coordDep1=[1,2], &
            coordDep2=[1,2], &
            coordSys=ESMF_COORDSYS_SPH_RAD, rc=status)
      _VERIFY(status)
      
      ! Allocate coords at default stagger location
      call ESMF_GridAddCoord(grid, rc=status)
      _VERIFY(status)
      call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=status)
      _VERIFY(status)
      
      if (this%lm /= MAPL_UNDEFINED_INTEGER) then
         call ESMF_AttributeSet(grid, name='GRID_LM', value=this%lm, rc=status)
         _VERIFY(status)
      end if
      
      call ESMF_AttributeSet(grid, 'GridType', 'XY', rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end function create_basic_grid

   subroutine add_horz_coordinates_from_file(this, grid, unusable, rc)
      use MAPL_BaseMod, only: MAPL_grid_interior, MAPL_gridget
      use MAPL_CommsMod
      use MAPL_IOMod
      use MAPL_Constants
      class (XYGridFactory), intent(in) :: this
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

      _UNUSED_DUMMY(unusable)

       
       lon_center_name = "lons"
       lat_center_name = "lats"
       lon_corner_name = "corner_lons"
       lat_corner_name = "corner_lats"
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

       call MAPL_AllocateShared(centers,[im_world,jm_world],transroot=.true.,_RC)

       call MAPL_SyncSharedMemory(_RC)

       ! do longitudes
       if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
          status = nf90_inq_varid(ncid,lon_center_name,varid)
          _VERIFY(status)
          status = nf90_get_var(ncid,varid,centers)
          _VERIFY(status)
          centers=centers*MAPL_DEGREES_TO_RADIANS_R8
       end if
       call MAPL_SyncSharedMemory(_RC)
 
       call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
          staggerloc=ESMF_STAGGERLOC_CENTER, &
          farrayPtr=fptr, rc=status)
       fptr=centers(i_1:i_n,j_1:j_n)
       ! do latitudes

       call MAPL_SyncSharedMemory(_RC)
       if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
          status = nf90_inq_varid(ncid,lat_center_name,varid)
          _VERIFY(status)
          status = nf90_get_var(ncid,varid,centers)
          _VERIFY(status)
           centers=centers*MAPL_DEGREES_TO_RADIANS_R8
       end if
       call MAPL_SyncSharedMemory(_RC)

       call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
          staggerloc=ESMF_STAGGERLOC_CENTER, &
          farrayPtr=fptr, rc=status)
       fptr=centers(i_1:i_n,j_1:j_n)

       call MAPL_SyncSharedMemory(_RC)
       if(MAPL_ShmInitialized) then
          call MAPL_DeAllocNodeArray(centers,_RC)
       else
          deallocate(centers)
       end if
       !! now repeat for corners
       if (this%has_corners) then
          call MAPL_AllocateShared(corners,[im_world+1,jm_world+1],transroot=.true.,_RC)

          ! do longitudes

          call MAPL_SyncSharedMemory(_RC)
          if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
             status = nf90_inq_varid(ncid,lon_corner_name,varid)
             _VERIFY(status)
             status = nf90_get_var(ncid,varid,corners)
             _VERIFY(status)
             corners=corners*MAPL_DEGREES_TO_RADIANS_R8
          end if
          call MAPL_SyncSharedMemory(_RC)

          call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
             staggerloc=ESMF_STAGGERLOC_CORNER, &
             farrayPtr=fptr, _RC)
          fptr=corners(ic_1:ic_n,jc_1:jc_n)
          ! do latitudes

          call MAPL_SyncSharedMemory(_RC)
          if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
             status = nf90_inq_varid(ncid,lat_corner_name,varid)
             _VERIFY(status)
             status = nf90_get_var(ncid,varid,corners)
             _VERIFY(status)
             corners=corners*MAPL_DEGREES_TO_RADIANS_R8
          end if
          call MAPL_SyncSharedMemory(_RC)

          call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
             staggerloc=ESMF_STAGGERLOC_CORNER, &
             farrayPtr=fptr, _RC)
          fptr=corners(ic_1:ic_n,jc_1:jc_n)

          call MAPL_SyncSharedMemory(_RC)
          if(MAPL_ShmInitialized) then
             call MAPL_DeAllocNodeArray(corners,_RC)
          else
             deallocate(corners)
          end if

      end if
      if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
         status=nf90_close(ncid)
         _VERIFY(status)
      end if

      _RETURN(_SUCCESS)

   end subroutine add_horz_coordinates_from_file

   subroutine initialize_from_file_metadata(this, file_metadata, unusable, force_file_coordinates, rc)
      use MAPL_KeywordEnforcerMod
      use MAPL_BaseMod, only: MAPL_DecomposeDim
      class (XYGridFactory), intent(inout)  :: this
      type (FileMetadata), target, intent(in) :: file_metadata
      class (KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in) :: force_file_coordinates
      integer, optional, intent(out) :: rc

      integer :: ncid,varid,status

      this%im_world = file_metadata%get_dimension('Xdim',_RC)
      this%jm_world = file_Metadata%get_dimension('Ydim',_RC)
      if (file_metadata%has_dimension('lev')) then
         this%lm = file_metadata%get_dimension('lev',_RC)
      end if 
   
      this%grid_file_name=file_metadata%get_source_file()

      this%initialized_from_metadata = .true.
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

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(rc)

   end subroutine initialize_from_file_metadata

   subroutine initialize_from_config_with_prefix(this, config, prefix, unusable, rc)
      use esmf
      class (XYGridFactory), intent(inout) :: this
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
      _VERIFY(status)
      this%grid_file_name = trim(tmp)

      call ESMF_ConfigGetAttribute(config, this%nx, label=prefix//'NX:', default=MAPL_UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%ny, label=prefix//'NY:', default=MAPL_UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%im_world, label=prefix//'IM_WORLD:', default=MAPL_UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%jm_world, label=prefix//'JM_WORLD:', default=MAPL_UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%lm, label=prefix//'LM:', default=MAPL_UNDEFINED_INTEGER)

      call this%check_and_fill_consistency(rc=status)

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
      class (XYGridFactory), intent(in) :: this

      _UNUSED_DUMMY(this)
      string = 'XYGridFactory'

   end function to_string



   subroutine check_and_fill_consistency(this, unusable, rc)
      use MAPL_BaseMod, only: MAPL_DecomposeDim
      class (XYGridFactory), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'check_and_fill_consistency'

      _UNUSED_DUMMY(unusable)

      if (.not. allocated(this%grid_name)) then
         this%grid_name = MAPL_GRID_NAME_DEFAULT
      end if
      ! local extents
      call verify(this%nx, this%im_world, this%ims, rc=status)
      call verify(this%ny, this%jm_world, this%jms, rc=status)
      call this%file_has_corners(_RC)
      
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

            if (n == MAPL_UNDEFINED_INTEGER) then
               n = size(ms)
            else
               _ASSERT(n == size(ms),"needs message")
            end if

            if (m_world == MAPL_UNDEFINED_INTEGER) then
               m_world = sum(ms)
            else
               _ASSERT(m_world == sum(ms),"needs message")
            end if

         else

            _ASSERT(n /= MAPL_UNDEFINED_INTEGER,"needs message")
            _ASSERT(m_world /= MAPL_UNDEFINED_INTEGER,"needs message")
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
      use MAPL_Constants, only: PI => MAPL_PI_R8
      class (XYGridFactory), intent(inout)  :: this
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
      _FAIL("XY initialize from distgrid non supported") 

   end subroutine initialize_from_esmf_distGrid

   function decomps_are_equal(this,a) result(equal)
      class (XYGridFactory), intent(in) :: this
      class (AbstractGridFactory), intent(in) :: a
      logical :: equal

      select type (a)
      class default
         equal = .false.
         return
      class is (XYGridFactory)
         equal = .true.

         ! same decomposition
         equal = a%nx == this%nx .and. a%ny == this%ny
         if (.not. equal) return
         
      end select
         
   end function decomps_are_equal

   
   function physical_params_are_equal(this, a) result(equal)
      class (XYGridFactory), intent(in) :: this
      class (AbstractGridFactory), intent(in) :: a
      logical :: equal

      select type (a)
      class default
         equal = .false.
         return
      class is (XYGridFactory)
         equal = .true.

         !equal = (a%grid_file_name == this%grid_file_name)
         !if (.not. equal) return

         equal = (a%im_world == this%im_world) .and. (a%jm_world == this%jm_world)
         if (.not. equal) return
         
      end select
         
   end function physical_params_are_equal


   logical function equals(a, b)
      class (XYGridFactory), intent(in) :: a
      class (AbstractGridFactory), intent(in) :: b

      select type (b)
      class default
         equals = .false.
         return
      class is (XYGridFactory)
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
      class (XYGridFactory), intent(in) :: this

      _UNUSED_DUMMY(this)

      name = ''
      ! needs to be implemented
      error stop -1

   end function generate_grid_name

   subroutine init_halo(this, unusable, rc)
      class (XYGridFactory), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN(_SUCCESS)

   end subroutine init_halo


   subroutine halo(this, array, unusable, halo_width, rc)
      use MAPL_CommsMod
      class (XYGridFactory), intent(inout) :: this
      real(kind=REAL32), intent(inout) :: array(:,:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: halo_width
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'halo'
         
   end subroutine halo
      
   subroutine append_metadata(this, metadata)
      class (XYGridFactory), intent(inout) :: this
      type (FileMetadata), intent(inout) :: metadata

      type (Variable) :: v
      real(kind=real64), allocatable :: fake_coord(:)
      integer :: i

      call metadata%add_dimension('Xdim', this%im_world)
      call metadata%add_dimension('Ydim', this%jm_world)
      !call metadata%add_dimension('XCdim', this%im_world+1)
      !call metadata%add_dimension('YCdim', this%jm_world+1)

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

      if (this%has_corners) then
         v = Variable(type=PFIO_REAL64, dimensions='XCdim,YCdim')
         call v%add_attribute('long_name','longitude')
         call v%add_attribute('units','degrees_east')
         call metadata%add_variable('corner_lons',v)

         v = Variable(type=PFIO_REAL64, dimensions='XCdim,YCdim')
         call v%add_attribute('long_name','latitude')
         call v%add_attribute('units','degrees_north')
         call metadata%add_variable('corner_lats',v)
      end if
!
     call metadata%add_attribute('grid_type','XY')

   end subroutine append_metadata

   function get_grid_vars(this) result(vars)
      class (XYGridFactory), intent(inout) :: this

      character(len=:), allocatable :: vars
      _UNUSED_DUMMY(this)

      vars = 'Xdim,Ydim'

   end function get_grid_vars

   function get_file_format_vars(this) result(vars)
      class (XYGridFactory), intent(inout) :: this

      character(len=:), allocatable :: vars
      _UNUSED_DUMMY(this)

      !vars = 'Xdim,Ydim,lons,lats,corner_lons,corner_lats'
      vars = 'Xdim,Ydim,lons,lats'

   end function get_file_format_vars

   subroutine append_variable_metadata(this,var)
      class (XYGridFactory), intent(inout) :: this
      type(Variable), intent(inout) :: var
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(var)
   end subroutine append_variable_metadata

   subroutine generate_file_bounds(this,grid,local_start,global_start,global_count,metadata,rc)
      use MAPL_BaseMod
      class(XYGridFactory), intent(inout) :: this
      type(ESMF_Grid),      intent(inout) :: grid
      integer, allocatable, intent(out) :: local_start(:)
      integer, allocatable, intent(out) :: global_start(:)
      integer, allocatable, intent(out) :: global_count(:)
      type(FileMetaData), intent(in), optional :: metaData
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
      use MAPL_BaseMod
      class (XYGridFactory), intent(inout) :: this
      type(ESMF_Grid), intent(inout)      :: grid
      integer, allocatable, intent(out) :: local_start(:)
      integer, allocatable, intent(out) :: global_start(:)
      integer, allocatable, intent(out) :: global_count(:)
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME // 'generate_file_corner_bounds'

      integer :: status
      integer :: global_dim(3), i1,j1,in,jn

      call MAPL_GridGet(grid,globalCellCountPerDim=global_dim,rc=status)
      _VERIFY(status)
      call MAPL_GridGetInterior(grid,i1,in,j1,jn)
      allocate(local_start,source=[i1,j1])
      allocate(global_start,source=[1,1])
      allocate(global_count,source=[global_dim(1)+1,global_dim(2)+1])

   end subroutine generate_file_corner_bounds


   function generate_file_reference2D(this,fpointer) result(ref)
      use pFIO
      type(ArrayReference) :: ref
      class(XYGridFactory), intent(inout) :: this
      real, pointer, intent(in) :: fpointer(:,:)
      _UNUSED_DUMMY(this)
      ref = ArrayReference(fpointer)
   end function generate_file_reference2D

   function generate_file_reference3D(this,fpointer,metadata) result(ref)
      use pFIO
      type(ArrayReference) :: ref
      class(XYGridFactory), intent(inout) :: this
      real, pointer, intent(in) :: fpointer(:,:,:)
      type(FileMetaData), intent(in), optional :: metaData
      _UNUSED_DUMMY(this)
      ref = ArrayReference(fpointer)
   end function generate_file_reference3D

   subroutine file_has_corners(this,rc)
      use MAPL_CommsMod
      class(XYGridFactory), intent(inout) :: this
      integer, intent(out), optional :: rc

      integer :: status, ncid,varid
      type(ESMF_VM) :: vm
      integer :: log_array(1)

      if (mapl_am_i_root()) then
         status = nf90_open(this%grid_file_name,NF90_NOWRITE,ncid)
         _VERIFY(status)
         status = NF90_inq_varid(ncid,"corner_lons",varid)
         if (status == 0) then
            this%has_corners = .true.
            log_array(1) = 1
         else
            this%has_corners = .false.
            log_array(1) = 0
         end if
      end if
      call ESMF_VMGetCurrent(vm,_RC)
      call ESMF_VMBroadcast(vm,log_array,1,0,_RC)
      this%has_corners = (1 == log_array(1))

      _RETURN(_SUCCESS)
   end subroutine

end module MAPL_XYGridFactoryMod
