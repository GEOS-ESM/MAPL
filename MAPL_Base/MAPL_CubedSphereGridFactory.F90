!-----------------------------------------------------
! Note that this implementation only supports
! "square" faces on the cube.   I.e. the number of
! cells along each axis (of each face) are the same.
! IM_WORLD is used for this quantity, and there is no
! equivalent for the "other" axis.
!-----------------------------------------------------

#define _SUCCESS      0
#define _FAILURE     1
#define _VERIFY(A)   if(  A/=0) then; if(present(rc)) rc=A; PRINT *, Iam, __LINE__; return; endif
#define _ASSERT(A)   if(.not.(A)) then; if(present(rc)) rc=_FAILURE; PRINT *, Iam, __LINE__; return; endif
#define _RETURN(A)   if(present(rc)) rc=A; return
#include "unused_dummy.H"


module MAPL_CubedSphereGridFactoryMod
   use MAPL_AbstractGridFactoryMod
   use MAPL_MinMaxMod
   use MAPL_KeywordEnforcerMod
   use ESMF
   use MAPL_CommsMod
   use MAPL_ConstantsMod
   use MAPL_IOMod, only : GETFILE, FREE_FILE 
   use, intrinsic :: iso_fortran_env, only: REAL64,REAL32
   implicit none
   private

   public :: CubedSphereGridFactory

   integer, parameter :: ndims = 2
   integer, parameter :: UNDEFINED_INTEGER = 1-huge(1)
   real, parameter :: UNDEFINED_REAL = huge(1.)
   real(REAL64), parameter :: UNDEFINED_REAL64 = huge(1.d0)
   character(len=*), parameter :: UNDEFINED_CHAR = '**'

   integer, parameter :: FV_GRID_TYPE_DEFAULT = 0
   character(len=*), parameter :: GRID_NAME_DEFAULT = 'UNKNOWN'

   integer, parameter :: NUM_CUBE_FACES = 6

   type, extends(AbstractGridFactory) :: CubedSphereGridFactory
      private

      
      character(len=:), allocatable :: grid_name
      integer :: grid_type = UNDEFINED_INTEGER

      ! Grid dimensions - Note that we only support "square" grids
      integer :: im_world = UNDEFINED_INTEGER
      integer :: lm = UNDEFINED_INTEGER
      integer :: ntiles = NUM_CUBE_FACES

      ! Domain decomposition: - note that we only support "square" dec
      integer :: nx = UNDEFINED_INTEGER
      integer :: ny = UNDEFINED_INTEGER
      integer, allocatable :: ims(:)
      integer, allocatable :: jms(:)
      ! rectangle decomposition
      integer, allocatable :: jms_2d(:,:)
      ! stretching parameters
      real :: stretch_factor, target_lon, target_lat
      logical :: stretched_cube = .false.

      ! For halo
      type(ESMF_RouteHandle) :: rh

      logical :: halo_initialized = .false.

   contains

      procedure :: make_new_grid
      procedure :: create_basic_grid

      procedure :: initialize_from_file_metadata
      procedure :: initialize_from_config_with_prefix
      procedure :: initialize_from_esmf_distGrid

      procedure :: halo_init
      procedure :: halo

      procedure :: check_and_fill_consistency
      procedure :: equals
      procedure :: generate_grid_name
      procedure :: to_string

      procedure :: append_metadata
      procedure :: get_grid_vars
      procedure :: append_variable_metadata
      procedure :: get_fake_longitudes
      procedure :: get_fake_latitudes
   end type CubedSphereGridFactory
   
   character(len=*), parameter :: MOD_NAME = 'CubedSphereGridFactory::'
   
   interface CubedSphereGridFactory
      module procedure CubedSphereGridFactory_from_parameters
   end interface CubedSphereGridFactory

   interface set_with_default
      module procedure set_with_default_integer
      module procedure set_with_default_real
      module procedure set_with_default_real64
      module procedure set_with_default_character
      module procedure set_with_default_bounds
   end interface set_with_default


contains


   function CubedSphereGridFactory_from_parameters(unusable, grid_name, grid_type, &
        & im_world, lm, nx, ny, ims, jms, stretch_factor, target_lon, target_lat, &
        & rc) result(factory)
      type (CubedSphereGridFactory) :: factory
      class (KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: grid_name
      integer, optional, intent(in) :: grid_type

      ! grid details:
      integer, optional, intent(in) :: im_world
      integer, optional, intent(in) :: lm

      ! decomposition:
      integer, optional, intent(in) :: nx
      integer, optional, intent(in) :: ny
      integer, optional, intent(in) :: ims(:)
      integer, optional, intent(in) :: jms(:)

      ! stretched grid
      real(REAL32), optional, intent(in) :: stretch_factor, target_lon, target_lat 

      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'CubedSphereGridFactory_from_parameters'

      if (present(unusable)) print*,shape(unusable)

      call set_with_default(factory%grid_name, grid_name, GRID_NAME_DEFAULT)
      call set_with_default(factory%grid_type, grid_type, FV_GRID_TYPE_DEFAULT)

      call set_with_default(factory%nx, nx, UNDEFINED_INTEGER)
      call set_with_default(factory%ny, ny, UNDEFINED_INTEGER)

      call set_with_default(factory%im_world, im_world, UNDEFINED_INTEGER)
      call set_with_default(factory%lm, lm, UNDEFINED_INTEGER)

      call set_with_default(factory%stretch_factor,stretch_factor,UNDEFINED_REAL)
      call set_with_default(factory%target_lon,target_lon,UNDEFINED_REAL)
      call set_with_default(factory%target_lat,target_lat,UNDEFINED_REAL)

      ! default is unallocated
      if (present(ims)) factory%ims = ims
      if (present(jms)) factory%jms = jms

      call factory%check_and_fill_consistency(rc=status)

      _VERIFY(status)

      _RETURN(_SUCCESS)

   end function CubedSphereGridFactory_from_parameters


   function make_new_grid(this, unusable, rc) result(grid)
      type (ESMF_Grid) :: grid
      class (CubedSphereGridFactory), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'make_grid'

      grid = this%create_basic_grid(rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)

   end function make_new_grid


   
   function create_basic_grid(this, unusable, rc) result(grid)
      type (ESMF_Grid) :: grid
      class (CubedSphereGridFactory), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: i,nTile
      integer, allocatable :: ims(:,:), jms(:,:)
      real(kind=ESMF_KIND_R8), pointer :: lats(:,:),lons(:,:)
      type(ESMF_CubedSphereTransform_Args) :: transformArgument
      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'create_basic_grid'

      if (this%grid_type <=3) then
         nTile=6
      else
         nTile=1
      end if

      allocate(ims(this%nx,nTile))
      do i=1,nTile
         ims(:,i)=this%ims
      enddo

      if(allocated(this%jms_2d)) then
         _ASSERT(size(this%jms_2d,2) == 6) 
         allocate(jms, source = this%jms_2d)
      else
         allocate(jms(this%ny,nTile))
         do i=1,nTile
            jms(:,i)=this%jms
         end do
      endif

      if (this%grid_type <= 3) then
         if (this%stretched_cube) then
            transformArgument%stretch_factor=this%stretch_factor
            transformArgument%target_lon=this%target_lon
            transformArgument%target_lat=this%target_lat
            grid = ESMF_GridCreateCubedSPhere(this%im_world,countsPerDEDim1PTile=ims, &
                      countsPerDEDim2PTile=jms ,name=this%grid_name, &
                      staggerLocList=[ESMF_STAGGERLOC_CENTER,ESMF_STAGGERLOC_CORNER], coordSys=ESMF_COORDSYS_SPH_RAD, & 
                      transformArgs=transformArgument,rc=status)
            _VERIFY(status)
            call ESMF_AttributeSet(grid, name='STRETCH_FACTOR', value=this%stretch_factor,rc=status)
            _VERIFY(status)
            call ESMF_AttributeSet(grid, name='TARGET_LON', value=this%target_lon,rc=status)
            _VERIFY(status)
            call ESMF_AttributeSet(grid, name='TARGET_LAT', value=this%target_lat,rc=status)
            _VERIFY(status)
         else
            grid = ESMF_GridCreateCubedSPhere(this%im_world,countsPerDEDim1PTile=ims, &
                      countsPerDEDim2PTile=jms ,name=this%grid_name, &
                      staggerLocList=[ESMF_STAGGERLOC_CENTER,ESMF_STAGGERLOC_CORNER], coordSys=ESMF_COORDSYS_SPH_RAD, rc=status)
            _VERIFY(status)
         end if
         call ESMF_AttributeSet(grid, 'GridType', 'Cubed-Sphere', rc=status)
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
         call ESMF_AttributeSet(grid, 'GridType', 'Doubly-Periodic', rc=status)
         _VERIFY(status)
         call ESMF_GridAddCoord(grid,rc=status)
         _VERIFY(status)
         call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, &
           farrayPtr=lons, rc=status)
         _VERIFY(status)
         lons=0.0
         call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, &
           farrayPtr=lats, rc=status)
         _VERIFY(status)
         lats=0.0

      end if

      deallocate(ims,jms)

      if (this%lm /= UNDEFINED_INTEGER) then
         call ESMF_AttributeSet(grid, name='GRID_LM', value=this%lm, rc=status)
         _VERIFY(status)
      end if

      call ESMF_AttributeSet(grid, name='NEW_CUBE', value=1,rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end function create_basic_grid
   
   subroutine initialize_from_file_metadata(this, file_metadata, unusable, rc)
      use pFIO_FileMetadataMod
      use pFIO_NetCDF4_FileFormatterMod
      use MAPL_KeywordEnforcerMod
      use MAPL_BaseMod, only: MAPL_DecomposeDim

      class (CubedSphereGridFactory), intent(inout)  :: this
      type (FileMetadata), target, intent(in) :: file_metadata
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam= MOD_NAME // 'initialize_from_file_metadata()'
      integer :: status

      associate(im => this%im_world)
         im = file_metadata%get_dimension('Xdim',rc=status)
         _VERIFY(status)
      end associate
      call this%make_arbitrary_decomposition(this%nx, this%ny, reduceFactor=6, rc=status)
      _VERIFY(status)
      allocate(this%ims(0:this%nx-1))
      allocate(this%jms(0:this%ny-1))
      call MAPL_DecomposeDim(this%im_world, this%ims, this%nx, min_DE_extent=2)
      call MAPL_DecomposeDim(this%im_world, this%jms, this%ny, min_DE_extent=2)
      call this%check_and_fill_consistency(rc=status)
      _VERIFY(status)

      _UNUSED_DUMMY(unusable)

   end subroutine initialize_from_file_metadata


   subroutine initialize_from_config_with_prefix(this, config, prefix, unusable, rc)
      use esmf
      class (CubedSphereGridFactory), intent(inout) :: this
      type (ESMF_Config), intent(inout) :: config
      character(len=*), intent(in) :: prefix  ! effectively optional due to overload without this argument
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME//'make_geos_grid_from_config'
      character(len=ESMF_MAXSTR) :: tmp
      type (ESMF_VM) :: vm
      integer :: vmcomm, ndes

      if (present(unusable)) print*,shape(unusable)

      call ESMF_VMGetCurrent(vm, rc=status)
      _VERIFY(status)

      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'GRIDNAME:', default=GRID_NAME_DEFAULT)
      this%grid_name = trim(tmp)

      call ESMF_ConfigGetAttribute(config, this%grid_type, label=prefix//'CS_GRID_TYPE:', default=FV_GRID_TYPE_DEFAULT)

      call ESMF_ConfigGetAttribute(config, this%nx, label=prefix//'NX:', default=UNDEFINED_INTEGER)
      call ESMF_ConfigGetAttribute(config, this%ny, label=prefix//'NY:', default=UNDEFINED_INTEGER)

      call ESMF_ConfigGetAttribute(config, this%im_world, label=prefix//'IM_WORLD:', default=UNDEFINED_INTEGER)

      call ESMF_ConfigGetAttribute(config, this%stretch_factor, label=prefix//'STRETCH_FACTOR:', default=UNDEFINED_REAL)
      call ESMF_ConfigGetAttribute(config, this%target_lon, label=prefix//'TARGET_LON:', default=UNDEFINED_REAL)
      call ESMF_ConfigGetAttribute(config, this%target_lat, label=prefix//'TARGET_LAT:', default=UNDEFINED_REAL)

      call get_multi_integer(this%ims, 'IMS:', rc=status)
      _VERIFY(status)

      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'JMS_FILE:', rc=status)
      if (status == _SUCCESS) then
         call get_jms_from_file(this%jms_2d, trim(tmp),this%ny, rc=status)
         _VERIFY(status)
      else
         call get_multi_integer(this%jms, 'JMS:', rc=status)
         _VERIFY(status)
      endif

      call ESMF_ConfigGetAttribute(config, this%lm, label=prefix//'LM:', default=UNDEFINED_INTEGER)

      call this%check_and_fill_consistency(rc=status)
      _VERIFY(status)

      ! halo initialization
        
      call ESMF_VmGet(VM, mpicommunicator=vmcomm, petCount=ndes, rc=status)
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
         
         call ESMF_ConfigFindLabel(config, label=prefix//label,isPresent=isPresent,rc=status)
         _VERIFY(status)
         if (.not. isPresent) then
            _RETURN(_SUCCESS)
         end if

         ! First pass:  count values
         n = 0
         do
            call ESMF_ConfigGetAttribute(config, tmp, default=UNDEFINED_INTEGER, rc=status)
            if (status /= _SUCCESS) then
               exit
            else
               n = n + 1
            end if
         end do

         ! Second pass: allocate and fill
         allocate(values(n), stat=status) ! no point in checking status
         _VERIFY(status)
         call ESMF_ConfigFindLabel(config, label=prefix//label,isPresent=isPresent,rc=status)
         _VERIFY(status)
         do i = 1, n
            call ESMF_ConfigGetAttribute(config, values(i), rc=status)
            _VERIFY(status)
         end do

         _RETURN(_SUCCESS)

      end subroutine get_multi_integer

      subroutine get_jms_from_file(values, file_name, n, rc)
         integer, allocatable, intent(out) :: values(:,:)
         character(len=*), intent(in) :: file_name
         integer, intent(in) :: n
         integer, optional, intent(out) :: rc

         logical :: FileExists
         integer :: i,k,face,total, unit, max_procs
         integer :: status, N_proc,NF
         integer, allocatable :: values_tmp(:), values_(:,:)

    
         N_proc = n*6 ! it has been devided by 6. get back the original NY
         allocate(values_tmp(N_proc), stat=status) ! no point in checking status
         _VERIFY(status)

         inquire(FILE = trim(file_name), EXIST=FileExists)
         if ( .not. FileExists) then
            print*, file_name //  " does not exist"
             _RETURN(_FAILURE)

         elseif (MAPL_AM_I_Root(VM)) then

            UNIT = GETFILE ( trim(file_name), form="formatted", rc=status )
            _VERIFY(STATUS)
            read(UNIT,*) total, max_procs
            if (total /= N_proc) then
                print*, "n /= total"
                _RETURN(_FAILURE)
            endif
            do i = 1,total
                read(UNIT,*) values_tmp(i)
            enddo
            call FREE_FILE(UNIT)
         endif

         call MAPL_CommsBcast(VM, max_procs,  n=1, ROOT=MAPL_Root, rc=status)
         call MAPL_CommsBcast(VM, values_tmp, n=N_proc, ROOT=MAPL_Root, rc=status)
         _VERIFY(STATUS)

         ! distributed to 6 faces
         allocate(values_(max_procs,6))
         values_ = 0
         k = 1
         do NF = 1, 6
            face = 0
            do i = 1, max_procs
               values_(i,NF) = values_tmp(k)
               face = face + values_tmp(k)
               k = k+1
               if (face == this%im_world) exit
            enddo             
          enddo
          values = values_

         _RETURN(_SUCCESS)

      end subroutine get_jms_from_file

      subroutine get_bounds(bounds, label, rc)
         type(RealMinMax), intent(out) :: bounds
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
         call ESMF_ConfigGetAttribute(config, bounds%min, rc=status)
         _VERIFY(status)
         call ESMF_ConfigGetAttribute(config, bounds%max, rc=status)
         _VERIFY(status)

         _RETURN(_SUCCESS)

      end subroutine get_bounds

      
   end subroutine initialize_from_config_with_prefix
   
   subroutine halo_init(this, halo_width,rc)
      class (CubedSphereGridFactory), intent(inout) :: this
      integer, optional, intent(in) :: halo_width
      integer, optional, intent(out) :: rc

      type(ESMF_Field) :: field
      type(ESMF_Grid), pointer :: grid
      integer :: useableHalo_width,status
      real, pointer :: ptr(:,:)
      character(len=*), parameter :: Iam = MOD_NAME // 'halo_init'

      if (present(halo_width)) then
         useableHalo_width=halo_width
      else
         useableHalo_width=1
      end if

      grid => this%get_grid(rc=status)
      _VERIFY(status)
      field = ESMF_FieldCreate(grid,ESMF_TYPEKIND_R4, &
              totalLWidth=[useableHalo_width,useableHalo_width], &
              totalUWidth=[useableHalo_width,useableHalo_width], &
              rc=status)
      _VERIFY(status)
      call ESMF_FieldGet(field,farrayPtr=ptr,rc=status)
      _VERIFY(status)
      ptr=0.0
      call ESMF_FieldHaloStore(field,this%rh,rc=status)
      _VERIFY(status)
      call ESMF_FieldDestroy(field,rc=status)
      _VERIFY(status)
      
   end subroutine halo_init

   function to_string(this) result(string)
      character(len=:), allocatable :: string
      class (CubedSphereGridFactory), intent(in) :: this

      _UNUSED_DUMMY(this)
      string = 'CubedSphereGridFactory'

   end function to_string

   subroutine check_and_fill_consistency(this, unusable, rc)
      use MAPL_BaseMod, only: MAPL_DecomposeDim
      use MAPL_ConstantsMod, only: PI => MAPL_PI_R8
      class (CubedSphereGridFactory), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'check_and_fill_consistency'


      if (.not. allocated(this%grid_name)) then
         this%grid_name = GRID_NAME_DEFAULT
      end if

      if (this%grid_type == UNDEFINED_INTEGER) then
         this%grid_type = FV_GRID_TYPE_DEFAULT ! fv default
      end if

      if ( (this%target_lon /= UNDEFINED_REAL) .and. &
           (this%target_lat /= UNDEFINED_REAL) .and. &
           (this%stretch_factor /= UNDEFINED_REAL) ) then
         this%stretched_cube = .true.
         this%target_lon=this%target_lon*pi/180.d0
         this%target_lat=this%target_lat*pi/180.d0
      end if

      ! Check decomposition/bounds
      ! WY notes: not necessary for this assert
      !_ASSERT(allocated(this%ims) .eqv. allocated(this%jms))
      call verify(this%nx, this%im_world, this%ims, rc=status)
      if (allocated(this%jms_2d)) then
        _ASSERT(size(this%jms_2d,2)==6) 
        _ASSERT(sum(this%jms_2d) == 6*this%im_world)
      else
         call verify(this%ny, this%im_world, this%jms, rc=status)
      endif
      
      _RETURN(_SUCCESS)
         
   contains

      subroutine verify(n, m_world, ms, rc)
         integer, intent(inout) :: n
         integer, intent(inout) :: m_world
         integer, allocatable, intent(inout) :: ms(:)
         integer, optional, intent(out) :: rc

         integer :: status

         if (allocated(ms)) then
            _ASSERT(size(ms) > 0)

            if (n == UNDEFINED_INTEGER) then
               n = size(ms)
            else
               _ASSERT(n == size(ms))
            end if

            if (m_world == UNDEFINED_INTEGER) then
               m_world = sum(ms)
            else
               _ASSERT(m_world == sum(ms))
            end if

         else

            _ASSERT(n /= UNDEFINED_INTEGER)
            _ASSERT(m_world /= UNDEFINED_INTEGER)
            allocate(ms(n), stat=status)
            _VERIFY(status)

            call MAPL_DecomposeDim (m_world, ms, n, symmetric=.true.)

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
   

   logical function equals(a, b)
      class (CubedSphereGridFactory), intent(in) :: a
      class (AbstractGridFactory), intent(in) :: b

      select type (b)
      class default
         equals = .false.
         return
      class is (CubedSphereGridFactory)
         equals = .true.

         equals = (a%im_world == b%im_world)
         if (.not. equals) return
         
         equals = (a%lm == b%lm)
         if (.not. equals) return
         
         ! same decomposition
         equals = all(a%ims == b%ims) 
         if (.not. equals) return

         if ( allocated(a%jms) .and. allocated(b%jms)) then
            equals = all(a%jms == b%jms)
            if (.not. equals) return
         else
            equals = all(a%jms_2d == b%jms_2d)
            if (.not. equals) return
         endif
         
      end select
         
   end function equals

   subroutine initialize_from_esmf_distGrid(this, dist_grid, lon_array, lat_array, unusable, rc)
      class (CubedSphereGridFactory), intent(inout)  :: this
      type (ESMF_DistGrid), intent(in) :: dist_grid
      type (ESMF_LocalArray), intent(in) :: lon_array
      type (ESMF_LocalArray), intent(in) :: lat_array
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'CubedSphereGridFactory_initialize_from_esmf_distGrid'

      ! not implemented
      _ASSERT(.false.)

   end subroutine initialize_from_esmf_distGrid

   subroutine halo(this, array, unusable, halo_width, rc)
      use, intrinsic :: iso_fortran_env, only: REAL32
      class (CubedSphereGridFactory), intent(inout) :: this
      real(kind=REAL32), intent(inout) :: array(:,:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: halo_width
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'halo'
      type(ESMF_Field) :: field
      type(ESMF_Grid), pointer :: grid
      real, pointer :: ptr(:,:)
      integer :: useableHalo_width

      if (.not. this%halo_initialized) then
         call this%halo_init(halo_width = halo_width)
         this%halo_initialized = .true.
      end if

      if (present(halo_width)) then
         useableHalo_width=halo_width
      else
         useableHalo_width=1
      end if
      grid => this%get_grid()
      field = ESMF_FieldCreate(grid,ESMF_TYPEKIND_R4, &
              totalLWidth=[useableHalo_width,useableHalo_width], &
              totalUWidth=[useableHalo_width,useableHalo_width], &
              rc=status)
      _VERIFY(status)
      call ESMF_FieldGet(field,farrayPtr=ptr,rc=status)
      _VERIFY(status)
      ptr = array
      call ESMF_FieldHalo(field,this%rh,rc=status)
      _VERIFY(status)
      call ESMF_FieldDestroy(field,rc=status)
      _VERIFY(status)
      
      _RETURN(_SUCCESS)

   end subroutine halo

   function generate_grid_name(this) result(name)
      class (CubedSphereGridFactory), intent(in) :: this
      character(len=:), allocatable :: name

      character(len=4) :: im_string

      write(im_string,'(i4.4)') this%im_world

      name = 'CF' // im_string //'x6C'

   end function generate_grid_name

   subroutine append_metadata(this, metadata)!, unusable, rc)
      use pFIO
      class (CubedSphereGridFactory), intent(inout) :: this
      type (FileMetadata), intent(inout) :: metadata
!!$      class (KeywordEnforcer), optional, intent(in) :: unusable
!!$      integer, optional, intent(out) :: rc

      integer :: im
      type (Variable) :: v
      integer, parameter :: MAXLEN=80
      character(len=MAXLEN) :: gridspec_file_name
      character(len=5), allocatable :: cvar(:,:)
      integer, allocatable :: ivar(:,:)
      integer, allocatable :: ivar2(:,:,:)

      integer :: status

      ! Grid dimensinos
      call metadata%add_dimension('Xdim', this%im_world, rc=status)
      call metadata%add_dimension('Ydim', this%im_world, rc=status)
      call metadata%add_dimension('nf', 6, rc=status)
      call metadata%add_dimension('ncontact', 4, rc=status)
      call metadata%add_dimension('orientationStrLen', 5, rc=status)

      ! Coordinate variables
      v = Variable(PFIO_REAL64, dimensions='Xdim')
      call v%add_attribute('long_name', 'Fake Longitude for GrADS Compatibility')
      call v%add_attribute('units', 'degrees_east')
      call metadata%add_variable('Xdim', CoordinateVariable(v, this%get_fake_longitudes()))

      v = Variable(PFIO_REAL64, dimensions='Ydim')
      call v%add_attribute('long_name', 'Fake Latitude for GrADS Compatibility')
      call v%add_attribute('units', 'degrees_north')
      call metadata%add_variable('Ydim', CoordinateVariable(v, this%get_fake_latitudes()))

      v = Variable(PFIO_INT32, dimensions='nf')
      call v%add_attribute('long_name','cubed-sphere face')
      call v%add_attribute('axis','e')
      call v%add_attribute('grads_dim','e')
      call v%add_const_value(UnlimitedEntity((/1,2,3,4,5,6/)))
      call metadata%add_variable('nf',v)

      v = Variable(PFIO_INT32, dimensions='ncontact')
      call v%add_attribute('long_name','number of contact points')
      call v%add_const_value(UnlimitedEntity((/1,2,3,4/)))
      call metadata%add_variable('ncontact',v)

      v = Variable(PFIO_STRING)
      call  v%add_attribute('grid_mapping_name','gnomonic cubed-sphere')
      call  v%add_attribute('file_format_version','2.90')
      call  v%add_attribute('additional_vars','contacts,orientation,anchor')
      call  v%add_attribute('gridspec_file','gridspec.nc4')
      call metadata%add_variable('cubed_sphere',v)

      ! Other variables
      allocate(ivar(4,6))
      ivar = reshape( (/5, 3, 2, 6, &
                     1, 3, 4, 6, &
                     1, 5, 4, 2, &
                     3, 5, 6, 2, &
                     3, 1, 6, 4, &
                     5, 1, 2, 4 /), (/4,6/))
      v = Variable(PFIO_INT32, dimensions='ncontact,nf')
      call v%add_attribute('long_name', 'adjacent face starting from left side going clockwise')
      call v%add_const_value(UnlimitedEntity(ivar))
      call metadata%add_variable('contacts', v)

      allocate(cvar(4,6))
      !cvar =reshape((/" Y:-X", &
               !" X:-Y", &
               !" Y:Y ", &
               !" X:X ", &
               !" Y:Y ", &
               !" X:X ", &
               !" Y:-X", &
               !" X:-Y", &
               !" Y:-X", &
               !" X:-Y", &
               !" Y:Y ", &
               !" X:X ", &
               !" Y:Y ", &
               !!" X:X ", &
               !" Y:-X", &
               !" X:-Y", &
               !" Y:-X", &
               !" X:-Y", &
               !" Y:Y ", &
               !" X:X ", &
               !" Y:Y ", &
               !" X:X ", &
               !" Y:-X", &
               !" X:-Y" /),(/4,6/))
      v = Variable(PFIO_STRING, dimensions='orientationStrLen,ncontact,nf')
      call v%add_attribute('long_name', 'orientation of boundary')
      !call v%add_const_value(UnlimitedEntity(cvar))
      call metadata%add_variable('orientation', v)

      im = this%im_world
      allocate(ivar2(4,4,6))
      ivar2 = reshape(        & 
                 (/im, im,  1, im, &
                  1, im,  1,  1, &
                  1, im,  1,  1, &
                 im, im,  1, im, &
                 im,  1, im, im, &
                  1,  1, im,  1, &
                  1,  1, im,  1, &
                 im,  1, im,  im, &
                 im, im,  1, im, &
                  1, im,  1,  1, &
                  1, im,  1,  1, &
                 im, im,  1, im, &
                 im,  1, im, im, &
                  1,  1, im,  1, &
                  1,  1, im,  1, &
                 im,  1, im, im, &
                 im, im,  1, im, &
                  1, im,  1,  1, &
                  1, im,  1,  1, &
                 im, im,  1, im, &
                 im,  1, im, im, &
                  1,  1, im,  1, &
                  1,  1, im,  1, &
                 im,  1, im, im  /), (/4,4,6/))
      v = Variable(PFIO_INT32, dimensions='ncontact,ncontact,nf')
      call v%add_attribute('long_name', 'anchor point')
      call v%add_const_value(UnlimitedEntity(ivar2))
      call metadata%add_variable('anchor', v)

      call Metadata%add_attribute('grid_mapping_name', 'gnomonic cubed-sphere')
      call Metadata%add_attribute('file_format_version', '2.90')
      call Metadata%add_attribute('additional_vars', 'contacts,orientation,anchor')
      write(gridspec_file_name,'("C",i0,"_gridspec.nc4")') this%im_world
      call Metadata%add_attribute('gridspec_file', trim(gridspec_file_name))

      v = Variable(PFIO_REAL32, dimensions='Xdim,Ydim,nf')
      call v%add_attribute('long_name','longitude')
      call v%add_attribute('units','degrees_east')
      call metadata%add_variable('lons',v)

      v = Variable(PFIO_REAL32, dimensions='Xdim,Ydim,nf')
      call v%add_attribute('long_name','latitude')
      call v%add_attribute('units','degrees_north')
      call metadata%add_variable('lats',v)

   end subroutine append_metadata

   function get_grid_vars(this) result(vars)
      use pFIO
      class (CubedSphereGridFactory), intent(inout) :: this

      character(len=:), allocatable :: vars

      vars = 'Xdim,Ydim,nf'

   end function get_grid_vars

   subroutine append_variable_metadata(this,var)
      use pFIO
      class (CubedSphereGridFactory), intent(inout) :: this
      type(Variable), intent(inout) :: var

      call var%add_attribute('coordinates','lons lats')
      call var%add_attribute('grid_mapping','cubed_sphere')

   end subroutine append_variable_metadata

   ! Routine to return fake coordinates for Grads+netCDF use cases.
   ! Result is the longitudes (in degrees) of cell centers along the
   ! "equator" of face 1.
   function get_fake_longitudes(this, unusable, rc) result(longitudes)
      use mpi
      use MAPL_BaseMod, only: MAPL_Grid_Interior
      real (kind=REAL64), allocatable :: longitudes(:)
      class (CubedSphereGridFactory), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type (ESMF_Grid) :: grid
      real (kind=ESMF_KIND_R8), pointer :: centers(:,:)
      real (kind=REAL64), allocatable :: piece(:)
      integer :: n_loc
      integer, allocatable :: counts(:), displs(:)
      type (ESMF_VM) :: vm
      integer :: ierror
      integer :: npes, p, pet
      integer :: comm_grid
      integer :: i_1, i_n, j_1, j_n
      integer :: j_mid
      integer :: tile
      integer :: status
      
      character(len=*), parameter :: Iam = MOD_NAME // 'get_fake_longitudes()'
      
      grid = this%make_grid()

      call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, &
           farrayPtr=centers, rc=status)
      _VERIFY(status)

      call ESMF_VMGetCurrent(vm, rc=status)
      _VERIFY(status)

      call ESMF_VMGet(vm, mpiCommunicator=comm_grid, petcount=npes, localpet=pet, rc=status)
      _VERIFY(status)
      
      call MAPL_grid_interior(grid, i_1, i_n, j_1, j_n)

      j_mid = 1 + this%im_world/2

      tile = 1 + pet/(npes/this%nTiles)
      if (tile == 1 .and. (j_1 <= j_mid) .and. (j_mid <= j_n)) then
         allocate(piece(i_1:i_n))
         piece(:) = centers(:,j_mid-(j_1-1))
         n_loc = (i_n - i_1 + 1)
      else
         allocate(piece(1)) ! MPI does not like 0-sized arrays.
         piece(1) = 0
         n_loc = 0
      end if

      allocate(counts(0:npes-1), displs(0:npes-1))
      
      call MPI_Allgather(n_loc, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, comm_grid, ierror)
      _VERIFY(ierror)

      displs(0) = 0
      do p = 1, npes-1
         displs(p) = displs(p-1) + counts(p-1)
      end do

      allocate(longitudes(this%im_world))
      call MPI_Allgatherv(piece, n_loc, MPI_REAL8, longitudes, counts, displs, MPI_REAL8, comm_grid, ierror)
      _VERIFY(ierror)

      longitudes = longitudes * MAPL_RADIANS_TO_DEGREES
      
   end function get_fake_longitudes

   function get_fake_latitudes(this, unusable, rc) result(latitudes)
      use mpi
      use MAPL_BaseMod, only: MAPL_Grid_Interior
      real (kind=REAL64), allocatable :: latitudes(:)
      class (CubedSphereGridFactory), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type (ESMF_Grid) :: grid
      real (kind=ESMF_KIND_R8), pointer :: centers(:,:)
      real (kind=REAL64), allocatable :: piece(:)
      integer :: n_loc
      integer, allocatable :: counts(:), displs(:)
      type (ESMF_VM) :: vm
      integer :: ierror
      integer :: npes, p, pet
      integer :: comm_grid
      integer :: i_1, i_n, j_1, j_n
      integer :: j_mid
      integer :: tile
      integer :: status
      
      character(len=*), parameter :: Iam = MOD_NAME // 'get_fake_latitudes()'
      
      grid = this%make_grid()

      call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, &
           farrayPtr=centers, rc=status)
      _VERIFY(status)

      call ESMF_VMGetCurrent(vm, rc=status)
      _VERIFY(status)

      call ESMF_VMGet(vm, mpiCommunicator=comm_grid, petcount=npes, localpet=pet, rc=status)
      _VERIFY(status)
      
      call MAPL_grid_interior(grid, i_1, i_n, j_1, j_n)

      j_mid = 1 + this%im_world/2

      tile = 1 + pet/(npes/this%nTiles)
      if (tile == 1 .and. (i_1 <= j_mid) .and. (j_mid <= i_n)) then
         allocate(piece(j_1:j_n))
         piece(:) = centers(j_mid-(i_1-1),:)
         n_loc = (j_n - j_1 + 1)
      else
         allocate(piece(1)) ! MPI does not like 0-sized arrays.
         piece(1) = 0
         n_loc = 0
      end if

      allocate(counts(0:npes-1), displs(0:npes-1))
      
      call MPI_Allgather(n_loc, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, comm_grid, ierror)
      _VERIFY(ierror)

      displs(0) = 0
      do p = 1, npes-1
         displs(p) = displs(p-1) + counts(p-1)
      end do

      allocate(latitudes(this%im_world))
      call MPI_Allgatherv(piece, n_loc, MPI_REAL8, latitudes, counts, displs, MPI_REAL8, comm_grid, ierror)
      _VERIFY(ierror)

      latitudes = latitudes * MAPL_RADIANS_TO_DEGREES
      
   end function get_fake_latitudes
 
end module MAPL_CubedSphereGridFactoryMod
