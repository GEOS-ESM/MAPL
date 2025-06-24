#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_XYGridFactoryMod
   use MAPL_AbstractGridFactoryMod
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_ShmemMod
   use MAPL_Constants
   use MAPL_CommsMod
   use MAPL_BaseMod
   use ESMF
   use pFIO
   use NetCDF
   !   use Plain_netCDF_Time, only : get_ncfile_dimension
   use Plain_netCDF_Time
   use MAPL_ObsUtilMod, only : ABI_XY_2_lonlat
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

      character(len=ESMF_MAXSTR) :: index_name_x
      character(len=ESMF_MAXSTR) :: index_name_y
      character(len=ESMF_MAXSTR) :: var_name_x
      character(len=ESMF_MAXSTR) :: var_name_y
      character(len=ESMF_MAXSTR) :: var_name_proj
      character(len=ESMF_MAXSTR) :: att_name_proj

      integer :: xdim_true
      integer :: ydim_true
      integer :: thin_factor
   contains
      procedure :: make_new_grid
      procedure :: create_basic_grid
      procedure :: add_horz_coordinates_from_file
      procedure :: add_horz_coordinates_from_ABIfile
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
      procedure :: add_mask
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
      __VERIFY(status)
      __RETURN(__SUCCESS)

   end function XYGridFactory_from_parameters


   function make_new_grid(this, unusable, rc) result(grid)
      type (ESMF_Grid) :: grid
      class (XYGridFactory), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'make_geos_grid'

      __UNUSED_DUMMY(unusable)

      grid = this%create_basic_grid(__RC)
      if ( index(trim(adjustl(this%grid_name)), 'ABI') == 0 ) then
         call this%add_horz_coordinates_from_file(grid, __RC)
      else
         call this%add_horz_coordinates_from_ABIfile(grid, __RC)
      end if
      call this%add_mask(grid,__RC)

      __RETURN(__SUCCESS)

   end function make_new_grid



   function create_basic_grid(this, unusable, rc) result(grid)
      type (ESMF_Grid) :: grid
      class (XYGridFactory), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'create_basic_grid'

      __UNUSED_DUMMY(unusable)

       grid = ESMF_GridCreateNoPeriDim( &
            name=trim(this%grid_name) ,&
            countsPerDEDim1=this%ims, &
            countsPerDEDim2=this%jms, &
            indexFlag=ESMF_INDEX_DELOCAL, &
            gridEdgeLWidth=[0,0], &
            gridEdgeUWidth=[0,1], &
            coordDep1=[1,2], &
            coordDep2=[1,2], &
            coordSys=ESMF_COORDSYS_SPH_RAD, __RC)

      ! Allocate coords at default stagger location
      call ESMF_GridAddCoord(grid, rc=status)
      __VERIFY(status)
      if (this%has_corners) then
         call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=status)
      end if

      __VERIFY(status)

      if (this%lm /= MAPL_UNDEFINED_INTEGER) then
         call ESMF_AttributeSet(grid, name='GRID_LM', value=this%lm, __RC)
      end if

      call ESMF_AttributeSet(grid, 'GridType', 'XY', __RC)

      __RETURN(__SUCCESS)
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

      __UNUSED_DUMMY(unusable)

       lon_center_name = "lons"
       lat_center_name = "lats"
       lon_corner_name = "corner_lons"
       lat_corner_name = "corner_lats"
       call MAPL_GridGet(GRID, localCellCountPerDim=COUNTS, globalCellCountPerDim=DIMS, RC=STATUS)
       __VERIFY(STATUS)
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
          __VERIFY(status)
       end if

       call MAPL_AllocateShared(centers,[im_world,jm_world],transroot=.true.,__RC)

       call MAPL_SyncSharedMemory(__RC)

       ! do longitudes
       if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
          status = nf90_inq_varid(ncid,lon_center_name,varid)
          __VERIFY(status)
          status = nf90_get_var(ncid,varid,centers)
          __VERIFY(status)
          where(centers /= MAPL_UNDEF) centers=centers*MAPL_DEGREES_TO_RADIANS_R8
       end if
       call MAPL_SyncSharedMemory(__RC)

       call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
          staggerloc=ESMF_STAGGERLOC_CENTER, &
          farrayPtr=fptr, rc=status)
       fptr=centers(i_1:i_n,j_1:j_n)
       ! do latitudes

       call MAPL_SyncSharedMemory(__RC)
       if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
          status = nf90_inq_varid(ncid,lat_center_name,varid)
          __VERIFY(status)
          status = nf90_get_var(ncid,varid,centers)
          __VERIFY(status)
          where(centers /= MAPL_UNDEF) centers=centers*MAPL_DEGREES_TO_RADIANS_R8
       end if
       call MAPL_SyncSharedMemory(__RC)

       call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
          staggerloc=ESMF_STAGGERLOC_CENTER, &
          farrayPtr=fptr, rc=status)
       fptr=centers(i_1:i_n,j_1:j_n)

       call MAPL_SyncSharedMemory(__RC)
       if(MAPL_ShmInitialized) then
          call MAPL_DeAllocNodeArray(centers,__RC)
       else
          deallocate(centers)
       end if
       !! now repeat for corners
       if (this%has_corners) then
          call MAPL_AllocateShared(corners,[im_world+1,jm_world+1],transroot=.true.,__RC)

          ! do longitudes

          call MAPL_SyncSharedMemory(__RC)
          if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
             status = nf90_inq_varid(ncid,lon_corner_name,varid)
             __VERIFY(status)
             status = nf90_get_var(ncid,varid,corners)
             __VERIFY(status)
             where(corners /= MAPL_UNDEF) corners=corners*MAPL_DEGREES_TO_RADIANS_R8
          end if
          call MAPL_SyncSharedMemory(__RC)

          call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
             staggerloc=ESMF_STAGGERLOC_CORNER, &
             farrayPtr=fptr, __RC)
          fptr=corners(ic_1:ic_n,jc_1:jc_n)
          ! do latitudes

          call MAPL_SyncSharedMemory(__RC)
          if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
             status = nf90_inq_varid(ncid,lat_corner_name,varid)
             __VERIFY(status)
             status = nf90_get_var(ncid,varid,corners)
             __VERIFY(status)
             where(corners /= MAPL_UNDEF) corners=corners*MAPL_DEGREES_TO_RADIANS_R8
          end if
          call MAPL_SyncSharedMemory(__RC)

          call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
             staggerloc=ESMF_STAGGERLOC_CORNER, &
             farrayPtr=fptr, __RC)
          fptr=corners(ic_1:ic_n,jc_1:jc_n)

          call MAPL_SyncSharedMemory(__RC)
          if(MAPL_ShmInitialized) then
             call MAPL_DeAllocNodeArray(corners,__RC)
          else
             deallocate(corners)
          end if

      end if
      if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
         status=nf90_close(ncid)
         __VERIFY(status)
      end if

      __RETURN(__SUCCESS)

   end subroutine add_horz_coordinates_from_file


   subroutine add_horz_coordinates_from_ABIfile(this, grid, unusable, rc)
      use MAPL_CommsMod
      use MAPL_IOMod
      use MAPL_Constants
      class (XYGridFactory), intent(in) :: this
      type (ESMF_Grid), intent(inout) :: grid
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status

      type(ESMF_VM) :: vm
      integer :: i, j
      integer :: ix, jx
      integer :: i_1, i_n, j_1, j_n
      real(REAL64), pointer :: fptr_x(:,:)   ! lon
      real(REAL64), pointer :: fptr_y(:,:)   ! lat
      real(REAL64), pointer :: x(:)
      real(REAL64), pointer :: y(:)
      real(REAL64), pointer :: lambda0(:)
      real(REAL64) :: lambda0_deg
      real(REAL64) :: x0, y0
      real(REAL64) :: lam_sat
      character(len=ESMF_MAXSTR) :: fn, key_x, key_y, key_p, key_p_att

      __UNUSED_DUMMY(unusable)

      call MAPL_Grid_Interior (grid, i_1, i_n, j_1, j_n)
      call MAPL_AllocateShared(x,[this%Xdim_true],transroot=.true.,__RC)
      call MAPL_AllocateShared(y,[this%Ydim_true],transroot=.true.,__RC)
      call MAPL_AllocateShared(lambda0,[1],transroot=.true.,__RC)
      call MAPL_SyncSharedMemory(__RC)

      if (mapl_am_i_root()) then
         fn    = this%grid_file_name
         key_x = this%var_name_x
         key_y = this%var_name_y
         key_p = this%var_name_proj
         key_p_att = this%att_name_proj
         call get_v1d_netcdf_R8_complete (fn, key_x, x, __RC)
         call get_v1d_netcdf_R8_complete (fn, key_y, y, __RC)
         call get_att_real_netcdf (fn, key_p, key_p_att, lambda0_deg, __RC)
         lambda0 = lambda0_deg*MAPL_DEGREES_TO_RADIANS_R8
      end if
      call MAPL_SyncSharedMemory(__RC)

      call ESMF_VMGetCurrent(vm, __RC)
      call MAPL_BcastShared (vm, data=x, N=this%Xdim_true, Root=MAPL_ROOT, RootOnly=.false., __RC)
      call MAPL_BcastShared (vm, data=y, N=this%Ydim_true, Root=MAPL_ROOT, RootOnly=.false., __RC)
      call MAPL_BcastShared (vm, data=lambda0, N=1,        Root=MAPL_ROOT, RootOnly=.false., __RC)

      call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=fptr_x, __RC)
      call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=fptr_y, __RC)
      lam_sat = lambda0(1)
      do i = i_1, i_n
         ix = i - i_1 + 1
         do j= j_1, j_n
            jx = j - j_1 + 1
            x0 = x( i * this%thin_factor )
            y0 = y( j * this%thin_factor )
            call ABI_XY_2_lonlat (x0, y0, lam_sat, fptr_x(ix, jx), fptr_y(ix, jx) )
         end do
      end do
      call MAPL_SyncSharedMemory(__RC)

      if(MAPL_ShmInitialized) then
         call MAPL_DeAllocNodeArray(x,__RC)
         call MAPL_DeAllocNodeArray(y,__RC)
      else
         deallocate(x)
         deallocate(y)
      end if

      __RETURN(__SUCCESS)

    end subroutine add_horz_coordinates_from_ABIfile


   subroutine initialize_from_file_metadata(this, file_metadata, unusable, force_file_coordinates, rc)
      use MAPL_KeywordEnforcerMod
      use MAPL_BaseMod, only: MAPL_DecomposeDim
      class (XYGridFactory), intent(inout)  :: this
      type (FileMetadata), target, intent(in) :: file_metadata
      class (KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in) :: force_file_coordinates
      integer, optional, intent(out) :: rc

      integer :: status

      this%im_world = file_metadata%get_dimension('Xdim',__RC)
      this%jm_world = file_Metadata%get_dimension('Ydim',__RC)
      if (file_metadata%has_dimension('lev')) then
         this%lm = file_metadata%get_dimension('lev',__RC)
      end if

      this%grid_file_name=file_metadata%get_source_file()

      this%initialized_from_metadata = .true.
      call this%make_arbitrary_decomposition(this%nx, this%ny, rc=status)
      __VERIFY(status)

      ! Determine IMS and JMS with constraint for ESMF that each DE has at least an extent
      ! of 2.  Required for ESMF_FieldRegrid().
      allocate(this%ims(0:this%nx-1))
      allocate(this%jms(0:this%ny-1))
      call MAPL_DecomposeDim(this%im_world, this%ims, this%nx, min_DE_extent=2)
      call MAPL_DecomposeDim(this%jm_world, this%jms, this%ny, min_DE_extent=2)

      call this%check_and_fill_consistency(rc=status)
      __VERIFY(status)

      __UNUSED_DUMMY(this)
      __UNUSED_DUMMY(unusable)
      __UNUSED_DUMMY(rc)
      __UNUSED_DUMMY(force_file_coordinates)

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
      integer :: n1, n2
      integer :: arr(2)
      type(ESMF_VM) :: vm

      if (present(unusable)) print*,shape(unusable)

      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'GRIDNAME:', default=MAPL_GRID_NAME_DEFAULT, __RC)
      this%grid_name = trim(tmp)
      call ESMF_ConfigGetAttribute(config, tmp, label=prefix//'GRID_FILENAME:', default='', __RC)
      this%grid_file_name = trim(tmp)

      call ESMF_ConfigGetAttribute(config, this%nx, label=prefix//'NX:', default=MAPL_UNDEFINED_INTEGER, __RC)
      call ESMF_ConfigGetAttribute(config, this%ny, label=prefix//'NY:', default=MAPL_UNDEFINED_INTEGER, __RC)
      call ESMF_ConfigGetAttribute(config, this%lm, label=prefix//'LM:', default=MAPL_UNDEFINED_INTEGER, __RC)

      call ESMF_ConfigGetAttribute(config, this%index_name_x, label=prefix//'index_name_x:', default="x", __RC)
      call ESMF_ConfigGetAttribute(config, this%index_name_y, label=prefix//'index_name_y:', default="y", __RC)
      call ESMF_ConfigGetAttribute(config, this%var_name_x,   label=prefix//'var_name_x:',   default="x", __RC)
      call ESMF_ConfigGetAttribute(config, this%var_name_y,   label=prefix//'var_name_y:',   default="y", __RC)
      call ESMF_ConfigGetAttribute(config, this%var_name_proj,label=prefix//'var_name_proj:',default="",  __RC)
      call ESMF_ConfigGetAttribute(config, this%att_name_proj,label=prefix//'att_name_proj:',default="",  __RC)
      call ESMF_ConfigGetAttribute(config, this%thin_factor,  label=prefix//'thin_factor:',  default=1,   __RC)

      if (mapl_am_i_root()) then
         call get_ncfile_dimension(this%grid_file_name, nlon=n1, nlat=n2, &
              key_lon=this%index_name_x, key_lat=this%index_name_y, __RC)
         arr(1)=n1
         arr(2)=n2
      end if
      call ESMF_VMGetCurrent(vm,__RC)
      call ESMF_VMBroadcast (vm, arr, 2, 0, __RC)
      !
      ! use thin_factor to reduce regridding matrix size
      !
      this%xdim_true = arr(1)
      this%ydim_true = arr(2)
      this%im_world  = arr(1) / this%thin_factor
      this%jm_world  = arr(2) / this%thin_factor

      call this%check_and_fill_consistency(rc=status)

      __RETURN(__SUCCESS)

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
         __VERIFY(status)
         if (.not. isPresent) then
            __RETURN(__SUCCESS)
         end if

         ! First pass:  count values
         n = 0
         do
            call ESMF_ConfigGetAttribute(config, tmp, rc=status)
            if (status /= __SUCCESS) then
               exit
            else
               n = n + 1
            end if
         end do

         ! Second pass: allocate and fill
         allocate(values(n), stat=status) ! no point in checking status
         __VERIFY(status)
         call ESMF_ConfigFindLabel(config, label=prefix//label,rc=status)
         __VERIFY(status)
         do i = 1, n
            call ESMF_ConfigGetAttribute(config, values(i), rc=status)
            __VERIFY(status)
         end do

         __RETURN(__SUCCESS)

      end subroutine get_multi_integer

   end subroutine initialize_from_config_with_prefix



   function to_string(this) result(string)
      character(len=:), allocatable :: string
      class (XYGridFactory), intent(in) :: this

      __UNUSED_DUMMY(this)
      string = 'XYGridFactory'

   end function to_string



   subroutine check_and_fill_consistency(this, unusable, rc)
      use MAPL_BaseMod, only: MAPL_DecomposeDim
      class (XYGridFactory), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME // 'check_and_fill_consistency'

      __UNUSED_DUMMY(unusable)

      if (.not. allocated(this%grid_name)) then
         this%grid_name = MAPL_GRID_NAME_DEFAULT
      end if
      ! local extents
      call verify(this%nx, this%im_world, this%ims, __RC)
      call verify(this%ny, this%jm_world, this%jms, __RC)
      call this%file_has_corners(__RC)

      __RETURN(__SUCCESS)

   contains

      subroutine verify(n, m_world, ms, rc)
         integer, intent(inout) :: n
         integer, intent(inout) :: m_world
         integer, allocatable, intent(inout) :: ms(:)
         integer, optional, intent(out) :: rc

         integer :: status

         if (allocated(ms)) then
            __ASSERT(size(ms) > 0,"needs message")

            if (n == MAPL_UNDEFINED_INTEGER) then
               n = size(ms)
            else
               __ASSERT(n == size(ms),"needs message")
            end if

            if (m_world == MAPL_UNDEFINED_INTEGER) then
               m_world = sum(ms)
            else
               __ASSERT(m_world == sum(ms),"needs message")
            end if

         else
            __ASSERT(n /= MAPL_UNDEFINED_INTEGER,"needs message")
            __ASSERT(m_world /= MAPL_UNDEFINED_INTEGER,"needs message")
            allocate(ms(n), stat=status)
            __VERIFY(status)
            call MAPL_DecomposeDim(m_world, ms, n)
         end if

         __RETURN(__SUCCESS)

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

      __UNUSED_DUMMY(this)
      __UNUSED_DUMMY(unusable)
      __UNUSED_DUMMY(dist_grid)
      __UNUSED_DUMMY(lon_array)
      __UNUSED_DUMMY(lat_array)


      ! not supported
      __FAIL("XY initialize from distgrid non supported")

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

      __UNUSED_DUMMY(this)

      name = ''
      ! needs to be implemented
      error stop -1

   end function generate_grid_name

   subroutine init_halo(this, unusable, rc)
      class (XYGridFactory), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      __RETURN(__SUCCESS)

      __UNUSED_DUMMY(this)
      __UNUSED_DUMMY(unusable)

   end subroutine init_halo


   subroutine halo(this, array, unusable, halo_width, rc)
      use MAPL_CommsMod
      class (XYGridFactory), intent(inout) :: this
      real(kind=REAL32), intent(inout) :: array(:,:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: halo_width
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME // 'halo'

      __UNUSED_DUMMY(this)
      __UNUSED_DUMMY(array)
      __UNUSED_DUMMY(unusable)
      __UNUSED_DUMMY(halo_width)
      __UNUSED_DUMMY(rc)

   end subroutine halo

   subroutine append_metadata(this, metadata)
      class (XYGridFactory), intent(inout) :: this
      type (FileMetadata), intent(inout) :: metadata

      type (Variable) :: v
      real(kind=real64), allocatable :: fake_coord(:)
      integer :: i

      call metadata%add_dimension('Xdim', this%im_world)
      call metadata%add_dimension('Ydim', this%jm_world)
      if (this%has_corners) then
         call metadata%add_dimension('XCdim', this%im_world+1)
         call metadata%add_dimension('YCdim', this%jm_world+1)
      end if

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
      __UNUSED_DUMMY(this)

      vars = 'Xdim,Ydim'

   end function get_grid_vars

   function get_file_format_vars(this) result(vars)
      class (XYGridFactory), intent(inout) :: this

      character(len=:), allocatable :: vars
      __UNUSED_DUMMY(this)

      !vars = 'Xdim,Ydim,lons,lats,corner_lons,corner_lats'
      vars = 'Xdim,Ydim,lons,lats'

   end function get_file_format_vars

   subroutine append_variable_metadata(this,var)
      class (XYGridFactory), intent(inout) :: this
      type(Variable), intent(inout) :: var
      __UNUSED_DUMMY(this)
      __UNUSED_DUMMY(var)
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

      call MAPL_GridGet(grid,globalCellCountPerDim=global_dim,rc=status)
      __VERIFY(status)
      call MAPL_GridGetInterior(grid,i1,in,j1,jn)
      allocate(local_start,source=[i1,j1])
      allocate(global_start,source=[1,1])
      allocate(global_count,source=[global_dim(1),global_dim(2)])

      __UNUSED_DUMMY(this)
      __UNUSED_DUMMY(metadata)

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
      __VERIFY(status)
      call MAPL_GridGetInterior(grid,i1,in,j1,jn)
      allocate(local_start,source=[i1,j1])
      allocate(global_start,source=[1,1])
      allocate(global_count,source=[global_dim(1)+1,global_dim(2)+1])

      __UNUSED_DUMMY(this)

   end subroutine generate_file_corner_bounds


   function generate_file_reference2D(this,fpointer) result(ref)
      use pFIO
      type(ArrayReference) :: ref
      class(XYGridFactory), intent(inout) :: this
      real, pointer, intent(in) :: fpointer(:,:)
      ref = ArrayReference(fpointer)

      __UNUSED_DUMMY(this)
   end function generate_file_reference2D

   function generate_file_reference3D(this,fpointer,metadata) result(ref)
      use pFIO
      type(ArrayReference) :: ref
      class(XYGridFactory), intent(inout) :: this
      real, pointer, intent(in) :: fpointer(:,:,:)
      type(FileMetaData), intent(in), optional :: metaData
      ref = ArrayReference(fpointer)

      __UNUSED_DUMMY(this)
      __UNUSED_DUMMY(metadata)
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
         __VERIFY(status)
         status = NF90_inq_varid(ncid,"corner_lons",varid)
         if (status == 0) then
            this%has_corners = .true.
            log_array(1) = 1
         else
            this%has_corners = .false.
            log_array(1) = 0
         end if
      end if
      call ESMF_VMGetCurrent(vm,__RC)
      call ESMF_VMBroadcast(vm,log_array,1,0,__RC)
      this%has_corners = (1 == log_array(1))

      __RETURN(__SUCCESS)
   end subroutine

   subroutine add_mask(this,grid,rc)
      class(XYGridFactory), intent(in) :: this
      type(ESMF_Grid), intent(inout) :: grid
      integer, intent(out), optional :: rc

      integer(ESMF_KIND_I4), pointer :: mask(:,:)
      real(ESMF_KIND_R8), pointer :: fptr(:,:)
      integer :: status
      type(ESMF_VM) :: vm
      integer :: has_undef, local_has_undef

      call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
         staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=fptr, __RC)
      local_has_undef = 0
      if (any(fptr == MAPL_UNDEF)) local_has_undef = 1

      call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
         staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=fptr, __RC)
      if (any(fptr == MAPL_UNDEF)) local_has_undef = local_has_undef + 1

      call ESMF_VMGetCurrent(vm,__RC)
      call ESMF_VMAllFullReduce(vm, [local_has_undef], has_undef, 1, ESMF_REDUCE_MAX, __RC)
      __RETURN_IF(has_undef == 0)

      call ESMF_GridAddItem(grid,staggerLoc=ESMF_STAGGERLOC_CENTER,itemflag=ESMF_GRIDITEM_MASK,__RC)
      call ESMF_GridGetItem(grid,localDE=0,staggerLoc=ESMF_STAGGERLOC_CENTER, &
          itemflag=ESMF_GRIDITEM_MASK,farrayPtr=mask,__RC)

      mask = MAPL_MASK_IN
      where(fptr==MAPL_UNDEF) mask = MAPL_MASK_OUT

      call ESMF_AttributeSet(grid, name=MAPL_DESTINATIONMASK, &
           itemCount=1, valueList=[MAPL_MASK_OUT], __RC)

      __RETURN(__SUCCESS)
    end subroutine add_mask

end module MAPL_XYGridFactoryMod
