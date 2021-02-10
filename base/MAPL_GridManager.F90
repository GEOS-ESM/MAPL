#include "MAPL_Generic.h"

!!!  NOTE: This class implements the Singleton pattern - there should
!!!        be only one GridManager for the application.  However,
!!!        testing requires multiple instances.  To reflect this
!!!        intent this file contains 2 modules.  The first one
!!!        "MAPL_GridManager_private" makes the derived type public,
!!!        and is useful for testing.  The 2nd module
!!!        "MAPL_GridManagerMod" is intended for general use, and
!!!        does _not_ export the derived type.

module MAPL_GridManager_private
   use, intrinsic :: iso_fortran_env, only: INT64
   use MAPL_AbstractGridFactoryMod
   use MAPL_Integer64GridFactoryMapMod
   use MAPL_StringGridFactoryMapMod
   use MAPL_KeywordEnforcerMod
   use mapl_ErrorHandlingMod
   use ESMF
   use MAPL_ExceptionHandling, only: MAPL_throw_exception
   implicit none
   private

   public :: GridManager
   public :: factory_id_attribute

   ! singleton
   type :: GridManager
      private
      logical :: keep_grids = .false.
      integer(kind=ESMF_KIND_I8) :: counter = 0
      type (StringGridFactoryMap) :: prototypes
      type (Integer64GridFactoryMap) :: factories
   contains
      procedure :: add_prototype
      procedure :: delete
!!$   procedure :: make_field
!!$   procedure :: delete_field

      procedure :: make_factory_from_config
      procedure :: make_factory_from_distGrid
      procedure :: make_factory_from_file

      procedure :: make_grid_from_factory
      procedure :: make_grid_from_config
      procedure :: make_grid_from_distGrid

      generic :: make_factory => make_factory_from_config
      generic :: make_factory => make_factory_from_file
      generic :: make_factory => make_factory_from_distGrid

      generic :: make_grid => make_grid_from_factory
      generic :: make_grid => make_grid_from_config
      generic :: make_grid => make_grid_from_distGrid

      procedure :: get_factory ! from grid

      ! helper procedures
      procedure :: make_clone
      procedure :: get_id
      procedure :: add_factory

   end type GridManager

   integer(kind=INT64), parameter :: NOT_FOUND = 1 - HUGE(1_INT64)

   character(len=*), parameter :: MOD_NAME = 'MAPL_GridManager_private::'
   character(len=*), parameter :: factory_id_attribute = 'MAPL_grid_factory_id'


contains


   subroutine add_prototype(this, grid_type, prototype)
      class (GridManager), intent(inout) :: this
      character(len=*), intent(in) :: grid_type
      class (AbstractGridFactory), intent(in) :: prototype

      call this%prototypes%insert(grid_type, prototype)
      
   end subroutine add_prototype


   function make_clone(this, grid_type, unusable, rc) result(factory)
     use MAPL_LatLonGridFactoryMod, only: LatLonGridFactory
     use MAPL_CubedSphereGridFactoryMod, only: CubedSphereGridFactory
     use MAPL_TripolarGridFactoryMod, only: TripolarGridFactory
     use MAPL_LlcGridFactoryMod, only: LlcGridFactory
     use MAPL_ExternalGridFactoryMod, only: ExternalGridFactory
      class (AbstractGridFactory), allocatable :: factory
      class (GridManager), intent(inout) :: this
      character(len=*), intent(in) :: grid_type
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      class (AbstractGridFactory), pointer :: prototype
      integer :: status
      character(len=*), parameter :: Iam= MOD_NAME // 'make_clone'

      !---------------
      ! Note:
      ! We need to add LatLon prototype somewhere, and MAPL does not have
      ! a natural initialization.  Other grids can be added during
      ! setServices or initialize of the component that defines the grid.
      !---------------
      logical, save :: initialized = .false.
      type (LatLonGridFactory) :: latlon_factory
      type (CubedSphereGridFactory) :: cubed_factory
      type (TripolarGridFactory) :: tripolar_factory
      type (LlcGridFactory) :: llc_factory
      type (ExternalGridFactory) :: external_factory

      _UNUSED_DUMMY(unusable)

      if (.not. initialized) then
           call this%prototypes%insert('LatLon', latlon_factory)
           call this%prototypes%insert('Cubed-Sphere', cubed_factory)
           call this%prototypes%insert('Tripolar',  tripolar_factory)
           call this%prototypes%insert('llc',  llc_factory)
           call this%prototypes%insert('External', external_factory)
           initialized = .true.
      end if


      prototype => this%prototypes%at(grid_type)

      if (associated(prototype)) then
         allocate(factory, source=prototype%clone(), stat=status)
         _VERIFY(status)
      else
         _FAIL('prototype not found')
      end if

      _RETURN(_SUCCESS)
      
   end function make_clone
      

   subroutine add_factory(this, factory, id)
      class (GridManager), intent(inout) :: this
      class (AbstractGridFactory), intent(in) :: factory
      integer(kind=ESMF_KIND_I8), optional, intent(out)  :: id

      type (Integer64GridFactoryMapIterator) :: iter
      class (AbstractGridFactory), pointer :: other

      ! First check to see if we already have it:
      iter = this%factories%begin()
      do while (iter /= this%factories%end())
         other => iter%value()
         if (factory == other) then
            if (present(id)) then
               id = iter%key()
            end if
            return
         end if
         call iter%next()
      end do

      ! OK - it really is new:
      this%counter = this%counter + 1 ! unique ID
      call this%factories%insert(this%counter, factory)
      if (present(id)) then
         id = this%counter
      end if
      
   end subroutine add_factory


   function get_id(this, factory) result(id)
      integer(kind=INT64) :: id
      class (GridManager), intent(inout) :: this
      class (AbstractGridFactory), intent(in) :: factory
      
      call this%add_factory(factory, id)
      
   end function get_id
      

   function make_grid_from_factory(this, factory, unusable, rc) result(grid)

      use ESMF
      type (ESMF_Grid) :: grid
      class (GridManager), intent(inout) :: this
      class (AbstractGridFactory), intent(in) :: factory
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional,  intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam= MOD_NAME // 'make_grid'
      integer(kind=INT64) :: factory_id
      class (AbstractGridFactory), pointer :: f

      _UNUSED_DUMMY(unusable)

      call this%add_factory(factory, factory_id)
      
      f => this%factories%at(factory_id)

      grid = f%make_grid(rc=status)
      _VERIFY(status)

      ! TODO: this should only be done if the grid is new, rather than cached, in which case
      ! the attribute is already set.
      call ESMF_AttributeSet(grid, factory_id_attribute, factory_id, rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)

   end function make_grid_from_factory



   function make_grid_from_config(this, config, unusable, prefix, rc) result(grid)
      use ESMF
      type (ESMF_Grid) :: grid
      class (GridManager), intent(inout) :: this
      type (ESMF_Config), intent(inout) :: config
      class (KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: prefix
      integer, optional,  intent(out) :: rc

      class (AbstractGridFactory), allocatable :: factory
      integer :: status
      character(len=*), parameter :: Iam= MOD_NAME // 'make_grid_from_config'
      character(len=ESMF_MAXSTR) :: grid_type

      character(len=:), allocatable :: label

      character(len=*), parameter :: CF_COMPONENT_SEPARATOR = '.'

      _UNUSED_DUMMY(unusable)

      label = 'GRID_TYPE:'
      if (present(prefix)) then
         label = trim(prefix) // label
      end if

      call ESMF_ConfigGetAttribute(config, label=label, value=grid_type, rc=status)
      _ASSERT(status==0,'label not found')

      allocate(factory, source=this%make_factory(trim(grid_type), config, prefix=prefix, rc=status))
      _VERIFY(status)

      ! Making ESMF grids is expensive.  Add the factory to the cache (if new)
      ! and use the cached factory to create the grid.   Each factory
      ! only creates the grid once.
      grid = this%make_grid(factory, rc=status)
      _VERIFY(status)

      ! TLC: Using 'GridType' instead of 'GRID_TYPE' for legacy reasons.
      call ESMF_AttributeSet(grid, 'GridType', grid_type, rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)

   end function make_grid_from_config

   function make_grid_from_distGrid(this, grid_type, dist_grid, lon_array, lat_array, unusable, rc) result(grid)
      use ESMF
      type (ESMF_Grid) :: grid
      class (GridManager), intent(inout) :: this
      character(len=*), intent(in) :: grid_type
      type (ESMF_DistGrid), intent(in) :: dist_grid
      type (ESMF_LocalArray), intent(in) :: lon_array
      type (ESMF_LocalArray), intent(in) :: lat_array
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional,  intent(out) :: rc

      class (AbstractGridFactory), allocatable :: factory
      integer :: status
      character(len=*), parameter :: Iam= MOD_NAME // 'make_grid_from_distGrid'

      _UNUSED_DUMMY(unusable)

      allocate(factory, source=this%make_factory(grid_type, dist_grid, lon_array, lat_array, rc=status))
      _VERIFY(status)

      grid = this%make_grid(factory, rc=status)
      _VERIFY(status)

      ! TLC: Using 'GridType' instead of 'GRID_TYPE' for legacy reasons.
      call ESMF_AttributeSet(grid, 'GridType', grid_type, rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)

   end function make_grid_from_distGrid



   function make_factory_from_config(this, grid_type, config, unusable, prefix, rc) result(factory)
      use ESMF
      class (AbstractGridFactory), allocatable :: factory
      class (GridManager), intent(inout) :: this
      character(len=*), intent(in) :: grid_type
      type (ESMF_Config), intent(inout) :: config
      class (KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: prefix
      integer, optional,  intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam= MOD_NAME // 'make_factory_from_config'

      _UNUSED_DUMMY(unusable)

      allocate(factory, source=this%make_clone(trim(grid_type), rc=status))
      _VERIFY(status)

      call factory%initialize(config, prefix=prefix, rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)

   end function make_factory_from_config



   function make_factory_from_distGrid(this, grid_type, dist_grid, lon_array, lat_array, unusable, rc) result(factory)
      use ESMF
      class (AbstractGridFactory), allocatable :: factory
      class (GridManager), intent(inout) :: this
      character(len=*), intent(in) :: grid_type
      type (ESMF_DistGrid), intent(in) :: dist_grid
      type (ESMF_LocalArray), intent(in) :: lon_array
      type (ESMF_LocalArray), intent(in) :: lat_array
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional,  intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam= MOD_NAME // 'make_factory_from_distGrid'

      _UNUSED_DUMMY(unusable)

      allocate(factory, source=this%make_clone(trim(grid_type), rc=status))
      _VERIFY(status)

      call factory%initialize(dist_grid, lon_array, lat_array, rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)

   end function make_factory_from_distGrid



   ! Clients should use this procedure to release ESMF resources when a grid
   ! is no longer being used.
   ! If this implementation cache's grids, then the procedure should _not_
   ! invoke ESMF_GridDestroy ...
   
   subroutine delete(this, grid, unusable, rc)
      use ESMF
      class (GridManager), intent(in) :: this
      type (ESMF_Grid), intent(inout) :: grid
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam= MOD_NAME // 'destroy_grid'

      _UNUSED_DUMMY(unusable)

      if (.not. this%keep_grids) then
         call ESMF_GridDestroy(grid, rc=status)
         _ASSERT(status==0,'failed to destroy grid')
      end if

      _RETURN(_SUCCESS)
      
   end subroutine delete


   function get_factory(this, grid, unusable, rc) result(factory)
      class (AbstractGridFactory), pointer :: factory
      class (GridManager), intent(in) :: this
      type (ESMF_Grid), intent(in) :: grid
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer (kind=ESMF_KIND_I8) :: id
      integer :: status
      character(len=*), parameter :: Iam= MOD_NAME // 'get_factory'

      _UNUSED_DUMMY(unusable)

      call ESMF_AttributeGet(grid, factory_id_attribute, id, rc=status)
      _VERIFY(status)

      factory => this%factories%at(id)

      _RETURN(_SUCCESS)

   end function get_factory

   ! TODO: need to check on whether all factory constructors should be
   ! included in the factories component for grid_manager.

   function make_factory_from_file(this, file_name, unused, rc) result(factory)
      use pFIO
      class (AbstractGridFactory), allocatable :: factory
      class (GridManager), intent(inout) :: this
      character(*), intent(in) :: file_name
      class (KeywordEnforcer), optional, intent(in) :: unused
      integer, optional, intent(out) :: rc
      
      type (FileMetadata) :: file_metadata
      type (NetCDF4_FileFormatter) :: file_formatter
      integer :: im, jm, nf
      
      character(len=*), parameter :: Iam= MOD_NAME // 'make_factory_from_file()'
      integer :: status

      class (Attribute), pointer :: attr
      class(*), pointer :: attr_value
      character(len=:), pointer :: grid_type
      type (ESMF_VM) :: vm
      integer :: pet
      logical :: hasXdim      = .FALSE.
      logical :: hasLon       = .FALSE.
      logical :: hasLongitude = .FALSE.
      logical :: hasLat       = .FALSE.
      logical :: hasLatitude  = .FALSE.
      
      _UNUSED_DUMMY(unused)

      call ESMF_VMGetCurrent(vm, rc=status)
      _VERIFY(status)
      call ESMF_VMGet(vm, localPet=pet, rc=status)
      _VERIFY(status)

      call file_formatter%open(file_name, PFIO_READ, rc=status)
      _VERIFY(status)
      file_metadata = file_formatter%read(rc=status)
      _VERIFY(status)

      im = 0
      hasXdim = file_metadata%has_dimension('Xdim')
      if (hasXdim) then
         im = file_metadata%get_dimension('Xdim',rc=status)
         _VERIFY(status) 
      end if

      hasLon = file_metadata%has_dimension('lon')
      if (hasLon) then
         im = file_metadata%get_dimension('lon', rc=status)
         _VERIFY(status)
      else
         hasLongitude = file_metadata%has_dimension('longitude')
         if (hasLongitude) then
             im = file_metadata%get_dimension('longitude', rc=status)
             _VERIFY(status)
         end if
      end if

      if (file_metadata%has_attribute('grid_type')) then
         attr => file_metadata%get_attribute('grid_type')
         attr_value => attr%get_value()
         select type (attr_value)
         type is (StringWrap)
            grid_type => attr_value%value
         end select
         allocate(factory,source=this%make_clone(grid_type))
      else if (hasXdim) then
         im = file_metadata%get_dimension('Xdim',rc=status) 
         if (status == _SUCCESS) then
            jm = file_metadata%get_dimension('Ydim',rc=status)
            _VERIFY(status)
            if (jm == 6*im) then 
               allocate(factory, source=this%make_clone('Cubed-Sphere'))
            else
               nf = file_metadata%get_dimension('nf',rc=status)
               if (status == _SUCCESS) then
                  allocate(factory, source=this%make_clone('Cubed-Sphere'))
               end if
            end if
         end if
      else if (hasLon .or. hasLongitude) then

         hasLat = file_metadata%has_dimension('lat')
         if (hasLat) then 
            jm = file_metadata%get_dimension('lat', rc=status)
            _VERIFY(status)
         else
            hasLatitude = file_metadata%has_dimension('latitude')
            if (hasLatitude) then
               jm = file_metadata%get_dimension('latitude', rc=status)
               _VERIFY(status)
            end if
         end if

         if (jm == 6*im) then ! old-format cubed-sphere
            allocate(factory, source=this%make_clone('Cubed-Sphere'))
!!$        elseif (...) then ! something that is true for tripolar?
!!$           factory = this%make_clone('tripolar')
         else
            allocate(factory, source=this%make_clone('LatLon'))
        end if

     end if

     call factory%initialize(file_metadata, rc=status)
     _VERIFY(status)
     call file_formatter%close(rc=status)

     _RETURN(_SUCCESS)
     
   end function make_factory_from_file

end module MAPL_GridManager_private

module MAPL_GridManagerMod
   use MAPL_AbstractGridFactoryMod
   use MAPL_GridManager_private
   use MAPL_KeywordEnforcerMod
   use mapl_ErrorHandlingMod
   use MAPL_ExceptionHandling, only: MAPL_throw_exception
   use ESMF
   implicit none
   private

   public :: grid_manager
   public :: get_instance
   public :: get_factory_id
   public :: get_factory

   ! singleton instance
   type (GridManager), target, save :: grid_manager

   character(len=*), parameter :: MOD_NAME = 'MAPL_GridManager::'

contains

   
   function get_instance() result(instance)
      type (GridManager), pointer :: instance
      instance => grid_manager
   end function get_instance


   function get_factory_id(grid, unusable, rc) result(id)
      integer (kind=ESMF_KIND_I8) :: id
      type (ESMF_Grid) :: grid
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam= MOD_NAME // 'get_factory_id'

      _UNUSED_DUMMY(unusable)

      call ESMF_AttributeGet(grid, factory_id_attribute, id, rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)

   end function get_factory_id


   function get_factory(grid, unusable, rc) result(factory)
      class (AbstractGridFactory), pointer :: factory
      type (ESMF_Grid) :: grid
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam= MOD_NAME // 'get_factory'

      _UNUSED_DUMMY(unusable)

      factory => grid_manager%get_factory(grid, rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)

   end function get_factory

end module MAPL_GridManagerMod
