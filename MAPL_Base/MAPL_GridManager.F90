#define _SUCCESS      0
#define _FAILURE     1
#define _VERIFY(A)   if(  A/=0) then; call MAPL_throw_exception(__FILE__,__LINE__); return; endif
#define _VERIFY_MSG(A,msg)   if(  A/=0) then; call MAPL_throw_exception(__FILE__,__LINE__, msg); return; endif
#define _ASSERT(A)   if(.not.A) then; if(present(rc)) rc=_FAILURE; PRINT *, Iam, __LINE__; return; endif
#define _RETURN(A)   if(present(rc)) rc=A; return
#include "unused_dummy.H"

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
   use ESMF
   use MAPL_ThrowMod, only: MAPL_throw_exception
   implicit none
   private

   public :: GridManager

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

      procedure :: make_grid_from_factory
      procedure :: make_grid_from_config
      procedure :: make_grid_from_distGrid

      generic :: make_factory => make_factory_from_config
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


contains


   subroutine add_prototype(this, grid_type, prototype)
      class (GridManager), intent(inout) :: this
      character(len=*), intent(in) :: grid_type
      class (AbstractGridFactory), intent(in) :: prototype

      call this%prototypes%insert(grid_type, prototype)
      
   end subroutine add_prototype


   function make_clone(this, grid_type, unusable, rc) result(factory)
     use MAPL_LatLonGridFactoryMod, only: LatLonGridFactory
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

      _UNUSED_DUMMY(unusable)

      if (.not. initialized) then
           call this%prototypes%insert('LatLon', latlon_factory)
           initialized = .true.
      end if


      prototype => this%prototypes%at(grid_type)

      if (associated(prototype)) then
         allocate(factory, source=prototype%clone(), stat=status)
         _VERIFY(status)
      else
         _ASSERT(.false.)
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

      call ESMF_AttributeSet(grid, 'factory_id', factory_id, rc=status)
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
      _VERIFY_MSG(status,message='label not found')

      allocate(factory, source=this%make_factory(trim(grid_type), config, prefix=prefix, rc=status))
      _VERIFY(status)

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
         _VERIFY_MSG(status,message='failed to destroy grid')
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

      call ESMF_AttributeGet(grid, 'factory_id', id, rc=status)
      _VERIFY(status)

      factory => this%factories%at(id)

      _RETURN(_SUCCESS)

   end function get_factory

end module MAPL_GridManager_private

module MAPL_GridManagerMod
   use MAPL_AbstractGridFactoryMod
   use MAPL_GridManager_private
   use MAPL_KeywordEnforcerMod
   use MAPL_ThrowMod, only: MAPL_throw_exception
   use ESMF
   implicit none
   private

   public :: grid_manager
   public :: get_instance
   public :: get_factory_id
   public :: get_factory
   public :: a2d2c

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

      call ESMF_AttributeGet(grid, 'factory_id', id, rc=status)
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

   subroutine A2D2C(grid, u, v, lm, getC, unusable, rc)
      type (ESMF_Grid), intent(in) :: grid
      real, intent(inout) :: u(:,:,:)
      real, intent(inout) :: v(:,:,:)
      integer, intent(in) :: lm
      logical, intent(in) :: getC
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam= MOD_NAME // 'get_a2d2c'
      class (AbstractGridFactory), pointer :: factory

      _UNUSED_DUMMY(unusable)
      factory => grid_manager%get_factory(grid, rc=status)
      _VERIFY(status)

      call factory%a2d2c(u, v, lm, getC, rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)

   end subroutine A2D2C

end module MAPL_GridManagerMod

