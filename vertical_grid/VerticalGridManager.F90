#include "MAPL.h"
module mapl3g_VerticalGridManager
   use mapl3g_IntegerVerticalGridMap
   use mapl3g_VerticalGrid, only: VerticalGrid
   use mapl3g_VerticalGridSpec, only: VerticalGridSpec
   use mapl3g_VerticalGridFactory, only: VerticalGridFactory
   use mapl3g_VerticalGridFactoryMap
   use pfio, only: FileMetadata
   use mapl_ErrorHandling
   use esmf, only: esmf_HConfig, esmf_HConfigLog
   use gfTL2_StringVector
   implicit none(type,external)
   private
   
   public :: VerticalGridManager
   public :: get_vertical_grid_manager
   
   type :: VerticalGridManager
      private
      type(IntegerVerticalGridMap) :: grid_map
      integer :: next_id = 1
      type(VerticalGridFactoryMap) :: factories
      logical :: initialized = .false.
   contains
      procedure :: initialize
      procedure :: add_grid_by_grid
      procedure :: add_grid_by_spec
      generic :: add_grid => add_grid_by_grid, add_grid_by_spec
      procedure :: get_grid
      procedure :: remove_grid
      procedure :: has_id
      procedure :: get_size
      procedure :: find_factory_for_spec
      procedure :: find_factory_for_config
      procedure :: find_factory_for_file_metadata
      generic :: find_factory => find_factory_for_spec, find_factory_for_config, find_factory_for_file_metadata
      generic :: create_grid => create_grid_from_config, create_grid_from_file_metadata, create_grid_from_spec
      procedure :: create_grid_from_config
      procedure :: create_grid_from_file_metadata
      procedure :: create_grid_from_spec
      procedure :: register_factory
      procedure :: list_factories
      procedure :: get_next_id
   end type VerticalGridManager
   
   ! Singleton instance
   type(VerticalGridManager), save, target :: the_manager

contains


   function get_vertical_grid_manager(rc) result(manager)
      type(VerticalGridManager), pointer :: manager
      integer, optional, intent(out) :: rc

      integer :: status
      
      manager => the_manager
      if (.not. manager%initialized) then
         call manager%initialize(_RC)
      end if
      _RETURN(_SUCCESS)
   end function get_vertical_grid_manager


   subroutine initialize(this, rc)
      use mapl3g_BasicVerticalGrid, only: BasicVerticalGridFactory

      class(VerticalGridManager), target, intent(inout) :: this
      integer, intent(out), optional :: rc
      
      
      type(BasicVerticalGridFactory) :: basic_factory
      integer :: status
      
      _RETURN_IF(this%initialized)
      
      ! Register built-in factories
      call this%register_factory("Basic", basic_factory, _RC)
      
      this%initialized = .true.
      
      _RETURN(_SUCCESS)
   end subroutine initialize


   function get_next_id(this, rc) result(id)
      integer :: id
      class(VerticalGridManager), intent(inout) :: this
      integer, intent(out), optional :: rc
      
      integer :: status
      
      _ASSERT(this%next_id < huge(this%next_id), 'Integer overflow in ID generation')
      
      id = this%next_id
      this%next_id = this%next_id + 1
      
      _RETURN(_SUCCESS)
   end function get_next_id


   function add_grid_by_grid(this, grid, rc) result(grid_ptr)
      class(VerticalGrid), pointer :: grid_ptr
      class(VerticalGridManager), target, intent(inout) :: this
      class(VerticalGrid), intent(in) :: grid
      integer, intent(out), optional :: rc
      
      integer :: id, status
      
      id = this%get_next_id(_RC)
      call this%grid_map%insert(id, grid)
      
      grid_ptr => this%get_grid(id, _RC)
      _ASSERT(associated(grid_ptr), 'Failed to retrieve grid after insertion into map')
      
      call grid_ptr%set_id(id)
      
      _RETURN(_SUCCESS)
   end function add_grid_by_grid

   function add_grid_by_spec(this, spec, rc) result(grid_ptr)
      class(VerticalGrid), pointer :: grid_ptr
      class(VerticalGridManager), intent(inout) :: this
      class(VerticalGridSpec), intent(in) :: spec
      integer, intent(out), optional :: rc
      
      class(VerticalGridFactory), pointer :: factory
      class(VerticalGrid), allocatable :: new_grid
      integer :: status
      
      ! Find appropriate factory
      factory => this%find_factory_for_spec(spec, _RC)
      _ASSERT(associated(factory), 'No factory found that supports the provided specification')
      
      ! Create grid using factory
      new_grid = factory%create_grid_from_spec(spec, _RC)
      
      ! Add grid to manager and get reference to stored copy
      grid_ptr => this%add_grid(new_grid, _RC)
      
      _RETURN(_SUCCESS)
   end function add_grid_by_spec


   function get_grid(this, id, rc) result(grid_ptr)
      class(VerticalGrid), pointer :: grid_ptr
      class(VerticalGridManager), target, intent(in) :: this
      integer, intent(in) :: id
      integer, intent(out), optional :: rc
      
      type(IntegerVerticalGridMapIterator) :: iter

      grid_ptr => null()

      iter = this%grid_map%find(id)
      _ASSERT(iter /= this%grid_map%end(), 'Invalid id')
      
      grid_ptr => iter%second()
      _ASSERT(associated(grid_ptr), 'Map iterator returned null pointer for valid ID')
      
      _RETURN(_SUCCESS)
   end function get_grid


   subroutine remove_grid(this, id, rc)
      class(VerticalGridManager), target, intent(inout) :: this
      integer, intent(in) :: id
      integer, intent(out), optional :: rc
      
      type(IntegerVerticalGridMapIterator) :: iter
      class(VerticalGrid), pointer :: grid_ptr
      integer :: erase_count, status
      
      _ASSERT(this%has_id(id), 'Cannot remove grid: ID not found in manager')
      
      ! Clear the grid's ID before removing
      iter = this%grid_map%find(id)
      _ASSERT(iter /= this%grid_map%end(), 'Grid ID disappeared between has_id check and removal')
      
      grid_ptr => iter%second()
      _ASSERT(associated(grid_ptr), 'Map iterator returned null pointer during removal')
      
      call grid_ptr%set_id(-1)
      
      erase_count = this%grid_map%erase(id)
      _ASSERT(erase_count == 1, 'Expected to erase exactly one grid entry')
      
      _RETURN(_SUCCESS)
   end subroutine remove_grid


   function get_size(this) result(size)
      integer :: size
      class(VerticalGridManager), intent(in) :: this
      
      size = this%grid_map%size()
   end function get_size


   function has_id(this, id) result(found)
      logical :: found
      class(VerticalGridManager), target, intent(in) :: this
      integer, intent(in) :: id
      
      type(IntegerVerticalGridMapIterator) :: iter
      
      iter = this%grid_map%find(id)
      found = (iter /= this%grid_map%end())
   end function has_id


   subroutine register_factory(this, name, factory, rc)
      class(VerticalGridManager), target, intent(inout) :: this
      character(len=*), intent(in) :: name
      class(VerticalGridFactory), intent(in) :: factory
      integer, intent(out), optional :: rc
      
      _ASSERT(len_trim(name) > 0, 'Factory name cannot be empty')
      
      ! Add factory to registry (container makes deep copy)
      call this%factories%insert(name, factory)
      
      _RETURN(_SUCCESS)
   end subroutine register_factory

function find_factory_for_spec(this, spec, rc) result(factory_ptr)
      class(VerticalGridFactory), pointer :: factory_ptr
      class(VerticalGridManager), target, intent(inout) :: this
      class(VerticalGridSpec), intent(in) :: spec
      integer, intent(out), optional :: rc

      type(VerticalGridFactoryIterator) :: iter
      class(VerticalGridFactory), pointer :: candidate
      integer :: i, status
      
      call this%initialize(_RC)  ! Ensure initialized
      
      factory_ptr => null()
      
      ! Try each factory
      iter = this%factories%ftn_begin()
      associate (e => this%factories%ftn_end())
        do while (iter /= e)
           call iter%next()
           candidate => iter%second()
           _ASSERT(associated(candidate), 'Factory registry returned null factory pointer')
            
           if (candidate%supports(spec)) then
              factory_ptr => candidate
              _RETURN(_SUCCESS)
           end if
        end do
      end associate

      _FAIL('No suitable factory found.')
   end function find_factory_for_spec

   function find_factory_for_config(this, config, rc) result(factory_ptr)
      class(VerticalGridFactory), pointer :: factory_ptr
      class(VerticalGridManager), target, intent(inout) :: this
      type(esmf_HConfig), intent(in), target :: config
      integer, intent(out), optional :: rc
      
      type(VerticalGridFactoryIterator) :: iter
      class(VerticalGridFactory), pointer :: candidate
      integer :: i, status

      factory_ptr => null() ! Ensure defined result

      call this%initialize(_RC)  ! Ensure initialized

      ! Try each factory
      iter = this%factories%ftn_begin()
      associate (e => this%factories%ftn_end())
        do while (iter /= e)
           call iter%next()
           candidate => iter%second()
           _ASSERT(associated(candidate), 'Factory registry returned null factory pointer')
            
            if (candidate%supports(config)) then
              factory_ptr => candidate
              _RETURN(_SUCCESS)
           end if
        end do
      end associate

      call esmf_HConfigLog(config, _RC)  ! Log the config for debugging
      _FAIL('No suitable factory found. (See esmf log for config.)')

   end function find_factory_for_config


   function find_factory_for_file_metadata(this, file_metadata, rc) result(factory_ptr)
      class(VerticalGridFactory), pointer :: factory_ptr
      class(VerticalGridManager), intent(inout) :: this
      type(FileMetadata), intent(in), target :: file_metadata
      integer, intent(out), optional :: rc
      
      type(VerticalGridFactoryIterator) :: iter
      class(VerticalGridFactory), pointer :: candidate
      integer :: i, status
      
      call this%initialize(_RC)  ! Ensure initialized
      factory_ptr => null()
      
      ! Try each factory
      iter = this%factories%ftn_begin()
      associate (e => this%factories%ftn_end())
        do while (iter /= e)
           call iter%next()
           candidate => iter%second()
           _ASSERT(associated(candidate), 'Factory registry returned null factory pointer')
            
            if (candidate%supports(file_metadata)) then
              factory_ptr => candidate
              _RETURN(_SUCCESS)
           end if
        end do
      end associate
      _FAIL('No suitable factory found.')

   end function find_factory_for_file_metadata

   function create_grid_from_spec(this, spec, rc) result(grid_ptr)
      class(VerticalGrid), pointer :: grid_ptr
      class(VerticalGridManager), target, intent(inout) :: this
      class(VerticalGridSpec), intent(in) :: spec
      integer, intent(out), optional :: rc
      
      class(VerticalGridFactory), pointer :: factory
      class(VerticalGrid), allocatable :: new_grid
      integer :: status

      ! Find appropriate factory
      factory => this%find_factory_for_spec(spec, _RC)
      _ASSERT(associated(factory), 'No factory found that supports the provided configuration')

      ! Create grid using factory
      new_grid = factory%create_grid_from_spec(spec, _RC)

      ! Add grid to manager and get reference to stored copy
      grid_ptr => this%add_grid(new_grid, _RC)
      
      _RETURN(_SUCCESS)
   end function create_grid_from_spec

   function create_grid_from_config(this, config, rc) result(grid_ptr)
      class(VerticalGrid), pointer :: grid_ptr
      class(VerticalGridManager), target, intent(inout) :: this
      type(esmf_HConfig), intent(in), target :: config
      integer, intent(out), optional :: rc
      
      class(VerticalGridFactory), pointer :: factory
      class(VerticalGrid), allocatable :: new_grid
      integer :: status

      ! Find appropriate factory
      factory => this%find_factory_for_config(config, _RC)
      _ASSERT(associated(factory), 'No factory found that supports the provided configuration')

      ! Create grid using factory
      allocate(new_grid, source=factory%create_grid_from_config(config,rc=status))
      _VERIFY(status)
      ! Add grid to manager and get reference to stored copy
      grid_ptr => this%add_grid(new_grid, _RC)
      
      _RETURN(_SUCCESS)
   end function create_grid_from_config


   function create_grid_from_file_metadata(this, file_metadata, rc) result(grid_ptr)
      class(VerticalGrid), pointer :: grid_ptr
      class(VerticalGridManager), intent(inout) :: this
      type(FileMetadata), intent(in), target :: file_metadata
      integer, intent(out), optional :: rc
      
      class(VerticalGridFactory), pointer :: factory
      class(VerticalGrid), allocatable :: new_grid
      integer :: status
      
      ! Find appropriate factory
      factory => this%find_factory_for_file_metadata(file_metadata, _RC)
      _ASSERT(associated(factory), 'No factory found that supports the provided file metadata')
      
      ! Create grid using factory
      new_grid = factory%create_grid_from_file_metadata(file_metadata, _RC)
      
      ! Add grid to manager and get reference to stored copy
      grid_ptr => this%add_grid(new_grid, _RC)
      
      _RETURN(_SUCCESS)
   end function create_grid_from_file_metadata


   function list_factories(this) result(names)
      type(StringVector) :: names
      class(VerticalGridManager), target, intent(in) :: this
      
      type(VerticalGridFactoryIterator) :: iter
      
      iter = this%factories%ftn_begin()
      associate (e => this%factories%ftn_end())
        do while (iter /= e)
           call iter%next()
           call names%push_back(iter%first())
        end do
      end associate

   end function list_factories

end module mapl3g_VerticalGridManager
