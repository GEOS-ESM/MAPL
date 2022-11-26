#include "MAPL_Generic.h"

module mapl3g_HierarchicalRegistry
   use mapl3g_AbstractRegistry
   use mapl3g_AbstractStateItemSpec
   use mapl3g_StateItemSpecPtr
   use mapl3g_RelConnPtStateItemPtrMap
   use mapl3g_ConnectionPoint
   use mapl3g_RelativeConnectionPoint
   use mapl3g_StateItemVector
   use mapl3g_RegistryPtr
   use mapl3g_RegistryPtrMap
   use mapl3g_ConnectionSpec
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   implicit none
   private
  
   public :: HierarchicalRegistry
  
   type, extends(AbstractRegistry) :: HierarchicalRegistry
      private
      type(StateItemVector) :: specs
      type(RelConnPtStateItemPtrMap) :: specs_map
      type(RegistryPtrMap) :: subregistries
   contains
      procedure :: get_item_spec_ptr
      procedure :: get_item_spec
      procedure :: add_item_spec
      procedure :: has_item_spec
      procedure :: set_active

      procedure :: add_subregistry
      procedure :: get_subregistry_comp
      procedure :: get_subregistry_conn
      generic :: get_subregistry => get_subregistry_comp, get_subregistry_conn
      procedure :: has_subregistry

      procedure :: terminate_import
      procedure :: add_connection

      procedure :: connect_sibling
      procedure :: propagate_ptr
   end type HierarchicalRegistry

   interface HierarchicalRegistry
      module procedure new_HierarchicalRegistry_leaf
      module procedure new_HierarchicalRegistry_subregistries
   end interface HierarchicalRegistry

   ! Submodule implementations
   interface
      module function new_HierarchicalRegistry_children(children, rc) result(registry)
         use mapl3g_ChildComponentMap
         type(HierarchicalRegistry) :: registry
         type(ChildComponentMap), intent(in) :: children
         integer, optional, intent(out) :: rc
      end function
   end interface

contains

   function new_HierarchicalRegistry_leaf() result(registry)
      type(HierarchicalRegistry) :: registry
   end function new_HierarchicalRegistry_leaf

      
   function new_HierarchicalRegistry_subregistries(subregistries) result(registry)
      type(HierarchicalRegistry) :: registry
      type(RegistryPtrMap), intent(in) :: subregistries

      registry%subregistries = subregistries
   end function new_HierarchicalRegistry_subregistries

      
   function get_item_spec_ptr(this, conn_pt) result(spec_ptr)
      class(StateItemSpecPtr), pointer :: spec_ptr
      class(HierarchicalRegistry), intent(in) :: this
      type(RelativeConnectionPoint), intent(in) :: conn_pt

      integer :: status

      ! failure is ok; return null ptr
      spec_ptr => this%specs_map%at(conn_pt, rc=status)

   end function get_item_spec_ptr

   function get_item_spec(this, conn_pt) result(spec)
      class(AbstractStateItemSpec), pointer :: spec
      class(HierarchicalRegistry), intent(in) :: this
      type(RelativeConnectionPoint), intent(in) :: conn_pt

      integer :: status
      type(StateItemSpecPtr), pointer :: wrap

      ! failure is ok; return null ptr
      wrap => this%specs_map%at(conn_pt, rc=status)
      if (associated(wrap)) then
         spec => wrap%ptr
      else
         spec => null()
      end if

   end function get_item_spec

   subroutine add_item_spec(this, conn_pt, spec, rc)
      class(HierarchicalRegistry), intent(inout) :: this
      type(RelativeConnectionPoint), intent(in) :: conn_pt
      class(AbstractStateItemSpec), target, intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemSpecPtr) :: wrap


      _ASSERT(.not. this%has_item_spec(conn_pt), 'Duplicate item name.')

      call this%specs%push_back(spec)
      wrap = StateItemSpecPtr(this%specs%back())
      call this%specs_map%insert(conn_pt, wrap)

      ! Internal state items are always active.
      if (conn_pt%is_internal()) call this%set_active(conn_pt)

      _RETURN(_SUCCESS)
   end subroutine add_item_spec

   logical function has_item_spec(this, conn_pt)
      class(HierarchicalRegistry), intent(in) :: this
      type(RelativeConnectionPoint), intent(in) :: conn_pt
      has_item_spec = (this%specs_map%count(conn_pt) > 0)
   end function has_item_spec

   subroutine set_active(this, conn_pt, unusable, require_inactive, rc)
      class(HierarchicalRegistry), intent(inout) :: this
      class(RelativeConnectionPoint), intent(in) :: conn_pt
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in) :: require_inactive
      integer, optional, intent(out) :: rc

      class(AbstractStateItemSpec), pointer :: spec
      logical :: require_inactive_

      spec => this%get_item_spec(conn_pt)
      _ASSERT(associated(spec), 'unknown connection point')

      require_inactive_ = .false.
      if (present(require_inactive)) require_inactive_ = require_inactive

      if (require_inactive_) then
         _ASSERT(.not. spec%is_active(), 'Cannot terminate import that is already satisfied.')
      end if

      call spec%set_active()

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine set_active


   subroutine add_subregistry(this, name, subregistry, rc)
      class(HierarchicalRegistry), intent(inout) :: this
      character(len=*), intent(in) :: name
      class(HierarchicalRegistry), target :: subregistry
      integer, optional, intent(out) :: rc

      type(RegistryPtr) :: wrap

      _ASSERT(.not. this%has_subregistry(name), 'Duplicate subregistry entry.')
      wrap%registry => subregistry
      call this%subregistries%insert(name, wrap)

      _RETURN(_SUCCESS)
   end subroutine add_subregistry

   ! Returns null() if not found.
   function get_subregistry_comp(this, comp_name) result(subregistry)
      class(AbstractRegistry), pointer :: subregistry
      class(HierarchicalRegistry), target, intent(in) :: this
      character(len=*), intent(in) :: comp_name

      type(RegistryPtr), pointer :: wrap
      integer :: status

      if (comp_name == SELF) then
         subregistry => this
         return
      end if
      
      wrap => this%subregistries%at(comp_name,rc=status)
      if (associated(wrap)) then
         subregistry => wrap%registry
         return
      end if

      subregistry => null()

      

   end function get_subregistry_comp


   function get_subregistry_conn(this, conn_pt) result(subregistry)
      class(AbstractRegistry), pointer :: subregistry
      class(HierarchicalRegistry), target, intent(in) :: this
      type(ConnectionPoint), intent(in) :: conn_pt

      subregistry => this%get_subregistry(conn_pt%component_name)

   end function get_subregistry_conn


   logical function has_subregistry(this, name)
      class(HierarchicalRegistry), intent(in) :: this
      character(len=*), intent(in) :: name
      has_subregistry = (this%subregistries%count(name) > 0)
   end function has_subregistry


   subroutine add_connection(this, connection, rc)
      class(HierarchicalRegistry), target, intent(inout) :: this
      type(ConnectionSpec), intent(in) :: connection
      integer, optional, intent(out) :: rc

      class(AbstractRegistry), pointer :: src_registry, dst_registry
      class(AbstractStateItemSpec), pointer :: dst_spec, src_spec
      integer :: status

      associate(src_pt => connection%source, dst_pt => connection%destination)
        src_registry => this%get_subregistry(src_pt)
        dst_registry => this%get_subregistry(dst_pt)

        _ASSERT(associated(src_registry), 'Unknown source registry')
        _ASSERT(associated(dst_registry), 'Unknown destination registry')

        if (connection%is_sibling()) then
           call dst_registry%connect_sibling(src_registry, connection, _RC)
           _RETURN(_SUCCESS)
        end if

        call dst_registry%propagate_ptr(src_registry, connection, _RC)

      end associate
      
      _RETURN(_SUCCESS)
   end subroutine add_connection


   subroutine connect_sibling(this, src_registry, connection, unusable, rc)
      class(HierarchicalRegistry), intent(in) :: this
      class(AbstractRegistry), intent(in) :: src_registry
      type(ConnectionSpec), intent(in) :: connection
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      class(AbstractStateItemSpec), pointer :: dst_spec, src_spec
      integer :: status
      
      associate (src_pt => connection%source, dst_pt => connection%destination)
        dst_spec => this%get_item_spec(dst_pt%relative_pt)
        _ASSERT(associated(dst_spec), 'no such dst pt')
        
        src_spec => src_registry%get_item_spec(src_pt%relative_pt)
        _ASSERT(associated(src_spec), 'no such src pt')

        call src_spec%set_active()
        call dst_spec%connect_to(src_spec, _RC)
      end associate

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine connect_sibling

   subroutine propagate_ptr(this, src_registry, connection, unusable, rc)
      class(HierarchicalRegistry), intent(in) :: this
      class(AbstractRegistry), intent(in) :: src_registry
      type(ConnectionSpec), intent(in) :: connection
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(StateItemSpecPtr), pointer :: dst_wrap, src_wrap

      associate (src_pt => connection%source, dst_pt => connection%destination)
        dst_wrap => this%get_item_spec_ptr(dst_pt%relative_pt)
        
        _ASSERT(associated(dst_wrap), 'no such dst pt')
        _ASSERT(associated(dst_wrap%ptr), 'uninitialized dst wrapper')
        
        src_wrap => src_registry%get_item_spec_ptr(src_pt%relative_pt)
        _ASSERT(associated(src_wrap), 'no such src pt')
        _ASSERT(associated(src_wrap%ptr), 'uninitialized src wrapper')
        
        dst_wrap = src_wrap
      end associate
      
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine propagate_ptr

   subroutine terminate_import(this, conn_pt, rc)
      class(HierarchicalRegistry), target, intent(inout) :: this
      type(ConnectionPoint), intent(in) :: conn_pt
      integer, optional, intent(out) :: rc

      class(AbstractRegistry), pointer :: subregistry
      integer :: status

      _ASSERT(conn_pt%is_import(), 'Cannot terminate import on item that is not an import.')

      subregistry => this%get_subregistry(conn_pt)
      _ASSERT(associated(subregistry), 'Cannot terminate import on unregistered item.')

      call subregistry%set_active(conn_pt%relative_pt, require_inactive=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine terminate_import
end module mapl3g_HierarchicalRegistry
