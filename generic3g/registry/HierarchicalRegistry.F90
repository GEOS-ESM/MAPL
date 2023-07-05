#include "MAPL_Generic.h"

module mapl3g_HierarchicalRegistry
   use mapl3g_AbstractRegistry
   use mapl3g_AbstractStateItemSpec
   use mapl3g_ActualPtSpecPtrMap
   use mapl3g_ConnectionPt
   use mapl3g_VirtualConnectionPt
   use mapl3g_VirtualConnectionPtVector
   use mapl3g_ActualConnectionPt
   use mapl3g_StateItemVector
   use mapl3g_RegistryPtr
   use mapl3g_RegistryPtrMap
   use mapl3g_ActualPtVector
   use mapl3g_ActualPtSpecPtrMap
   use mapl3g_ActualPtVec_Map
   use mapl3g_ESMF_Utilities
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling

   use mapl3g_StateExtension
   use mapl3g_ExtensionVector
   use mapl3g_ExtensionAction

   implicit none
   private

   public :: HierarchicalRegistry
   ! To avoid circular dependencies, this module defines a 2nd collaborating
   ! base type: Connection
   public :: Connection

   type, extends(AbstractRegistry) :: HierarchicalRegistry
      private
      character(:), allocatable :: name
      
      type(StateItemVector) :: local_specs ! specs for items "owned" by gridcomp
      type(ActualPtSpecPtrMap) :: actual_specs_map ! all items in states of gridcomp
      type(ActualPtVec_Map) :: virtual_pts     ! Grouping of items with shared virtual connection point

      ! Hierarchy/tree aspect
      type(RegistryPtrMap) :: subregistries

      type(ExtensionVector) :: extensions

   contains

      ! getters
      procedure :: get_name
      procedure :: get_item_spec
      procedure :: get_actual_pts
      procedure :: get_actual_pt_SpecPtrs
      procedure :: has_item_spec_actual
      procedure :: has_item_spec_virtual
      generic :: has_item_spec => has_item_spec_actual, has_item_spec_virtual
      procedure :: has_subregistry

      procedure :: add_to_states
      procedure :: get_extensions
      
      procedure :: add_subregistry
      procedure :: get_subregistry_comp
      procedure :: get_subregistry_conn
      generic :: get_subregistry => get_subregistry_comp, get_subregistry_conn
      procedure :: add_item_spec_virtual
      procedure :: add_item_spec_virtual_override
      procedure :: add_item_spec_actual
      generic :: add_item_spec => add_item_spec_virtual
      generic :: add_item_spec => add_item_spec_virtual_override
      generic :: add_item_spec => add_item_spec_actual
      procedure :: link_item_spec_actual
      procedure :: link_item_spec_virtual
      generic :: link_item_spec => link_item_spec_actual, link_item_spec_virtual

      procedure :: add_extension_pt

      procedure :: propagate_unsatisfied_imports_all
      procedure :: propagate_unsatisfied_imports_child
      procedure :: propagate_unsatisfied_imports_virtual_pt
      generic :: propagate_unsatisfied_imports => propagate_unsatisfied_imports_all
      generic :: propagate_unsatisfied_imports => propagate_unsatisfied_imports_child
      generic :: propagate_unsatisfied_imports => propagate_unsatisfied_imports_virtual_pt
      procedure :: propagate_exports_all
      procedure :: propagate_exports_child
      procedure :: propagate_exports_virtual_pt
      generic :: propagate_exports => propagate_exports_all
      generic :: propagate_exports => propagate_exports_child
      generic :: propagate_exports => propagate_exports_virtual_pt

      procedure :: add_connection
      procedure :: extend => extend_
      procedure :: add_state_extension

      procedure :: allocate

!!$      procedure :: get_range
      procedure :: filter

      procedure :: write_formatted
      generic :: write(formatted) => write_formatted
      procedure :: report
   end type HierarchicalRegistry

   interface HierarchicalRegistry
      module procedure new_HierarchicalRegistry_leaf
      module procedure new_HierarchicalRegistry_parent
   end interface HierarchicalRegistry

   type, abstract :: Connection
   contains
      procedure(I_get), deferred :: get_source
      procedure(I_get), deferred :: get_destination
      procedure(I_connect), deferred :: connect
   end type Connection

   abstract interface
      function I_get(this) result(source)
         use mapl3g_ConnectionPt
         import Connection
         type(ConnectionPt) :: source
         class(Connection), intent(in) :: this
      end function I_get
      subroutine I_connect(this, registry, rc)
         import HierarchicalRegistry
         import Connection
         class(Connection), intent(in) :: this
         type(HierarchicalRegistry), target, intent(inout) :: registry
         integer, optional, intent(out) :: rc
      end subroutine I_connect
   end interface

   ! Submodule implementations
   interface
      module function new_HierarchicalRegistry_children(children, rc) result(registry)
         use mapl3g_ChildComponentMap
         type(HierarchicalRegistry) :: registry
         type(ChildComponentMap), intent(in) :: children
         integer, optional, intent(out) :: rc
      end function
   end interface

   character(*), parameter :: SELF = "<self>"

contains


   ! Constructors
   function new_HierarchicalRegistry_leaf(name) result(registry)
      type(HierarchicalRegistry) :: registry
      character(*), intent(in) :: name
      registry = HierarchicalRegistry(name, RegistryPtrMap())
   end function new_HierarchicalRegistry_leaf

   function new_HierarchicalRegistry_parent(name, subregistries) result(registry)
      type(HierarchicalRegistry) :: registry
      character(*), intent(in) :: name
      type(RegistryPtrMap), intent(in) :: subregistries
      registry%name = name
      registry%subregistries = subregistries
   end function new_HierarchicalRegistry_parent


   function get_name(this) result(name)
      character(:), allocatable:: name
      class(HierarchicalRegistry), intent(in) :: this
      name = this%name
   end function get_name

   ! Retrieve a pointer to the item spect associated with an actual pt
   ! in this registry.  Failure returns null pointer.
   function get_item_spec(this, actual_pt, rc) result(spec)
      class(AbstractStateItemSpec), pointer :: spec
      class(HierarchicalRegistry), target, intent(in) :: this
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemSpecPtr), pointer :: wrap

      spec => null()

      wrap => this%actual_specs_map%at(actual_pt, _RC)
      if (associated(wrap)) spec => wrap%ptr

      _RETURN(_SUCCESS)
   end function get_item_spec

   function get_actual_pt_SpecPtrs(this, virtual_pt, rc) result(specs)
      type(StateItemSpecPtr), allocatable :: specs(:)
      class(HierarchicalRegistry), target, intent(in) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      integer, optional, intent(out) :: rc
      
      integer :: status
      integer :: i, n
      type(ActualPtVector), pointer :: actual_pts
      type(ActualConnectionPt), pointer :: actual_pt

      actual_pts => this%virtual_pts%at(virtual_pt, rc=status)
      if (status /= 0) allocate(specs(0))
      if (status /= 0) then
         _HERE, 'status = ', status
         _HERE, virtual_pt
      end if
      _VERIFY(status)
         
      n = actual_pts%size()
      allocate(specs(n))
      do i = 1, n
         actual_pt => actual_pts%of(i)
         specs(i)%ptr => this%get_item_spec(actual_pt, _RC)
      end do

      _RETURN(_SUCCESS)
   end function get_actual_pt_SpecPtrs

   subroutine add_item_spec_actual(this, actual_pt, spec, rc)
      class(HierarchicalRegistry), intent(inout) :: this
      type(ActualConnectionPt), intent(in) :: actual_pt
      class(AbstractStateItemSpec), target, intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      class(AbstractStateItemSpec), pointer :: internal_spec

      _ASSERT(.not. this%has_item_spec(actual_pt), 'Duplicate item name.')

      call this%local_specs%push_back(spec)
      internal_spec => this%local_specs%back()
      call this%link_item_spec_actual(actual_pt, internal_spec, _RC)

      ! Internal state items are always active.
      if (actual_pt%is_internal()) call internal_spec%set_active()

      _RETURN(_SUCCESS)
   end subroutine add_item_spec_actual

   subroutine link_item_spec_actual(this, actual_pt, spec, unusable, rc)
      class(HierarchicalRegistry), intent(inout) :: this
      type(ActualConnectionPt), intent(in) :: actual_pt
      class(AbstractStateItemSpec), target :: spec
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(StateItemSpecPtr) :: wrap

      _ASSERT(.not. this%has_item_spec(actual_pt), 'Duplicate item name.')
      wrap = StateItemSpecPtr(spec)
      call this%actual_specs_map%insert(actual_pt, wrap)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine link_item_spec_actual


   ! This is an interface intended for client code establishing a
   ! user-specified virtual connection pt.  As such, the associated
   ! actual connection pt is _not_ an extension.  This is likely
   ! the only exception to the general rule that registry generated
   ! actual pts should be extension pts.
   subroutine add_item_spec_virtual(this, virtual_pt, spec, rc)
      class(HierarchicalRegistry), intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      class(AbstractStateItemSpec), target, intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(ActualConnectionPt) :: actual_pt

      actual_pt = ActualConnectionPt(virtual_pt)
      call this%add_item_spec(virtual_pt, spec, actual_pt, _RC)

      _RETURN(_SUCCESS)
   end subroutine add_item_spec_virtual

   ! Do not add a new actual_pt, but instead point to an existing one.
   ! This is used for associating a spec form a child registry in a
   ! parent registry.
   subroutine add_item_spec_virtual_override(this, virtual_pt, spec, actual_pt, rc)
      class(HierarchicalRegistry), intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      class(AbstractStateItemSpec), target, intent(in) :: spec
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      integer :: status

      call this%add_extension_pt(virtual_pt, actual_pt)
      call this%add_item_spec(actual_pt, spec, _RC)

      _RETURN(_SUCCESS)
   end subroutine add_item_spec_virtual_override
   

   subroutine add_extension_pt(this, virtual_pt, actual_pt)
      class(HierarchicalRegistry), target, intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      type(ActualConnectionPt), intent(in) :: actual_pt

      type(ActualPtVector), pointer :: actual_pts

      associate (extensions => this%virtual_pts)
        if (extensions%count(virtual_pt) == 0) then
           call extensions%insert(virtual_pt, ActualPtVector())
        end if
        actual_pts => this%virtual_pts%of(virtual_pt)
        call actual_pts%push_back(actual_pt)
      end associate
      
   end subroutine add_extension_pt


   ! This procedure is used when a child import/export must be propagated to parent.
   subroutine link_item_spec_virtual(this, virtual_pt, spec, actual_pt, rc)
      class(HierarchicalRegistry), intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      class(AbstractStateItemSpec), target :: spec
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      integer :: status

      call this%add_extension_pt(virtual_pt, actual_pt)
      if (this%has_item_spec(actual_pt)) then ! that's ok?
         _RETURN(_SUCCESS)
      end if
      call this%link_item_spec(actual_pt, spec, _RC)

      _RETURN(_SUCCESS)
   end subroutine link_item_spec_virtual

   logical function has_item_spec_actual(this, actual_pt) result(has_item_spec)
      class(HierarchicalRegistry), intent(in) :: this
      type(ActualConnectionPt), intent(in) :: actual_pt
      has_item_spec = (this%actual_specs_map%count(actual_pt) > 0)
   end function has_item_spec_actual

   logical function has_item_spec_virtual(this, virtual_pt) result(has_item_spec)
      class(HierarchicalRegistry), intent(in) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      has_item_spec = (this%virtual_pts%count(virtual_pt) > 0)
   end function has_item_spec_virtual


   subroutine add_subregistry(this, subregistry, rc)
      class(HierarchicalRegistry), target, intent(inout) :: this
      class(HierarchicalRegistry), target :: subregistry
      integer, optional, intent(out) :: rc

      type(RegistryPtr) :: wrap
      character(:), allocatable :: name

      name = subregistry%get_name()
      _ASSERT(.not. this%has_subregistry(name), 'Duplicate subregistry entry.')
      wrap%registry => subregistry
      call this%subregistries%insert(name, wrap)

      _RETURN(_SUCCESS)
   end subroutine add_subregistry

   ! We need a special accessor to retrieve child registries due to the use of gFTL.
   ! To avoid circularity HierarchicalRegistry inherits from AbstractRegistry and children
   ! are stored as class(AbstractRegistry).  This routine does the casting.
   !
   ! Returns null() if not found.
   function get_subregistry_comp(this, comp_name, rc) result(subregistry)
      type(HierarchicalRegistry), pointer :: subregistry
      class(HierarchicalRegistry), target, intent(in) :: this
      character(len=*), intent(in) :: comp_name
      integer, optional, intent(out) :: rc

      type(RegistryPtr), pointer :: wrap
      integer :: status

      subregistry => null()
      if (comp_name == this%get_name() .or. comp_name == SELF) then
         subregistry => this
         _RETURN(_SUCCESS)
      end if

      wrap => this%subregistries%at(comp_name,_RC)
      _ASSERT(associated(wrap%registry), 'null pointer encountered for subregistry.')

      select type (q => wrap%registry)
      type is (HierarchicalRegistry)
         subregistry => q
         _RETURN(_SUCCESS)
      class default
         _FAIL('Illegal subtype of AbstractRegistry encountered.')
      end select

   end function get_subregistry_comp


   function get_subregistry_conn(this, conn_pt, rc) result(subregistry)
      type(HierarchicalRegistry), pointer :: subregistry
      class(HierarchicalRegistry), target, intent(in) :: this
      type(ConnectionPt), intent(in) :: conn_pt
      integer, optional, intent(out) :: rc
      
      integer :: status
      
      subregistry => this%get_subregistry(conn_pt%component_name,_RC)

      _RETURN(_SUCCESS)
   end function get_subregistry_conn


   logical function has_subregistry(this, name)
      class(HierarchicalRegistry), intent(in) :: this
      character(len=*), intent(in) :: name
      has_subregistry = (this%subregistries%count(name) > 0)
   end function has_subregistry


   ! Connect two _virtual_ connection points.
   ! Use extension map to find actual connection points.
   recursive subroutine add_connection(this, conn, rc)
      class(HierarchicalRegistry), target, intent(inout) :: this
      class(Connection), intent(in) :: conn
      integer, optional, intent(out) :: rc

      integer :: status
      call conn%connect(this, _RC)

      _RETURN(_SUCCESS)
   end subroutine add_connection


   subroutine extend_(this, v_pt, spec, rc)
      class(HierarchicalRegistry), target, intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: v_pt
      class(AbstractStateItemSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(ActualConnectionPt) :: extension_pt
      type(ActualPtVector), pointer :: actual_pts
      type(ActualConnectionPt), pointer :: actual_pt

      ! 1. Get existing actual pts for v_pt
      actual_pts => this%get_actual_pts(v_pt)
      _ASSERT(associated(actual_pts), 'No actual pts found for v_pt')
      ! 2. Get last actual_pt so that we can generate "next" name
      actual_pt => actual_pts%back()
      
      ! 3. Create extension pt that is an extension of last actual_pt in list.
      extension_pt = actual_pt%extend()
      ! 4. Put spec in registry under actual_pt
      call this%add_item_spec(v_pt, spec, extension_pt, _RC)
      call this%add_state_extension(v_pt, extension_pt, spec, _RC)

      _RETURN(_SUCCESS)
   end subroutine extend_

   subroutine add_state_extension(this, v_pt, a_pt, dst_spec, rc)
      class(HierarchicalRegistry), target, intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: v_pt
      type(ActualConnectionPt), intent(in) :: a_pt
      class(AbstractStateItemSpec), intent(in) :: dst_spec
      integer, optional, intent(out) :: rc

      integer :: status
      class(ExtensionAction), allocatable :: action
      class(AbstractStateItemSpec), pointer :: src_spec
      type(ActualPtVector), pointer :: actual_pts

      ! Determine which actual_pt in v_p we should use as the starting
      ! point.
      actual_pts => this%get_actual_pts(v_pt)
      _ASSERT(associated(actual_pts), 'No actual pts found for v_pt')
      src_spec => this%get_item_spec(actual_pts%front(), _RC)

      action = src_spec%make_action(dst_spec, _RC)
      call this%extensions%push_back(StateExtension(action))

      _RETURN(_SUCCESS)
   end subroutine add_state_extension


   ! Loop over children and propagate unsatisfied imports of each
   subroutine propagate_unsatisfied_imports_all(this, rc)
      class(HierarchicalRegistry), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      type(RegistryPtrMapIterator) :: iter
      type(HierarchicalRegistry), pointer :: child
      integer :: status

      associate (e => this%subregistries%end())
        iter = this%subregistries%begin()
        do while (iter /= e)
           child => this%get_subregistry(iter%first(), _RC)
           call this%propagate_unsatisfied_imports(child, _RC)
           call iter%next()
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine propagate_unsatisfied_imports_all

   ! Loop over virtual pts and propagate any unsatisfied actual pts.
   subroutine propagate_unsatisfied_imports_child(this, child_r, rc)
      class(HierarchicalRegistry), intent(inout) :: this
      type(HierarchicalRegistry), target, intent(in) :: child_r
      integer, optional, intent(out) :: rc

      type(ActualPtVector), pointer :: actual_pts_vector
      type(ActualPtVec_MapIterator) :: iter
      integer :: status

      associate (e => child_r%virtual_pts%end())
        iter = child_r%virtual_pts%begin()
        do while (iter /= e)
           call this%propagate_unsatisfied_imports(child_r, iter, _RC)
           call iter%next()
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine propagate_unsatisfied_imports_child

   ! Loop over unsatisfied imports of child registry and propagate to
   ! parent.
   subroutine propagate_unsatisfied_imports_virtual_pt(this, child_r, iter, rc)
      class(HierarchicalRegistry), intent(inout) :: this
      type(HierarchicalRegistry), target, intent(in) :: child_r
      type(ActualPtVec_MapIterator), intent(in) :: iter
      integer, optional, intent(out) :: rc

      integer :: i
      integer :: status
      class(AbstractStateItemSpec), pointer :: item
      type(VirtualConnectionPt), pointer :: virtual_pt
      type(ActualPtVector), pointer :: actual_pts
      type(ActualConnectionPt), pointer :: actual_pt

      virtual_pt => iter%first()
      actual_pts => iter%second()
      do i = 1, actual_pts%size()
         actual_pt => actual_pts%of(i)
         item => child_r%get_item_spec(actual_pt)
         _ASSERT(associated(item), 'Should not happen.')

         if (actual_pt%is_import() .and. .not. item%is_active()) then
            call this%link_item_spec_virtual(virtual_pt, item, actual_pt%add_comp_name(child_r%get_name()), _RC)
         end if

      end do
      _RETURN(_SUCCESS)

   end subroutine propagate_unsatisfied_imports_virtual_pt

   logical function opt(arg)
      logical, optional, intent(in) :: arg

      opt = .false.
      if (present(arg)) then
         opt = arg
      end if

   end function opt


   function get_actual_pts(this, virtual_pt) result(actual_pts)
      type(ActualPtVector), pointer :: actual_pts
      class(HierarchicalRegistry), target, intent(in) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt

      integer :: status

      ! failure is ok; just returns null pointer
      actual_pts => this%virtual_pts%at(virtual_pt, rc=status)

   end function get_actual_pts

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(HierarchicalRegistry), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      type(ActualPtVec_MapIterator) :: virtual_iter
      type(ActualConnectionPt), pointer :: actual_pt

      write(unit,*,iostat=iostat,iomsg=iomsg) new_line('a')
      if (iostat /= 0) return

      call write_header(this, iostat=iostat, iomsg=iomsg)
      if (iostat /= 0) return

      call write_virtual_pts(this, iostat=iostat, iomsg=iomsg)
      if (iostat /= 0) return

   contains
      
      subroutine write_header(this, iostat, iomsg)
         class(HierarchicalRegistry), intent(in) :: this
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
         
         write(unit,'(a,a,a,i0,a,i0,a,i0,a)',iostat=iostat,iomsg=iomsg) &
              'HierarchicalRegistry(name=', this%name, &
              ', n_local=', this%local_specs%size(), &
              ', n_actual=', this%actual_specs_map%size(), &
              ', n_virtual=', this%virtual_pts%size(), ')'// new_line('a')
         if (iostat /= 0) return
         write(unit,*,iostat=iostat,iomsg=iomsg) '   actuals: '// new_line('a')
      end subroutine write_header

      subroutine write_virtual_pts(this, iostat, iomsg)
         class(HierarchicalRegistry), target, intent(in) :: this
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write(unit,*,iostat=iostat,iomsg=iomsg) '   virtuals: '// new_line('a')
         if (iostat /= 0) return
         associate (e => this%virtual_pts%end())
           virtual_iter = this%virtual_pts%begin()
           do while (virtual_iter /= e)
              associate (virtual_pt => virtual_iter%first())
                write(unit,*,iostat=iostat,iomsg=iomsg)'        ',virtual_pt,  new_line('a')
                if (iostat /= 0) return
                call write_actual_pts(this, virtual_pt, iostat=iostat, iomsg=iomsg)
                if (iostat /= 0) return

              end associate
              call virtual_iter%next()
           end do
         end associate
      end subroutine write_virtual_pts

      subroutine write_actual_pts(this, virtual_pt, iostat, iomsg)
         class(HierarchicalRegistry), target, intent(in) :: this
         type(VirtualConnectionPt), intent(in) :: virtual_pt
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         type(ActualPtVector), pointer :: actual_pts
         type(ActualConnectionPt), pointer :: actual_pt
         integer :: i

         actual_pts => this%virtual_pts%at(virtual_pt, rc=iostat)
         if (iostat /= 0) return

         do i = 1, actual_pts%size()
            actual_pt => actual_pts%of(i)
            write(unit,*,iostat=iostat,iomsg=iomsg)'           ',actual_pt, new_line('a')
            if (iostat /= 0) return
         end do

      end subroutine write_actual_pts
      
   end subroutine write_formatted

   subroutine allocate(this, rc)
      class(HierarchicalRegistry), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i, j
      type(ActualPtVector) :: dependencies
      type(StateItemSpecPtr), allocatable :: dependency_specs(:)
      class(AbstractStateItemSpec), pointer :: item_spec

      do i = 1, this%local_specs%size()
         item_spec => this%local_specs%of(i)
         if (item_spec%is_active()) then
            call item_spec%allocate(_RC)
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine allocate

   function get_extensions(this) result(extensions)
      type(ExtensionVector) :: extensions
      class(HierarchicalRegistry), intent(in) :: this

      extensions = this%extensions
   end function get_extensions

   subroutine add_to_states(this, multi_state, mode, rc)
      use esmf
      use mapl3g_MultiState
      class(HierarchicalRegistry), target, intent(inout) :: this
      type(MultiState), intent(inout) :: multi_state
      character(*), intent(in) :: mode
      integer, optional, intent(out) :: rc
      
      integer :: status
      type(ActualPtSpecPtrMapIterator) :: actual_iter
      type(ActualConnectionPt), pointer :: actual_pt
      type(StateItemSpecPtr), pointer :: item_spec_ptr
      class(AbstractStateItemSpec), pointer :: item_spec

      _ASSERT(any([mode == 'user', mode == 'outer']), 'invalid mode: <' // mode // '>')

      associate (e => this%actual_specs_map%end())

        actual_iter = this%actual_specs_map%begin()
        do while (actual_iter /= e)

           actual_pt => actual_iter%first()

           if (actual_pt%is_represented_in(mode)) then
              item_spec_ptr =>  actual_iter%second()
              item_spec => item_spec_ptr%ptr
              call item_spec%add_to_state(multi_state, actual_pt, _RC)
           end if

           call actual_iter%next()
        end do
      end associate

      _RETURN(_SUCCESS)

   end subroutine add_to_states

   subroutine report(this, rc)
      use mapl3g_FieldSpec
      class(HierarchicalRegistry), target, intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ActualPtSpecPtrMapIterator) :: actual_iter
      type(ActualConnectionPt), pointer :: actual_pt
      type(StateItemSpecPtr), pointer :: item_spec_ptr
      class(AbstractStateItemSpec), pointer :: item_spec

      associate (e => this%actual_specs_map%end())
        actual_iter = this%actual_specs_map%begin()
        do while (actual_iter /= e)
           actual_pt => actual_iter%first()
           item_spec_ptr =>  actual_iter%second()
           item_spec => item_spec_ptr%ptr
           call actual_iter%next()
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine report


   ! Loop over children and propagate unsatisfied imports of each
   subroutine propagate_exports_all(this, rc)
      class(HierarchicalRegistry), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      type(RegistryPtrMapIterator) :: iter
      type(HierarchicalRegistry), pointer :: child
      integer :: status

      associate (e => this%subregistries%end())
        iter = this%subregistries%begin()
        do while (iter /= e)
           child => this%get_subregistry(iter%first(), _RC)
           call this%propagate_exports(child, _RC)
           call iter%next()
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine propagate_exports_all


   subroutine propagate_exports_child(this, child_r, rc)
      class(HierarchicalRegistry), intent(inout) :: this
      type(HierarchicalRegistry), target, intent(in) :: child_r
      integer, optional, intent(out) :: rc

      type(ActualPtVector), pointer :: actual_pts_vector
      type(ActualPtVec_MapIterator) :: iter
      integer :: status

      associate (e => child_r%virtual_pts%end())
        iter = child_r%virtual_pts%begin()
        do while (iter /= e)
           call this%propagate_exports(child_r, iter, _RC)
           call iter%next()
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine propagate_exports_child

   subroutine propagate_exports_virtual_pt(this, child_r, iter, rc)
      class(HierarchicalRegistry), intent(inout) :: this
      type(HierarchicalRegistry), target, intent(in) :: child_r
      type(ActualPtVec_MapIterator), intent(in) :: iter
      integer, optional, intent(out) :: rc

      integer :: i
      integer :: status
      class(AbstractStateItemSpec), pointer :: item
      type(VirtualConnectionPt), pointer :: virtual_pt
      type(VirtualConnectionPt) :: parent_vpt
      type(ActualPtVector), pointer :: actual_pts
      type(ActualConnectionPt), pointer :: actual_pt

      virtual_pt => iter%first()
      actual_pts => iter%second()

      do i = 1, actual_pts%size()

         actual_pt => actual_pts%of(i)
         if (.not. actual_pt%is_export()) cycle

         item => child_r%get_item_spec(actual_pt)
         _ASSERT(associated(item), 'Inconsistent map in hierarchy.')

         parent_vpt = virtual_pt%add_comp_name(child_r%name)
         call this%link_item_spec_virtual(parent_vpt, item, actual_pt%add_comp_name(child_r%get_name()), _RC)

      end do

      _RETURN(_SUCCESS)
   end subroutine propagate_exports_virtual_pt



   function get_range(this) result(range)
      type(ActualPtVec_MapIterator) :: range(2)
      class(HierarchicalRegistry), target, intent(in) :: this

      range(1) = this%virtual_pts%begin()
      range(2) = this%virtual_pts%end()
   end function get_range


   function filter(this, pattern) result(matches)
      type(VirtualConnectionPtVector) :: matches
      class(HierarchicalRegistry), target, intent(in) :: this
      type(VirtualConnectionPt), intent(in) :: pattern

      type(VirtualConnectionPt), pointer :: v_pt
      type(ActualPtVec_MapIterator) :: iter
      
      associate (e => this%virtual_pts%end())
        iter = this%virtual_pts%begin()
        do while (iter /= e)
           v_pt => iter%first()

           if (pattern%matches(v_pt)) call matches%push_back(v_pt)
           
           call iter%next()
        end do
      end associate

   end function filter

end module mapl3g_HierarchicalRegistry
