
#include "MAPL_Generic.h"

module mapl3g_HierarchicalRegistry
   use mapl3g_AbstractRegistry
   use mapl3g_AbstractStateItemSpec
   use mapl3g_StateItemSpecPtr
   use mapl3g_ActualPtSpecPtrMap
   use mapl3g_ConnectionPt
   use mapl3g_VirtualConnectionPt
   use mapl3g_ActualConnectionPt
   use mapl3g_StateItemVector
   use mapl3g_RegistryPtr
   use mapl3g_RegistryPtrMap
   use mapl3g_ActualPtVector
   use mapl3g_ActualPtSpecPtrMap
   use mapl3g_ActualPtVec_Map
   use mapl3g_ConnectionSpec
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   implicit none
   private
  
   public :: HierarchicalRegistry
  
   type, extends(AbstractRegistry) :: HierarchicalRegistry
      private
      character(:), allocatable :: name
      
      type(StateItemVector) :: local_specs ! specs for items "owned" by gridcomp
      type(ActualPtSpecPtrMap) :: actual_specs_map ! all items in states of gridcomp
      type(ActualPtVec_Map) :: actual_pts_map     ! Grouping of items with shared virtual connection point

      ! Hierarchy/tree aspect
      type(RegistryPtrMap) :: subregistries
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

      procedure :: add_extension

      procedure :: propagate_unsatisfied_imports_all
      procedure :: propagate_unsatisfied_imports_child
      procedure :: propagate_unsatisfied_imports_virtual_pt
      generic :: propagate_unsatisfied_imports => propagate_unsatisfied_imports_all
      generic :: propagate_unsatisfied_imports => propagate_unsatisfied_imports_child
      generic :: propagate_unsatisfied_imports => propagate_unsatisfied_imports_virtual_pt

      procedure :: add_connection
      procedure :: connect_sibling
      procedure :: connect_export2export

      procedure :: allocate

      procedure :: write_formatted
      generic :: write(formatted) => write_formatted
      procedure :: report
   end type HierarchicalRegistry

   interface HierarchicalRegistry
      module procedure new_HierarchicalRegistry_leaf
      module procedure new_HierarchicalRegistry_parent
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
      integer :: i
      type(ActualPtVector), pointer :: actual_pts
      type(ActualConnectionPt), pointer :: actual_pt

      actual_pts => this%actual_pts_map%at(virtual_pt, _RC)

      associate ( n => actual_pts%size() )
        allocate(specs(n))
        do i = 1, n
           actual_pt => actual_pts%of(i)
           specs(i)%ptr => this%get_item_spec(actual_pt, _RC)
        end do
      end associate

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
   
   subroutine add_item_spec_virtual_override(this, virtual_pt, spec, actual_pt, rc)
      class(HierarchicalRegistry), intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      class(AbstractStateItemSpec), target, intent(in) :: spec
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      integer :: status

      call this%add_extension(virtual_pt, actual_pt)
      call this%add_item_spec(actual_pt, spec, _RC)

      _RETURN(_SUCCESS)
   end subroutine add_item_spec_virtual_override
   

   subroutine add_extension(this, virtual_pt, actual_pt)
      class(HierarchicalRegistry), target, intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      type(ActualConnectionPt), intent(in) :: actual_pt

      type(ActualPtVector), pointer :: actual_pts

      associate (extensions => this%actual_pts_map)
        if (extensions%count(virtual_pt) == 0) then
           call extensions%insert(virtual_pt, ActualPtVector())
        end if
        actual_pts => this%actual_pts_map%of(virtual_pt)
        call actual_pts%push_back(actual_pt)
      end associate
      
   end subroutine add_extension


   ! This procedure is used when a child import/export must be propagated to parent.
   subroutine link_item_spec_virtual(this, virtual_pt, spec, actual_pt, rc)
      class(HierarchicalRegistry), intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      class(AbstractStateItemSpec), target :: spec
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      integer :: status

      call this%add_extension(virtual_pt, actual_pt)
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
      has_item_spec = (this%actual_pts_map%count(virtual_pt) > 0)
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
      if (comp_name == this%get_name()) then
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
   subroutine add_connection(this, connection, rc)
      class(HierarchicalRegistry), target, intent(inout) :: this
      type(ConnectionSpec), intent(in) :: connection
      integer, optional, intent(out) :: rc

      type(HierarchicalRegistry), pointer :: src_registry, dst_registry
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

        ! Non-sibling connection: just propagate pointer "up"
           call this%connect_export2export(src_registry, connection, _RC)
      end associate
      
      _RETURN(_SUCCESS)
   end subroutine add_connection

   subroutine connect_sibling(this, src_registry, connection, unusable, rc)
      class(HierarchicalRegistry), target, intent(in) :: this
      type(HierarchicalRegistry), target, intent(in) :: src_registry
      type(ConnectionSpec), intent(in) :: connection
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(StateItemSpecPtr), allocatable :: export_specs(:), import_specs(:)
      class(AbstractStateItemSpec), pointer :: export_spec, import_spec
      integer :: i, j
      logical :: satisfied
      integer :: status

      associate (src_pt => connection%source, dst_pt => connection%destination)

        import_specs = this%get_actual_pt_SpecPtrs(dst_pt%v_pt, _RC)
        export_specs = src_registry%get_actual_pt_SpecPtrs(src_pt%v_pt, _RC)
        do i = 1, size(import_specs)
           import_spec => import_specs(i)%ptr
           satisfied = .true.
           do j = 1, size(export_specs)
              export_spec => export_specs(j)%ptr
              if (import_spec%can_connect_to(export_spec)) then
                 call export_spec%set_active()
                 call import_spec%connect_to(export_spec, _RC)
                 satisfied = .true.
                 exit
              end if
           end do

           _ASSERT(satisfied,'no matching actual export spec found')
        end do
      end associate

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine connect_sibling

   subroutine connect_export2export(this, src_registry, connection, unusable, rc)
      class(HierarchicalRegistry), intent(inout) :: this
      type(HierarchicalRegistry), intent(in) :: src_registry
      type(ConnectionSpec), intent(in) :: connection
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ActualPtVectorIterator) :: iter
      class(AbstractStateItemSpec), pointer :: spec
      type(ActualConnectionPt), pointer :: src_actual_pt
      type(ActualConnectionPt), allocatable :: dst_actual_pt
      type(ActualPtVector), pointer :: actual_pts
      integer :: status

      associate (src_pt => connection%source%v_pt, dst_pt => connection%destination%v_pt)
        _ASSERT(this%actual_pts_map%count(dst_pt) == 0, 'Specified virtual point already exists in this registry')
        actual_pts => src_registry%get_actual_pts(src_pt)
        associate (e => actual_pts%end())
          iter = actual_pts%begin()
          do while (iter /= e)
             src_actual_pt => iter%of()
             if (src_actual_pt%is_internal()) then
                ! Don't encode with comp name
                dst_actual_pt = ActualConnectionPt(dst_pt)
             else
                dst_actual_pt = ActualConnectionPt(dst_pt%add_comp_name(src_registry%get_name()))
             end if
             dst_actual_pt = extend(dst_actual_pt)
             
             spec => src_registry%get_item_spec(src_actual_pt)
             _ASSERT(associated(spec), 'This should not happen.')
             call this%link_item_spec(dst_pt, spec, dst_actual_pt, _RC)
             call iter%next()
          end do
        end associate
      end associate
      
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   contains

      function str_replace(buffer, pattern, replacement) result(new_str)
         character(:), allocatable :: new_str
         character(*), intent(in) :: buffer
         character(*), intent(in) :: pattern
         character(*), intent(in) :: replacement

         integer :: idx

         idx = scan(buffer, pattern)
         new_str = buffer(:idx-1) // replacement // buffer(idx+len(pattern):)
      end function str_replace

   end subroutine connect_export2export

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

      associate (e => child_r%actual_pts_map%end())
        iter = child_r%actual_pts_map%begin()
        do while (iter /= e)
           call this%propagate_unsatisfied_imports_virtual_pt(child_r, iter, _RC)
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
            call this%link_item_spec_virtual(virtual_pt, item, extend(actual_pt%add_comp_name(child_r%get_name())), _RC)
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
      actual_pts => this%actual_pts_map%at(virtual_pt, rc=status)

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

      call write_actual_pts(this, iostat=iostat, iomsg=iomsg)
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
              ', n_virtual=', this%actual_pts_map%size(), ')'// new_line('a')
         if (iostat /= 0) return
         write(unit,*,iostat=iostat,iomsg=iomsg) '   actuals: '// new_line('a')
      end subroutine write_header

      subroutine write_virtual_pts(this, iostat, iomsg)
         class(HierarchicalRegistry), target, intent(in) :: this
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write(unit,*,iostat=iostat,iomsg=iomsg) '   virtuals: '// new_line('a')
         if (iostat /= 0) return
         associate (e => this%actual_pts_map%end())
           virtual_iter = this%actual_pts_map%begin()
           do while (virtual_iter /= e)
              associate (virtual_pt => virtual_iter%first())
                write(unit,*,iostat=iostat,iomsg=iomsg)'        ',virtual_pt,  new_line('a')
                if (iostat /= 0) return
              end associate
              call virtual_iter%next()
           end do
         end associate
      end subroutine write_virtual_pts

      subroutine write_actual_pts(this, iostat, iomsg)
         class(HierarchicalRegistry), target, intent(in) :: this
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
         
         type(ActualPtSpecPtrMapIterator) :: actual_iter

         associate (e => this%actual_specs_map%end())
           actual_iter = this%actual_specs_map%begin()
           do while (actual_iter /= e)
              actual_pt => actual_iter%first()
              write(unit,*,iostat=iostat,iomsg=iomsg)'        ',actual_pt, new_line('a')
              if (iostat /= 0) return
              call actual_iter%next()
           end do
         end associate
      end subroutine write_actual_pts
      
   end subroutine write_formatted

   subroutine allocate(this, rc)
      class(HierarchicalRegistry), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      class(AbstractStateItemSpec), pointer :: item_spec

      do i = 1, this%local_specs%size()
         item_spec => this%local_specs%of(i)
         if (item_spec%is_active()) then
            call item_spec%allocate(_RC)
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine allocate

   subroutine add_to_states(this, unusable, importState, exportState, internalState, rc)
      use esmf
      class(HierarchicalRegistry), target, intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_State), intent(inout) :: importState, exportState, internalState
      integer, optional, intent(out) :: rc
      
      integer :: status
      type(ActualPtSpecPtrMapIterator) :: actual_iter
      type(ActualConnectionPt), pointer :: actual_pt
      type(StateItemSpecPtr), pointer :: item_spec_ptr
      class(AbstractStateItemSpec), pointer :: item_spec
      character(:), allocatable :: name

      associate (e => this%actual_specs_map%end())

        actual_iter = this%actual_specs_map%begin()
        do while (actual_iter /= e)

           actual_pt => actual_iter%first()
           name = actual_pt%get_esmf_name()

           item_spec_ptr =>  actual_iter%second()
           item_spec => item_spec_ptr%ptr

           select case (actual_pt%get_state_intent())
           case ('import')
              call item_spec%add_to_state(importState, name, _RC)
           case ('export')
              call item_spec%add_to_state(exportState, name, _RC)
           case ('internal')
              call item_spec%add_to_state(internalState, name, _RC)
           case default
              _FAIL('Incorrect specification of state intent for <'//actual_pt%get_esmf_name()//'>.')
           end select

           call actual_iter%next()
        end do
      end associate
      
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
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

           select type (item_spec)
           type is (FieldSpec)
              print*, this%name, '::',actual_pt, '; complete? ', item_spec%check_complete()
           end select
           call actual_iter%next()
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine report

end module mapl3g_HierarchicalRegistry
