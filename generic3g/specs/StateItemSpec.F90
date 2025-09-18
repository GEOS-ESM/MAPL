#include "MAPL.h"

module mapl3g_StateItemSpec
   use mapl3g_AspectId
   use mapl3g_ActualConnectionPt
   use mapl3g_VirtualConnectionPtVector
   use mapl3g_ExtensionTransform
   use mapl3g_MultiState
   use mapl3g_StateItemAspect
   use mapl3g_GeomAspect
   use mapl3g_VerticalGridAspect
   use mapl3g_ClassAspect
   use mapl3g_VerticalGrid
   use mapl_ErrorHandling
   use mapl3g_Field_API
   use mapl3g_UnitsAspect
   use mapl3g_TypeKindAspect
   use esmf
   use gftl2_stringvector
   implicit none
   private

   public :: check
   public :: StateItemSpec
   public :: new_StateItemSpec
   public :: StateItemSpecPtr
#ifndef __GFORTRAN__
   public :: assignment(=)
#endif
   type :: StateItemSpec
      private

      type(StateItemAllocation) :: allocation_status = STATEITEM_ALLOCATION_INVALID
      type(VirtualConnectionPtVector) :: dependencies

      type(AspectMap) :: aspects
      logical :: has_deferred_aspects_ = .false.
      type(esmf_StateIntent_Flag) :: state_intent
   contains

      procedure :: get_aspect_order ! as string vector
      procedure :: get_aspect_priorities ! default implementation as aid to refactoring
      procedure :: make_extension

!#      procedure(I_write_formatted), deferred :: write_formatted
!##ifndef __GFORTRAN__
!#      generic :: write(formatted) => write_formatted
!##endif

      procedure, non_overridable :: set_allocation_status
      procedure, non_overridable :: get_allocation_status

      procedure, non_overridable :: set_allocated
      procedure, non_overridable :: is_allocated
      procedure, non_overridable :: is_active
      procedure, non_overridable :: activate
      procedure, non_overridable :: has_deferred_aspects
      procedure, non_overridable :: set_has_deferred_aspects
      procedure :: get_aspect_by_id
      generic :: get_aspect => get_aspect_by_id
      procedure :: get_aspects
      procedure :: set_aspect

      procedure :: get_dependencies
      procedure :: set_dependencies

      procedure :: create
      procedure :: destroy
      procedure :: allocate

      procedure :: connect
      procedure :: connect_to_import
      procedure :: connect_to_export
      procedure :: can_connect_to
      procedure :: add_to_state

      procedure :: set_geometry
   end type StateItemSpec

   type :: StateItemSpecPtr
      class(StateItemSpec), pointer :: ptr => null()
   end type StateItemSpecPtr

   interface StateItemSpec
      procedure :: new_StateItemSpec
      procedure :: new_StateItemSpec_copy
   end interface StateItemSpec

#ifndef __GFORTRAN__
   interface assignment(=)
      procedure :: copy_item_spec
   end interface assignment(=)
#endif

contains

   function new_StateItemSpec(state_intent, aspects, dependencies, has_deferred_aspects) result(spec)
      type(StateItemSpec) :: spec
      type(AspectMap), intent(in) :: aspects
      type(esmf_StateIntent_Flag), intent(in) :: state_intent
      type(VirtualConnectionPtVector), intent(in) :: dependencies
      logical, optional, intent(in) :: has_deferred_aspects

      spec%state_intent = state_intent
      spec%aspects = aspects
      spec%dependencies = dependencies
      if (present(has_deferred_aspects)) spec%has_deferred_aspects_ = has_deferred_aspects

   end function new_StateItemSpec

   ! New spec should not be in "CREATED" state yet.  It also does not make sense
   ! to propagate dependencies.
   function new_StateItemSpec_copy(orig) result(spec)
      type(StateItemSpec) :: spec
      type(StateItemSpec), intent(in) :: orig

      spec%allocation_status = STATEITEM_ALLOCATION_INVALID
      spec%aspects = orig%aspects
      spec%has_deferred_aspects_ = orig%has_deferred_aspects_
      spec%state_intent = orig%state_intent

   end function new_StateItemSpec_copy

   function new_StateItemSpecPtr(state_item) result(wrap)
      type(StateItemSpecPtr) :: wrap
      class(StateItemSpec), target :: state_item

      wrap%ptr => state_item
   end function new_StateItemSpecPtr
  

   pure subroutine set_allocated(this, allocated)
      class(StateItemSpec), intent(inout) :: this
      logical, optional, intent(in) :: allocated

      
      this%allocation_status =  STATEITEM_ALLOCATION_ALLOCATED
      if (present(allocated)) then
         if (allocated) then
            this%allocation_status = STATEITEM_ALLOCATION_ALLOCATED
         end if
      end if

   end subroutine set_allocated

   pure logical function is_allocated(this)
      class(StateItemSpec), intent(in) :: this
      is_allocated = (this%allocation_status >= STATEITEM_ALLOCATION_ALLOCATED)
   end function is_allocated

   recursive subroutine activate(this, rc)
      class(StateItemSpec), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      class(ClassAspect), pointer :: class_aspect

      call this%set_allocation_status(STATEITEM_ALLOCATION_ACTIVE)

      class_aspect => to_ClassAspect(this%aspects, _RC)
      call class_aspect%activate(_RC)

      _RETURN(_SUCCESS)
   end subroutine activate

   pure logical function is_active(this)
      class(StateItemSpec), intent(in) :: this
      is_active = (this%allocation_status >= STATEITEM_ALLOCATION_ACTIVE)
   end function is_active

   function get_dependencies(this) result(dependencies)
      type(VirtualConnectionPtVector) :: dependencies
      class(StateItemSpec), intent(in) :: this
      dependencies = this%dependencies
   end function get_dependencies

   subroutine set_dependencies(this, dependencies)
      class(StateItemSpec), intent(inout) :: this
      type(VirtualConnectionPtVector), intent(in):: dependencies
      this%dependencies = dependencies
   end subroutine set_dependencies

   function get_aspect_by_id(this, aspect_id, rc) result(aspect)
      class(StateItemAspect), pointer :: aspect
      type(AspectId), intent(in) :: aspect_id
      class(StateItemSpec), target, intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      class(ClassAspect), pointer :: class_aspect
      type(esmf_Field), allocatable :: field
      type(esmf_FieldBundle), allocatable :: bundle
      type(esmf_State), allocatable :: state

      aspect => this%aspects%at(aspect_id, _RC)

      if (this%allocation_status >= STATEITEM_ALLOCATION_CREATED) then
         _ASSERT(this%aspects%count(CLASS_ASPECT_ID) > 0, 'Must have a ClassAspect')

         class_aspect => to_ClassAspect(this%aspects, _RC)
         select case (aspect_id%to_string())
         case ('UNITS', 'TYPEKIND', 'UNGRIDDED_DIMS')
            call class_aspect%get_payload(field, bundle, state, _RC)
            call aspect%update_from_payload(field, bundle, state, _RC)
         case default
            ! do nothing for now
         end select

      end if

      _RETURN(_SUCCESS)
   end function get_aspect_by_id

   function get_aspects(this) result(aspects)
      type(AspectMap), pointer :: aspects
      class(StateItemSpec), target, intent(in) :: this
      aspects => this%aspects
   end function get_aspects

   subroutine set_aspect(this, aspect, rc)
      class(StateItemSpec), target, intent(inout) :: this
      class(StateItemAspect), intent(in) :: aspect
      integer, optional, intent(out) :: rc

      integer :: status
      type(AspectId) :: id
      type(AspectMapIterator) :: iter
      type(AspectPair), pointer :: pair
      class(ClassAspect), pointer :: class_aspect
      type(esmf_Field), allocatable :: field
      type(esmf_FieldBundle), allocatable :: bundle
      type(esmf_State), allocatable :: state

      id = aspect%get_aspect_id()
      iter = this%aspects%find(id)
      pair => iter%of()
      deallocate(pair%second)
      allocate(pair%second, source=aspect)
! Following line breaks under ifort 2021.13
!      call this%aspects%insert(aspect%get_aspect_id(), aspect)

      if (this%allocation_status >= STATEITEM_ALLOCATION_CREATED) then
         class_aspect => to_ClassAspect(this%aspects, _RC)
         select case (id%to_string())
         case ('UNITS', 'TYPEKIND','GEOM','UNGRIDDED_DIMS', 'ATTRIBUTES')
            call class_aspect%get_payload(field, bundle, state, _RC)
            call aspect%update_payload(field, bundle, state, _RC)
         case default
            ! do nothing for now
         end select
      end if

      _RETURN(_SUCCESS)
   end subroutine set_aspect

   ! Delegate to CLASS aspect
   function get_aspect_order(src_spec, dst_spec, rc) result(ids)
      type(AspectId), allocatable :: ids(:)
      class(StateItemSpec), target, intent(in) :: src_spec
      class(StateItemSpec), target, intent(in) :: dst_spec
      integer, optional, intent(out) :: rc

      integer :: status
      class(ClassAspect), pointer :: src_class_aspect

      src_class_aspect => to_ClassAspect(src_spec%aspects, _RC)
      ids = src_class_aspect%get_aspect_order(dst_spec%get_aspects(), _RC)

      _RETURN(_SUCCESS)
   end function get_aspect_order


   ! This procedure should be deleted once extant subclasses of
   ! StateItemSpec have been updated and implement their own.
   function get_aspect_priorities(src_spec, dst_spec) result(order)
      character(:), allocatable :: order
      class(StateItemSpec), intent(in) :: src_spec
      class(StateItemSpec), intent(in) :: dst_spec

      order = ''
   end function get_aspect_priorities

   function make_extension(this, aspect_name, aspect, rc) result(new_spec)
      class(StateItemSpec), allocatable :: new_spec
      class(StateItemSpec), intent(in) :: this
      character(*), intent(in) :: aspect_name
      class(StateItemAspect), intent(in) :: aspect
      integer, optional, intent(out) :: rc

      integer :: status
      
      new_spec = this
      call new_spec%set_aspect(aspect, _RC)
      
      _RETURN(_SUCCESS)
   end function make_extension

   ! Will use ESMF so cannot be PURE
   subroutine create(this, rc)
      class(StateItemSpec), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      class(ClassAspect), pointer :: class_aspect
      integer, allocatable :: handle(:)

      class_aspect => to_ClassAspect(this%aspects, _RC)
      call class_aspect%create(this%aspects, make_handle(this), _RC)

      _RETURN(_SUCCESS)
   contains

      function make_handle(this) result(handle)
         use, intrinsic :: iso_c_binding, only: c_ptr, c_loc
         integer, allocatable :: handle(:)
         type(StateItemSpec), target, intent(in) :: this
         type(c_ptr) :: ptr

         ptr = c_loc(this)
         handle = transfer(ptr, [1])
      end function make_handle

   end subroutine create

   subroutine destroy(this, rc)
      class(StateItemSpec), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      class(ClassAspect), pointer :: class_aspect

      class_aspect => to_ClassAspect(this%aspects, _RC)
      call class_aspect%destroy(_RC)

      _RETURN(_SUCCESS)
   end subroutine destroy

   subroutine allocate(this, rc)
      class(StateItemSpec), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      class(ClassAspect), pointer :: class_aspect
      logical, allocatable :: active, not_connected

      if (this%state_intent == ESMF_STATEINTENT_IMPORT) then
         ! Allow allocation of non-connected imports to support some testing modes
         active = (this%allocation_status >= STATEITEM_ALLOCATION_ACTIVE)
         not_connected = (this%allocation_status < STATEITEM_ALLOCATION_CONNECTED)
         _RETURN_UNLESS(active .and. not_connected)
      end if

      class_aspect => to_ClassAspect(this%aspects, _RC)

      call class_aspect%allocate(this%aspects, _RC)
      call this%set_allocated()

      _RETURN(_SUCCESS)
   end subroutine allocate


   subroutine connect_to_import(this, import, rc)
      class(StateItemSpec), target, intent(inout) :: this
      class(StateItemSpec), target, intent(inout) :: import
      integer, optional, intent(out) :: rc

      integer :: status

      class(ClassAspect), pointer :: src_class_aspect, dst_class_aspect
      type(AspectId) :: aspect_id

      src_class_aspect => to_ClassAspect(this%aspects, _RC)
      dst_class_aspect => to_ClassAspect(import%aspects, _RC)

      aspect_id = src_class_aspect%get_aspect_id()
      aspect_id = dst_class_aspect%get_aspect_id()
      call src_class_aspect%connect_to_import(dst_class_aspect, _RC)

      _RETURN(_SUCCESS)
   end subroutine connect_to_import

   subroutine connect_to_export(this, export, actual_pt, rc)
      class(StateItemSpec), target, intent(inout) :: this
      class(StateItemSpec), target, intent(inout) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      integer :: status

      class(StateItemAspect), pointer :: src_aspect
      class(ClassAspect), pointer :: dst_class_aspect

      src_aspect => export%aspects%at(CLASS_ASPECT_ID, _RC)
      dst_class_aspect => to_ClassAspect(this%aspects, _RC)
      call dst_class_aspect%connect_to_export(src_aspect, actual_pt, _RC)

      _RETURN(_SUCCESS)
   end subroutine connect_to_export

   subroutine connect(import, export, actual_pt, rc)
      class(StateItemSpec), intent(inout) :: import
      class(StateItemSpec), intent(inout) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      integer :: status

      call import%connect_to_export(export, actual_pt, _RC)
      call export%connect_to_import(import, _RC)
      import%allocation_status = STATEITEM_ALLOCATION_CONNECTED
      export%allocation_status = STATEITEM_ALLOCATION_CONNECTED

      _RETURN(_SUCCESS)
   end subroutine connect

   logical function can_connect_to(this, export, rc)
      class(StateItemSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: export
      integer, optional, intent(out) :: rc
      
      integer :: status
      type(AspectMapIterator) :: iter
      type(AspectId) :: aspect_id
      class(StateItemAspect), pointer :: dst_aspect, export_aspect

      can_connect_to = .false.

      iter = this%aspects%ftn_begin()
      associate(e => this%aspects%ftn_end())
        do while (iter /= e)
           call iter%next()

           aspect_id = iter%first()
           dst_aspect => this%aspects%at(aspect_id, _RC)
           export_aspect => export%aspects%at(aspect_id, _RC)
           can_connect_to =  export_aspect%can_connect_to(dst_aspect)
           _RETURN_UNLESS(can_connect_to)

        end do
      end associate

      _RETURN(_SUCCESS)
  end function can_connect_to


  subroutine add_to_state(this, multi_state, actual_pt, rc)
     class(StateItemSpec), target, intent(in) :: this
     type(MultiState), intent(inout) :: multi_state
     type(ActualConnectionPt), intent(in) :: actual_pt
     integer, optional, intent(out) :: rc
  
      integer :: status
      class(ClassAspect), pointer :: class_aspect

      class_aspect => to_ClassAspect(this%aspects, _RC)
      call class_aspect%add_to_state(multi_state, actual_pt, _RC)

      _RETURN(_SUCCESS)
      
   end subroutine add_to_state

   subroutine set_geometry(this, geom, vertical_grid, rc)
      class(StateItemSpec), intent(inout) :: this
      type(ESMF_Geom), optional, intent(in) :: geom
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      integer, optional, intent(out) :: rc

      integer :: status

      call target_set_geom(this, geom, vertical_grid)

      _RETURN(_SUCCESS)

   contains

      ! Helper needed to add target attribute to "this"
      subroutine target_set_geom(this, geom, vertical_grid)
         class(StateItemSpec), target, intent(inout) :: this
         type(ESMF_Geom), optional, intent(in) :: geom
         class(VerticalGrid), optional, intent(in) :: vertical_grid

         class(StateItemAspect), pointer :: poly_aspect
         type(GeomAspect) :: geom_aspect
         type(VerticalGridAspect) :: vertical_grid_aspect

         if (present(geom)) then
            if (this%aspects%count(GEOM_ASPECT_ID) > 0) then
               poly_aspect => this%aspects%at(GEOM_ASPECT_ID, _RC)

               select type (poly_aspect)
               type is (GeomAspect)
                  call poly_aspect%set_geom(geom)
               end select

            else
               call this%aspects%insert(GEOM_ASPECT_ID, GeomAspect(geom))
            end if

         end if

         if (present(vertical_grid)) then
            if (this%aspects%count(VERTICAL_GRID_ASPECT_ID) > 0) then
               poly_aspect => this%aspects%at(VERTICAL_GRID_ASPECT_ID, _RC)

               select type (poly_aspect)
               type is (VerticalGridAspect)
                  call poly_aspect%set_vertical_grid(vertical_grid)
               end select
            else
               call this%aspects%insert(VERTICAL_GRID_ASPECT_ID,  VerticalGridAspect(vertical_grid=vertical_grid, geom=geom))
            end if

         end if

      end subroutine target_set_geom

   end subroutine set_geometry

   recursive subroutine copy_item_spec(a, b)
      type(StateItemSpec), intent(out) :: a
      type(StateItemSpec), intent(in) :: b

      a%state_intent = b%state_intent
      a%aspects = b%aspects
      a%allocation_status = b%allocation_status
      a%dependencies = b%dependencies
      a%has_deferred_aspects_ = b%has_deferred_aspects_

   end subroutine copy_item_spec

   subroutine check(this, file, line)
      class(StateItemSpec), target, intent(in) :: this
      character(*), intent(in) :: file
      integer, intent(in) :: line

      type(AspectMapIterator) :: iter
      type(AspectId) :: aspect_id

      iter = this%aspects%ftn_begin()
      associate(e => this%aspects%ftn_end())
        do while (iter /= e)
           call iter%next()
           aspect_id = iter%first()
        end do
      end associate
      
   end subroutine check

   subroutine set_has_deferred_aspects(this, has_deferred_aspects)
      class(StateItemSpec), intent(inout) :: this
      logical, intent(in) :: has_deferred_aspects

      this%has_deferred_aspects_ = has_deferred_aspects
   end subroutine set_has_deferred_aspects

   logical function has_deferred_aspects(this) result(flag)
      class(StateItemSpec), intent(in) :: this

      flag = this%has_deferred_aspects_

   end function has_deferred_aspects

   subroutine set_allocation_status(this, allocation_status)
      class(StateItemSpec), intent(inout) :: this
      type(StateItemAllocation), intent(in) :: allocation_status

      this%allocation_status = allocation_status
   end subroutine set_allocation_status

   function get_allocation_status(this) result(allocation_status)
      type(StateItemAllocation) :: allocation_status
      class(StateItemSpec), intent(in) :: this

      allocation_status = this%allocation_status
   end function get_allocation_status

end module mapl3g_StateItemSpec
