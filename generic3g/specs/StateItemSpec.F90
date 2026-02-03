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
   use mapl3g_FieldBundle_API
   use mapl3g_ComponentDriver
   use mapl3g_GriddedComponentDriver
   use mapl3g_ComponentDriverVector
   use mapl3g_GenericCoupler
   use esmf
   use gftl2_stringvector
   implicit none
   private

   public :: check
   public :: StateItemSpec
   public :: new_StateItemSpec
   public :: StateItemSpecPtr
   type :: StateItemSpec
      private

      type(VirtualConnectionPtVector) :: dependencies

      type(AspectMap) :: aspects
      logical :: has_deferred_aspects_ = .false.
      type(esmf_StateIntent_Flag) :: state_intent

      ! Producer/consumer tracking (merged from StateItemExtension)
      type(ComponentDriverVector) :: consumers ! couplers that depend on this spec
      class(ComponentDriver), pointer :: producer => null() ! coupler that computes this spec
   contains

      procedure :: get_aspect_order ! as string vector
      procedure :: get_aspect_priorities ! default implementation as aid to refactoring
      procedure :: clone_base
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

      ! Producer/consumer methods (merged from StateItemExtension)
      procedure :: has_producer
      procedure :: get_producer
      procedure :: set_producer
      procedure :: has_consumers
      procedure :: add_consumer
      procedure :: get_consumers

      procedure :: set_geometry
      procedure :: print_spec
      procedure :: update_from_payload
   end type StateItemSpec

   type :: StateItemSpecPtr
      class(StateItemSpec), pointer :: ptr => null()
   end type StateItemSpecPtr

   interface StateItemSpec
      procedure :: new_StateItemSpec
   end interface StateItemSpec

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


   function new_StateItemSpecPtr(state_item) result(wrap)
      type(StateItemSpecPtr) :: wrap
      class(StateItemSpec), target :: state_item

      wrap%ptr => state_item
   end function new_StateItemSpecPtr
  

   subroutine set_allocated(this, allocated, rc)
      class(StateItemSpec), target, intent(inout) :: this
      logical, optional, intent(in) :: allocated
      integer, optional, intent(out) :: rc

      integer :: status
      
      call this%set_allocation_status(STATEITEM_ALLOCATION_ALLOCATED, _RC)
      if (present(allocated)) then
         if (allocated) then
            call this%set_allocation_status(STATEITEM_ALLOCATION_ALLOCATED, _RC)
         end if
      end if

      _RETURN(_SUCCESS)
   end subroutine set_allocated

   logical function is_allocated(this, rc)
      class(StateItemSpec), target, intent(in) :: this
      integer, optional, intent(out) :: rc
      integer :: status
      type(StateItemAllocation) :: allocation_status
      
      allocation_status = this%get_allocation_status(_RC)
      is_allocated = (allocation_status >= STATEITEM_ALLOCATION_ALLOCATED)
      _RETURN(_SUCCESS)
   end function is_allocated

   recursive subroutine activate(this, rc)
      class(StateItemSpec), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      class(ClassAspect), pointer :: class_aspect

      call this%set_allocation_status(STATEITEM_ALLOCATION_ACTIVE, _RC)

      class_aspect => to_ClassAspect(this%aspects, _RC)
      call class_aspect%activate(_RC)

      _RETURN(_SUCCESS)
   end subroutine activate

   logical function is_active(this, rc)
      class(StateItemSpec), target, intent(in) :: this
      integer, optional, intent(out) :: rc
      integer :: status
      type(StateItemAllocation) :: allocation_status
      
      allocation_status = this%get_allocation_status(_RC)
      is_active = (allocation_status >= STATEITEM_ALLOCATION_ACTIVE)
      _RETURN(_SUCCESS)
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

      class_aspect => to_ClassAspect(this%aspects, _RC)
      call class_aspect%get_payload(field=field, bundle=bundle, state=state, _RC)
      call aspect%update_from_payload(field=field, bundle=bundle, state=state, _RC)

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

      type(AspectId) :: id
      type(AspectMapIterator) :: iter
      type(AspectPair), pointer :: pair

      id = aspect%get_aspect_id()
      iter = this%aspects%find(id)
      pair => iter%of()
      deallocate(pair%second)

      allocate(pair%second, source=aspect)
! Following line breaks under ifort 2021.13
      !      call this%aspects%insert(aspect%get_aspect_id(), aspect)

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

      _UNUSED_DUMMY(src_spec)
      _UNUSED_DUMMY(dst_spec)
   end function get_aspect_priorities

   ! Factory method to create a base for an extension
   ! Copies metadata and aspects but NOT producer/consumer chain
   function clone_base(this, rc) result(new_spec)
      type(StateItemSpec) :: new_spec
      class(StateItemSpec), target, intent(in) :: this
      integer, optional, intent(out) :: rc

      ! Copy basic metadata using regular assignment
      ! This includes aspects, which will be copied by AspectMap's assignment
      new_spec%state_intent = this%state_intent
      new_spec%aspects = this%aspects
      new_spec%dependencies = this%dependencies  
      new_spec%has_deferred_aspects_ = this%has_deferred_aspects_
      
      ! Producer/consumers are intentionally NOT copied (left as null/empty)
      ! This is the key difference from regular assignment
      
      _RETURN(_SUCCESS)
   end function clone_base

   ! Factory method to create an extension with couplers
   ! This creates a new spec that extends this one toward the goal_spec,
   ! setting up the necessary transform couplers
   recursive function make_extension(this, goal_spec, rc) result(new_spec)
      type(StateItemSpec), target :: new_spec
      class(StateItemSpec), target, intent(inout) :: this
      type(StateItemSpec), target, intent(in) :: goal_spec
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      class(ExtensionTransform), allocatable :: transform
      class(ComponentDriver), pointer :: producer
      class(ComponentDriver), pointer :: source
      type(ESMF_GridComp) :: coupler_gridcomp
      type(AspectId), allocatable :: aspect_ids(:)
      class(StateItemAspect), pointer :: src_aspect, dst_aspect
      type(AspectMap), pointer :: other_aspects

      call this%activate(_RC)
      call this%update_from_payload(_RC)

      new_spec = this%clone_base()

      aspect_ids = this%get_aspect_order(goal_spec)
      do i = 1, size(aspect_ids)

         src_aspect => new_spec%get_aspect(aspect_ids(i), _RC)
         _ASSERT(associated(src_aspect), 'src aspect not found')

         dst_aspect => goal_spec%get_aspect(aspect_ids(i), _RC)
         _ASSERT(associated(dst_aspect), 'dst aspect not found')

         _ASSERT(src_aspect%can_connect_to(dst_aspect), 'cannot connect aspect ' // aspect_ids(i)%to_string())
         if (src_aspect%needs_extension_for(dst_aspect)) then
            other_aspects => new_spec%get_aspects()
            allocate(transform, source=src_aspect%make_transform(dst_aspect, other_aspects, rc=status))
            _VERIFY(status)

            call new_spec%set_aspect(dst_aspect, _RC)

            exit
         end if

      end do

      if (allocated(transform)) then
         
         call new_spec%create(_RC)
         call new_spec%activate(_RC)
         source => this%get_producer()
         coupler_gridcomp = make_coupler(transform, source, _RC)
         producer => this%add_consumer(GriddedComponentDriver(coupler_gridcomp), _RC)
         call new_spec%set_producer(producer, _RC)

         _RETURN(_SUCCESS)
      end if

      _RETURN(_SUCCESS)
   end function make_extension

   ! Will use ESMF so cannot be PURE
   subroutine create(this, rc)
      class(StateItemSpec), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      class(ClassAspect), pointer :: class_aspect
      type(esmf_Field), allocatable :: field
      type(esmf_FieldBundle), allocatable :: bundle
      type(esmf_State), allocatable :: state
      
      class_aspect => to_ClassAspect(this%aspects, _RC)
      call class_aspect%create(this%aspects, _RC)
      call class_aspect%get_payload(field=field, bundle=bundle, state=state, _RC)
      call update_payload_from_aspects(this, field=field, bundle=bundle, state=state, _RC)

      if (allocated(field)) then
         call mapl_FieldSet(field, has_deferred_aspects=this%has_deferred_aspects_, _RC)
      end if
      if (allocated(bundle)) then
         call mapl_FieldBundleSet(bundle, has_deferred_aspects=this%has_deferred_aspects_, _RC)
      end if
      
      _RETURN(_SUCCESS)
   contains

      subroutine update_payload_from_aspects(this, field, bundle, state, rc)
         class(StateItemSpec), target, intent(in) :: this
         type(esmf_Field), optional, intent(inout) :: field
         type(esmf_FieldBundle), optional, intent(inout) :: bundle
         type(esmf_State), optional, intent(inout) :: state
         integer, optional, intent(out) :: rc

         ! allocatable to be "not-present" in other calls
         type(AspectMapIterator) :: iter
         class(StateItemAspect), pointer :: aspect
         integer :: status

         associate(e => this%aspects%ftn_end())
           iter = this%aspects%ftn_begin()
           do while (iter /= e)
             call iter%next()
             aspect => iter%second()
             call aspect%update_payload(field=field, bundle=bundle, state=state, _RC)
          end do
         end associate

         _RETURN(_SUCCESS)
      end subroutine update_payload_from_aspects

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
      type(StateItemAllocation) :: allocation_status

      if (this%state_intent == ESMF_STATEINTENT_IMPORT) then
         ! Allow allocation of non-connected imports to support some testing modes
         allocation_status = this%get_allocation_status(_RC)
         active = (allocation_status >= STATEITEM_ALLOCATION_ACTIVE)
         not_connected = (allocation_status < STATEITEM_ALLOCATION_CONNECTED)
         _RETURN_UNLESS(active .and. not_connected)
      end if

      class_aspect => to_ClassAspect(this%aspects, _RC)

      call class_aspect%allocate(this%aspects, _RC)
      call this%set_allocated(_RC)

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
      call import%set_allocation_status(STATEITEM_ALLOCATION_CONNECTED, _RC)
      call export%set_allocation_status(STATEITEM_ALLOCATION_CONNECTED, _RC)

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
      
      _UNUSED_DUMMY(file)
      _UNUSED_DUMMY(line)
   end subroutine check

   subroutine set_has_deferred_aspects(this, has_deferred_aspects)
      class(StateItemSpec), intent(inout) :: this
      logical, intent(in) :: has_deferred_aspects

      this%has_deferred_aspects_ = has_deferred_aspects
   end subroutine set_has_deferred_aspects

   logical function has_deferred_aspects(this, rc)
      class(StateItemSpec), target, intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      class(ClassAspect), pointer :: class_aspect
      type(esmf_Field), allocatable :: field
      type(esmf_FieldBundle), allocatable :: bundle
      type(esmf_State), allocatable :: state

      has_deferred_aspects = .false. ! default

      class_aspect => to_ClassAspect(this%aspects, _RC)
      call class_aspect%get_payload(field=field, bundle=bundle, state=state, _RC)

      if (allocated(field)) then
         call mapl_FieldGet(field, has_deferred_aspects=has_deferred_aspects, _RC)
      end if

      if (allocated(bundle)) then
         call mapl_FieldBundleGet(bundle, has_deferred_aspects=has_deferred_aspects, _RC)
      end if

      if (allocated(state)) then
         _FAIL('unsupported use case')
      end if
      
      _RETURN(_SUCCESS)
   end function has_deferred_aspects

   subroutine set_allocation_status(this, allocation_status, rc)
      class(StateItemSpec), target, intent(inout) :: this
      type(StateItemAllocation), intent(in) :: allocation_status
      integer, optional, intent(out) :: rc

      integer :: status
      class(ClassAspect), pointer :: class_aspect
      type(esmf_Field), allocatable :: field
      type(esmf_FieldBundle), allocatable :: bundle

      class_aspect => to_ClassAspect(this%aspects, _RC)
      call class_aspect%get_payload(field=field, bundle=bundle, _RC)
      
      if (allocated(field)) then
         call MAPL_FieldSet(field, allocation_status=allocation_status, _RC)
      end if
      
      if (allocated(bundle)) then
         call MAPL_FieldBundleSet(bundle, allocation_status=allocation_status, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine set_allocation_status

   function get_allocation_status(this, rc) result(allocation_status)
      type(StateItemAllocation) :: allocation_status
      class(StateItemSpec), target, intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      class(ClassAspect), pointer :: class_aspect
      type(esmf_Field), allocatable :: field
      type(esmf_FieldBundle), allocatable :: bundle

      ! Default to INVALID in case we can't get it from the payload
      allocation_status = STATEITEM_ALLOCATION_INVALID

      class_aspect => to_ClassAspect(this%aspects, _RC)
      call class_aspect%get_payload(field=field, bundle=bundle, _RC)
      
      if (allocated(field)) then
         call MAPL_FieldGet(field, allocation_status=allocation_status, _RC)
      end if
      
      if (allocated(bundle)) then
         call MAPL_FieldBundleGet(bundle, allocation_status=allocation_status, _RC)
      end if

      _RETURN(_SUCCESS)
   end function get_allocation_status

   subroutine print_spec(this, file, line, rc)
      class(StateItemSpec), target, intent(in) :: this
      character(*), intent(in) :: file
      integer, intent(in) :: line
      integer, optional, intent(out) :: rc

      integer :: status
      class(ClassAspect), pointer :: class_aspect
      type(esmf_field), allocatable :: field
      type(esmf_fieldbundle), allocatable :: bundle
      type(esmf_info) :: info
      
      class_aspect => to_ClassAspect(this%aspects, _RC)
      call class_aspect%get_payload(field=field, bundle=bundle, _RC)
      if (allocated(field)) then
         call esmf_infogetfromhost(field, info, _RC)
         print*, __FILE__, __LINE__, file, line, 'field: '
         call esmf_infoprint(info, _RC)
      end if
      if (allocated(bundle)) then
         call esmf_infogetfromhost(bundle, info, _RC)
         print*, __FILE__,__LINE__, file, line, 'bundle: '
         call esmf_infoprint(info, _RC)
      end if
      _RETURN(_SUCCESS)
   end subroutine print_spec

   subroutine update_from_payload(this, rc)
      class(StateItemSpec), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      class(ClassAspect), pointer :: class_aspect
      type(AspectMapIterator) :: iter
      class(StateItemAspect), pointer :: aspect
      type(esmf_Field), allocatable :: field
      type(esmf_FieldBundle), allocatable :: bundle
      type(esmf_State), allocatable :: state

      class_aspect => to_ClassAspect(this%aspects, _RC)
      call class_aspect%get_payload(field=field, bundle=bundle, state=state, _RC)

      associate(e => this%aspects%ftn_end())
        iter = this%aspects%ftn_begin()
        do while (iter /= e)
           call iter%next()
           ! Must skip "class" or it will overwrite aspects in info ...
           if (iter%first() == CLASS_ASPECT_ID) cycle
           aspect => iter%second()
           call aspect%update_from_payload(field=field, bundle=bundle, state=state, _RC)
        end do
      end associate

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
      
   end subroutine update_from_payload

   ! ========================================================================
   ! Producer/consumer methods (merged from StateItemExtension)
   ! ========================================================================

   logical function has_producer(this)
      class(StateItemSpec), target, intent(in) :: this
      has_producer = associated(this%producer)
   end function has_producer

   function get_producer(this) result(producer)
      class(StateItemSpec), target, intent(in) :: this
      class(ComponentDriver), pointer :: producer
      producer => this%producer
   end function get_producer

   subroutine set_producer(this, producer, rc)
      class(StateItemSpec), intent(inout) :: this
      class(ComponentDriver), pointer, intent(in) :: producer
      integer, optional, intent(out) :: rc

      _ASSERT(.not. this%has_producer(), 'cannot set producer for spec that already has one')
      this%producer => producer

      _RETURN(_SUCCESS)
   end subroutine set_producer

   logical function has_consumers(this)
      class(StateItemSpec), target, intent(in) :: this
      has_consumers = this%consumers%size() > 0
   end function has_consumers

   function get_consumers(this) result(consumers)
      class(StateItemSpec), target, intent(in) :: this
      type(ComponentDriverVector), pointer :: consumers
      consumers => this%consumers
   end function get_consumers

   function add_consumer(this, consumer, rc) result(reference)
      use mapl3g_GenericCoupler
      class(ComponentDriver), pointer :: reference
      class(StateItemSpec), target, intent(inout) :: this
      type(GriddedComponentDriver), intent(in) :: consumer
      integer, optional, intent(out) :: rc

      integer :: status

      call this%consumers%push_back(consumer)
      reference => this%consumers%back()
      _RETURN_UNLESS(associated(this%producer))
      
      call mapl_CouplerAddConsumer(this%producer, reference, _RC)

      _RETURN(_SUCCESS)
   end function add_consumer

end module mapl3g_StateItemSpec
