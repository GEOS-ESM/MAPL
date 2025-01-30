#include "MAPL_Generic.h"

module mapl3g_WildcardSpec

   use mapl3g_StateItemSpec
   use mapl3g_StateItemAspect
   use mapl3g_AspectCollection
   use mapl3g_ActualPtStateItemSpecMap
   use mapl3g_ActualConnectionPt
   use mapl3g_MultiState
   use mapl3g_ActualPtVector
   use mapl3g_ActualConnectionPt
   use mapl3g_ExtensionAction
   use mapl3g_NullAction
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use mapl3g_VerticalGrid
   use esmf
   use pFlogger

   implicit none
   private

   public :: WildcardSpec

   type, extends(StateItemSpec) :: WildcardSpec
      private
      class(StateItemSpec), allocatable :: reference_spec
      type(ActualPtStateItemSpecMap), pointer :: matched_items
   contains
      procedure :: create
      procedure :: destroy
      procedure :: allocate

      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: add_to_state
      procedure :: add_to_bundle
      procedure :: set_geometry

      procedure :: write_formatted

      procedure :: get_reference_spec
      ! These might be unnecessary once aspects are fully integrated
      procedure :: get_aspect_by_name
      procedure :: get_aspects
      procedure :: set_aspect
   end type WildcardSpec

   interface WildcardSpec
      module procedure new_WildcardSpec
   end interface WildcardSpec

contains

   function new_WildcardSpec(reference_spec) result(wildcard_spec)
      type(WildcardSpec) :: wildcard_spec
      class(StateItemSpec), intent(in) :: reference_spec

      wildcard_spec%reference_spec = reference_spec
      allocate(wildcard_spec%matched_items)
   end function new_WildcardSpec

   ! No-op
   subroutine create(this, rc)
      class(WildcardSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc


      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(this)
   end subroutine create

   ! No-op
   subroutine destroy(this, rc)
      class(WildcardSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(this)
   end subroutine destroy

   ! No-op
   ! The contained fields are separately allocated on the export side.
   ! Wildcard is always an import.
   subroutine allocate(this, rc)
      class(WildcardSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(this)
   end subroutine allocate

   subroutine connect_to(this, src_spec, actual_pt, rc)
      class(WildcardSpec), intent(inout) :: this
      class(StateItemSpec), intent(inout) :: src_spec
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      integer :: status 
      call with_target_attribute(this, src_spec, actual_pt, _RC)

      _RETURN(_SUCCESS)
   contains
      subroutine with_target_attribute(this, src_spec, actual_pt, rc)
         class(WildcardSpec), target, intent(inout) :: this
         class(StateItemSpec), intent(inout) :: src_spec
         type(ActualConnectionPt), intent(in) :: actual_pt
         integer, optional, intent(out) :: rc

         integer :: status
         class(StateItemSpec), pointer :: spec
         logical :: can_connect

         can_connect = this%can_connect_to(src_spec, _RC)
         _ASSERT(can_connect, 'illegal connection')
         _ASSERT(this%matched_items%count(actual_pt) == 0, 'duplicate connection pt')
         
         call this%matched_items%insert(actual_pt, this%reference_spec)
         spec => this%matched_items%of(actual_pt)
         call spec%create(_RC)
         call spec%connect_to(src_spec, actual_pt, _RC)

         _RETURN(ESMF_SUCCESS)
      end subroutine with_target_attribute
   end subroutine connect_to

   logical function can_connect_to(this, src_spec, rc)
      class(WildcardSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc

      integer :: status
      can_connect_to = this%reference_spec%can_connect_to(src_spec, _RC)

      _RETURN(_SUCCESS)
   end function can_connect_to

   subroutine add_to_state(this, multi_state, actual_pt, rc)

      class(WildcardSpec), intent(in) :: this
      type(MultiState), intent(inout) :: multi_state
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      integer :: status

      call with_target_attribute(this, multi_state, actual_pt, _RC)

      _RETURN(_SUCCESS)

   contains
      
      subroutine with_target_attribute(this, multi_state, actual_pt, rc)
         class(WildcardSpec), target, intent(in) :: this
         type(MultiState), intent(inout) :: multi_state
         type(ActualConnectionPt), intent(in) :: actual_pt
         integer, optional, intent(out) :: rc
         
         integer :: status
         type(ActualPtStateItemSpecMapIterator) :: iter
         class(StateItemSpec), pointer :: spec_ptr
         type(ActualConnectionPt), pointer :: effective_pt
         type(ActualConnectionPt) :: use_pt
         character(:), allocatable :: comp_name
         integer :: label

         associate (e => this%matched_items%ftn_end())
           iter = this%matched_items%ftn_begin()
           do while (iter /= e)
              iter = next(iter)
              ! Ignore actual_pt argument and use internally recorded name
              effective_pt => iter%first()
              comp_name = actual_pt%get_comp_name()
              label = actual_pt%get_label()
              use_pt = effective_pt

              if (label /= -1) then ! not primary
                 use_pt = use_pt%extend()
              end if

              if (comp_name /= '') then
                 use_pt = use_pt%add_comp_name(comp_name)
              end if
              spec_ptr => iter%second()
              call spec_ptr%add_to_state(multi_state, use_pt, _RC)
           end do
         end associate
         
         _RETURN(_SUCCESS)
      end subroutine with_target_attribute

   end subroutine add_to_state

   subroutine add_to_bundle(this, bundle, rc)
      class(WildcardSpec), intent(in) :: this
      type(ESMF_FieldBundle), intent(inout) :: bundle
      integer, optional, intent(out) :: rc

      integer :: status

      _FAIL('not implemented')

      _RETURN(_SUCCESS)
   end subroutine add_to_bundle

   subroutine set_geometry(this, geom, vertical_grid, rc)
      class(WildcardSpec), intent(inout) :: this
      type(ESMF_Geom), optional, intent(in) :: geom
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      integer, optional, intent(out) :: rc

      integer :: status

      call this%reference_spec%set_geometry(geom, vertical_grid, _RC)

      _RETURN(_SUCCESS)
   end subroutine set_geometry

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(WildcardSpec), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      write(unit, "(a)", iostat=iostat, iomsg=iomsg) "WildcardSpec(write not implemented yet)"
   end subroutine write_formatted

   function get_reference_spec(this) result(reference_spec)
      class(WildcardSpec), target, intent(in) :: this
      class(StateItemSpec), pointer :: reference_spec
      reference_spec => this%reference_spec
   end function get_reference_spec

   function get_aspect_by_name(this, name, rc) result(aspect)
      class(StateItemAspect), pointer :: aspect
      character(*), intent(in) :: name
      class(WildcardSpec), target, intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      aspect => this%reference_spec%get_aspect(name, _RC)

      _RETURN(_SUCCESS)
   end function get_aspect_by_name

   function get_aspects(this) result(aspects)
      type(AspectCollection), pointer :: aspects
      class(WildcardSpec), target, intent(in) :: this
      aspects => this%reference_spec%get_aspects()
   end function get_aspects

   subroutine set_aspect(this, aspect, rc)
      class(WildcardSpec), target, intent(inout) :: this
      class(StateItemAspect), intent(in) :: aspect
      integer, optional, intent(out) :: rc

      integer :: status

      call this%reference_spec%set_aspect(aspect, _RC)

      _RETURN(_SUCCESS)
   end subroutine set_aspect

end module mapl3g_WildcardSpec
