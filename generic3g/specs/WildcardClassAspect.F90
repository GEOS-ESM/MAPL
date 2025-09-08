#include "MAPL.h"

module mapl3g_WildcardClassAspect
   use mapl3g_ActualPtFieldAspectMap
   use mapl3g_ActualConnectionPt
   use mapl3g_StateItemSpec
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl3g_ClassAspect
   use mapl3g_FieldClassAspect
   use mapl3g_ExtensionTransform
   use mapl3g_NullTransform
   use mapl3g_MultiState
   use mapl_ErrorHandling
   use esmf
   implicit none(type,external)
   private

   public :: WildcardClassAspect

   type, extends(ClassAspect) :: WildcardClassAspect
     private
     type(ActualPtFieldAspectMap) :: matched_items
   contains

      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: matches
      procedure :: make_transform
      procedure :: connect_to_export

      procedure :: get_aspect_order
      procedure :: create
      procedure :: activate
      procedure :: allocate
      procedure :: destroy
      procedure :: add_to_state
 
   end type WildcardClassAspect

   interface WildcardClassAspect
      procedure :: new_WildcardClassAspect
   end interface WildcardClassAspect

contains

   function new_WildcardClassAspect() result(wildcard_aspect)
      type(WildcardClassAspect) :: wildcard_aspect
   end function new_WildcardClassAspect


   ! Wildcard not permitted as an export.
   logical function matches(src, dst)
      class(WildcardClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      matches = .false.

   end function matches

   ! Wildcard not permitted as an export.
   function make_transform(src, dst, other_aspects, rc) result(transform)
      class(ExtensionTransform), allocatable :: transform
      class(WildcardClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      type(AspectMap), target, intent(in) :: other_aspects
      integer, optional, intent(out) :: rc
      
      transform = NullTransform()

      _RETURN(_SUCCESS)
   end function make_transform


   subroutine connect_to_export(this, export, actual_pt, rc)
      class(WildcardClassAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(FieldClassAspect) :: export_
      integer :: status

     ! Export must be a field - all other cases should fail
      export_ = to_FieldClassAspect(export, _RC)
      call typesafe_connect_to_export(this, export_, actual_pt, _RC)

      _RETURN(_SUCCESS)
   end subroutine connect_to_export

   subroutine typesafe_connect_to_export(this, export, actual_pt, rc)
      class(WildcardClassAspect), target, intent(inout) :: this
      type(FieldClassAspect), intent(in) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      class(StateItemSpec), pointer :: spec
      class(StateItemAspect), pointer :: import_class_aspect
      integer :: status

      ! Do not record duplicates (arises in multiple passes of
      ! advertise_modify()
      _RETURN_IF(this%matched_items%count(actual_pt) > 0)

      call this%matched_items%insert(actual_pt, export)
      
      _RETURN(_SUCCESS)
   end subroutine typesafe_connect_to_export
   
   ! No-op
   subroutine create(this, other_aspects, handle, rc)
      class(WildcardClassAspect), intent(inout) :: this
      type(AspectMap), intent(in) :: other_aspects
      integer, optional, intent(in) :: handle(:)
      integer, optional, intent(out) :: rc

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end subroutine create

   ! No-op
   subroutine activate(this, rc)
      class(WildcardClassAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end subroutine activate

   ! No-op
   subroutine destroy(this, rc)
      class(WildcardClassAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end subroutine destroy

   ! No-op
   ! Wildcard is always an import, and allocation is on exports.
   subroutine allocate(this, other_aspects, rc)
      class(WildcardClassAspect), intent(inout) :: this
      type(AspectMap), intent(in) :: other_aspects
      integer, optional, intent(out) :: rc

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end subroutine allocate

   subroutine add_to_state(this, multi_state, actual_pt, rc)
      class(WildcardClassAspect), intent(in) :: this
      type(MultiState), intent(inout) :: multi_state
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      integer :: status

      call with_target_attribute(this, multi_state, actual_pt, _RC)

      _RETURN(_SUCCESS)

   contains
      
      subroutine with_target_attribute(this, multi_state, actual_pt, rc)
         class(WildcardClassAspect), target, intent(in) :: this
         type(MultiState), intent(inout) :: multi_state
         type(ActualConnectionPt), intent(in) :: actual_pt
         integer, optional, intent(out) :: rc
         
         integer :: status
         type(ActualConnectionPt), pointer :: effective_pt
         type(ActualConnectionPt) :: use_pt
         character(:), allocatable :: comp_name
         integer :: label
         type(ActualPtFieldAspectMapIterator) :: iter
         type(FieldClassAspect), pointer :: ptr

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
              ptr => iter%second()
              call ptr%add_to_state(multi_state, use_pt, _RC)
           end do
         end associate
         
         _RETURN(_SUCCESS)
      end subroutine with_target_attribute

   end subroutine add_to_state

   ! Wildcard is never an export
   logical function supports_conversion_general(src)
      class(WildcardClassAspect), intent(in) :: src
      supports_conversion_general = .false.
   end function supports_conversion_general

   logical function supports_conversion_specific(src, dst)
      class(WildcardClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      supports_conversion_specific = .false.

      _UNUSED_DUMMY(dst)
   end function supports_conversion_specific

   ! Cannot be an export - should not call this
   function get_aspect_order(this, goal_aspects, rc) result(aspect_ids)
      type(AspectId), allocatable :: aspect_ids(:)
      class(WildcardClassAspect), intent(in) :: this
      type(AspectMap), intent(in) :: goal_aspects
      integer, optional, intent(out) :: rc

      aspect_ids = [AspectId :: ] ! empty

      _RETURN(_SUCCESS)

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(goal_aspects)
   end function get_aspect_order
 
end module mapl3g_WildcardClassAspect
