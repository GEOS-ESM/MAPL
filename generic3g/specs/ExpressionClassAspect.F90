#include "MAPL_Generic.h"

module mapl3g_ExpressionClassAspect
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl3g_ClassAspect
   use mapl3g_FieldClassAspect
   use mapl3g_GeomAspect
   use mapl3g_HorizontalDimsSpec
   use mapl3g_VerticalGridAspect
   use mapl3g_UnitsAspect
   use mapl3g_TypekindAspect
   use mapl3g_UngriddedDimsAspect

   use mapl3g_StateRegistry
!#   use mapl3g_VerticalGrid
!#   use mapl3g_VerticalStaggerLoc
!#   use mapl3g_VerticalStaggerLoc
!#   use mapl3g_UngriddedDims

   use mapl3g_EvalTransform
   use mapl3g_NullTransform
   use mapl3g_ComponentDriver
   use mapl3g_ComponentDriver
   use mapl3g_ComponentDriverVector
   use mapl3g_ExtensionTransform
   use mapl3g_MultiState
   use mapl3g_ESMF_Utilities, only: get_substate

   use mapl3g_VirtualConnectionPt
   use mapl3g_VirtualConnectionPtVector
   use mapl3g_ActualConnectionPt
   use mapl3g_StateItemSpec
   use mapl3g_StateItemExtension

   use mapl3g_Field_API
   use mapl3g_FieldInfo
   use mapl_FieldUtilities


   use mapl_ErrorHandling
   use esmf
   implicit none(type,external)
   private

   public :: ExpressionClassAspect
   public :: to_ExpressionClassAspect

   interface to_ExpressionClassAspect
      procedure :: to_expressionclassaspect_from_poly
      procedure :: to_expressionclassaspect_from_map
   end interface to_ExpressionClassAspect

   ! No payload - just a placehold for expression
   type, extends(ClassAspect) :: ExpressionClassAspect
      private
      character(:), allocatable :: expression
      type(StateRegistry), pointer :: registry => null()
   contains
      procedure :: get_aspect_order
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: make_transform
      procedure :: matches
      procedure :: connect_to_import
      procedure :: connect_to_export

      procedure :: create
      procedure :: activate
      procedure :: allocate
      procedure :: destroy
      procedure :: add_to_state
      procedure :: add_to_bundle

      procedure, nopass :: get_aspect_id
   end type ExpressionClassAspect

   interface ExpressionClassAspect
      procedure :: new_ExpressionClassAspect
   end interface ExpressionClassAspect


contains

   function new_ExpressionClassAspect(expression, registry) result(aspect)
      type(ExpressionClassAspect) :: aspect
      character(*), intent(in) :: expression
      type(StateRegistry), target, intent(in) :: registry

      aspect%expression = expression
      aspect%registry => registry

   end function new_ExpressionClassAspect

   function get_aspect_order(this, goal_aspects, rc) result(aspect_ids)
      type(AspectId), allocatable :: aspect_ids(:)
      class(ExpressionClassAspect), intent(in) :: this
      type(AspectMap), intent(in) :: goal_aspects
      integer, optional, intent(out) :: rc

      aspect_ids = [ &
           GEOM_ASPECT_ID, &
           VERTICAL_GRID_ASPECT_ID, &
           TYPEKIND_ASPECT_ID, &
           CLASS_ASPECT_ID &
           ]

      _RETURN(_SUCCESS)

      _UNUSED_DUMMY(goal_aspects)
   end function get_aspect_order


   ! No op
   subroutine create(this, rc)
      class(ExpressionClassAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN(ESMF_SUCCESS)
   end subroutine create

   subroutine activate(this, rc)
      class(ExpressionClassAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemExtension), pointer :: extension
      type(StateItemSpec), pointer :: spec

      extension => this%registry%get_primary_extension(VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, 'A'), _RC)
      spec => extension%get_spec()
      call spec%activate()

      extension => this%registry%get_primary_extension(VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, 'B'), _RC)
      spec => extension%get_spec()
      call spec%activate()

      _RETURN(ESMF_SUCCESS)
   end subroutine activate

   ! noop
   subroutine allocate(this, other_aspects, rc)
      class(ExpressionClassAspect), intent(inout) :: this
      type(AspectMap), intent(in) :: other_aspects
      integer, optional, intent(out) :: rc

      integer :: status
 
      _RETURN(ESMF_SUCCESS)
   end subroutine allocate


   ! no op
   subroutine destroy(this, rc)
      class(ExpressionClassAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc

      _RETURN(ESMF_SUCCESS)
   end subroutine destroy

   ! no op
   subroutine connect_to_import(this, import, rc)
      class(ExpressionClassAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: import
      integer, optional, intent(out) :: rc

      _RETURN(_SUCCESS)
   end subroutine connect_to_import

   ! no op
   subroutine connect_to_export(this, export, actual_pt, rc)
      class(ExpressionClassAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(actual_pt)
   end subroutine connect_to_export

   function to_expressionclassaspect_from_poly(aspect, rc) result(expression_aspect)
      type(ExpressionClassAspect) :: expression_aspect
      class(StateItemAspect), intent(in) :: aspect
      integer, optional, intent(out) :: rc

      select type(aspect)
      class is (ExpressionClassAspect)
         expression_aspect = aspect
      class default
         _FAIL('aspect is not ExpressionClassAspect')
      end select

      _RETURN(_SUCCESS)
   end function to_expressionclassaspect_from_poly

   function to_expressionclassaspect_from_map(map, rc) result(expression_aspect)
      type(ExpressionClassAspect) :: expression_aspect
      type(AspectMap), target, intent(in) :: map
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateItemAspect), pointer :: poly

      poly => map%at(CLASS_ASPECT_ID, _RC)
      expression_aspect = to_ExpressionClassAspect(poly, _RC)

      _RETURN(_SUCCESS)
   end function to_expressionclassaspect_from_map

   function make_transform(src, dst, other_aspects, rc) result(transform)
      class(ExtensionTransform), allocatable :: transform
      class(ExpressionClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      type(AspectMap), target, intent(in) :: other_aspects
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      type(ComponentDriverVector), target :: input_couplers
      class(ComponentDriver), pointer :: coupler
      type(VirtualConnectionPtVector), target :: inputs

      type(MultiState) :: multi_state
      type(VirtualConnectionPt), pointer :: v_pt
      type(ActualConnectionPt) :: a_pt
      type(StateItemExtension), pointer :: new_extension
      type(StateItemSpec), pointer :: new_spec
      type(StateItemSpec), target :: goal_spec
      type(AspectMap), pointer :: aspects
      class(StateItemAspect), pointer :: class_aspect
      type(ESMF_Field) :: field

      multi_state = MultiState()

      select type (dst)
      type is (FieldClassAspect)
         ! Hardwire for now
         call inputs%push_back(VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, 'A'))
         call inputs%push_back(VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, 'B'))

!#         goal_spec = ...

         do i = 1, inputs%size()
            v_pt => inputs%of(i)
!#            new_extension = src%registry%extend(v_pt, goal_spec, _RC)
            new_extension => src%registry%get_primary_extension(v_pt, _RC)
            coupler => new_extension%get_producer()
            if (associated(coupler)) then
               call input_couplers%push_back(coupler)
            end if
            new_spec => new_extension%get_spec()

            class_aspect => new_spec%get_aspect(CLASS_ASPECT_ID, _RC)
            select type(class_aspect)
            type is (FieldClassAspect)
               field = class_aspect%get_payload()
               a_pt = ActualConnectionPt(v_pt)
               call class_aspect%add_to_state(multi_state, a_pt, _RC)
            class default
               _FAIL("unsupported aspect type; must be FieldClassAspect")
            end select
         end do

         allocate(transform, source=EvalTransform(src%expression, multi_state%exportState, input_couplers))
      class default
         transform = NullTransform()
         _FAIL('expression connected to non-field')
      end select

      _RETURN(_SUCCESS)
   end function make_transform

   logical function supports_conversion_general(src)
      class(ExpressionClassAspect), intent(in) :: src
      supports_conversion_general = .true.
   end function supports_conversion_general

   ! Expressions can only evaluate to fields
   logical function supports_conversion_specific(src, dst)
      class(ExpressionClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      supports_conversion_specific = .false.
      select type (dst)
      type is (FieldClassAspect)
         supports_conversion_specific = .true.
      end select

      _UNUSED_DUMMY(dst)
   end function supports_conversion_specific

   ! No op
   subroutine add_to_state(this, multi_state, actual_pt, rc)
      class(ExpressionClassAspect), intent(in) :: this
      type(MultiState), intent(inout) :: multi_state
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      _RETURN(_SUCCESS)
   end subroutine add_to_state

   ! noop
   subroutine add_to_bundle(this, field_bundle, rc)
      class(ExpressionClassAspect), intent(in) :: this
      type(ESMF_FieldBundle), intent(inout) :: field_bundle
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN(_SUCCESS)
   end subroutine add_to_bundle

   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = CLASS_ASPECT_ID
   end function get_aspect_id

  function matches(src, dst)
     logical :: matches
      class(ExpressionClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      matches = .false.
!#      select type(dst)
!#      class is (FieldClassAspect)
!#         _HERE
!#         matches = .true.
!#      end select
   end function matches

   
end module mapl3g_ExpressionClassAspect
