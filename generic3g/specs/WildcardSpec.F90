#include "MAPL_Generic.h"

module mapl3g_WildcardSpec
   use mapl3g_AbstractStateItemSpec
   use mapl3g_ActualConnectionPt
   use mapl3g_ActualPtVector
   use mapl3g_ActualPtSpecPtrMap
   use mapl3g_MultiState
   use mapl3g_ActualConnectionPt
   use mapl3g_ExtensionAction
   use mapl3g_NullAction
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use esmf
   use nuopc

   implicit none
   private

   public :: WildcardSpec

   type, extends(AbstractStateItemSpec) :: WildcardSpec
      private
      class(AbstractStateItemSpec), allocatable :: reference_spec
      type(ActualPtSpecPtrMap), pointer :: matched_specs
   contains
      procedure :: create
      procedure :: destroy
      procedure :: allocate
      procedure :: get_dependencies

      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: make_extension
      procedure :: make_action
      procedure :: add_to_state
      procedure :: add_to_bundle

      procedure :: extension_cost

   end type WildcardSpec

   interface WildcardSpec
      module procedure new_WildcardSpec
   end interface WildcardSpec

contains


   function new_WildcardSpec(reference_spec) result(wildcard_spec)
      type(WildcardSpec) :: wildcard_spec
      class(AbstractStateItemSpec), intent(in) :: reference_spec

      _HERE
      wildcard_spec%reference_spec = reference_spec
      allocate(wildcard_spec%matched_specs)

   end function new_WildcardSpec

   ! No-op
   subroutine create(this, dependency_specs, rc)
      class(WildcardSpec), intent(inout) :: this
      type(StateItemSpecPtr), intent(in) :: dependency_specs(:)
      integer, optional, intent(out) :: rc

      integer :: status

      call this%set_created()

      _RETURN(ESMF_SUCCESS)
   end subroutine create

   ! No-op
   subroutine destroy(this, rc)
      class(WildcardSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call this%set_created(.false.)

      _RETURN(ESMF_SUCCESS)
   end subroutine destroy


   subroutine allocate(this, rc)
      class(WildcardSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ActualPtSpecPtrMapIterator) :: iter
      class(StateItemSpecPtr), pointer :: spec_ptr

!!$      _FAIL('should not do anything?')
!!$      associate (e => this%matched_specs%end())
!!$        iter = this%matched_specs%begin()
!!$        do while (iter /= e)
!!$           spec_ptr => iter%second()
!!$           call spec_ptr%ptr%allocate(_RC)
!!$           iter = next(iter)
!!$        end do
!!$      end associate
   
      _RETURN(ESMF_SUCCESS)
   end subroutine allocate

   function get_dependencies(this, rc) result(dependencies)
      type(ActualPtVector) :: dependencies
      class(WildcardSpec), intent(in) :: this
      integer, optional, intent(out) :: rc

      dependencies = ActualPtVector()

      _RETURN(_SUCCESS)
   end function get_dependencies

   subroutine connect_to(this, src_spec, actual_pt, rc)
      class(WildcardSpec), intent(inout) :: this
      class(AbstractStateItemSpec), intent(inout) :: src_spec
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemSpecPtr), pointer :: spec_ptr

      _HERE
      _ASSERT(this%can_connect_to(src_spec), 'illegal connection')
      _ASSERT(this%matched_specs%count(actual_pt) == 0, 'duplicate connection pt')

      _HERE,'Warning - this is a memory leak.'
      allocate(spec_ptr)
      allocate(spec_ptr%ptr, source=this%reference_spec)

      call this%matched_specs%insert(actual_pt, spec_ptr)
      spec_ptr => this%matched_specs%of(actual_pt)
      call spec_ptr%ptr%create([StateItemSpecPtr::], _RC)
      call spec_ptr%ptr%connect_to(src_spec, actual_pt, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine connect_to


   logical function can_connect_to(this, src_spec)
      class(WildcardSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec

      can_connect_to = this%reference_spec%can_connect_to(src_spec)

   end function can_connect_to

   subroutine add_to_state(this, multi_state, actual_pt, rc)
      class(WildcardSpec), intent(in) :: this
      type(MultiState), intent(inout) :: multi_state
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(ActualPtSpecPtrMapIterator) :: iter
      integer :: status
      class(StateItemSpecPtr), pointer :: spec_ptr
      type(ActualConnectionPt), pointer :: effective_pt

      _HERE
      _HERE, this%matched_specs%size()
      associate (e => this%matched_specs%end())
        _HERE
        iter = this%matched_specs%begin()
        _HERE
        do while (iter /= e)
           _HERE
           ! Ignore actual_pt argument and use internally recorded name
           effective_pt => iter%first()
           _HERE, 'adding to state: ', effective_pt
           spec_ptr => iter%second()
           call spec_ptr%ptr%add_to_state(multi_state, effective_pt, _RC)
           iter = next(iter)
        end do
      end associate
      _HERE

      _RETURN(_SUCCESS)
   end subroutine add_to_state

   subroutine add_to_bundle(this, bundle, rc)
      class(WildcardSpec), intent(in) :: this
      type(ESMF_FieldBundle), intent(inout) :: bundle
      integer, optional, intent(out) :: rc

      integer :: status

      _FAIL('not implemented')

      _RETURN(_SUCCESS)
   end subroutine add_to_bundle

   function make_extension(this, src_spec, rc) result(extension)
      class(AbstractStateItemSpec), allocatable :: extension
      class(WildcardSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc

      _FAIL('wildcard cannot be extended - only used for imports')
   end function make_extension

   function make_action(this, dst_spec, rc) result(action)
      class(ExtensionAction), allocatable :: action
      class(WildcardSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: dst_spec
      integer, optional, intent(out) :: rc

      integer :: status

      action = NullAction()
      _FAIL('wildcard cannot be extended - only used for imports')
   end function make_action

   integer function extension_cost(this, src_spec, rc) result(cost)
      class(WildcardSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc

      integer :: status

      cost = this%reference_spec%extension_cost(src_spec, _RC)

      _RETURN(_SUCCESS)
   end function extension_cost

end module mapl3g_WildcardSpec
