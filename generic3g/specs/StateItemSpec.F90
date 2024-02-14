#include "MAPL_Generic.h"

module mapl3g_StateItemSpec
   use mapl_ErrorHandling
   implicit none
   private

   public :: StateItemSpec
   public :: StateItemSpecPtr
  
   type, abstract :: StateItemSpec
      private

      logical :: active = .false.
      logical :: created = .false.
      logical :: allocated = .false.

   contains

      procedure(I_create), deferred :: create
      procedure(I_destroy), deferred :: destroy
      procedure(I_allocate), deferred :: allocate
      procedure(I_get_dependencies), deferred :: get_dependencies

      procedure(I_connect), deferred :: connect_to
      procedure(I_can_connect), deferred :: can_connect_to
      procedure(I_make_extension), deferred :: make_extension
      procedure(I_extension_cost), deferred :: extension_cost

      procedure(I_add_to_state), deferred :: add_to_state
      procedure(I_add_to_bundle), deferred :: add_to_bundle

      procedure, non_overridable :: set_created
      procedure, non_overridable :: is_created
      procedure, non_overridable :: set_allocated
      procedure, non_overridable :: is_allocated
      procedure, non_overridable :: is_active
      procedure, non_overridable :: set_active

      procedure :: make_action
   end type StateItemSpec

   type :: StateItemSpecPtr
      class(StateItemSpec), pointer :: ptr
   end type StateItemSpecPtr


   abstract interface

      subroutine I_connect(this, src_spec, actual_pt, rc)
         use mapl3g_ActualConnectionPt
         import StateItemSpec
         class(StateItemSpec), intent(inout) :: this
         class(StateItemSpec), intent(inout) :: src_spec
         type(ActualConnectionPt), intent(in) :: actual_pt
         integer, optional, intent(out) :: rc
      end subroutine I_connect

      logical function I_can_connect(this, src_spec, rc)
         import StateItemSpec
         class(StateItemSpec), intent(in) :: this
         class(StateItemSpec), intent(in) :: src_spec
         integer, optional, intent(out) :: rc
      end function I_can_connect

      ! Will use ESMF so cannot be PURE
      subroutine I_create(this, rc)
         import StateItemSpec
         class(StateItemSpec), intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine I_create

      subroutine I_destroy(this, rc)
         import StateItemSpec
         class(StateItemSpec), intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine I_destroy

      ! Will use ESMF so cannot be PURE
      subroutine I_allocate(this, rc)
         import StateItemSpec
         class(StateItemSpec), intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine I_allocate

      function I_get_dependencies(this, rc) result(dependencies)
         use mapl3g_ActualPtVector
         import StateItemSpec
         type(ActualPtVector) :: dependencies
         class(StateItemSpec), intent(in) :: this
         integer, optional, intent(out) :: rc
      end function I_get_dependencies

      function I_make_extension(this, dst_spec, rc) result(extension)
         import StateItemSpec
         class(StateItemSpec), allocatable :: extension
         class(StateItemSpec), intent(in) :: this
         class(StateItemSpec), intent(in) :: dst_spec
         integer, optional, intent(out) :: rc
      end function I_make_extension
         
      integer function I_extension_cost(this, src_spec, rc) result(cost)
         import StateItemSpec
         class(StateItemSpec), intent(in) :: this
         class(StateItemSpec), intent(in) :: src_spec
         integer, optional, intent(out) :: rc
       end function I_extension_cost

      subroutine I_add_to_state(this, multi_state, actual_pt, rc)
         use mapl3g_MultiState
         use mapl3g_ActualConnectionPt
         import StateItemSpec
         class(StateItemSpec), intent(in) :: this
         type(MultiState), intent(inout) :: multi_state
         type(ActualConnectionPt), intent(in) :: actual_pt
         integer, optional, intent(out) :: rc
      end subroutine I_add_to_state

      subroutine I_add_to_bundle(this, bundle, rc)
         use esmf, only: ESMF_FieldBundle
         use mapl3g_ActualConnectionPt
         import StateItemSpec
         class(StateItemSpec), intent(in) :: this
         type(ESMF_FieldBundle), intent(inout) :: bundle
         integer, optional, intent(out) :: rc
      end subroutine I_add_to_bundle

   end interface

contains
   
   function new_StateItemSpecPtr(state_item) result(wrap)
      type(StateItemSpecPtr) :: wrap
      class(StateItemSpec), target :: state_item

      wrap%ptr => state_item
   end function new_StateItemSpecPtr
  

   pure subroutine set_allocated(this, allocated)
      class(StateItemSpec), intent(inout) :: this
      logical, optional, intent(in) :: allocated

      if (present(allocated)) then
         this%allocated = allocated
      else
         this%allocated =  .true.
      end if

   end subroutine set_allocated

   pure logical function is_allocated(this)
      class(StateItemSpec), intent(in) :: this
      is_allocated = this%allocated
   end function is_allocated

   pure subroutine set_created(this, created)
      class(StateItemSpec), intent(inout) :: this
      logical, optional, intent(in) :: created

      if (present(created)) then
         this%created = created
      else
         this%created =  .true.
      end if

   end subroutine set_created

   pure logical function is_created(this)
      class(StateItemSpec), intent(in) :: this
      is_created = this%created
   end function is_created

   pure subroutine set_active(this, active)
      class(StateItemSpec), intent(inout) :: this
      logical, optional, intent(in) :: active

      if (present(active)) then
         this%active = active
      else
         this%active =  .true.
      end if

   end subroutine set_active

   pure logical function is_active(this)
      class(StateItemSpec), intent(in) :: this
      is_active = this%active
   end function is_active


   function make_action(this, dst_spec, rc) result(action)
      use mapl3g_ExtensionAction
      use mapl3g_NullAction
      class(ExtensionAction), allocatable :: action
      class(StateItemSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: dst_spec
      integer, optional, intent(out) :: rc

      action = NullAction()
      _FAIL('Subclass has not implemented make_action')
   end function make_action

end module mapl3g_StateItemSpec
