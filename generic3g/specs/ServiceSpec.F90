#include "MAPL_Generic.h"

module mapl3g_ServiceSpec
   use mapl3g_AbstractStateItemSpec
   use mapl3g_StateItemVector
   use mapl3g_MultiState
   use mapl3g_ActualConnectionPt
   use mapl3g_ExtensionAction
   use mapl3g_NullAction
   use mapl3g_AbstractActionSpec
   use mapl3g_ESMF_Utilities, only: get_substate
   use esmf
   use mapl_ErrorHandling
   implicit none
   private

   public :: ServiceSpec

   type, extends(AbstractStateItemSpec) :: ServiceSpec
      private
      type(ESMF_Typekind_Flag), allocatable :: typekind
      type(ESMF_FieldBundle) :: payload
      type(StateItemVector) :: items
   contains
      procedure :: create
      procedure :: destroy
      procedure :: allocate

      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: requires_extension
      procedure :: make_extension
      procedure :: make_action
      procedure :: add_to_state
!!$      procedure :: check_complete
   end type ServiceSpec

   interface ServiceSpec
      module procedure new_ServiceSpec
   end interface ServiceSpec

contains

   function new_ServiceSpec() result(spec)
      type(ServiceSpec) :: spec
   end function new_ServiceSpec

   subroutine create(this, rc)
      class(ServiceSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      this%payload = ESMF_FieldBundleCreate(_RC)

      _RETURN(_SUCCESS)
   end subroutine create

   subroutine allocate(this, rc)
      class(ServiceSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      ! TBD
      ! Add fields that have been put into the service.
      
      _RETURN(_SUCCESS)
   end subroutine allocate

   subroutine add_to_state(this, multi_state, actual_pt, rc)
      class(ServiceSpec), intent(in) :: this
      type(MultiState), intent(inout) :: multi_state
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(ESMF_FieldBundle) :: alias
      character(:), allocatable :: short_name
      type(ESMF_State) :: substate
      integer :: status

      short_name = actual_pt%get_esmf_name()
      alias = ESMF_NamedAlias(this%payload, name=short_name, _RC)

      ! Add bundle to both import and export specs.
      call get_substate(multi_state%importstate, actual_pt%get_comp_name(), substate=substate, _RC)
      call ESMF_StateAdd(substate, [alias], _RC)
      call get_substate(multi_state%exportstate, actual_pt%get_comp_name(), substate=substate, _RC)
      call ESMF_StateAdd(substate, [alias], _RC)

      _RETURN(_SUCCESS)
   end subroutine add_to_state

   
   subroutine connect_to(this, src_spec, rc)
      class(ServiceSpec), intent(inout) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc

      integer :: i
      integer :: status

      _ASSERT(this%can_connect_to(src_spec), 'illegal connection')

      select type (src_spec)
      class is (ServiceSpec)
         ! ok
         do i = 1, this%items%size()
            call src_spec%items%push_back(this%items%of(i))
         end do
      class default
         _FAIL('Cannot connect field spec to non field spec.')
      end select

      _RETURN(ESMF_SUCCESS)
   end subroutine connect_to

    logical function can_connect_to(this, src_spec)
      class(ServiceSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec

      select type(src_spec)
      class is (ServiceSpec)
         can_connect_to = .true.
      class default
         can_connect_to = .false.
      end select

   end function can_connect_to


  subroutine destroy(this, rc)
      class(ServiceSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_FieldBundleDestroy(this%payload, noGarbage=.true., _RC)
      call this%set_created(.false.)

      _RETURN(ESMF_SUCCESS)
   end subroutine destroy


   logical function requires_extension(this, src_spec)
      class(ServiceSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec

      type(ESMF_GeomType_Flag) :: geom_type
      integer :: status
      
      requires_extension = .false.

   end function requires_extension

   function make_action(this, dst_spec, rc) result(action)
      class(ExtensionAction), allocatable :: action
      class(ServiceSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: dst_spec
      integer, optional, intent(out) :: rc

      integer :: status

      action = NullAction()

      _RETURN(_SUCCESS)
   end function make_action

   function make_extension(this, src_spec, rc) result(action_spec)
      class(AbstractActionSpec), allocatable :: action_spec
      class(ServiceSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc 
   end function make_extension
   


end module mapl3g_ServiceSpec
