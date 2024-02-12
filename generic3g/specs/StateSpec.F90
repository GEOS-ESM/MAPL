#include "MAPL_Generic.h"

module mapl3g_StateSpec
   use mapl3g_StateItemSpec
   use mapl3g_AbstractActionSpec
   use mapl3g_StateItemSpecMap
   use mapl3g_VariableSpec
   use mapl3g_MultiState
   use mapl3g_ActualConnectionPt
   use mapl3g_ActualPtVector
   use mapl_ErrorHandling
   use ESMF
   use mapl_KeywordEnforcer
   implicit none
   private

   public :: StateSpec
   type, extends(StateItemSpec) :: StateSpec
      private
      type(ESMF_State) :: payload
      type(StateItemSpecMap) :: item_specs
   contains
!!$      procedure :: initialize
      procedure :: add_item
      procedure :: get_item

      procedure :: create
      procedure :: destroy
      procedure :: allocate
      procedure :: get_dependencies
      
      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: make_extension
      procedure :: extension_cost
      procedure :: add_to_state
      procedure :: add_to_bundle

   end type StateSpec


contains

!!$   ! Nothing defined at this time.
!!$   subroutine initialize(this, geom, var_spec, unusable, rc)
!!$      class(StateSpec), intent(inout) :: this
!!$      type(ESMF_Geom), intent(in) :: geom
!!$      type(VariableSpec), intent(in) :: var_spec
!!$      class(KeywordEnforcer), optional, intent(in) :: unusable
!!$      integer, optional, intent(out) :: rc
!!$
!!$      character(:), allocatable :: units
!!$      integer :: status
!!$
!!$      _RETURN(_SUCCESS)
!!$      _UNUSED_DUMMY(unusable)
!!$   end subroutine initialize

   subroutine add_item(this, name, item)
      class(StateSpec), target, intent(inout) :: this
      character(len=*), intent(in) :: name
      class(StateItemSpec), intent(in) :: item

      call this%item_specs%insert(name, item)

   end subroutine add_item

   function get_item(this, name) result(item)
      class(StateItemSpec), pointer :: item
      class(StateSpec), target, intent(inout) :: this
      character(len=*), intent(in) :: name

      integer :: status

      item => this%item_specs%at(name, rc=status)

   end function get_item


   subroutine create(this, dependency_specs, rc)
      class(StateSpec), intent(inout) :: this
      type(StateItemSpecPtr), intent(in) :: dependency_specs(:)
      integer, optional, intent(out) :: rc

      integer :: status
      
      this%payload = ESMF_StateCreate(_RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine create

   subroutine destroy(this, rc)
      class(StateSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_StateDestroy(this%payload, _RC)
      call this%set_created(.false.)

      _RETURN(ESMF_SUCCESS)
   end subroutine destroy


   ! NO-OP
   subroutine allocate(this, rc)
      class(StateSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN(ESMF_SUCCESS)
   end subroutine allocate

   function get_dependencies(this, rc) result(dependencies)
      type(ActualPtVector) :: dependencies
      class(StateSpec), intent(in) :: this
      integer, optional, intent(out) :: rc

      dependencies = ActualPtVector()

      _RETURN(_SUCCESS)
   end function get_dependencies
   
   subroutine connect_to(this, src_spec, actual_pt, rc)
      class(StateSpec), intent(inout) :: this
      class(StateItemSpec), intent(inout) :: src_spec
      type(ActualConnectionPt), intent(in) :: actual_pt ! unused
      integer, optional, intent(out) :: rc

      integer :: status

      select type (src_spec)
      class is (StateSpec)
         this%payload = src_spec%payload
      class default
         _FAIL('Cannot connect field spec to non field spec.')
      end select

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(actual_pt)
   end subroutine connect_to


   logical function can_connect_to(this, src_spec, rc)
      class(StateSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc

      can_connect_to = same_type_as(src_spec, this)

      _RETURN(_SUCCESS)

   end function can_connect_to


   subroutine add_to_state(this, multi_state, actual_pt, rc)
      class(StateSpec), intent(in) :: this
      type(MultiState), intent(inout) :: multi_state
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(ESMF_State) :: alias
      integer :: status

      _FAIL('unimplemented')

!!$      alias = ESMF_NamedAlias(this%payload, name=short_name, _RC)
!!$      call ESMF_StateAdd(state, this%payload, short_name, _RC)
!!$

   end subroutine add_to_state


   subroutine add_to_bundle(this, bundle, rc)
      class(StateSpec), intent(in) :: this
      type(ESMF_FieldBundle), intent(inout) :: bundle
      integer, optional, intent(out) :: rc

      _FAIL('Attempt to use item of type InvalidSpec')

      _RETURN(_SUCCESS)
   end subroutine add_to_bundle
   

   function make_extension(this, dst_spec, rc) result(extension)
      class(StateItemSpec), allocatable :: extension
      class(StateSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: dst_spec
      integer, optional, intent(out) :: rc
      _RETURN(_SUCCESS)
   end function make_extension
   
   integer function extension_cost(this, src_spec, rc) result(cost)
      class(StateSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc
      cost = 0
      _RETURN(_SUCCESS)
   end function extension_cost


end module mapl3g_StateSpec
