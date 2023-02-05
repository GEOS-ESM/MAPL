#include "MAPL_Generic.h"

module mapl3g_StateSpec
   use mapl3g_AbstractStateItemSpec
   use mapl3g_AbstractActionSpec
   use mapl3g_StateItemSpecMap
   use mapl3g_VariableSpec
   use mapl_ErrorHandling
   use ESMF
   use mapl_KeywordEnforcer
   implicit none
   private

   public :: StateSpec
   type, extends(AbstractStateItemSpec) :: StateSpec
      private
      type(ESMF_State) :: payload
      type(StateItemSpecMap) :: item_specs
   contains
      procedure :: initialize
      procedure :: add_item
      procedure :: get_item

      procedure :: create
      procedure :: destroy
      procedure :: allocate
      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: requires_extension
      procedure :: make_extension
      procedure :: add_to_state

   end type StateSpec


contains

   ! Nothing defined at this time.
   subroutine initialize(this, geom_base, var_spec, unusable, rc)
      class(StateSpec), intent(inout) :: this
      type(ESMF_GeomBase), intent(in) :: geom_base
      type(VariableSpec), intent(in) :: var_spec
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      character(:), allocatable :: units
      integer :: status

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize

   subroutine add_item(this, name, item)
      class(StateSpec), target, intent(inout) :: this
      character(len=*), intent(in) :: name
      class(AbstractStateItemSpec), intent(in) :: item

      call this%item_specs%insert(name, item)

   end subroutine add_item

   function get_item(this, name) result(item)
      class(AbstractStateItemSpec), pointer :: item
      class(StateSpec), target, intent(inout) :: this
      character(len=*), intent(in) :: name

      integer :: status

      item => this%item_specs%at(name, rc=status)

   end function get_item


   subroutine create(this, rc)
      class(StateSpec), intent(inout) :: this
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
   
   subroutine connect_to(this, src_spec, rc)
      class(StateSpec), intent(inout) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc

      integer :: status

      select type (src_spec)
      class is (StateSpec)
         this%payload = src_spec%payload
      class default
         _FAIL('Cannot connect field spec to non field spec.')
      end select

      _RETURN(ESMF_SUCCESS)

   end subroutine connect_to


   logical function can_connect_to(this, src_spec)
      class(StateSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec

      can_connect_to = same_type_as(src_spec, this)

   end function can_connect_to


   logical function requires_extension(this, src_spec)
      class(StateSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec

      requires_extension = .false.
      error stop "unimplemented procedure StateSpec::requires_extension"

   end function requires_extension

   subroutine add_to_state(this, state, short_name, rc)
      class(StateSpec), intent(in) :: this
      type(ESMF_State), intent(inout) :: state
      character(*), intent(in) :: short_name
      integer, optional, intent(out) :: rc

      type(ESMF_State) :: alias
      integer :: status

      _FAIL('unimplemented')

!!$      alias = ESMF_NamedAlias(this%payload, name=short_name, _RC)
!!$      call ESMF_StateAdd(state, this%payload, short_name, _RC)
!!$

   end subroutine add_to_state

   function make_extension(this, src_spec, rc) result(action_spec)
      class(AbstractActionSpec), allocatable :: action_spec
      class(StateSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc 
   end function make_extension

end module mapl3g_StateSpec
