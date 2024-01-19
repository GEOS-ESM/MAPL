#include "MAPL_Generic.h"

module mapl3g_BracketSpec
   use mapl3g_FieldSpec
   use mapl3g_AbstractStateItemSpec
   use mapl3g_UngriddedDimsSpec
   use mapl3g_ActualConnectionPt
   use mapl3g_ESMF_Utilities, only: get_substate
   use mapl3g_ActualPtSpecPtrMap
   use mapl3g_MultiState
   use mapl3g_ActualPtVector
   use mapl3g_ActualConnectionPt
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use mapl3g_ExtensionAction
   use mapl3g_BundleAction
   use mapl3g_VerticalGeom
   use mapl3g_VerticalDimSpec
   use mapl3g_AbstractActionSpec
   use mapl3g_NullAction
   use gftl2_StringVector
   use esmf
   use nuopc

   implicit none
   private

   public :: BracketSpec
   public :: new_BracketSpec_geom

   type, extends(AbstractStateItemSpec) :: BracketSpec
      private

      type(FieldSpec) :: reference_spec
      integer, allocatable :: bracket_size ! unallocated implies mirror value in connection
      type(FieldSpec), allocatable :: field_specs(:)
      type(ESMF_FieldBundle) :: payload

   contains
      procedure :: create
      procedure :: destroy
      procedure :: allocate
      procedure :: get_dependencies

      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: add_to_state
      procedure :: add_to_bundle

      procedure :: extension_cost
      procedure :: make_extension
      procedure :: make_action
   end type BracketSpec

   interface BracketSpec
      module procedure new_BracketSpec_geom
   end interface BracketSpec

contains

   function new_BracketSpec_geom(field_spec, bracket_size) result(bracket_spec)
      
      type(BracketSpec) :: bracket_spec
      type(FieldSpec), optional, intent(in) :: field_spec
      integer, optional, intent(in) :: bracket_size

      bracket_spec%reference_spec = field_spec
      if (present(bracket_size)) bracket_spec%bracket_size = bracket_size

   end function new_BracketSpec_geom


   subroutine create(this, dependency_specs, rc)
      class(BracketSpec), intent(inout) :: this
      type(StateItemSpecPtr), intent(in) :: dependency_specs(:)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i

      this%payload = ESMF_FieldBundleCreate(_RC)
      call this%set_created()

      _RETURN(ESMF_SUCCESS)
   end subroutine create

   ! Tile / Grid   X  or X, Y
   subroutine allocate(this, rc)
      class(BracketSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      type(ESMF_Field) :: field, alias

      do i = 1, this%bracket_size
         call this%field_specs(i)%allocate(_RC)
         field = this%field_specs(i)%get_payload()
         alias = ESMF_NamedAlias(field, name=int_to_string(i), _RC)
         call ESMF_FieldBundleAdd(this%payload, [alias], multiflag=.true., _RC)
      end do

      _RETURN(ESMF_SUCCESS)
   contains

      function int_to_string(i) result(s)
         character(:), allocatable :: s
         integer, intent(in) :: i
         character(len=20) :: buffer
         write(buffer, '(i0)') i
         s = trim(buffer)
      end function int_to_string

   end subroutine allocate


   subroutine destroy(this, rc)
      class(BracketSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call destroy_component_fields(this, _RC)
      call ESMF_FieldBundleDestroy(this%payload, nogarbage=.true., _RC)
      call this%set_created(.false.)

      _RETURN(ESMF_SUCCESS)

   contains

      subroutine destroy_component_fields(this, rc)
         class(BracketSpec), intent(inout) :: this
         integer, optional, intent(out) :: rc

         integer :: status
         integer :: i
         type(ESMF_Field), allocatable :: fields(:)
         integer :: fieldCount

         if (allocated(this%field_specs)) then
            do i = 1, this%bracket_size
               call this%field_specs(i)%destroy(_RC)
            end do
         end if

         _RETURN(_SUCCESS)
      end subroutine destroy_component_fields

   end subroutine destroy


   function get_dependencies(this, rc) result(dependencies)
      type(ActualPtVector) :: dependencies
      class(BracketSpec), intent(in) :: this
      integer, optional, intent(out) :: rc

      dependencies = ActualPtVector()

      _RETURN(_SUCCESS)
   end function get_dependencies

   logical function can_connect_to(this, src_spec)
      class(BracketSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec

      select type(src_spec)
      class is (BracketSpec)
         can_connect_to = all ([ &
              this%reference_spec%can_connect_to(src_spec%reference_spec), &
              match_integer(this%bracket_size, src_spec%bracket_size) & ! allow for mirroring
              ])
      class default
         can_connect_to = .false.
      end select

   contains

      ! At least one of src/dst must have allocated a bracket size.
      ! THe other can mirror.
      logical function match_integer(dst, src) result(match)
         integer, allocatable, intent(in) :: dst, src
         
         match = allocated(dst) .or. allocated(src)
         if (allocated(dst) .and. allocated(src)) then
            match = (src == dst)
         end if
      end function match_integer

   end function can_connect_to

   subroutine connect_to(this, src_spec, actual_pt, rc)
      class(BracketSpec), intent(inout) :: this
      class(AbstractStateItemSpec), intent(inout) :: src_spec
      type(ActualConnectionPt), intent(in) :: actual_pt ! unused
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      type(StateItemSpecPtr) :: dependency_specs(0)

      _ASSERT(this%can_connect_to(src_spec), 'illegal connection')

      select type (src_spec)
      class is (BracketSpec)
         call this%destroy(_RC) ! use bundle from src
         this%payload = src_spec%payload
         call mirror_bracket(dst=this%bracket_size, src=src_spec%bracket_size)

         associate (n => this%bracket_size)
           this%field_specs = [(this%reference_spec, i=1,n)]
           src_spec%field_specs = [(src_spec%reference_spec, i=1,n)]
           
           do i = 1, this%bracket_size
              call src_spec%field_specs(i)%create(dependency_specs, _RC)
              call this%field_specs(i)%connect_to(src_spec%field_specs(i), actual_pt, _RC)
           end do
         end associate
         call this%set_created()

      class default
         _FAIL('Cannot connect field spec to non field spec.')
      end select

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(actual_pt)
   contains

      subroutine mirror_bracket(dst, src)
         integer, allocatable, intent(inout) :: dst
         integer, allocatable, intent(inout) :: src

         if (.not. allocated(src)) then
            _ASSERT(allocated(dst), 'cannot mirror unallocated bracket size')
            src = dst
         end if
         if (.not. allocated(dst)) then
            _ASSERT(allocated(src), 'cannot mirror unallocated bracket size')
            dst = src
         end if

      end subroutine mirror_bracket

   end subroutine connect_to


   subroutine add_to_state(this, multi_state, actual_pt, rc)
      class(BracketSpec), intent(in) :: this
      type(MultiState), intent(inout) :: multi_state
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(ESMF_FieldBundle) :: alias
      integer :: status
      type(ESMF_State) :: state, substate
      character(:), allocatable :: short_name

      call multi_state%get_state(state, actual_pt%get_state_intent(), _RC)
      call get_substate(state, actual_pt%get_comp_name(), substate=substate, _RC)

      short_name = actual_pt%get_esmf_name()
      alias = ESMF_NamedAlias(this%payload, name=short_name, _RC)
      call ESMF_StateAdd(substate, [alias], _RC)

      _RETURN(_SUCCESS)
   end subroutine add_to_state

   subroutine add_to_bundle(this, bundle, rc)
      class(BracketSpec), intent(in) :: this
      type(ESMF_FieldBundle), intent(inout) :: bundle
      integer, optional, intent(out) :: rc


      _FAIL("Cannot add bundle (bracket) to ESMF bundle.")

      _RETURN(_SUCCESS)
   end subroutine add_to_bundle


   integer function extension_cost(this, src_spec, rc) result(cost)
      class(BracketSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc

      integer :: status

      select type (src_spec)
      type is (BracketSpec)
         cost = this%reference_spec%extension_cost(src_spec%reference_spec, _RC)
      class default
         _FAIL('Cannot extend BracketSpec with non BracketSpec.')
      end select

      _RETURN(_SUCCESS)
   end function extension_cost

   function make_extension(this, dst_spec, rc) result(extension)
      class(AbstractStateItemSpec), allocatable :: extension
      class(BracketSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: dst_spec
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i

!#      extension = this
!#      do i = 1, this%bracket_size
!#         extension%field_specs(i) = this%field_specs(i)%make_extension(dst_spec, _RC)
!#      end do
!#      call extension%create(_RC)

      _RETURN(_SUCCESS)
   end function make_extension


   ! Return an atomic action that tranforms payload of "this"
   ! to payload of "goal".
   function make_action(this, dst_spec, rc) result(action)
      class(ExtensionAction), allocatable :: action
      class(BracketSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: dst_spec
      integer, optional, intent(out) :: rc

      integer :: status
      class(ExtensionAction), allocatable :: subaction
      integer :: i
      type(BundleAction) :: bundle_action

      action = NullAction() ! default

      select type (dst_spec)
      type is (BracketSpec)
         _ASSERT(this%bracket_size == dst_spec%bracket_size, 'bracket size mismatch')
         bundle_action = BundleAction()
         do i = 1, this%bracket_size
            subaction = this%field_specs(i)%make_action(dst_spec%field_specs(i), _RC)
            call bundle_action%add_action(subaction)
         end do
!##ifdef __GFORTRAN__
!#         deallocate(action)
!##endif
         action = bundle_action
      class default
         _FAIL('Dst_spec is incompatible with BracketSpec.')
      end select

      _RETURN(_SUCCESS)
   end function make_action


end module mapl3g_BracketSpec
