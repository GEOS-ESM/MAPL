#include "MAPL_Generic.h"

module mapl3g_StateItemSpec
   use mapl_ErrorHandling
   use mapl3g_ActualPtVector
   use gftl2_stringvector
   implicit none
   private

   public :: StateItemSpec
   public :: StateItemSpecPtr
   public :: StateItemFilter
   public :: StateItemFilterWrapper

   ! Concrete filter subclasses are used to identify members of an
   ! ExtensionFamily that match some aspect of a "goal" spec.
   ! A sequence of filters can then be used.
   ! Note that to avoid circularity, Filters actually act on
   ! an array of ptr wrappers of StateItemSpecs.
   type, abstract :: StateItemFilter
   contains
      procedure(I_apply_one), deferred :: apply_one
      procedure :: apply_ptr
      generic :: apply => apply_one, apply_ptr
   end type StateItemFilter

   type :: StateItemFilterWrapper
      class(StateItemFilter), allocatable :: filter
   end type StateItemFilterWrapper

   type, abstract :: StateItemSpec
      private

      logical :: active = .false.
      logical :: allocated = .false.
      type(StringVector) :: raw_dependencies
      type(ActualPtVector) :: dependencies

   contains

      procedure(I_create), deferred :: create
      procedure(I_destroy), deferred :: destroy
      procedure(I_allocate), deferred :: allocate

      procedure(I_connect), deferred :: connect_to
      procedure(I_can_connect), deferred :: can_connect_to
      procedure(I_make_extension), deferred :: make_extension
      procedure(I_extension_cost), deferred :: extension_cost

      procedure(I_make_filters), deferred :: make_filters

      procedure(I_add_to_state), deferred :: add_to_state
      procedure(I_add_to_bundle), deferred :: add_to_bundle
      procedure(I_set_geometry), deferred :: set_geometry

      procedure, non_overridable :: set_allocated
      procedure, non_overridable :: is_allocated
      procedure, non_overridable :: is_active
      procedure, non_overridable :: set_active

      procedure :: get_dependencies
      procedure :: get_raw_dependencies
      procedure :: set_dependencies
      procedure :: set_raw_dependencies
  end type StateItemSpec

   type :: StateItemSpecPtr
      class(StateItemSpec), pointer :: ptr => null()
   end type StateItemSpecPtr

   abstract interface

      logical function I_apply_one(this, spec)
         import StateItemFilter
         import StateItemSpec
         class(StateItemFilter), intent(in) :: this
         class(StateItemSpec), intent(in) :: spec
      end function I_apply_one

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

      subroutine I_make_extension(this, dst_spec, new_spec, action, rc)
         use mapl3g_ExtensionAction
         import StateItemSpec
         class(StateItemSpec), intent(in) :: this
         class(StateItemSpec), intent(in) :: dst_spec
         class(StateItemSpec), allocatable, intent(out) :: new_spec
         class(ExtensionAction), allocatable, intent(out) :: action
         integer, optional, intent(out) :: rc
      end subroutine I_make_extension

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

      subroutine I_set_geometry(this, geom, vertical_grid, rc)
         use esmf, only: ESMF_Geom
         use mapl3g_VerticalGrid, only: VerticalGrid
         import StateItemSpec
         class(StateItemSpec), intent(inout) :: this
         type(ESMF_Geom), optional, intent(in) :: geom
         class(VerticalGrid), optional, intent(in) :: vertical_grid
         integer, optional, intent(out) :: rc
      end subroutine I_set_geometry


      ! Returns an ordered list of filters that priorities matching
      ! rules for connecting a family of extension to a goal spec.
      ! The intent is that the filters are ordered to prioritize
      ! coupling to avoid more expensive and/or diffusive couplers.
      ! E.g., The first filter for a FieldSpec is expected to be
      ! a GeomFilter so that a new RegridAction is only needed when
      ! no existing extensions match the geom of the goal_spec.
      function I_make_filters(this, goal_spec, rc) result(filters)
         import StateITemSpec
         import StateItemFilterWrapper
         type(StateItemFilterWrapper), allocatable :: filters(:)
         class(StateItemSpec), intent(in) :: this
         class(StateItemSpec), intent(in) :: goal_spec
         integer, optional, intent(out) :: rc
      end function I_make_filters
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

   function get_dependencies(this) result(dependencies)
      type(ActualPtVector) :: dependencies
      class(StateItemSpec), intent(in) :: this
      dependencies = this%dependencies
   end function get_dependencies

   function get_raw_dependencies(this) result(raw_dependencies)
      type(StringVector) :: raw_dependencies
      class(StateItemSpec), intent(in) :: this
      raw_dependencies = this%raw_dependencies
   end function get_raw_dependencies

   subroutine set_dependencies(this, dependencies)
      class(StateItemSpec), intent(inout) :: this
      type(ActualPtVector), intent(in):: dependencies
      this%dependencies = dependencies
   end subroutine set_dependencies

   subroutine set_raw_dependencies(this, raw_dependencies)
      class(StateItemSpec), intent(inout) :: this
      type(StringVector), intent(in):: raw_dependencies
      this%raw_dependencies = raw_dependencies
   end subroutine set_raw_dependencies

   logical function apply_ptr(this, spec_ptr) result(match)
      class(StateItemFilter), intent(in) :: this
      type(StateItemSpecPtr), intent(in) :: spec_ptr
      match = this%apply(spec_ptr%ptr)
   end function apply_ptr

end module mapl3g_StateItemSpec
