#include "MAPL_Generic.h"

module mapl3g_StateItemSpec

   use mapl_ErrorHandling
   use mapl3g_ActualPtVector
   use mapl3g_ExtensionAction
   use mapl3g_StateItemAspect
   use mapl3g_AspectMap
   use gftl2_stringvector
   implicit none
   private

   public :: StateItemSpec
   public :: StateItemSpecPtr
   public :: StateItemAdapter
   public :: StateItemAdapterWrapper

   ! Concrete adapter subclasses are used to identify members of an
   ! ExtensionFamily that match some aspect of a "goal" spec.  A
   ! sequence of adapters can then be used.  Note that to avoid
   ! circularity, Adapters actually act on an array of ptr wrappers of
   ! StateItemSpecs.
   type, abstract :: StateItemAdapter
   contains
      generic :: adapt => adapt_one
      generic :: match => match_one
      procedure(I_adapt_one), deferred :: adapt_one
      procedure(I_match_one), deferred :: match_one
   end type StateItemAdapter

   type :: StateItemAdapterWrapper
      class(StateItemAdapter), allocatable :: adapter
   end type StateItemAdapterWrapper

   type, abstract :: StateItemSpec
      private

      logical :: active = .false.
      logical :: allocated = .false.
      type(StringVector) :: raw_dependencies
      type(ActualPtVector) :: dependencies

      type(AspectMap) :: aspects

   contains

      procedure(I_create), deferred :: create
      procedure(I_destroy), deferred :: destroy
      procedure(I_allocate), deferred :: allocate

      procedure(I_connect), deferred :: connect_to
      procedure(I_can_connect), deferred :: can_connect_to
      procedure(I_make_adapters), deferred :: make_adapters

      procedure :: get_aspect_order ! as string vector
!#      procedure(I_get_aspect_priorities), deferred :: get_aspect_priorities ! as colon-separated string
      procedure :: get_aspect_priorities ! default implementation as aid to refactoring

      procedure(I_add_to_state), deferred :: add_to_state
      procedure(I_add_to_bundle), deferred :: add_to_bundle
      procedure(I_set_geometry), deferred :: set_geometry

      procedure(I_write_formatted), deferred :: write_formatted
#ifndef __GFORTRAN__
      generic :: write(formatted) => write_formatted
#endif

      procedure, non_overridable :: set_allocated
      procedure, non_overridable :: is_allocated
      procedure, non_overridable :: is_active
      procedure, non_overridable :: set_active
      procedure, non_overridable :: get_aspect
      procedure, non_overridable :: get_aspects
      procedure, non_overridable :: set_aspect

      procedure :: get_dependencies
      procedure :: get_raw_dependencies
      procedure :: set_dependencies
      procedure :: set_raw_dependencies
   end type StateItemSpec

   type :: StateItemSpecPtr
      class(StateItemSpec), pointer :: ptr => null()
   end type StateItemSpecPtr

   abstract interface

      ! Modify "this" to match attribute in spec.
      subroutine I_adapt_one(this, spec, action, rc)
         import StateItemAdapter
         import StateItemSpec
         import ExtensionAction
         class(StateItemAdapter), intent(in) :: this
         class(StateItemSpec), intent(inout) :: spec
         class(ExtensionAction), allocatable, intent(out) :: action
         integer, optional, intent(out) :: rc
      end subroutine I_adapt_one

      ! Detect if "this" matches attribute in spec.
      logical function I_match_one(this, spec, rc) result(match)
         import StateItemAdapter
         import StateItemSpec
         class(StateItemAdapter), intent(in) :: this
         class(StateItemSpec), intent(in) :: spec
         integer, optional, intent(out) :: rc
      end function I_match_one

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

      subroutine I_set_geometry(this, geom, vertical_grid, run_dt, rc)
         use esmf, only: ESMF_Geom, ESMF_TimeInterval
         use mapl3g_VerticalGrid, only: VerticalGrid
         import StateItemSpec
         class(StateItemSpec), intent(inout) :: this
         type(ESMF_Geom), optional, intent(in) :: geom
         class(VerticalGrid), optional, intent(in) :: vertical_grid
         type(ESMF_TimeInterval), optional, intent(in) :: run_dt
         integer, optional, intent(out) :: rc
      end subroutine I_set_geometry

      subroutine I_write_formatted(this, unit, iotype, v_list, iostat, iomsg)
         import StateItemSpec
         class(StateItemSpec), intent(in) :: this
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in) :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine I_write_formatted

      ! Returns an ordered list of adapters that priorities matching
      ! rules for connecting a family of extension to a goal spec.
      ! The intent is that the adapters are ordered to prioritize
      ! coupling to avoid more expensive and/or diffusive couplers.
      ! E.g., The first adapter for a FieldSpec is expected to be
      ! a GeomAdapter so that a new RegridAction is only needed when
      ! no existing extensions match the geom of the goal_spec.
      function I_make_adapters(this, goal_spec, rc) result(adapters)
         import StateItemSpec
         import StateItemAdapterWrapper
         type(StateItemAdapterWrapper), allocatable :: adapters(:)
         class(StateItemSpec), intent(in) :: this
         class(StateItemSpec), intent(in) :: goal_spec
         integer, optional, intent(out) :: rc
      end function I_make_adapters

      function I_get_aspect_priorities(src_spec, dst_spec) result(aspect_order)
         import StateItemSpec
         character(:), allocatable :: order
         class(StateItemSpec), intent(in) :: src_spec
         class(StateItemSpec), intent(in) :: dst_spec
      end function I_get_aspect_priorities

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

   function get_aspect(this, name, rc) result(aspect)
      class(StateItemAspect), pointer :: aspect
      character(*), intent(in) :: name
      class(StateItemSpec), target, intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      aspect => null()
      _ASSERT(this%aspects%count(name) == 1, 'Aspect ' // name // ' not found.')
      
      aspect => this%aspects%at(name)

      _RETURN(_SUCCESS)
   end function get_aspect

   function get_aspects(this) result(aspects)
      type(AspectMap), pointer :: aspects
      class(StateItemSpec), target, intent(in) :: this
      aspects => this%aspects
   end function get_aspects

   subroutine set_aspect(this, name, aspect)
      class(StateItemSpec), target, intent(inout) :: this
      character(*), intent(in) :: name
      class(StateItemAspect), intent(in) :: aspect

      call this%aspects%insert(name, aspect)

   end subroutine set_aspect

   function get_aspect_order(src_spec, dst_spec) result(names)
      type(StringVector) :: names
      class(StateItemSpec), intent(in) :: src_spec
      class(StateItemSpec), intent(in) :: dst_spec

      character(:), allocatable :: str
      character(*), parameter :: SEPARATOR = '::'
      integer :: idx

      str = src_spec%get_aspect_priorities(dst_spec)
      if (len(str) == 0) then ! empty list
         return
      end if

      do
         idx = index(str, SEPARATOR)
         if (idx == 0) then
            call names%push_back(str)
            exit
         end if
         call names%push_back(str(1:idx-1))
         str = str(idx+len(SEPARATOR):)
      end do
   end function get_aspect_order


   ! This procedure should be deleted once extant subclasses of
   ! StateItemSpec have been updated and implement their own.
   function get_aspect_priorities(src_spec, dst_spec) result(order)
      character(:), allocatable :: order
      class(StateItemSpec), intent(in) :: src_spec
      class(StateItemSpec), intent(in) :: dst_spec

      order = ''
   end function get_aspect_priorities

end module mapl3g_StateItemSpec
