#include "MAPL_Generic.h"

module MockAspect_mod
   use mapl3g_ActualConnectionPt
   use mapl3g_AspectId
   use mapl3g_StateItemASpect
   use mapl3g_ExtensionAction
   use mapl3g_NullAction
   use mapl_ErrorHandling
   implicit none
   private

   public :: MockAspect

   type, extends(StateItemAspect) :: MockAspect
      integer :: value
      logical :: supports_conversion_
   contains
      procedure :: matches
      procedure :: make_action
      procedure :: make_action2
      procedure :: connect_to_export
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure, nopass :: get_aspect_id
   end type MockAspect

   interface MockAspect
      procedure :: new_MockAspect
   end interface MockAspect

contains

   function new_MockAspect(mirror, time_dependent, value, supports_conversion) result(aspect)
      type(MockAspect) :: aspect
      logical, intent(in) :: mirror
      logical, intent(in) :: time_dependent
      integer, intent(in) :: value
      logical, intent(in) :: supports_conversion

      call aspect%set_mirror(mirror)
      call aspect%set_time_dependent(time_dependent)

      aspect%value = value
      aspect%supports_conversion_ = supports_conversion

   end function new_MockAspect

   logical function matches(src, dst)
      class(MockAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      select type (dst)
      type is (MockAspect)
         matches = (src%value == dst%value)
      class default
         matches = .false.
      end select
   end function matches

   logical function supports_conversion_general(src)
      class(MockAspect), intent(in) :: src
      supports_conversion_general = src%supports_conversion_
   end function supports_conversion_general

   logical function supports_conversion_specific(src, dst)
      class(MockAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      supports_conversion_specific = src%supports_conversion_
   end function supports_conversion_specific

   function make_action(src, dst, rc) result(action)
      class(ExtensionAction), allocatable :: action
      class(MockAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      integer, optional, intent(out) :: rc

      action = NullAction()
      if (present(rc)) rc = 0
   end function make_action
   
   function make_action2(src, dst, other_aspects, rc) result(action)
      class(ExtensionAction), allocatable :: action
      class(MockAspect), intent(in) :: src
      class(StateItemAspect), intent(in)  :: dst
      type(AspectMap), target, intent(in)  :: other_aspects
      integer, optional, intent(out) :: rc

      allocate(action,source=NullAction()) ! just in case
      if (present(rc)) rc = 0

   end function make_action2

   subroutine connect_to_export(this, export, actual_pt, rc)
      class(MockAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      integer :: status

      select type (this)
      type is (MockAspect)
         select type (export)
         type is (MockAspect)
            this = export
         class default
            _FAIL('bad subtype')
         end select
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(actual_pt)
   end subroutine connect_to_export

   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = MOCK_ASPECT_ID
   end function get_aspect_id

end module MockAspect_mod
