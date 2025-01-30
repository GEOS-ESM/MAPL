#include "MAPL_Generic.h"

module mapl3g_UnitsAspect
   use mapl3g_ActualConnectionPt
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl3g_ExtensionAction
   use mapl3g_ConvertUnitsAction
   use mapl3g_NullAction
   use mapl_ErrorHandling
   use udunits2f, only: are_convertible
   implicit none
   private

   public :: UnitsAspect
   public :: to_UnitsAspect

   interface to_UnitsAspect
      procedure :: to_units_from_poly
      procedure :: to_units_from_map
   end interface to_UnitsAspect

   type, extends(StateItemAspect) :: UnitsAspect
      private
      character(:), allocatable :: units
   contains
      procedure :: matches
      procedure :: make_action
      procedure :: make_action2
      procedure :: connect_to_export
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure, nopass :: get_aspect_id

      procedure :: get_units
   end type UnitsAspect

   interface UnitsAspect
      procedure new_UnitsAspect
   end interface

contains

   function new_UnitsAspect(units, is_time_dependent) result(aspect)
      type(UnitsAspect) :: aspect
      character(*), optional, intent(in) :: units
      logical, optional, intent(in) :: is_time_dependent

      call aspect%set_mirror(.true.)
      if (present(units)) then
         aspect%units = units
         call aspect%set_mirror(.false.)
      end if
      call aspect%set_mirror(is_time_dependent)

   end function new_UnitsAspect

   logical function supports_conversion_general(src)
      class(UnitsAspect), intent(in) :: src
      supports_conversion_general = .true.
   end function supports_conversion_general

   logical function supports_conversion_specific(src, dst)
      class(UnitsAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      integer :: ignore

      select type (dst)
      class is (UnitsAspect)
         supports_conversion_specific = .true.
         if (src%units == dst%units) return ! allow silly units so long as they are the same
         supports_conversion_specific = are_convertible(src%units, dst%units, rc=ignore)
      class default
         supports_conversion_specific = .false.
      end select

   end function supports_conversion_specific

   logical function matches(src, dst)
      class(UnitsAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      select type(dst)
      class is (UnitsAspect)
         matches = (src%units == dst%units)
      class default
         matches = .false.
      end select

   end function matches

   function make_action(src, dst, rc) result(action)
      class(ExtensionAction), allocatable :: action
      class(UnitsAspect), intent(in) :: src
      class(StateItemAspect), intent(in)  :: dst
      integer, optional, intent(out) :: rc

      integer :: status

      select type (dst)
      class is (UnitsAspect)
         ! gfortran ugh
!#         action = ConvertUnitsAction(src%units, dst%units)
         allocate(action, source=ConvertUnitsAction(src%units, dst%units))
      class default
         allocate(action, source=NullAction())
         _FAIL('UnitsApsect cannot convert from other supclass.')
      end select

      _RETURN(_SUCCESS)
   end function make_action

   function make_action2(src, dst, other_aspects, rc) result(action)
      class(ExtensionAction), allocatable :: action
      class(UnitsAspect), intent(in) :: src
      class(StateItemAspect), intent(in)  :: dst
      type(AspectMap), target, intent(in)  :: other_aspects
      integer, optional, intent(out) :: rc

      integer :: status

      select type (dst)
      class is (UnitsAspect)
         allocate(action, source=ConvertUnitsAction(src%units, dst%units))
      class default
         allocate(action, source=NullAction())
         _FAIL('UnitsApsect cannot convert from other supclass.')
      end select

      _RETURN(_SUCCESS)
   end function make_action2

   subroutine connect_to_export(this, export, actual_pt, rc)
      class(UnitsAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(UnitsAspect) :: export_
      integer :: status

      export_ = to_UnitsAspect(export, _RC)
      this%units = export_%units
      
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(actual_pt)
   end subroutine connect_to_export

   function to_units_from_poly(aspect, rc) result(units_aspect)
      type(UnitsAspect) :: units_aspect
      class(StateItemAspect), intent(in) :: aspect
      integer, optional, intent(out) :: rc

      integer :: status

      select type(aspect)
      class is (UnitsAspect)
         units_aspect = aspect
      class default
         _FAIL('aspect is not UnitsAspect')
      end select

      _RETURN(_SUCCESS)
   end function to_units_from_poly

   function to_units_from_map(map, rc) result(units_aspect)
      type(UnitsAspect) :: units_aspect
      type(AspectMap), target, intent(in) :: map
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateItemAspect), pointer :: poly

      poly => map%at(UNITS_ASPECT_ID, _RC)
      units_aspect = to_UnitsAspect(poly, _RC)

      _RETURN(_SUCCESS)
   end function to_units_from_map

   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = UNITS_ASPECT_ID
   end function get_aspect_id

   function get_units(this, rc) result(units)
      character(:), allocatable :: units
      class(UnitsAspect), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      units = '<unknown>'
      _ASSERT(allocated(this%units), 'UnitsAspect has no units')
      units = this%units

      _RETURN(_SUCCESS)
   end function get_units

end module mapl3g_UnitsAspect
