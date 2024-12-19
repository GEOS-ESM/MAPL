#include "MAPL_Generic.h"

module mapl3g_UnitsAspect
   use mapl3g_StateItemAspect
   use mapl3g_ExtensionAction
   use mapl3g_ConvertUnitsAction
   use mapl3g_NullAction
   use mapl_ErrorHandling
   use udunits2f, only: are_convertible
   implicit none
   private

   public :: UnitsAspect


   type, extends(StateItemAspect) :: UnitsAspect
      private
      character(:), allocatable :: units
   contains
      procedure :: matches
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: make_action
   end type UnitsAspect

   interface UnitsAspect
      procedure new_UnitsAspect
   end interface

contains

   function new_UnitsAspect(units, is_mirror, is_time_dependent) result(aspect)
      type(UnitsAspect) :: aspect
      character(*), intent(in) :: units
      logical, optional, intent(in) :: is_mirror
      logical, optional, intent(in) :: is_time_dependent

      aspect%units = units
      call aspect%set_mirror(is_mirror)
      call aspect%set_mirror(is_time_dependent)

   end function new_UnitsAspect

   logical function supports_conversion_general(src)
      class(UnitsAspect), intent(in) :: src
      supports_conversion_general = .true.
   end function supports_conversion_general

   logical function supports_conversion_specific(src, dst)
      class(UnitsAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      select type (dst)
      class is (UnitsAspect)
         supports_conversion_specific = are_convertible(src%units, dst%units)
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

      select type(dst)
      class is (UnitsAspect)
         action = ConvertUnitsAction(src%units, dst%units)
      class default
         _FAIL('UnitsApsect cannot convert from other supclass.')
      end select

      _RETURN(_SUCCESS)
   end function make_action

end module mapl3g_UnitsAspect
