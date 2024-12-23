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
!#      private
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

end module mapl3g_UnitsAspect
