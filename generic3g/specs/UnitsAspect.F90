#include "MAPL.h"

module mapl3g_UnitsAspect
   use mapl3g_ActualConnectionPt
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl3g_ExtensionTransform
   use mapl3g_ConvertUnitsTransform
   use mapl3g_NullTransform
   use mapl3g_Field_API
   use mapl3g_FieldBundle_API
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use udunits2f, only: are_convertible
   use esmf
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
      procedure :: make_transform
      procedure :: connect_to_export
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure, nopass :: get_aspect_id

      procedure :: get_units
      procedure :: set_units

      procedure :: update_from_payload
      procedure :: update_payload
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
         if (src%units == "<unknown>" .or. dst%units == "<unknown>") return 
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
         matches = (src%units == dst%units) .or. &
                   (src%units == "<unknown>") .or. &
                   (dst%units == "<unknown>")
      class default
         matches = .false.
      end select

   end function matches

   function make_transform(src, dst, other_aspects, rc) result(transform)
      class(ExtensionTransform), allocatable :: transform
      class(UnitsAspect), intent(in) :: src
      class(StateItemAspect), intent(in)  :: dst
      type(AspectMap), target, intent(in)  :: other_aspects
      integer, optional, intent(out) :: rc

      integer :: status

      select type (dst)
      class is (UnitsAspect)
         allocate(transform, source=ConvertUnitsTransform(src%units, dst%units))
      class default
         allocate(transform, source=NullTransform())
         _FAIL('UnitsApsect cannot convert from other supclass.')
      end select

      _RETURN(_SUCCESS)
   end function make_transform

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

   subroutine set_units(this, units, rc)
      class(UnitsAspect), intent(inout) :: this
      character(*), intent(in) :: units
      integer, optional, intent(out) :: rc

      integer :: status
      this%units = units

      _RETURN(_SUCCESS)
   end subroutine set_units

   subroutine update_from_payload(this, field, bundle, state, rc)
      class(UnitsAspect), intent(inout) :: this
      type(esmf_Field), optional, intent(in) :: field
      type(esmf_FieldBundle), optional, intent(in) :: bundle
      type(esmf_State), optional, intent(in) :: state
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN_UNLESS(present(field) .or. present(bundle))

      if (present(field)) then
         call mapl_FieldGet(field, units=this%units, _RC)
      else if (present(bundle)) then
         call mapl_FieldBundleGet(bundle, units=this%units, _RC)
      end if

      call this%set_mirror(.not. allocated(this%units))

      _RETURN(_SUCCESS)
   end subroutine update_from_payload

   subroutine update_payload(this, field, bundle, state, rc)
      class(UnitsAspect), intent(in) :: this
      type(esmf_Field), optional, intent(inout) :: field
      type(esmf_FieldBundle), optional, intent(inout) :: bundle
      type(esmf_State), optional, intent(inout) :: state
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN_UNLESS(present(field) .or. present(bundle))

      if (present(field)) then
         call mapl_FieldSet(field, units=this%units, _RC)
      else if (present(bundle)) then
         call mapl_FieldBundleSet(bundle, units=this%units, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine update_payload
 

end module mapl3g_UnitsAspect
