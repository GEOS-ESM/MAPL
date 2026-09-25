#include "MAPL.h"

! StandardNameAspect enforces the standard_name convention (GEOS-ESM/MAPL#5413)
! between a connected Import and Export: modeled directly on UnitsAspect
! (superstructure/generic/specs/UnitsAspect.F90) - equal-or-unchecked/wildcard
! agreement, checked via the standard AspectMap matches()/connection-resolution
! machinery, with a single unaliased field-wide value (like units) rather than
! the per-alias storage generic/field-name-propagation gave standard_name
! before this aspect existed. See openspec/changes/enforce-standard-name-
! convention (or, once archived, specs/generic/standard-name-enforcement).
module mapl_StandardNameAspect_mod

   use mapl_ActualConnectionPt_mod
   use mapl_AspectId_mod
   use mapl_AspectStatus_mod
   use mapl_StateItemAspect_mod
   use mapl_ExtensionTransform_mod
   use mapl_NullTransform_mod
   use mapl_field_api
   use mapl_field_bundle_api
   use mapl_FieldDictionaryConfig_mod, only: FieldDictionaryConfig, get_field_dictionary_config
   use mapl_ValidationMode_mod, only: MAPL_VALIDATION_MODE_STRICT => VALIDATION_MODE_STRICT
   use mapl_ValidationMode_mod, only: operator(==)
   use mapl_KeywordEnforcer_mod
   use mapl_ErrorHandling_mod
   use pflogger
   use esmf

   implicit none(type,external)
   private

   public :: StandardNameAspect
   public :: to_StandardNameAspect

   interface to_StandardNameAspect
      procedure :: to_standardname_from_poly
      procedure :: to_standardname_from_map
   end interface to_StandardNameAspect

   type, extends(StateItemAspect) :: StandardNameAspect
      private
      character(:), allocatable :: standard_name
   contains
      procedure :: matches
      procedure :: make_transform
      procedure :: connect_to_export
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure, nopass :: get_aspect_id

      procedure :: get_standard_name
      procedure :: set_standard_name

      procedure :: update_from_payload
      procedure :: update_payload
      procedure :: print_aspect
   end type StandardNameAspect

   interface StandardNameAspect
      procedure new_StandardNameAspect
   end interface

   ! Sentinel meaning "no standard_name asserted" - accepted explicitly as a
   ! wildcard declaration (an Import may write this to mean "match any
   ! Export"), and used internally as the fallback value returned when
   ! nothing was ever assigned to the underlying ESMF_Info key. Mirrors
   ! UnitsAspect's identical '<unknown>' convention.
   character(len=*), parameter :: UNKNOWN = '<unknown>'

contains

   function new_StandardNameAspect(standard_name) result(aspect)
      type(StandardNameAspect) :: aspect
      character(*), optional, intent(in) :: standard_name

      call aspect%set_characteristic_state(ASPECT_STATUS_UNCHECKED)
      if (present(standard_name)) then
         if (standard_name /= UNKNOWN) then
            aspect%standard_name = standard_name
            call aspect%set_characteristic_state(ASPECT_STATUS_SPECIFIED)
         end if
      end if

   end function new_StandardNameAspect

   logical function supports_conversion_general(src)
      class(StandardNameAspect), intent(in) :: src

      ! standard_name disagreement is never resolved by inserting a coupler/
      ! transform (unlike units) - it is either accepted (permissive mode,
      ! wildcard, or one side unchecked) directly in matches(), or rejected
      ! outright (strict mode).
      supports_conversion_general = .false.

      _UNUSED_DUMMY(src)
   end function supports_conversion_general

   logical function supports_conversion_specific(src, dst)
      class(StandardNameAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      supports_conversion_specific = .false.

      _UNUSED_DUMMY(src)
      _UNUSED_DUMMY(dst)
   end function supports_conversion_specific

   ! src = the candidate Export's aspect; dst = the Import/goal aspect (see
   ! StateItemSpec%make_extension / can_connect_to). Implements
   ! generic/standard-name-enforcement's agreement requirement.
   logical function matches(src, dst)
      class(StandardNameAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      class(Logger), pointer :: lgr
      type(FieldDictionaryConfig), pointer :: fd_config

      select type(dst)
      class is (StandardNameAspect)

         ! Import declares no standard_name (or an explicit wildcard):
         ! accepts any Export, silently - independent of the Export's own
         ! declaration.
         if (dst%is_unchecked()) then
            matches = .true.
            return
         end if

         ! Import declares one, Export does not: accept (never a fatal
         ! error - a data-source Export may legitimately carry no
         ! standard_name), but warn, independent of ValidationMode.
         if (src%is_unchecked()) then
            matches = .true.
            lgr => logging%get_logger('mapl.generic')
            call lgr%warning('standard_name convention: Import declares standard_name "' // &
                 dst%standard_name // '" but the connected Export declares none.')
            return
         end if

         ! Both specified.
         if (src%standard_name == dst%standard_name) then
            matches = .true.
            return
         end if

         ! Disagreement: severity governed by the active ValidationMode.
         lgr => logging%get_logger('mapl.generic')
         fd_config => get_field_dictionary_config()
         if (fd_config%get_validation_mode() == MAPL_VALIDATION_MODE_STRICT) then
            matches = .false.
            call lgr%error('standard_name convention violation (strict mode): Export declares ' // &
                 'standard_name "' // src%standard_name // '" but connected Import declares "' // &
                 dst%standard_name // '". These must agree.')
         else
            matches = .true.
            call lgr%warning('standard_name convention violation (permissive mode): Export declares ' // &
                 'standard_name "' // src%standard_name // '" but connected Import declares "' // &
                 dst%standard_name // '". Connecting anyway; the Export value wins.')
         end if

      class default
         matches = .false.
      end select

   end function matches

   function make_transform(src, dst, other_aspects, rc) result(transform)
      class(ExtensionTransform), allocatable :: transform
      class(StandardNameAspect), intent(in) :: src
      class(StateItemAspect), intent(in)  :: dst
      type(AspectMap), target, intent(in)  :: other_aspects
      integer, optional, intent(out) :: rc

      ! matches() never signals "needs_extension_for" (it either accepts the
      ! connection or the framework's own can_connect_to assertion rejects it
      ! outright) - this is never expected to actually run, but the deferred
      ! interface requires an implementation.
      transform = NullTransform()

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(src)
      _UNUSED_DUMMY(dst)
      _UNUSED_DUMMY(other_aspects)
   end function make_transform

   subroutine connect_to_export(this, export, actual_pt, rc)
      class(StandardNameAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(StandardNameAspect) :: export_
      integer :: status

      ! Already validated equal-or-accepted by matches(); this endpoint
      ! adopts the export's value whenever the export declares one - this
      ! is "the export's value wins" for both the agreeing and the
      ! permissive-mismatch case (see matches()). Bugfix (found via
      ! openspec change use-field-dictionary-in-scenario-tests):
      ! export_%standard_name is unallocated whenever the export side is
      ! itself unchecked/wildcard (it declared no standard_name at all -
      ! matches()'s "Import declares one, Export does not" branch accepts
      ! exactly this case). When that happens, there is nothing for the
      ! export to contribute, so this side's own already-declared value (if
      ! any) is left untouched rather than being overwritten with nothing -
      ! unconditionally copying export_%standard_name here would both crash
      ! on the unallocated assignment and incorrectly discard this side's
      ! only real information.
      export_ = to_StandardNameAspect(export, _RC)
      if (allocated(export_%standard_name)) then
         this%standard_name = export_%standard_name
         call this%set_characteristic_state(export_%get_characteristic_state())
      end if
      ! else: export contributed nothing - this side's own prior value and
      ! characteristic_state (whatever matches() already accepted) stand.

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(actual_pt)
   end subroutine connect_to_export

   function to_standardname_from_poly(aspect, rc) result(standard_name_aspect)
      type(StandardNameAspect) :: standard_name_aspect
      class(StateItemAspect), intent(in) :: aspect
      integer, optional, intent(out) :: rc

      select type(aspect)
      class is (StandardNameAspect)
         standard_name_aspect = aspect
      class default
         _FAIL('aspect is not StandardNameAspect')
      end select

      _RETURN(_SUCCESS)
   end function to_standardname_from_poly

   function to_standardname_from_map(map, rc) result(standard_name_aspect)
      type(StandardNameAspect) :: standard_name_aspect
      type(AspectMap), target, intent(in) :: map
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateItemAspect), pointer :: poly

      poly => map%at(STANDARD_NAME_ASPECT_ID, _RC)
      standard_name_aspect = to_StandardNameAspect(poly, _RC)

      _RETURN(_SUCCESS)
   end function to_standardname_from_map

   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = STANDARD_NAME_ASPECT_ID
   end function get_aspect_id

   subroutine get_standard_name(this, standard_name)
      class(StandardNameAspect), intent(in) :: this
      character(:), allocatable, intent(out) :: standard_name

      if (allocated(this%standard_name)) standard_name = this%standard_name
   end subroutine get_standard_name

   subroutine set_standard_name(this, standard_name, rc)
      class(StandardNameAspect), intent(inout) :: this
      character(*), intent(in) :: standard_name
      integer, optional, intent(out) :: rc

      if (standard_name == UNKNOWN) then
         call this%set_characteristic_state(ASPECT_STATUS_UNCHECKED)
      else
         this%standard_name = standard_name
         call this%set_characteristic_state(ASPECT_STATUS_SPECIFIED)
      end if

      _RETURN(_SUCCESS)
   end subroutine set_standard_name

   subroutine update_from_payload(this, field, bundle, state, rc)
      class(StandardNameAspect), intent(inout) :: this
      type(esmf_Field), optional, intent(in) :: field
      type(esmf_FieldBundle), optional, intent(in) :: bundle
      type(esmf_State), optional, intent(in) :: state
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN_UNLESS(present(field) .or. present(bundle))

      if (present(field)) then
         call mapl_FieldGet(field, standard_name=this%standard_name, _RC)
      else if (present(bundle)) then
         call mapl_FieldBundleGet(bundle, standard_name=this%standard_name, _RC)
      end if

      if (.not. allocated(this%standard_name)) then
         call this%set_characteristic_state(ASPECT_STATUS_UNCHECKED)
      else if (this%standard_name == UNKNOWN) then
         call this%set_characteristic_state(ASPECT_STATUS_UNCHECKED)
      else
         call this%set_characteristic_state(ASPECT_STATUS_SPECIFIED)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(state)
   end subroutine update_from_payload

   subroutine update_payload(this, field, bundle, state, rc)
      class(StandardNameAspect), intent(in) :: this
      type(esmf_Field), optional, intent(inout) :: field
      type(esmf_FieldBundle), optional, intent(inout) :: bundle
      type(esmf_State), optional, intent(inout) :: state
      integer, optional, intent(out) :: rc
      character(len=:), allocatable :: standard_name

      integer :: status

      _RETURN_UNLESS(present(field) .or. present(bundle))

      standard_name = UNKNOWN
      if (this%is_specified() .and. allocated(this%standard_name)) standard_name = this%standard_name

      if (present(field)) then
         call mapl_FieldSet(field, standard_name=standard_name, _RC)
      else if (present(bundle)) then
         call mapl_FieldBundleSet(bundle, standard_name=standard_name, _RC)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(state)
   end subroutine update_payload

   subroutine print_aspect(this, file, line, rc)
      class(StandardNameAspect), intent(in) :: this
      character(*), intent(in) :: file
      integer, intent(in) :: line
      integer, optional, intent(out) :: rc

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(file)
      _UNUSED_DUMMY(line)
   end subroutine print_aspect

end module mapl_StandardNameAspect_mod
