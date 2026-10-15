#include "MAPL.h"

! Wrapper around ESMF_NamedAlias for ESMF_Field, ESMF_FieldBundle, and
! ESMF_State handles.
!
! For Fields specifically: ESMF_NamedAlias only creates a new handle sharing
! the same underlying host - it has no knowledge that MAPL additionally
! stores certain per-connection-endpoint metadata (standard_name, long_name,
! restart_mode; see the generic/field-name-propagation OpenSpec change)
! namespaced by each handle's own ESMF_NamedAliasGet id. A field pulled out
! of one placement and re-aliased into a brand new one - anywhere outside of
! FieldClassAspect%add_to_state/connect_to_export, which already set this
! metadata explicitly for their own new placement - would otherwise resolve
! that new alias's own id to 'unknown'/defaults, even though the source
! Field it was aliased from carries real values (see #5407 and its
! follow-ups). Any such code must go through MAPL_NamedAlias instead of
! calling ESMF_NamedAlias directly.
!
! FieldBundle/State aliasing is a plain pass-through: MAPL does not (yet)
! attach any per-alias-id metadata to bundles or states the way it does for
! fields. FieldBundleInfo's own standard_name/long_name is a bundle-wide
! template stored at a fixed id (shared by every alias of the same bundle),
! so there is nothing to lose by aliasing a bundle; nested ESMF_State items
! carry no such metadata at all today. Both are included here purely so
! every ESMF_NamedAlias call site in MAPL can go through one consistent
! name, and so a future per-alias bundle/state feature has a single place to
! add real propagation logic.
module mapl_FieldNamedAlias_mod
   use mapl_FieldGet_mod, only: FieldGet
   use mapl_FieldSet_mod, only: FieldSet
   use mapl_FieldInfo_mod, only: FieldInfoGetInternal, FieldInfoSetInternal
   use mapl_RestartModes_mod, only: RestartMode
   use mapl_KeywordEnforcer_mod
   use mapl_ErrorHandling_mod
   use esmf

   implicit none(type,external)
   private

   public :: MAPL_NamedAlias

   interface MAPL_NamedAlias
      procedure :: field_named_alias
      procedure :: fieldbundle_named_alias
      procedure :: state_named_alias
   end interface MAPL_NamedAlias

contains

   function field_named_alias(field, unusable, name, rc) result(alias)
      type(ESMF_Field) :: alias
      type(ESMF_Field), intent(in) :: field
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: src_id, dst_id
      character(:), allocatable :: standard_name, long_name
      type(ESMF_Info) :: src_info, dst_info
      type(RestartMode) :: restart_mode

      alias = ESMF_NamedAlias(field, name=name, _RC)

      ! standard_name/long_name: MAPL_FieldGet/MAPL_FieldSet already resolve
      ! each handle's own alias id internally (via ESMF_NamedAliasGet), so a
      ! plain get-then-set round-trip scopes correctly to `alias`'s own
      ! (different) id. If `field` had no explicit value, this simply copies
      ! the same 'unknown' default `field` itself would already report, so
      ! it is always safe to do unconditionally.
      call FieldGet(field, standard_name=standard_name, long_name=long_name, _RC)
      call FieldSet(alias, standard_name=standard_name, long_name=long_name, _RC)

      ! restart_mode is not part of the public Field metadata API (it is
      ! MAPL's internal restart/checkpoint bookkeeping - see
      ! RestartHandler.F90), so go through FieldInfoGetInternal/SetInternal
      ! directly, resolving each handle's own alias id explicitly.
      call ESMF_InfoGetFromHost(field, src_info, _RC)
      call ESMF_NamedAliasGet(field, id=src_id, _RC)
      call FieldInfoGetInternal(src_info, src_id, restart_mode, _RC)

      call ESMF_InfoGetFromHost(alias, dst_info, _RC)
      call ESMF_NamedAliasGet(alias, id=dst_id, _RC)
      call FieldInfoSetInternal(dst_info, dst_id, restart_mode, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function field_named_alias

   ! Plain pass-through; see module header.
   function fieldbundle_named_alias(bundle, unusable, name, rc) result(alias)
      type(ESMF_FieldBundle) :: alias
      type(ESMF_FieldBundle), intent(in) :: bundle
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status

      alias = ESMF_NamedAlias(bundle, name=name, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function fieldbundle_named_alias

   ! Plain pass-through; see module header.
   function state_named_alias(state, unusable, name, rc) result(alias)
      type(ESMF_State) :: alias
      type(ESMF_State), intent(in) :: state
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status

      alias = ESMF_NamedAlias(state, name=name, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function state_named_alias

end module mapl_FieldNamedAlias_mod
