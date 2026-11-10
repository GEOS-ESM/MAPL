#include "MAPL.h"

!------------------------------------------------------------------------------
! BundleMemberSupport: what the Transforms that fill a bundle extension item
! (VECTOR or BRACKET) share. The output bundle is created without members,
! and the member count is not known when the chain is built (ExtData changes
! it at run time), so a Transform creates the output members at compute, one
! clone of each input member on the output bundle geom, and creates them
! again when the input member count differs from the output's.
!
! Independent of the legacy layer: must not use the legacy Aspects (specs/),
! registry/, or the legacy transforms/.
!------------------------------------------------------------------------------
module mapl_BundleMemberSupport_mod
   use mapl_field_api, only: MAPL_FieldSet
   use mapl_field_bundle_api, only: MAPL_FieldBundleGet, MAPL_FieldBundleSet
   use mapl_esmf_info_keys_mod, only: INFO_INTERNAL_NAMESPACE, &
        KEY_INTERPOLATION_WEIGHTS
   use mapl_enums_api, only: MAPL_FieldBundleType_Flag, &
        MAPL_FIELDBUNDLETYPE_BRACKET, MAPL_FIELDBUNDLETYPE_VECTOR, &
        MAPL_VectorBasisKind, operator(==)
   use ESMF, only: ESMF_Field, ESMF_FieldBundle, ESMF_Geom, ESMF_Info
   use ESMF, only: ESMF_FieldGet, ESMF_FieldCreate, ESMF_GeomGet
   use ESMF, only: ESMF_FieldBundleGet, ESMF_FieldBundleAdd
   use ESMF, only: ESMF_FieldBundleRemove, ESMF_InfoGetFromHost
   use ESMF, only: ESMF_InfoUpdate, ESMF_InfoIsPresent, ESMF_MAXSTR
   use ESMF, only: ESMF_TypeKind_Flag, ESMF_KIND_R4
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: ensure_output_members
   public :: copy_bundle_attributes
   public :: copy_vector_basis
   public :: copy_interpolation_weights

contains

   ! Makes fb_out hold one member per member of fb_in, created on fb_out's
   ! own geom and typekind, if the counts differ. A created member carries
   ! the units and typekind of fb_out, not those of the input member it was
   ! cloned from. Leaves the members alone otherwise: they are recreated only
   ! when the member count changes, not at every compute. `recreated` tells
   ! the caller which happened.
   subroutine ensure_output_members(fb_in, fb_out, recreated, rc)
      type(ESMF_FieldBundle), intent(inout) :: fb_in, fb_out
      logical, optional, intent(out) :: recreated
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      integer :: n_in, n_out
      type(ESMF_Field), allocatable :: members_in(:), members_out(:)
      type(ESMF_Geom), allocatable :: geom_out
      type(ESMF_TypeKind_Flag) :: typekind_out
      character(:), allocatable :: units_out

      if (present(recreated)) recreated = .false.
      call ESMF_FieldBundleGet(fb_in, fieldCount=n_in, _RC)
      call ESMF_FieldBundleGet(fb_out, fieldCount=n_out, _RC)
      _RETURN_IF(n_in == n_out)

      call MAPL_FieldBundleGet(fb_in, fieldList=members_in, _RC)
      call MAPL_FieldBundleGet(fb_out, geom=geom_out, typekind=typekind_out, &
           units=units_out, _RC)
      _ASSERT(allocated(geom_out), 'BundleMemberSupport: output bundle has no geom')
      call recreate_members(fb_out, members_in, geom_out, typekind_out, _RC)

      call MAPL_FieldBundleGet(fb_out, fieldList=members_out, _RC)
      do i = 1, size(members_out)
         if (allocated(units_out)) then
            call MAPL_FieldSet(members_out(i), typekind=typekind_out, &
                 units=units_out, _RC)
         else
            call MAPL_FieldSet(members_out(i), typekind=typekind_out, _RC)
         end if
      end do
      if (present(recreated)) recreated = .true.

      _RETURN(_SUCCESS)
   end subroutine ensure_output_members

   ! Replaces the output's members by one clone of each input member on
   ! geom_out. The old members are removed from the output bundle and are
   ! left undestroyed.
   subroutine recreate_members(fb_out, members_in, geom_out, typekind_out, rc)
      type(ESMF_FieldBundle), intent(inout) :: fb_out
      type(ESMF_Field), intent(inout) :: members_in(:)
      type(ESMF_Geom), intent(in) :: geom_out
      type(ESMF_TypeKind_Flag), intent(in) :: typekind_out
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i, n_old
      character(len=ESMF_MAXSTR), allocatable :: old_names(:)
      type(ESMF_Field), allocatable :: new_members(:)

      call ESMF_FieldBundleGet(fb_out, fieldCount=n_old, _RC)
      if (n_old > 0) then
         allocate(old_names(n_old))
         call ESMF_FieldBundleGet(fb_out, fieldNameList=old_names, _RC)
         call ESMF_FieldBundleRemove(fb_out, old_names, multiflag=.true., _RC)
      end if

      allocate(new_members(size(members_in)))
      do i = 1, size(members_in)
         call clone_onto(members_in(i), geom_out, typekind_out, &
              new_members(i), _RC)
      end do
      if (size(new_members) > 0) then
         call ESMF_FieldBundleAdd(fb_out, new_members, multiFlag=.true., &
              relaxedFlag=.true., _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine recreate_members

   ! A field like `archetype` (name, shape beyond the horizontal, metadata)
   ! on another geom, with its own allocation.
   subroutine clone_onto(archetype, geom, typekind, clone, rc)
      type(ESMF_Field), intent(inout) :: archetype
      type(ESMF_Geom), intent(in) :: geom
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      type(ESMF_Field), intent(out) :: clone
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: field_rank, geom_rank
      integer, allocatable :: grid_to_field_map(:)
      integer, allocatable :: ungridded_lbound(:), ungridded_ubound(:)
      character(len=ESMF_MAXSTR) :: name
      type(ESMF_Info) :: info_in, info_out

      call ESMF_FieldGet(archetype, rank=field_rank, name=name, _RC)
      call ESMF_GeomGet(geom, dimCount=geom_rank, _RC)
      allocate(grid_to_field_map(geom_rank))
      allocate(ungridded_lbound(field_rank - geom_rank))
      allocate(ungridded_ubound(field_rank - geom_rank))
      call ESMF_FieldGet(archetype, gridToFieldMap=grid_to_field_map, &
           ungriddedLBound=ungridded_lbound, &
           ungriddedUBound=ungridded_ubound, _RC)

      clone = ESMF_FieldCreate(geom, typekind=typekind, &
           gridToFieldMap=grid_to_field_map, &
           ungriddedLBound=ungridded_lbound, &
           ungriddedUBound=ungridded_ubound, name=trim(name), _RC)

      call ESMF_InfoGetFromHost(archetype, info_in, _RC)
      call ESMF_InfoGetFromHost(clone, info_out, _RC)
      call ESMF_InfoUpdate(info_out, info_in, recursive=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine clone_onto

   ! What a Transform that keeps the bundle's meaning carries to the output:
   ! the basis of a vector, the interpolation weights of a bracket.
   subroutine copy_bundle_attributes(fb_in, fb_out, rc)
      type(ESMF_FieldBundle), intent(inout) :: fb_in, fb_out
      integer, optional, intent(out) :: rc

      integer :: status
      type(MAPL_FieldBundleType_Flag) :: bundle_type

      call MAPL_FieldBundleGet(fb_in, fieldBundleType=bundle_type, _RC)
      if (bundle_type == MAPL_FIELDBUNDLETYPE_VECTOR) then
         call copy_vector_basis(fb_in, fb_out, _RC)
      else if (bundle_type == MAPL_FIELDBUNDLETYPE_BRACKET) then
         call copy_interpolation_weights(fb_in, fb_out, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine copy_bundle_attributes

   subroutine copy_vector_basis(fb_in, fb_out, rc)
      type(ESMF_FieldBundle), intent(inout) :: fb_in, fb_out
      integer, optional, intent(out) :: rc

      integer :: status
      type(MAPL_VectorBasisKind) :: basis_kind

      call MAPL_FieldBundleGet(fb_in, vector_basis_kind=basis_kind, _RC)
      call MAPL_FieldBundleSet(fb_out, vector_basis_kind=basis_kind, _RC)

      _RETURN(_SUCCESS)
   end subroutine copy_vector_basis

   subroutine copy_interpolation_weights(fb_in, fb_out, rc)
      type(ESMF_FieldBundle), intent(inout) :: fb_in, fb_out
      integer, optional, intent(out) :: rc

      integer :: status
      real(ESMF_KIND_R4), allocatable :: weights(:)
      type(ESMF_Info) :: info
      logical :: has_weights

      ! A bracket that has no weights yet has nothing to carry over.
      call ESMF_InfoGetFromHost(fb_in, info, _RC)
      has_weights = ESMF_InfoIsPresent(info, &
           key=INFO_INTERNAL_NAMESPACE // KEY_INTERPOLATION_WEIGHTS)
      _RETURN_UNLESS(has_weights)

      call MAPL_FieldBundleGet(fb_in, interpolation_weights=weights, _RC)
      call MAPL_FieldBundleSet(fb_out, interpolation_weights=weights, _RC)

      _RETURN(_SUCCESS)
   end subroutine copy_interpolation_weights

end module mapl_BundleMemberSupport_mod
