#include "MAPL.h"

!------------------------------------------------------------------------------
! BundleMemberSupport: what the Transforms that fill a bundle extension item
! (VECTOR or BRACKET) share. The output bundle is created without members,
! and the member count is not known when the chain is built (ExtData changes
! it at run time), so a Transform creates the output members at compute, one
! clone of each input member on the output bundle geom, and creates them
! again when the input member count differs from the output's.
!
! A vertical step changes the level count, the vertical grid, the stagger
! and the alignment between input and output, so ensure_vertical_output_members
! sizes and describes each member from the output bundle instead of from the
! input member, and also recreates members whose level count, vertical grid,
! stagger or alignment is not the output bundle's.
!
! Independent of the legacy layer: must not use the legacy Aspects (specs/),
! registry/, or the legacy transforms/.
!------------------------------------------------------------------------------
module mapl_BundleMemberSupport_mod
   use mapl_field_api, only: MAPL_FieldSet, MAPL_FieldGet
   use mapl_VerticalGrid_mod, only: VerticalGrid
   use mapl_VerticalStaggerLoc_mod, only: VerticalStaggerLoc
   use mapl_VerticalAlignment_mod, only: VerticalAlignment
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
   public :: ensure_vertical_output_members
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

      call ensure_members(fb_in, fb_out, .false., recreated, _RC)

      _RETURN(_SUCCESS)
   end subroutine ensure_output_members

   ! As ensure_output_members for a vertical step: a member is also recreated
   ! when its level count is not fb_out's, and a created member has fb_out's
   ! level count, vertical grid, stagger and alignment.
   subroutine ensure_vertical_output_members(fb_in, fb_out, recreated, rc)
      type(ESMF_FieldBundle), intent(inout) :: fb_in, fb_out
      logical, optional, intent(out) :: recreated
      integer, optional, intent(out) :: rc

      integer :: status

      call ensure_members(fb_in, fb_out, .true., recreated, _RC)

      _RETURN(_SUCCESS)
   end subroutine ensure_vertical_output_members

   subroutine ensure_members(fb_in, fb_out, vertical, recreated, rc)
      type(ESMF_FieldBundle), intent(inout) :: fb_in, fb_out
      logical, intent(in) :: vertical
      logical, optional, intent(out) :: recreated
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      integer :: n_in, n_out, num_levels
      type(ESMF_Field), allocatable :: members_in(:), members_out(:)
      type(ESMF_Geom), allocatable :: geom_out
      type(ESMF_TypeKind_Flag) :: typekind_out
      character(:), allocatable :: units_out
      class(VerticalGrid), pointer :: vgrid_out
      type(VerticalStaggerLoc) :: stagger_out
      type(VerticalAlignment) :: alignment_out
      logical :: stale, member_stale

      if (present(recreated)) recreated = .false.
      call ESMF_FieldBundleGet(fb_in, fieldCount=n_in, _RC)
      call ESMF_FieldBundleGet(fb_out, fieldCount=n_out, _RC)

      num_levels = 0
      vgrid_out => null()
      if (vertical .and. n_in > 0) then
         call MAPL_FieldBundleGet(fb_out, num_levels=num_levels, &
              vgrid=vgrid_out, vert_staggerloc=stagger_out, &
              vert_alignment=alignment_out, _RC)
         _ASSERT(associated(vgrid_out), 'BundleMemberSupport: output bundle has no vertical grid')
         _ASSERT(num_levels > 0, 'BundleMemberSupport: output bundle has no levels')
      end if

      stale = n_in /= n_out
      if (vertical .and. .not. stale .and. n_out > 0) then
         call MAPL_FieldBundleGet(fb_out, fieldList=members_out, _RC)
         do i = 1, size(members_out)
            call member_is_stale(members_out(i), num_levels, vgrid_out, &
                 stagger_out, alignment_out, member_stale, _RC)
            if (member_stale) stale = .true.
         end do
         deallocate(members_out)
      end if
      _RETURN_UNLESS(stale)

      call MAPL_FieldBundleGet(fb_in, fieldList=members_in, _RC)
      call MAPL_FieldBundleGet(fb_out, geom=geom_out, typekind=typekind_out, &
           units=units_out, _RC)
      _ASSERT(allocated(geom_out), 'BundleMemberSupport: output bundle has no geom')
      call recreate_members(fb_out, members_in, geom_out, typekind_out, &
           num_levels, _RC)

      call MAPL_FieldBundleGet(fb_out, fieldList=members_out, _RC)
      do i = 1, size(members_out)
         if (allocated(units_out)) then
            call MAPL_FieldSet(members_out(i), typekind=typekind_out, &
                 units=units_out, _RC)
         else
            call MAPL_FieldSet(members_out(i), typekind=typekind_out, _RC)
         end if
         if (vertical) then
            call MAPL_FieldSet(members_out(i), vgrid=vgrid_out, &
                 vert_staggerloc=stagger_out, vert_alignment=alignment_out, _RC)
         end if
      end do
      if (present(recreated)) recreated = .true.

      _RETURN(_SUCCESS)
   end subroutine ensure_members

   ! True when `member` does not have the level count (the first ungridded
   ! dimension is the vertical one), vertical grid, stagger or alignment the
   ! output bundle records. A graph-made link starts without members, but a
   ! reused chain or an earlier compute can leave members that no longer match.
   subroutine member_is_stale(member, num_levels, vgrid, stagger, alignment, &
        stale, rc)
      type(ESMF_Field), intent(inout) :: member
      integer, intent(in) :: num_levels
      class(VerticalGrid), intent(in) :: vgrid
      type(VerticalStaggerLoc), intent(in) :: stagger
      type(VerticalAlignment), intent(in) :: alignment
      logical, intent(out) :: stale
      integer, optional, intent(out) :: rc

      integer :: status, n
      integer, allocatable :: lbound_(:), ubound_(:)
      class(VerticalGrid), pointer :: vgrid_m
      type(VerticalStaggerLoc) :: stagger_m
      type(VerticalAlignment) :: alignment_m

      call ESMF_FieldGet(member, ungriddedDimCount=n, _RC)
      stale = (n == 0)
      if (n > 0) then
         allocate(lbound_(n), ubound_(n))
         call ESMF_FieldGet(member, ungriddedLBound=lbound_, &
              ungriddedUBound=ubound_, _RC)
         stale = (ubound_(1) - lbound_(1) + 1 /= num_levels)
      end if
      _RETURN_IF(stale)

      call MAPL_FieldGet(member, vgrid=vgrid_m, vert_staggerloc=stagger_m, &
           vert_alignment=alignment_m, _RC)
      stale = .not. associated(vgrid_m)
      _RETURN_IF(stale)
      stale = (vgrid_m%get_id() /= vgrid%get_id()) &
           .or. (stagger_m%to_string() /= stagger%to_string()) &
           .or. (alignment_m%to_string() /= alignment%to_string())

      _RETURN(_SUCCESS)
   end subroutine member_is_stale

   ! Replaces the output's members by one clone of each input member on
   ! geom_out. The old members are removed from the output bundle and are
   ! left undestroyed. A positive num_levels replaces the input member's level
   ! count.
   subroutine recreate_members(fb_out, members_in, geom_out, typekind_out, &
        num_levels, rc)
      type(ESMF_FieldBundle), intent(inout) :: fb_out
      type(ESMF_Field), intent(inout) :: members_in(:)
      type(ESMF_Geom), intent(in) :: geom_out
      type(ESMF_TypeKind_Flag), intent(in) :: typekind_out
      integer, intent(in) :: num_levels
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
         call clone_onto(members_in(i), geom_out, typekind_out, num_levels, &
              new_members(i), _RC)
      end do
      if (size(new_members) > 0) then
         call ESMF_FieldBundleAdd(fb_out, new_members, multiFlag=.true., &
              relaxedFlag=.true., _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine recreate_members

   ! A field like `archetype` (name, shape beyond the horizontal, metadata)
   ! on another geom, with its own allocation. A positive num_levels sets the
   ! first ungridded dimension, the vertical one.
   subroutine clone_onto(archetype, geom, typekind, num_levels, clone, rc)
      type(ESMF_Field), intent(inout) :: archetype
      type(ESMF_Geom), intent(in) :: geom
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      integer, intent(in) :: num_levels
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
      if (num_levels > 0) then
         _ASSERT(size(ungridded_ubound) > 0, 'BundleMemberSupport: member has no vertical dimension')
         ungridded_lbound(1) = 1
         ungridded_ubound(1) = num_levels
      end if

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
