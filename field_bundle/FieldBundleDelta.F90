! This class is to support propagation of time-dependent Field
! attributes across couplers as well as to provide guidance to the
! containt Action objects on when to recompute internal items.

#include "MAPL_Exceptions.h"
module mapl3g_FieldBundleDelta
   use mapl3g_FieldBundleGet
   use mapl3g_FieldBundleSet
   use mapl3g_FieldBundleType_Flag
   use mapl3g_LU_Bound
   use mapl3g_FieldDelta
   use mapl3g_InfoUtilities
   use mapl3g_VerticalStaggerLoc
   use mapl3g_FieldCreate
   use mapl3g_FieldGet
   use mapl3g_FieldInfo
   use mapl_FieldUtilities
   use mapl3g_UngriddedDims
   use mapl_FieldPointerUtilities
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use esmf
   implicit none(type, external)
   private

   public :: FieldBundleDelta

   ! Note fieldCount can be derivedy from weights
   type :: FieldBundleDelta
      private
      type(FieldDelta) :: field_delta ! constant across bundle
      real(ESMF_KIND_R4), allocatable :: interpolation_weights(:)
   contains
      procedure :: initialize_bundle_delta
      generic :: initialize => initialize_bundle_delta
      procedure :: update_bundle
      procedure :: reallocate_bundle
   end type FieldBundleDelta


   interface FieldBundleDelta
      procedure new_FieldBundleDelta
      procedure new_FieldBundleDelta_field_delta
   end interface FieldBundleDelta

contains

   function new_FieldBundleDelta(fieldCount, geom, typekind, num_levels, units, interpolation_weights) result(bundle_delta)
      type(FieldBundleDelta) :: bundle_delta
      integer, optional, intent(in) :: fieldCount
      type(ESMF_Geom), optional, intent(in) :: geom
      type(ESMF_TypeKind_Flag), optional, intent(in) :: typekind
      integer, optional, intent(in) :: num_levels
      character(*), optional, intent(in) :: units
      real(ESMF_KIND_R4), intent(in), optional :: interpolation_weights(:)

      associate (field_delta => FieldDelta(geom=geom, typekind=typekind, num_levels=num_levels, units=units))
        bundle_delta = FieldBundleDelta(field_delta, fieldCount, interpolation_weights)
      end associate

   end function new_FieldBundleDelta

   function new_FieldBundleDelta_field_delta(field_delta, fieldCount, interpolation_weights) result(bundle_delta)
      type(FieldBundleDelta) :: bundle_delta
      type(FieldDelta), intent(in) :: field_delta
      integer, optional, intent(in) :: fieldCount
      real(ESMF_KIND_R4), optional, intent(in) :: interpolation_weights(:)

      bundle_delta%field_delta = field_delta

      if (present(interpolation_weights)) then
         bundle_delta%interpolation_weights = interpolation_weights
      end if

   end function new_FieldBundleDelta_field_delta


   ! delta = bundle_b - bundle_a
   subroutine initialize_bundle_delta(this, bundle_a, bundle_b, rc) 
      class(FieldBundleDelta), intent(out) :: this
      type(ESMF_FieldBundle), intent(in) :: bundle_a
      type(ESMF_FieldBundle), intent(in) :: bundle_b
      integer, optional, intent(out) :: rc

      integer :: status

      call compute_interpolation_weights_delta(this%interpolation_weights, bundle_a, bundle_b, _RC)
      call compute_field_delta(this%field_delta, bundle_a, bundle_b, _RC)

      _RETURN(_SUCCESS)


   contains

      subroutine compute_interpolation_weights_delta(interpolation_weights, bundle_a, bundle_b, rc)
         real(ESMF_KIND_R4), allocatable, intent(out) :: interpolation_weights(:)
         type(ESMF_FieldBundle), intent(in) :: bundle_a
         type(ESMF_FieldBundle), intent(in) :: bundle_b
         integer, optional, intent(out) :: rc

         integer :: status
         real(ESMF_KIND_R4), allocatable :: weights_a(:), weights_b(:)

         call FieldBundleGet(bundle_a, interpolation_weights=weights_a, _RC)
         call FieldBundleGet(bundle_b, interpolation_weights=weights_b, _RC)

         if (any(weights_a /= weights_b)) then
            interpolation_weights = weights_b
         end if

         _RETURN(_SUCCESS)

      end subroutine compute_interpolation_weights_delta

      subroutine compute_field_delta(field_delta, bundle_a, bundle_b, rc)
         type(FieldDelta), intent(out) :: field_delta
         type(ESMF_FieldBundle), intent(in) :: bundle_a
         type(ESMF_FieldBundle), intent(in) :: bundle_b
         integer, optional, intent(out) :: rc

         integer :: status
         integer :: fieldCount_a, fieldCount_b
         type(ESMF_Field), allocatable :: fieldList_a(:), fieldList_b(:)
         type(FieldBundleType_Flag) :: fieldBundleType_a, fieldBundleType_b

         call FieldBundleGet(bundle_a, &
              fieldCount=fieldCount_a, fieldBundleType=fieldBundleType_a, fieldList=fieldList_a, _RC)
         call FieldBundleGet(bundle_b, &
              fieldCount=fieldCount_b, fieldBundleType=fieldBundleType_b, fieldList=fieldList_b, _RC)
         
         _ASSERT(fieldBundleType_a == FIELDBUNDLETYPE_BRACKET, 'incorrect type of FieldBundle')
         _ASSERT(fieldBundleType_b == FIELDBUNDLETYPE_BRACKET, 'incorrect type of FieldBundle')

         ! TODO: add check thta name of 1st field is "bracket-prototype" or similar.
         if (fieldCount_a > 0 .and. fieldCount_b > 0) then
            call field_delta%initialize(fieldList_a(1), fieldList_b(1), _RC)
            _RETURN(_SUCCESS)
         end if

         if (fieldCount_b > 1) then
            ! full FieldDelta
            call field_delta%initialize(fieldList_b(1), _RC)
            _RETURN(_SUCCESS)
         end if

         ! Otherwise nothing to do. Fields are either going away
         ! (n_fields_b = 0) or there are no fields on either side
         ! (n_fields_a = 0 and n_fields_b = 0).
            
         _RETURN(_SUCCESS)
      end subroutine compute_field_delta
      

   end subroutine initialize_bundle_delta

   subroutine update_bundle(this, bundle, ignore, rc)
      class(FieldBundleDelta), intent(in) :: this
      type(ESMF_FieldBundle), intent(inout) :: bundle
      character(*), intent(in), optional :: ignore
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: ignore_
      type(ESMF_Field), allocatable :: fieldList(:)

      ignore_ = ''
      if (present(ignore)) ignore_ = ignore

      call this%reallocate_bundle(bundle, ignore=ignore_, _RC)
      call FieldBundleGet(bundle, fieldList=fieldList, _RC)
      call this%field_delta%update_fields(fieldList, ignore=ignore_, _RC)

      ! unique attribute in bundle
      call update_interpolation_weights(this%interpolation_weights, bundle, ignore=ignore_, _RC)

      _RETURN(_SUCCESS)
   contains


      subroutine update_interpolation_weights(interpolation_weights, bundle, ignore, rc)
         real(ESMF_KIND_R4), optional, intent(in) :: interpolation_weights(:)
         type(ESMF_FieldBundle), intent(inout) :: bundle
         character(*), intent(in) :: ignore
         integer, optional, intent(out) :: rc

         integer :: status

         _RETURN_UNLESS(present(interpolation_weights))
         _RETURN_IF(ignore == 'interpolation_weights')

         call FieldBundleSet(bundle, interpolation_weights=interpolation_weights, _RC)

         _RETURN(_SUCCESS)
      end subroutine update_interpolation_weights

   end subroutine update_bundle


   ! If the size of the bundle is not changing, then any reallocation is
   ! relegated to fields through the FieldDelta component.
   ! Otherwise we need to create or destroy fields in the bundle.
   
   subroutine reallocate_bundle(this, bundle, ignore, unusable, rc)
      class(FieldBundleDelta), intent(in) :: this
      type(ESMF_FieldBundle), intent(inout) :: bundle
      character(*), intent(in) :: ignore
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field), allocatable :: fieldList(:)
      type(ESMF_Geom), allocatable :: bundle_geom
      integer :: i
      type(LU_Bound), allocatable :: bounds(:)
      type(LU_Bound) :: vertical_bounds
      type(ESMF_TypeKind_Flag) :: typekind
      integer, allocatable :: ungriddedLbound(:), ungriddedUbound(:)
      integer :: old_field_count, new_field_count
      integer, allocatable :: num_levels
      character(:), allocatable :: units, vert_staggerloc_str
      type(VerticalStaggerLoc) :: vert_staggerloc
      character(ESMF_MAXSTR), allocatable :: fieldNameList(:)
      type(UngriddedDims) :: ungridded_dims

      ! Easy case 1: field count unchanged
      call FieldBundleGet(bundle, fieldList=fieldList, _RC)
      _RETURN_UNLESS(allocated(this%interpolation_weights))
      ! The number of weights is always one larger than the number of fields to support a constant
      ! offset.  ("Weights" is a funny term in that case.)
      new_field_count = size(this%interpolation_weights) - 1
      old_field_count = size(fieldList)
      _RETURN_IF(new_field_count == old_field_count)

      ! Easy case 2: field count changing to zero
      if (new_field_count == 0) then! "/dev/null" case
         call destroy_fields(fieldList, _RC)
         _RETURN(_SUCCESS)
      end if

      ! Hard case: need to create new fields?
      _ASSERT(size(fieldList) == 0, 'fieldCount should only change to or from zero.  ExtData use case.')
      deallocate(fieldList)
      allocate(fieldList(new_field_count))

      ! Need geom, typekind, and bounds to allocate fields before 
      call FieldBundleGet(bundle, geom=bundle_geom, &
           typekind=typekind, &
           ungridded_dims=ungridded_dims, &
           units=units, &
           vert_staggerloc=vert_staggerloc, &
           _RC)

      _ASSERT(allocated(bundle_geom), 'geom should be allocated by this point')
      _ASSERT(vert_staggerloc /= VERTICAL_STAGGER_INVALID, 'Vert stagger is INVALID.')
      if (vert_staggerloc /= VERTICAL_STAGGER_NONE) then
         ! Allocate num_levels so that it is PRESENT() int FieldEmptyComplete() below.
         allocate(num_levels)
         call FieldBundleGet(bundle, num_levels=num_levels, _RC)
      end if

      do i = 1, new_field_count
         fieldList(i) = ESMF_FieldEmptyCreate(_RC)
         call ESMF_FieldEmptySet(fieldList(i), geom=bundle_geom, _RC)
         call MAPL_FieldEmptyComplete(fieldList(i), typekind=typekind, &
              ungridded_dims=ungridded_dims, &
              num_levels=num_levels, vert_staggerLoc=vert_staggerLoc, &
              units=units, _RC)
      end do

      allocate(fieldNameList(old_field_count))
      call ESMF_FieldBundleGet(bundle, fieldNameList=fieldNameList, _RC)
      call ESMF_FieldBundleRemove(bundle, fieldNameList, multiflag=.true., _RC)

      call ESMF_FieldBundleAdd(bundle, fieldList, multiFlag=.true., relaxedFlag=.true., _RC)

      _RETURN(_SUCCESS)

   contains

      subroutine destroy_fields(fieldList, rc)
         type(ESMF_Field), intent(inout) :: fieldList(:)
         integer, optional, intent(out) :: rc

         integer :: status
         integer :: i

         do i = 1, size(fieldList)
            call ESMF_FieldDestroy(fieldList(i), _RC)
         end do

         _RETURN(_SUCCESS)
      end subroutine destroy_fields
      
   end subroutine reallocate_bundle

end module mapl3g_FieldBundleDelta
