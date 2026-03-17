#include "MAPL.h"

submodule (mapl3g_GeomAspect) make_transform_smod

   use mapl3g_VerticalGridAspect
   use mapl3g_NormalizationType, only: NORMALIZE_NONE, operator(==)

   implicit none(type,external)
contains

   module function make_transform(src, dst, other_aspects, rc) result(transform)
      class(ExtensionTransform), allocatable :: transform
      class(GeomAspect), intent(in) :: src
      class(StateItemAspect), intent(in)  :: dst
      type(AspectMap), target, intent(in)  :: other_aspects
      integer, optional, intent(out) :: rc

      integer :: status
      type(GeomAspect) :: dst_
      type(EsmfRegridderParam) :: regridder_param
      type(NormalizationMetadata) :: norm_metadata
      logical :: use_normalization

      allocate(transform,source=NullTransform()) ! just in case
      dst_ = to_GeomAspect(dst, _RC)
      deallocate(transform)

      ! Handle mirror case - needs geometry extension
      if (src%is_mirror()) then
         allocate(transform, source=ExtendTransform())
         _RETURN(_SUCCESS)
      end if

      ! Standard or normalized regridding case
      regridder_param = get_regridder_param(src, dst_, _RC)
      
      ! Check if normalization should be used (error handling first)
      use_normalization = should_use_normalization(regridder_param, other_aspects, &
                                                     norm_metadata, status)
      _VERIFY(status)
      
      ! Build appropriate transform based on normalization decision
      if (use_normalization) then
         transform = build_normalized_regrid_transform(src%geom, dst_%geom, &
                                                        regridder_param, other_aspects, &
                                                        norm_metadata, _RC)
      else
         allocate(transform, source=RegridTransform(src%geom, dst_%geom, regridder_param))
      end if

      _RETURN(_SUCCESS)
   end function make_transform

   !---------------------------------------------------------------------------
   ! Helper: Determine if normalization should be applied
   !---------------------------------------------------------------------------
   function should_use_normalization(regridder_param, other_aspects, norm_metadata, rc) result(use_it)
      logical :: use_it
      type(EsmfRegridderParam), intent(in) :: regridder_param
      type(AspectMap), target, intent(in) :: other_aspects
      type(NormalizationMetadata), intent(out) :: norm_metadata
      integer, optional, intent(out) :: rc

      integer :: status
      type(NormalizationAspect) :: norm_aspect
      type(NormalizationType) :: norm_type
      real :: scale_factor

      use_it = .false.

      ! Only conservative regridding can use integrated normalization
      _RETURN_UNLESS(regridder_param%is_conservative())

      ! Try to get normalization aspect (may not exist - that's OK)
      norm_aspect = to_NormalizationAspect(other_aspects, rc=status)
      _RETURN_UNLESS(status == _SUCCESS)

      ! Check if normalization is actually requested
      norm_type = norm_aspect%get_normalization_type(_RC)
      _RETURN_IF(norm_type == NORMALIZE_NONE)

      ! Get scale factor and verify it succeeded
      scale_factor = norm_aspect%get_scale_factor(rc=status)
      _VERIFY(status)

      ! Build normalization metadata (now safe - all inputs validated)
      norm_metadata = NormalizationMetadata( &
           normalization_type=norm_type, &
           normalization_scale=scale_factor)

      use_it = .true.
      _RETURN(_SUCCESS)
   end function should_use_normalization

   !---------------------------------------------------------------------------
   ! Helper: Build a regrid transform with integrated normalization
   !---------------------------------------------------------------------------
   function build_normalized_regrid_transform(src_geom, dst_geom, regridder_param, &
                                               other_aspects, norm_metadata, rc) result(transform)
      class(ExtensionTransform), allocatable :: transform
      type(ESMF_Geom), intent(in) :: src_geom, dst_geom
      type(EsmfRegridderParam), intent(in) :: regridder_param
      type(AspectMap), target, intent(in) :: other_aspects
      type(NormalizationMetadata), intent(in) :: norm_metadata
      integer, optional, intent(out) :: rc

      type(VerticalGridAspect) :: vert_aspect
      class(VerticalGrid), pointer :: vert_grid
      type(ESMF_Field) :: vcoord_field
      class(ComponentDriver), pointer :: vcoord_coupler
      character(:), allocatable :: physical_dimension
      type(AspectMap) :: coord_aspects
      type(NormalizationType) :: norm_type
      integer :: status

      ! Get vertical grid from aspect map
      vert_aspect = to_VerticalGridAspect(other_aspects, _RC)
      vert_grid => vert_aspect%get_vertical_grid(_RC)

      ! Determine physical dimension from normalization type
      norm_type = norm_metadata%get_normalization_type()
      physical_dimension = norm_type%get_physical_dimension()

      ! Get coordinate field with coupler
      coord_aspects = other_aspects
      vcoord_field = vert_grid%get_coordinate_field(physical_dimension, coord_aspects, &
                                                     coupler=vcoord_coupler, _RC)

      ! Create transform with integrated normalization support
      allocate(transform, source=RegridTransform(src_geom, dst_geom, regridder_param, &
                                                 vcoord_field, vcoord_coupler, norm_metadata))

      _RETURN(_SUCCESS)
   end function build_normalized_regrid_transform

end submodule make_transform_smod
