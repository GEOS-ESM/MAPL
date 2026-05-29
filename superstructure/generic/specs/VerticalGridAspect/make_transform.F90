#include "MAPL.h"

submodule (mapl_VerticalGridAspect_mod) make_transform_smod

   use mapl_ModelVerticalGrid_mod, only: ModelVerticalGrid
   use mapl_ComponentDriver_mod

   implicit none(type,external)

contains

   module function make_transform(src, dst, other_aspects, rc) result(transform)
      class(ExtensionTransform), allocatable :: transform
      class(VerticalGridAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      type(AspectMap), target, intent(in) :: other_aspects
      integer, optional, intent(out) :: rc

      class(ComponentDriver), pointer :: v_in_coupler
      class(ComponentDriver), pointer :: v_out_coupler
      type(ESMF_Field) :: v_in_field, v_out_field
      type(VerticalGridAspect) :: dst_
      type(NormalizationAspect) :: norm_aspect
      type(MAPL_NormalizationType) :: norm_type
      type(AspectMap) :: coord_aspects
      character(:), allocatable :: units
      character(:), allocatable :: physical_dimension
      type(VerticalCoordinateDirection) :: src_alignment, dst_alignment
      logical :: grids_match
      logical :: needs_normalization
      type(VerticalRegridParam) :: regrid_param
      integer :: status

      if (src%is_mirror()) then
         allocate(transform, source=ExtendTransform())
         _RETURN(_SUCCESS)
      end if

      allocate(transform, source=NullTransform()) ! just in case
      dst_ = to_VerticalGridAspect(dst, _RC)

      ! Query NormalizationAspect to determine if normalization is needed
      ! for conservative vertical regridding. If NormalizationAspect is not present,
      ! default to no normalization (status will be non-zero).
      needs_normalization = .false.
      if (dst_%regrid_method == VERTICAL_REGRID_CONSERVATIVE) then
         norm_aspect = to_NormalizationAspect(other_aspects, rc=status)
         if (status == _SUCCESS) then
            norm_type = norm_aspect%get_normalization_type(_RC)
            needs_normalization = (norm_type /= MAPL_NORMALIZE_NONE)
         end if
      end if

      physical_dimension = find_common_physical_dimension(src, dst_, _RC)
      units = dst_%vertical_grid%get_units(physical_dimension, _RC)

      ! Build aspect map for coordinate field creation
      coord_aspects = other_aspects
      call coord_aspects%insert(UNITS_ASPECT_ID, UnitsAspect(units))

      v_in_field = src%vertical_grid%get_coordinate_field(physical_dimension, coord_aspects, _RC)
      select type (vg => src%vertical_grid)
      type is (ModelVerticalGrid)
         v_in_field = vg%get_coordinate_field_with_coupler(physical_dimension, &
              coord_aspects, coupler=v_in_coupler, _RC)
      class default
         v_in_coupler => null()
      end select

      v_out_field = dst_%vertical_grid%get_coordinate_field(physical_dimension, coord_aspects, _RC)
      select type (vg => dst_%vertical_grid)
      type is (ModelVerticalGrid)
         v_out_field = vg%get_coordinate_field_with_coupler(physical_dimension, &
              coord_aspects, coupler=v_out_coupler, _RC)
      class default
         v_out_coupler => null()
      end select

      ! Get resolved alignments
      src_alignment = src%get_resolved_alignment()
      dst_alignment = dst_%get_resolved_alignment()

      ! Check if grids are the same (degenerate case)
      grids_match = dst_%vertical_grid%get_id() == src%vertical_grid%get_id()
      if (.not. grids_match) then
         grids_match = src%vertical_grid%matches(dst_%vertical_grid)
      end if

      ! If grids match AND alignments match, no transform needed
      if (grids_match .and. (src_alignment == dst_alignment)) then
         deallocate(transform)
         allocate(transform, source=NullTransform())
         _RETURN(_SUCCESS)
      end if

      ! Build regrid parameters
      regrid_param%stagger_in = src%vertical_stagger
      regrid_param%stagger_out = dst_%vertical_stagger
      regrid_param%method = dst_%regrid_method
      regrid_param%src_alignment = src_alignment
      regrid_param%dst_alignment = dst_alignment
      regrid_param%is_degenerate_case = grids_match
      regrid_param%needs_normalization = needs_normalization

      deallocate(transform)
      transform = VerticalRegridTransform(v_in_field, v_in_coupler, v_out_field, v_out_coupler, regrid_param)

      _RETURN(_SUCCESS)
   end function make_transform

end submodule make_transform_smod
