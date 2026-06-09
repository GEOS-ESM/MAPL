#include "MAPL.h"

module mapl_VerticalGridAspect_mod
   use mapl_ActualConnectionPt_mod
   use mapl_AspectId_mod
   use mapl_field_api
   use mapl_field_bundle_api
   use mapl_StateItemAspect_mod
   use mapl_ExtensionTransform_mod
   use mapl_ExtendTransform_mod
   use mapl_VerticalGrid_mod
   use mapl_VerticalCoordinateDirection_mod
   use mapl_VerticalAlignment_mod
   use mapl_NullTransform_mod
   use mapl_VerticalRegridTransform_mod
   use mapl_GeomAspect_mod
   use mapl_TypekindAspect_mod
   use mapl_UnitsAspect_mod
   use mapl_NormalizationAspect_mod
   use mapl_enums_api, only: MAPL_NormalizationType, MAPL_NORMALIZE_NONE, operator(/=)
   use mapl_VerticalRegridMethod_mod
   use mapl_VerticalStaggerLoc_mod
   use mapl_VerticalRegridMethod_mod
   use mapl_MirrorVerticalGrid_mod, only: MirrorVerticalGrid
   use mapl_ErrorHandling_mod
   use esmf
   use gftl2_StringVector
   implicit none(type,external)
   private

   public :: VerticalGridAspect
   public :: to_VerticalGridAspect

   interface to_VerticalGridAspect
      procedure :: to_vertical_grid_from_poly
      procedure :: to_vertical_grid_from_map
   end interface to_VerticalGridAspect

   type, extends(StateItemAspect) :: VerticalGridAspect
      private
      class(VerticalGrid), allocatable :: vertical_grid
      type(VerticalRegridMethod) :: regrid_method = VERTICAL_REGRID_LINEAR
      type(VerticalStaggerLoc), allocatable :: vertical_stagger
      type(VerticalAlignment) :: vertical_alignment = VALIGN_WITH_GRID
   contains
      procedure :: matches
      procedure :: typesafe_matches
      procedure :: make_transform
      procedure :: connect_to_export
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure, nopass :: get_aspect_id

      procedure :: set_vertical_grid
      procedure :: get_vertical_grid
      procedure :: get_vertical_stagger
      procedure :: set_vertical_stagger
      procedure :: get_vertical_alignment
      procedure :: set_vertical_alignment
      procedure :: get_resolved_alignment
      procedure :: is_conservative

      procedure :: update_from_payload
      procedure :: update_payload

   end type VerticalGridAspect

   interface VerticalGridAspect
      procedure new_VerticalGridAspect_specific
   end interface

   interface
      module function make_transform(src, dst, other_aspects, rc) result(transform)
         class(ExtensionTransform), allocatable :: transform
         class(VerticalGridAspect), intent(in) :: src
         class(StateItemAspect), intent(in) :: dst
         type(AspectMap), target, intent(in) :: other_aspects
         integer, optional, intent(out) :: rc
      end function make_transform

      module function find_common_physical_dimension(src, dst, rc) result(physical_dimension)
         character(:), allocatable :: physical_dimension
         class(VerticalGridAspect), intent(in) :: src
         class(VerticalGridAspect), intent(in) :: dst
         integer, optional, intent(out) :: rc
      end function find_common_physical_dimension
   end interface

contains

   function new_VerticalGridAspect_specific(vertical_grid, regrid_method, vertical_stagger, vertical_alignment, geom, typekind, time_dependent) result(aspect)
      type(VerticalGridAspect) :: aspect
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      type(VerticalRegridMethod), optional, intent(in) :: regrid_method
      type(VerticalStaggerLoc), optional, intent(in) :: vertical_stagger
      type(VerticalAlignment), optional, intent(in) :: vertical_alignment
      type(ESMF_Geom), optional, intent(in) :: geom
      type(ESMF_Typekind_Flag), optional, intent(in) :: typekind
      logical, optional, intent(in) :: time_dependent

      call aspect%set_mirror(.true.)
      if (present(vertical_grid)) then
         aspect%vertical_grid = vertical_grid
         call aspect%set_mirror(.false.)
      end if

      if (present(regrid_method)) then
         aspect%regrid_method = regrid_method
      end if

      aspect%vertical_stagger = VERTICAL_STAGGER_CENTER
      if (present(vertical_stagger)) then
         aspect%vertical_stagger = vertical_stagger
      end if
      
      ! Default vertical_alignment is already set to VALIGN_WITH_GRID
      if (present(vertical_alignment)) then
         aspect%vertical_alignment = vertical_alignment
      end if
    
      call aspect%set_time_dependent(time_dependent)

      _UNUSED_DUMMY(geom)
      _UNUSED_DUMMY(typekind)
   end function new_VerticalGridAspect_specific

   function new_VerticalGridAspect_mirror() result(aspect)
      type(VerticalGridAspect) :: aspect

      call aspect%set_mirror(.true.)
   end function new_VerticalGridAspect_mirror

   logical function supports_conversion_general(src)
      class(VerticalGridAspect), intent(in) :: src

      supports_conversion_general = .true.

      _UNUSED_DUMMY(src)
   end function supports_conversion_general


   logical function supports_conversion_specific(src, dst)
      class(VerticalGridAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      type(StringVector) :: vec_in
      type(StringVector) :: vec_out
      integer :: i

      supports_conversion_specific = .false.

      select type (dst)
      class is (VerticalGridAspect)

         vec_in = src%vertical_grid%get_supported_physical_dimensions()
         vec_out = dst%vertical_grid%get_supported_physical_dimensions()

         do i = 1, vec_in%size()
            if (find(vec_out%begin(), vec_out%end(), vec_in%of(i)) /= vec_out%end()) then
               supports_conversion_specific = .true.
               return
            end if
         end do
         supports_conversion_specific = .false.
      end select
   end function supports_conversion_specific

   logical function matches(src, dst)
      class(VerticalGridAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      matches = dst%is_mirror()
      if (matches) return

      select type (dst)
      type is (VerticalGridAspect)
         matches = src%typesafe_matches(dst)
      class default
         matches = .false.
      end select

   end function matches

   logical function typesafe_matches(src, dst) result(matches)
      class(VerticalGridAspect), intent(in) :: src
      type(VerticalGridAspect), intent(in) :: dst

      logical :: grids_match
      type(VerticalCoordinateDirection) :: src_resolved, dst_resolved

      if (src%is_mirror()) then
         matches = .false. ! need geom extension
         return
      end if

      if (any([src%vertical_stagger,dst%vertical_stagger] == VERTICAL_STAGGER_NONE)) then
         ! both must be 2D
         matches = src%vertical_stagger == dst%vertical_stagger
         return
      end if

      ! Both must have vertical grids to get here, so can compare ids.
      grids_match = dst%vertical_grid%get_id() == src%vertical_grid%get_id()
      if (.not. grids_match) then
         ! The following allows Basic to match to grids that have the same number of levels
         grids_match = src%vertical_grid%matches(dst%vertical_grid)
      end if

      if (.not. grids_match) then
         matches = .false.
         return
      end if

      ! Grids match - now check if alignments also match
      src_resolved = src%get_resolved_alignment()
      dst_resolved = dst%get_resolved_alignment()
      matches = (src_resolved == dst_resolved)

   end function typesafe_matches


   subroutine set_vertical_grid(self, vertical_grid)
      class(VerticalGridAspect), intent(inout) :: self
      class(VerticalGrid), intent(in) :: vertical_grid

      self%vertical_grid = vertical_grid
      call self%set_mirror(.false.)
   end subroutine set_vertical_grid

   subroutine set_vertical_stagger(self, vertical_stagger)
      class(VerticalGridAspect), intent(inout) :: self
      class(VerticalStaggerLoc), intent(in) :: vertical_stagger

      self%vertical_stagger = vertical_stagger
      call self%set_mirror(.false.)
   end subroutine set_vertical_stagger

   subroutine connect_to_export(this, export, actual_pt, rc)
      class(VerticalGridAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(VerticalGridAspect) :: export_
      integer :: status

      export_ = to_VerticalGridAspect(export, _RC)
      !wdb fixme deleteme Should this use set_vertical_grid to set mirror?
      this%vertical_grid = export_%vertical_grid

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(actual_pt)
   end subroutine connect_to_export

   function to_vertical_grid_from_poly(aspect, rc) result(vertical_grid_aspect)
      type(VerticalGridAspect) :: vertical_grid_aspect
      class(StateItemAspect), intent(in) :: aspect
      integer, optional, intent(out) :: rc

      select type(aspect)
      class is (VerticalGridAspect)
         vertical_grid_aspect = aspect
      class default
         _FAIL('aspect is not VerticalGridAspect')
      end select

      _RETURN(_SUCCESS)
   end function to_vertical_grid_from_poly

   function to_vertical_grid_from_map(map, rc) result(vertical_grid_aspect)
      type(VerticalGridAspect) :: vertical_grid_aspect
      type(AspectMap), target, intent(in) :: map
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateItemAspect), pointer :: poly

      poly => map%at(VERTICAL_GRID_ASPECT_ID, _RC)
      vertical_grid_aspect = to_VerticalGridAspect(poly, _RC)

      _RETURN(_SUCCESS)
   end function to_vertical_grid_from_map
   
   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = VERTICAL_GRID_ASPECT_ID
   end function get_aspect_id

   function get_vertical_grid(this, rc) result(vertical_grid)
      class(VerticalGridAspect), target, intent(in) :: this
      class(VerticalGrid), pointer :: vertical_grid
      integer, optional, intent(out) :: rc

      _ASSERT(allocated(this%vertical_grid), "vertical_grid not allocated.")
      vertical_grid => this%vertical_grid

      _RETURN(_SUCCESS)
   end function get_vertical_grid

   function get_vertical_stagger(this, rc) result(vertical_stagger)
      class(VerticalGridAspect), intent(in) :: this
      type(VerticalStaggerLoc) :: vertical_stagger
      integer, optional, intent(out) :: rc

      _ASSERT(allocated(this%vertical_stagger), "vertical_stagger not allocated.")
      vertical_stagger = this%vertical_stagger

      _RETURN(_SUCCESS)
   end function get_vertical_stagger

   function get_vertical_alignment(this, rc) result(vertical_alignment)
      class(VerticalGridAspect), intent(in) :: this
      type(VerticalAlignment) :: vertical_alignment
      integer, optional, intent(out) :: rc

      vertical_alignment = this%vertical_alignment

      _RETURN(_SUCCESS)
   end function get_vertical_alignment

   subroutine set_vertical_alignment(this, vertical_alignment)
      class(VerticalGridAspect), intent(inout) :: this
      type(VerticalAlignment), intent(in) :: vertical_alignment

      this%vertical_alignment = vertical_alignment
   end subroutine set_vertical_alignment

   function get_resolved_alignment(this) result(direction)
      class(VerticalGridAspect), intent(in) :: this
      type(VerticalCoordinateDirection) :: direction
      
      type(VerticalCoordinateDirection) :: grid_direction
      
      !wdb fixme deleteme Should this use is_mirror()?
      if (.not. allocated(this%vertical_grid)) then
         direction = VCOORD_DIRECTION_INVALID
         return
      end if
      

      grid_direction = this%vertical_grid%get_coordinate_direction()
      direction = this%vertical_alignment%resolve(grid_direction)

   end function get_resolved_alignment

   logical function is_conservative(this)
      class(VerticalGridAspect), intent(in) :: this
      
      is_conservative = (this%regrid_method == VERTICAL_REGRID_CONSERVATIVE)
   end function is_conservative

   subroutine update_from_payload(this, field, bundle, state, rc)
      class(VerticalGridAspect), intent(inout) :: this
      type(esmf_Field), optional, intent(in) :: field
      type(esmf_FieldBundle), optional, intent(in) :: bundle
      type(esmf_State), optional, intent(in) :: state
      integer, optional, intent(out) :: rc

      integer :: status
      ! Must use a pointer for get/set, but aspect uses an allocatable
      ! Future work should consider changing aspect to also be pointer.
      class(VerticalGrid), pointer :: vgrid
      logical :: is_mirror

      _RETURN_UNLESS(present(field) .or. present(bundle))

      if (present(field)) then
         call mapl_FieldGet(field, vgrid=vgrid, vert_staggerloc=this%vertical_stagger, vert_alignment=this%vertical_alignment, _RC)
      else if (present(bundle)) then
         call mapl_FieldBundleGet(bundle, vgrid=vgrid, vert_staggerloc=this%vertical_stagger, _RC)
      end if

      is_mirror = .not. allocated(this%vertical_stagger)
      if (.not. is_mirror) then
         if (this%vertical_stagger /= VERTICAL_STAGGER_NONE) then
            is_mirror = .not. associated(vgrid)
         end if
      end if
      call this%set_mirror(is_mirror)

      if (allocated(this%vertical_grid)) deallocate(this%vertical_grid)
      if (associated(vgrid)) then
         this%vertical_grid = vgrid
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(state)
   end subroutine update_from_payload

   subroutine update_payload(this, field, bundle, state, rc)
      class(VerticalGridAspect), intent(in) :: this
      type(esmf_Field), optional, intent(inout) :: field
      type(esmf_FieldBundle), optional, intent(inout) :: bundle
      type(esmf_State), optional, intent(inout) :: state
      integer, optional, intent(out) :: rc
      integer :: status
      type(MirrorVerticalGrid) :: mirror_grid
      class(VerticalGrid), allocatable :: vgrid

      _RETURN_UNLESS(present(field) .or. present(bundle))

      mirror_grid = MirrorVerticalGrid()
      vgrid = mirror_grid
      if(.not. this%is_mirror()) then
         deallocate(vgrid)      
         if (allocated(this%vertical_grid)) then
            vgrid = this%vertical_grid
         end if
      end if

      if (present(field)) then
         call mapl_FieldSet(field, vgrid=vgrid, vert_staggerloc=this%vertical_stagger, vert_alignment=this%vertical_alignment, _RC)
      else if (present(bundle)) then
         call mapl_FieldBundleSet(bundle, vgrid=vgrid, vert_staggerloc=this%vertical_stagger, _RC)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(state)
   end subroutine update_payload

end module mapl_VerticalGridAspect_mod
