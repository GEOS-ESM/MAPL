#include "MAPL.h"

module mapl3g_VerticalGridAspect
   use mapl3g_ActualConnectionPt
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl3g_ExtensionTransform
   use mapl3g_ExtendTransform
   use mapl3g_VerticalGrid
   use mapl3g_NullTransform
   use mapl3g_VerticalRegridTransform
   use mapl3g_GeomAspect
   use mapl3g_TypekindAspect
   use mapl3g_VerticalRegridMethod
   use mapl3g_VerticalStaggerLoc
   use mapl3g_VerticalRegridMethod
   use mapl3g_ComponentDriver
   use mapl_ErrorHandling
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
   contains
      procedure :: matches
      procedure :: make_transform
      procedure :: connect_to_export
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure, nopass :: get_aspect_id

      procedure :: set_vertical_grid
      procedure :: get_vertical_grid
      procedure :: get_vertical_stagger
      procedure :: set_vertical_stagger
   end type VerticalGridAspect

   interface VerticalGridAspect
      procedure new_VerticalGridAspect_specific
   end interface

contains

   function new_VerticalGridAspect_specific(vertical_grid, regrid_method, vertical_stagger, geom, typekind, time_dependent) result(aspect)
      type(VerticalGridAspect) :: aspect
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      type(VerticalRegridMethod), optional, intent(in) :: regrid_method
      type(VerticalStaggerLoc), optional, intent(in) :: vertical_stagger
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
    
      call aspect%set_time_dependent(time_dependent)
   end function new_VerticalGridAspect_specific

   function new_VerticalGridAspect_mirror() result(aspect)
      type(VerticalGridAspect) :: aspect

      call aspect%set_mirror(.true.)

   end function new_VerticalGridAspect_mirror

   logical function supports_conversion_general(src)
      class(VerticalGridAspect), intent(in) :: src
      supports_conversion_general = .true.
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
         if (src%is_mirror()) then
            matches = .false. ! need geom extension
            return
         else
            if (any([src%vertical_stagger,dst%vertical_stagger] == VERTICAL_STAGGER_NONE)) then
               ! both must be 2D
               matches = src%vertical_stagger == dst%vertical_stagger
               return
            end if
            ! Both must have vertical grids to get here, so can compare ids.
            matches = dst%vertical_grid%get_id() == src%vertical_grid%get_id()
            if (matches) return
            ! The following allows Basic to match to grids that have the same number of levels
            matches = src%vertical_grid%matches(dst%vertical_grid)
         end if
      class default
         matches = .false.
      end select

   end function matches

   function find_common_physical_dimension(src, dst, rc) result(physical_dimension)
      character(:), allocatable :: physical_dimension
      class(VerticalGridAspect), intent(in) :: src
      class(VerticalGridAspect), intent(in) :: dst
      integer, optional, intent(out) :: rc

      integer :: status
      type(StringVector) :: vec_in
      type(StringVector) :: vec_out
      integer :: i

      physical_dimension = 'not found'
      vec_in = src%vertical_grid%get_supported_physical_dimensions()
      vec_out = dst%vertical_grid%get_supported_physical_dimensions()

      do i = 1, vec_in%size()
         if (find(vec_out%begin(), vec_out%end(), vec_in%of(i)) /= vec_out%end()) then
            physical_dimension = vec_in%of(i)
            _RETURN(_SUCCESS)
         end if
      end do

      _FAIL('No common physical dimension found between source and destination VerticalGridAspect')
   end function find_common_physical_dimension

   function make_transform(src, dst, other_aspects, rc) result(transform)
      class(ExtensionTransform), allocatable :: transform
      class(VerticalGridAspect), intent(in) :: src
      class(StateItemAspect), intent(in)  :: dst
      type(AspectMap), target, intent(in)  :: other_aspects
      integer, optional, intent(out) :: rc

      class(ComponentDriver), pointer :: v_in_coupler
      class(ComponentDriver), pointer :: v_out_coupler
      type(ESMF_Field) :: v_in_field, v_out_field
      type(VerticalGridAspect) :: dst_
      type(GeomAspect) :: geom_aspect
      type(TypekindAspect) :: typekind_aspect
      character(:), allocatable :: units
      character(:), allocatable :: physical_dimension
      integer :: status

      if (src%is_mirror()) then
         allocate(transform, source=ExtendTransform())
         _RETURN(_SUCCESS)
      end if

      allocate(transform,source=NullTransform()) ! just in case
      dst_ = to_VerticalGridAspect(dst, _RC)

      geom_aspect = to_GeomAspect(other_aspects, _RC)
      typekind_aspect = to_TypekindAspect(other_aspects, _RC)


      physical_dimension = find_common_physical_dimension(src, dst_, _RC)
      units = dst_%vertical_grid%get_units(physical_dimension, _RC)
      
      v_in_field = src%vertical_grid%get_coordinate_field(geom_aspect%get_geom(), physical_dimension, &
           units, typekind_aspect%get_typekind(), coupler=v_in_coupler, _RC)
      v_out_field = dst_%vertical_grid%get_coordinate_field(geom_aspect%get_geom(), physical_dimension, &
           units, typekind_aspect%get_typekind(), coupler=v_out_coupler, _RC)
      deallocate(transform)
      transform = VerticalRegridTransform( &
           v_in_field, v_in_coupler, src%vertical_stagger, &
           v_out_field, v_out_coupler, dst_%vertical_stagger, &
           dst_%regrid_method)

      _RETURN(_SUCCESS)
   end function make_transform

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
      class(VerticalGridAspect), intent(in) :: this
      class(VerticalGrid), allocatable :: vertical_grid
      integer, optional, intent(out) :: rc

      _ASSERT(allocated(this%vertical_grid), "vertical_grid not allocated.")
      vertical_grid = this%vertical_grid

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

end module mapl3g_VerticalGridAspect
