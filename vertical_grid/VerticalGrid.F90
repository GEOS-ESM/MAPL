#include "MAPL.h"
module mapl3g_VerticalGrid
   use esmf, only: esmf_Field, esmf_Geom, esmf_TypeKind_Flag, ESMF_TYPEKIND_R4
   use mapl3g_VerticalStaggerLoc, only: VerticalStaggerLoc
   use mapl3g_VerticalCoordinateDirection
   use gftl2_StringVector, only: StringVector
   use mapl_ErrorHandling
   implicit none(type,external)
   private
   
   public :: VerticalGrid
   public :: VERTICAL_GRID_NOT_FOUND
   
   type, abstract :: VerticalGrid
      private
      integer :: id = -1
      type(VerticalCoordinateDirection) :: coordinate_direction = VCOORD_DIRECTION_DOWN
      character(:), allocatable :: primary_coordinate  ! "pressure" or "height" for conservative regridding
   contains
      procedure :: get_id
      procedure :: set_id
      procedure :: get_coordinate_direction
      procedure :: set_coordinate_direction
      procedure :: get_primary_coordinate
      procedure :: set_primary_coordinate
      procedure(I_get_coordinate_field), deferred :: get_coordinate_field
      procedure(I_get_supported_physical_dimensions), deferred :: get_supported_physical_dimensions
      procedure(I_get_units), deferred :: get_units
      procedure(I_get_num_layers), deferred :: get_num_layers
      procedure(I_get_num_levels), deferred :: get_num_levels
      procedure(I_matches), deferred :: matches
   end type VerticalGrid
   
   abstract interface
      ! Existing interface
      function I_get_coordinate_field(this, geom, physical_dimension, units, typekind, coupler, rc) result(field)
         use mapl3g_ComponentDriver, only: ComponentDriver
         use esmf, only: esmf_Field, esmf_Geom, esmf_TypeKind_Flag
         import VerticalGrid
         implicit none
         type(esmf_Field) :: field
         class(VerticalGrid), intent(in) :: this
         type(esmf_Geom), intent(in) :: geom
         character(len=*), intent(in) :: physical_dimension
         character(len=*), intent(in) :: units
         type(esmf_TypeKind_Flag), intent(in) :: typekind
         class(ComponentDriver), pointer, intent(out) :: coupler
         integer, intent(out), optional :: rc
      end function I_get_coordinate_field
      
      ! New interface for supported physical dimensions
      function I_get_supported_physical_dimensions(this) result(dimensions)
         use gftl2_StringVector, only: StringVector
         import VerticalGrid
         implicit none
         type(StringVector) :: dimensions
         class(VerticalGrid), target, intent(in) :: this
      end function I_get_supported_physical_dimensions
      
      ! New interface for getting units by dimension
      function I_get_units(this, physical_dimension, rc) result(units)
         import VerticalGrid
         implicit none
         character(len=:), allocatable :: units
         class(VerticalGrid), intent(in) :: this
         character(len=*), intent(in) :: physical_dimension
         integer, optional, intent(out) :: rc
      end function I_get_units

      integer function I_get_num_layers(this) result(num_layers)
         import VerticalGrid
         implicit none
         class(VerticalGrid), intent(in) :: this
      end function I_get_num_layers

      function I_matches(this, other) result(matches)
         import VerticalGrid
         implicit none
         logical :: matches
         class(VerticalGrid), intent(in) :: this
         class(VerticalGrid), intent(in) :: other
      end function I_matches

   end interface

   integer, parameter :: VERTICAL_GRID_NOT_FOUND = -1

contains

   function get_id(this) result(id)
      integer :: id
      class(VerticalGrid), intent(in) :: this
      id = this%id
   end function get_id
   
   subroutine set_id(this, id)
      class(VerticalGrid), intent(inout) :: this
      integer, intent(in) :: id
      this%id = id
   end subroutine set_id

   function get_coordinate_direction(this) result(coordinate_direction)
      type(VerticalCoordinateDirection) :: coordinate_direction
      class(VerticalGrid), intent(in) :: this
      
      coordinate_direction = this%coordinate_direction
   end function get_coordinate_direction

   subroutine set_coordinate_direction(this, coordinate_direction)
      class(VerticalGrid), intent(inout) :: this
      type(VerticalCoordinateDirection), intent(in) :: coordinate_direction
      
      this%coordinate_direction = coordinate_direction
   end subroutine set_coordinate_direction

   !> Get the primary vertical coordinate name for conservative regridding
   !!
   !! @return The primary coordinate name (e.g., "pressure" or "height")
   !!         Empty string if not set
   function get_primary_coordinate(this) result(primary_coordinate)
      character(:), allocatable :: primary_coordinate
      class(VerticalGrid), intent(in) :: this
      
      primary_coordinate = ""
      if (allocated(this%primary_coordinate)) then
         primary_coordinate = this%primary_coordinate
      end if
   end function get_primary_coordinate

   !> Set the primary vertical coordinate name for conservative regridding
   !!
   !! @param primary_coordinate The coordinate name (e.g., "pressure" or "height")
   !!
   !! This identifies which coordinate should be used for normalization:
   !! - "pressure" → use DELP for mass-weighted regridding
   !! - "height" → use DZ for height-weighted regridding
   subroutine set_primary_coordinate(this, primary_coordinate)
      class(VerticalGrid), intent(inout) :: this
      character(*), intent(in) :: primary_coordinate
      
      this%primary_coordinate = primary_coordinate
   end subroutine set_primary_coordinate
end module mapl3g_VerticalGrid

