#include "MAPL.h"
module mapl3g_VerticalGrid
   use esmf, only: esmf_Field, esmf_Geom, esmf_TypeKind_Flag, ESMF_TYPEKIND_R4
   use mapl3g_VerticalStaggerLoc, only: VerticalStaggerLoc
   use gftl2_StringVector, only: StringVector
   use mapl_ErrorHandling
   implicit none(type,external)
   private
   
   public :: VerticalGrid
   
   type, abstract :: VerticalGrid
      private
      integer :: id = -1
   contains
      procedure :: get_id
      procedure :: set_id
      procedure(I_get_coordinate_field), deferred :: get_coordinate_field
      procedure(I_get_supported_physical_dimensions), deferred :: get_supported_physical_dimensions
      procedure(I_get_units), deferred :: get_units
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

      integer function I_get_num_levels(this) result(num_levels)
         import VerticalGrid
         implicit none
         class(VerticalGrid), intent(in) :: this
      end function I_get_num_levels

      function I_matches(this, other) result(matches)
         import VerticalGrid
         implicit none
         logical :: matches
         class(VerticalGrid), intent(in) :: this
         class(VerticalGrid), intent(in) :: other
      end function I_matches

   end interface

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
end module mapl3g_VerticalGrid

