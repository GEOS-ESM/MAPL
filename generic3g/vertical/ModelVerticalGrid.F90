#include "MAPL_Generic.h"

module mapl3g_ModelVerticalGrid
   use mapl3g_VerticalGrid
   use mapl3g_StateRegistry
   use mapl_ErrorHandling
   use gftl2_StringVector
   implicit none
   private

   public :: ModelVerticalGrid

   type, extends(VerticalGrid) :: ModelVerticalGrid
      private
      integer :: num_levels = -1
      type(StringVector) :: variants

!#      character(:), allocatable :: short_name
!#      character(:), allocatable :: standard_name
!#      type(ESMF_Field) :: reference_field
      type(StateRegistry), pointer :: registry => null()
   contains
      procedure :: get_num_levels

      ! subclass-specific methods
      procedure :: add_variant
      procedure :: get_num_variants
      procedure :: set_registry
      procedure :: get_registry
   end type ModelVerticalGrid

   interface ModelVerticalGrid
      procedure new_ModelVerticalGrid_basic
   end interface ModelVerticalGrid


   ! TODO:
   ! - Ensure that there really is a vertical dimension

contains

   function new_ModelVerticalGrid_basic(num_levels) result(vgrid)
      type(ModelVerticalGrid) :: vgrid
      integer, intent(in) :: num_levels
!#      character(*), intent(in) :: short_name
!#      character(*), intent(in) :: standard_name
!#      type(StateRegistry), pointer, intent(in) :: registry

      vgrid%num_levels = num_levels
!#      vgrid%short_name = short_name
!#      vgrid%standard_name = standard_name
!#      vgrid%registry => registry

   end function new_ModelVerticalGrid_basic


   integer function get_num_levels(this) result(num_levels)
      class(ModelVerticalGrid), intent(in) :: this
      num_levels = this%num_levels
   end function get_num_levels

   subroutine add_variant(this, short_name)
      class(ModelVerticalGrid), intent(inout) :: this
      character(*), intent(in) :: short_name

      call this%variants%push_back(short_name)
   end subroutine add_variant

   integer function get_num_variants(this) result(num_variants)
      class(ModelVerticalGrid), intent(in) :: this
      num_variants = this%variants%size()
   end function get_num_variants

    subroutine set_registry(this, registry)
       class(ModelVerticalGrid), intent(inout) :: this
       type(StateRegistry), target, intent(in) :: registry
  
       this%registry => registry
    end subroutine set_registry

    function get_registry(this) result(registry)
       class(ModelVerticalGrid), intent(in) :: this
       type(StateRegistry), pointer :: registry
       registry => this%registry
    end function get_registry

end module mapl3g_ModelVerticalGrid
