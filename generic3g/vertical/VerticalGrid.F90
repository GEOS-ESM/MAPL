#include "MAPL_Generic.h"

module mapl3g_VerticalGrid
   use mapl_ErrorHandling
   implicit none (type, external)
   private

   public :: VerticalGrid

   type, abstract :: VerticalGrid
      private
      integer :: id = -1
   contains
      procedure(I_get_num_levels), deferred :: get_num_levels
      procedure(I_get_coordinate_field), deferred :: get_coordinate_field
      procedure(I_can_connect_to), deferred :: can_connect_to

      procedure :: set_id
      procedure :: get_id
      procedure :: same_id
      procedure :: make_info
   end type VerticalGrid

   integer :: global_id = 0

   abstract interface
      integer function I_get_num_levels(this) result(num_levels)
         import VerticalGrid
         class(VerticalGrid), intent(in) :: this
      end function I_get_num_levels

      subroutine I_get_coordinate_field(this, field, coupler, standard_name, geom, typekind, units, rc)
         use mapl3g_GriddedComponentDriver
         use esmf, only: ESMF_Geom, ESMF_TypeKind_Flag, ESMF_Field
         import VerticalGrid

         class(VerticalGrid), intent(in) :: this
         type(ESMF_Field), intent(out) :: field
         type(GriddedComponentDriver), pointer, intent(out) :: coupler
         character(*), intent(in) :: standard_name
         type(ESMF_Geom), intent(in) :: geom
         type(ESMF_TypeKind_Flag), intent(in) :: typekind
         character(*), intent(in) :: units
         integer, optional, intent(out) :: rc
      end subroutine I_get_coordinate_field

      logical function I_can_connect_to(this, src, rc) result(can_connect_to)
         import VerticalGrid
         class(VerticalGrid), intent(in) :: this
         class(VerticalGrid), intent(in) :: src
         integer, optional, intent(out) :: rc
      end function I_can_connect_to

   end interface

contains

   subroutine set_id(this)
      class(VerticalGrid), intent(inout) :: this
      global_id = global_id + 1
      this%id = global_id
   end subroutine set_id

   function get_id(this) result(id)
      class(VerticalGrid), intent(in) :: this
      integer :: id
      id = this%id
   end function get_id

   logical function same_id(this, other)
      class(VerticalGrid), intent(in) :: this
      class(VerticalGrid), intent(in) :: other
      same_id = (this%id == other%id)
   end function same_id

   function make_info(this, rc) result(info)
      use esmf
      type(ESMF_Info) :: info
      class(VerticalGrid), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      info =ESMF_InfoCreate(_RC)
      call ESMF_InfoSet(info, "num_levels", this%get_num_levels(), _RC)

      _RETURN(_SUCCESS)
   end function make_info

end module mapl3g_VerticalGrid
