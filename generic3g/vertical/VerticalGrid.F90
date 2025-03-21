#include "MAPL_Generic.h"

module mapl3g_VerticalGrid
   use mapl_ErrorHandling
   implicit none
   private

   public :: VerticalGrid

   type, abstract :: VerticalGrid
      private
      integer :: id = -1
      character(:), allocatable :: units
   contains
      procedure(I_get_num_levels), deferred :: get_num_levels
      procedure(I_get_coordinate_field), deferred :: get_coordinate_field
      procedure(I_can_connect_to), deferred :: can_connect_to
      procedure(I_is_identical_to), deferred :: is_identical_to
      procedure(I_write_formatted), deferred :: write_formatted
      generic :: write(formatted) => write_formatted

      procedure :: set_id
      procedure :: get_id
      procedure :: same_id
      procedure :: set_units
      procedure :: get_units
      procedure :: make_info
   end type VerticalGrid

   integer :: global_id = 0

   abstract interface

      integer function I_get_num_levels(this) result(num_levels)
         import VerticalGrid
         class(VerticalGrid), intent(in) :: this
      end function I_get_num_levels

      subroutine I_get_coordinate_field(this, field, coupler, standard_name, geom, typekind, units, vertical_stagger, rc)
         use mapl3g_ComponentDriver
         use mapl3g_VerticalStaggerLoc
         use esmf, only: ESMF_Geom, ESMF_TypeKind_Flag, ESMF_Field
         import VerticalGrid

         class(VerticalGrid), intent(in) :: this
         type(ESMF_Field), intent(out) :: field
         class(ComponentDriver), pointer, intent(out) :: coupler
         character(*), intent(in) :: standard_name
         type(ESMF_Geom), intent(in) :: geom
         type(ESMF_TypeKind_Flag), intent(in) :: typekind
         character(*), intent(in) :: units
         type(VerticalStaggerLoc), intent(in) :: vertical_stagger
         integer, optional, intent(out) :: rc
      end subroutine I_get_coordinate_field

      logical function I_can_connect_to(this, dst, rc) result(can_connect_to)
         import VerticalGrid
         class(VerticalGrid), intent(in) :: this
         class(VerticalGrid), intent(in) :: dst
         integer, optional, intent(out) :: rc
      end function I_can_connect_to

      logical function I_is_identical_to(this, that, rc) result(is_identical_to)
         import VerticalGrid
         class(VerticalGrid), intent(in) :: this
         class(VerticalGrid), allocatable, intent(in) :: that
         integer, optional, intent(out) :: rc
      end function I_is_identical_to

      subroutine I_write_formatted(this, unit, iotype, v_list, iostat, iomsg)
         import VerticalGrid
         class(VerticalGrid), intent(in) :: this
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in) :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine I_write_formatted

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

   subroutine set_units(this, units)
      class(VerticalGrid), intent(inout) :: this
      character(*), intent(in) :: units
      this%units = units
   end subroutine set_units

   function get_units(this) result(units)
      character(:), allocatable :: units
      class(VerticalGrid), intent(in) :: this
      units = this%units
   end function get_units

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
