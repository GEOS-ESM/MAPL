#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) get_child_name_smod

   use mapl3g_GriddedComponentDriverMap
   use mapl_ErrorHandling

   implicit none

contains

   module function get_child_name(this, index, rc) result(name)
      class(OuterMetaComponent), target, intent(in) :: this
      integer, intent(in) :: index
      integer, optional, intent(out) :: rc
      character(len=:), allocatable :: name

      type(GriddedComponentDriverMapIterator) :: iter
      integer :: i

      _ASSERT(index > 0, "index should be >= 1")
      _ASSERT(index <= this%get_num_children(), "index should be <= num_children")

      iter = this%children%ftn_begin()
      call advance(iter, index)
      name = iter%first()

      _RETURN(_SUCCESS)
   end function get_child_name

end submodule get_child_name_smod
