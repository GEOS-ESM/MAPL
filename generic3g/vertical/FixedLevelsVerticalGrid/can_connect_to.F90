#include "MAPL_ErrLog.h"
submodule (mapl3g_FixedLevelsVerticalGrid) can_connect_to_smod
   use mapl3g_MirrorVerticalGrid
   use mapl3g_ModelVerticalGrid
   use mapl3g_BasicVerticalGrid

contains

   logical module function can_connect_to(this, src, rc)
      class(FixedLevelsVerticalGrid), intent(in) :: this
      class(VerticalGrid), intent(in) :: src
      integer, optional, intent(out) :: rc

      select type(src)
      type is (FixedLevelsVeritcalGrid)
         can_connect_to = this == src
      type is (BasicVerticalGrid)
         can_connect_to = (this%get_num_levels() == src%get_num_levels())
      type is (MirrorVerticalGrid)
         can_connect_to = .true.
      type is (ModelVerticalGrid)
         can_connect_to = (this%get_num_levels() == src%get_num_levels())
      class default
         _FAIL('BasicVerticalGrid can only connect to src BasicVerticalGrid, MirrorVerticalGrid, or ModelVerticalGrid instances.')
      end select

      _RETURN(_SUCCESS)
   end function can_connect_to

end submodule
