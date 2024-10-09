#include "MAPL_ErrLog.h"
submodule (mapl3g_ModelVerticalGrid) can_connect_to_smod
   use mapl3g_BasicVerticalGrid
   use mapl3g_MirrorVerticalGrid

contains

    logical module function can_connect_to(this, src, rc)
       use mapl3g_MirrorVerticalGrid, only: MirrorVerticalGrid
       use mapl3g_BasicVerticalGrid, only: BasicVerticalGrid
       class(ModelVerticalGrid), intent(in) :: this
       class(VerticalGrid), intent(in) :: src
       integer, optional, intent(out) :: rc
       
       integer :: status
       
       if (this%same_id(src)) then
          can_connect_to = .true.
          _RETURN(_SUCCESS)
       end if
       
       select type (src)
       type is (MirrorVerticalGrid)
          can_connect_to = .true.
          _RETURN(_SUCCESS)
       type is (BasicVerticalGrid)
          can_connect_to = (this%get_num_levels() == src%get_num_levels())
          _RETURN(_SUCCESS)
       class default
          _FAIL('unsupported subclass of VerticalGrid')
       end select

        _RETURN(_SUCCESS)
    end function can_connect_to

end submodule
