#include "MAPL_Generic.h"

submodule (mapl3g_GeomManager) get_geom_manager_smod

contains
   
   module function get_geom_manager() result(geom_mgr)
      type(GeomManager), pointer :: geom_mgr
      logical :: init = .false.

      if (.not. init) then
         call geom_manager%initialize()
         init = .true.
      end if

      geom_mgr => geom_manager
   end function get_geom_manager

end submodule get_geom_manager_smod
