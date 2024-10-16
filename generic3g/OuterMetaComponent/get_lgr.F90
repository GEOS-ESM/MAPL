#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) get_lgr_smod
   implicit none (type, external)

contains

   module function get_lgr(this) result(lgr)
      class(Logger), pointer :: lgr
      class(OuterMetaComponent), target, intent(in) :: this

      lgr => this%lgr

   end function get_lgr

end submodule get_lgr_smod
