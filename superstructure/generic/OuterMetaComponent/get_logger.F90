#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) get_logger_smod
   implicit none(type,external)

contains

   module function get_logger(this) result(lgr)
      class(Logger), pointer :: lgr
      class(OuterMetaComponent), target, intent(in) :: this

      lgr => this%lgr

   end function get_logger

end submodule get_logger_smod
