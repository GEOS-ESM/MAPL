#include "MAPL.h"

submodule (mapl3g_FieldClassAspect) FieldClassAspect_smod
   use mapl3g_WildcardClassAspect
   implicit none(type,external)

contains

  module function matches_a(src, dst) result(matches)
     logical :: matches
      class(FieldClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      matches = .false.
      select type(dst)
      class is (FieldClassAspect)
         matches = .true.
      class is (WildcardClassAspect)
         matches = .true.
      end select
   end function matches_a

end submodule FieldClassAspect_smod
   
