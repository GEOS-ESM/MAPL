#include "MAPL_Generic.h"

submodule (mapl3g_FieldClassAspect) FieldClassAspect_smod
   use mapl3g_WildcardClassAspect
   implicit none(type,external)

contains

   module logical function matches(src, dst)
      class(FieldClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      matches = .false.
      select type(dst)
      class is (FieldClassAspect)
         matches = .true.
      class is (WildcardClassAspect)
         matches = .true.
      end select

   end function matches

end submodule FieldClassAspect_smod
   
