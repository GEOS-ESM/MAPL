#include "MAPL_Generic.h"

module mapl3g_ESMF_HConfigUtilities
   use esmf
   use mapl_ErrorHandling
   implicit none
   private

   public :: write(formatted)
   public :: MAPL_HConfigMatch

   interface write(formatted)
      procedure write_hconfig
   end interface write(formatted)

   INTERFACE
      module subroutine write_hconfig(hconfig, unit, iotype, v_list, iostat, iomsg)
         type(ESMF_Hconfig), intent(in) :: hconfig
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in) :: v_list(:)
         integer, intent(out) :: iostat 
         character(*), intent(inout) :: iomsg
      end subroutine write_hconfig

      module function MAPL_HConfigMatch(a, b, rc) result(match)
         logical :: match
         type(ESMF_HConfig), intent(in) :: a, b
         integer, optional, intent(out) :: rc
      end function MAPL_HConfigMatch

   END INTERFACE

end module mapl3g_ESMF_HConfigUtilities
