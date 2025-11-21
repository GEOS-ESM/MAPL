#include "MAPL.h"

module mapl3g_ESMF_HConfigUtilities
   use esmf
   use mapl_ErrorHandling
   implicit none
   private

   public :: write(formatted)
   public :: MAPL_HConfigMatch

   character(*), parameter :: CORE_SCHEMA_INT_TAG = 'tag:yaml.org,2002:int'
   character(*), parameter :: CORE_SCHEMA_FLOAT_TAG = 'tag:yaml.org,2002:float'
   character(*), parameter :: CORE_SCHEMA_STR_TAG = 'tag:yaml.org,2002:str'
   character(*), parameter :: CORE_SCHEMA_BOOL_TAG = 'tag:yaml.org,2002:bool'

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
