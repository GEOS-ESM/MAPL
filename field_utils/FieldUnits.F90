#include "MAPL_Generic.h"
module FieldUnits

   use udunits2mod, only: FieldUnitsConverter => MAPL_UDUNITS_Converter
   use udunits2mod, only: GetUnitsConverter => Get_MAPL_UDUNITS_Converter
   use udunits2mod, only: ShutdownFieldUnits => shutdown_system_instance
   use ESMF
   use MAPL_ExceptionHandling

   implicit none

   public :: GetFieldUnitsConverter
   !private

contains
    
   subroutine GetFieldUnitsConverter(e1, e2, conv, path, rc)
      type(ESMF_Field), intent(inout) :: e1, e2
      type(FieldUnitsConverter), intent(out) :: conv
      character(len=*), optional, intent(in) :: path
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=*) :: from
      character(len=*) :: to

      call GetUnitsConverter(conv, from, to, path, rc=status)

   end subroutine GetFieldUnitsConverter

 end module FieldUnits
