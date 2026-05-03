#include "MAPL.h"

module mapl3g_FileIO
   use MAPL_CommsMod, only: MAPL_AM_I_ROOT
   implicit none
   private

   public :: WRITE_PARALLEL

   interface WRITE_PARALLEL
      module procedure write_parallel_string
   end interface WRITE_PARALLEL

contains

   subroutine write_parallel_string(data, unit, format, rc)
      character(len=*),    intent(in   )           :: data
      integer,             intent(in   ), optional :: unit
      character(len=*),    intent(in   ), optional :: format
      integer,             intent(  out), optional :: rc

      integer :: unit_

      unit_ = merge(unit, 6, present(unit))

      if (MAPL_AM_I_ROOT()) then
         if (present(format)) then
            write(unit_, format) data
         else
            write(unit_, *) data
         end if
      end if

      if (present(rc)) rc = 0

   end subroutine write_parallel_string

end module mapl3g_FileIO
