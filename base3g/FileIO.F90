#include "MAPL.h"

module mapl_FileIO_mod
   use MAPL_CommsMod, only: MAPL_AM_I_ROOT
   use, intrinsic :: iso_fortran_env, only: INT32, REAL32, REAL64, OUTPUT_UNIT
   implicit none
   private

   public :: WRITE_PARALLEL

   interface WRITE_PARALLEL
      module procedure write_parallel_string
      module procedure write_parallel_i32_scalar
      module procedure write_parallel_i32_1d
      module procedure write_parallel_r32_scalar
      module procedure write_parallel_r32_1d
      module procedure write_parallel_r64_scalar
      module procedure write_parallel_r64_1d
   end interface WRITE_PARALLEL

contains

   subroutine write_parallel_string(data, unit, format, rc)
      character(len=*),    intent(in   )           :: data
      integer,             intent(in   ), optional :: unit
      character(len=*),    intent(in   ), optional :: format
      integer,             intent(  out), optional :: rc

      integer :: unit_

      unit_ = OUTPUT_UNIT
      if (present(unit)) unit_ = unit

      if (MAPL_AM_I_ROOT()) then
         if (present(format)) then
            write(unit_, format) data
         else
            write(unit_, *) data
         end if
      end if

      if (present(rc)) rc = 0

   end subroutine write_parallel_string

   subroutine write_parallel_i32_scalar(data, unit, format, rc)
      integer(INT32),      intent(in   )           :: data
      integer,             intent(in   ), optional :: unit
      character(len=*),    intent(in   ), optional :: format
      integer,             intent(  out), optional :: rc

      integer :: unit_

      unit_ = OUTPUT_UNIT
      if (present(unit)) unit_ = unit

      if (MAPL_AM_I_ROOT()) then
         if (present(format)) then
            write(unit_, format) data
         else
            write(unit_, *) data
         end if
      end if

      if (present(rc)) rc = 0

   end subroutine write_parallel_i32_scalar

   subroutine write_parallel_i32_1d(data, unit, format, rc)
      integer(INT32),      intent(in   )           :: data(:)
      integer,             intent(in   ), optional :: unit
      character(len=*),    intent(in   ), optional :: format
      integer,             intent(  out), optional :: rc

      integer :: unit_

      unit_ = OUTPUT_UNIT
      if (present(unit)) unit_ = unit

      if (MAPL_AM_I_ROOT()) then
         if (present(format)) then
            write(unit_, format) data
         else
            write(unit_, *) data
         end if
      end if

      if (present(rc)) rc = 0

   end subroutine write_parallel_i32_1d

   subroutine write_parallel_r32_scalar(data, unit, format, rc)
      real(REAL32),        intent(in   )           :: data
      integer,             intent(in   ), optional :: unit
      character(len=*),    intent(in   ), optional :: format
      integer,             intent(  out), optional :: rc

      integer :: unit_

      unit_ = OUTPUT_UNIT
      if (present(unit)) unit_ = unit

      if (MAPL_AM_I_ROOT()) then
         if (present(format)) then
            write(unit_, format) data
         else
            write(unit_, *) data
         end if
      end if

      if (present(rc)) rc = 0

   end subroutine write_parallel_r32_scalar

   subroutine write_parallel_r32_1d(data, unit, format, rc)
      real(REAL32),        intent(in   )           :: data(:)
      integer,             intent(in   ), optional :: unit
      character(len=*),    intent(in   ), optional :: format
      integer,             intent(  out), optional :: rc

      integer :: unit_

      unit_ = OUTPUT_UNIT
      if (present(unit)) unit_ = unit

      if (MAPL_AM_I_ROOT()) then
         if (present(format)) then
            write(unit_, format) data
         else
            write(unit_, *) data
         end if
      end if

      if (present(rc)) rc = 0

   end subroutine write_parallel_r32_1d

   subroutine write_parallel_r64_scalar(data, unit, format, rc)
      real(REAL64),        intent(in   )           :: data
      integer,             intent(in   ), optional :: unit
      character(len=*),    intent(in   ), optional :: format
      integer,             intent(  out), optional :: rc

      integer :: unit_

      unit_ = OUTPUT_UNIT
      if (present(unit)) unit_ = unit

      if (MAPL_AM_I_ROOT()) then
         if (present(format)) then
            write(unit_, format) data
         else
            write(unit_, *) data
         end if
      end if

      if (present(rc)) rc = 0

   end subroutine write_parallel_r64_scalar

   subroutine write_parallel_r64_1d(data, unit, format, rc)
      real(REAL64),        intent(in   )           :: data(:)
      integer,             intent(in   ), optional :: unit
      character(len=*),    intent(in   ), optional :: format
      integer,             intent(  out), optional :: rc

      integer :: unit_

      unit_ = OUTPUT_UNIT
      if (present(unit)) unit_ = unit

      if (MAPL_AM_I_ROOT()) then
         if (present(format)) then
            write(unit_, format) data
         else
            write(unit_, *) data
         end if
      end if

      if (present(rc)) rc = 0

   end subroutine write_parallel_r64_1d

end module mapl_FileIO_mod
