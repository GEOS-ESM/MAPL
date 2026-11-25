#include "MAPL.h"

! Table-driven CRC-32 (IEEE 802.3, as used by zlib and gzip) over a byte
! buffer or a file. Detects accidental edits; it is not a cryptographic hash.
module mapl_CRC32_mod
   use mapl_ErrorHandling_mod
   use, intrinsic :: iso_fortran_env, only: INT32, INT64
   implicit none(type,external)
   private

   public :: CRC32
   public :: CRC32File
   public :: CRC32Hex

   ! Reflected form of the polynomial 0x04C11DB7, i.e. 0xEDB88320, written as
   ! a signed 32-bit value.
   integer(INT32), parameter :: POLYNOMIAL = -306674912_INT32
   integer, parameter :: CHUNK_SIZE = 4096

contains

   pure function make_table() result(table)
      integer(INT32) :: table(0:255)
      integer :: i, k
      integer(INT32) :: c

      do i = 0, 255
         c = int(i, INT32)
         do k = 1, 8
            if (btest(c, 0)) then
               c = ieor(ishft(c, -1), POLYNOMIAL)
            else
               c = ishft(c, -1)
            end if
         end do
         table(i) = c
      end do
   end function make_table

   ! Fold the bytes of buffer into a running (pre-inverted) CRC state.
   pure function update(table, state, buffer) result(new_state)
      integer(INT32), intent(in) :: table(0:255)
      integer(INT32), intent(in) :: state
      character(len=*), intent(in) :: buffer
      integer(INT32) :: new_state
      integer :: i, idx

      new_state = state
      do i = 1, len(buffer)
         idx = iand(ieor(new_state, int(ichar(buffer(i:i)), INT32)), 255_INT32)
         new_state = ieor(ishft(new_state, -8), table(idx))
      end do
   end function update

   ! CRC-32 of the bytes of a character buffer. The result is the 32-bit
   ! pattern as a signed integer; use CRC32Hex to format it.
   pure function CRC32(buffer) result(crc)
      character(len=*), intent(in) :: buffer
      integer(INT32) :: crc

      crc = not(update(make_table(), not(0_INT32), buffer))
   end function CRC32

   ! CRC-32 of the bytes of a file, read as a stream. rc /= SUCCESS if the
   ! file cannot be opened or read.
   function CRC32File(filename, rc) result(crc)
      character(len=*), intent(in) :: filename
      integer, optional, intent(out) :: rc
      integer(INT32) :: crc

      integer :: unit, iostat
      integer(INT64) :: remaining, nbytes
      character(len=CHUNK_SIZE) :: buffer
      integer(INT32) :: table(0:255), state

      crc = 0_INT32
      table = make_table()
      state = not(0_INT32)

      open(newunit=unit, file=filename, access='stream', form='unformatted', &
           action='read', status='old', iostat=iostat)
      _ASSERT(iostat == 0, 'Unable to open file for CRC-32: ' // trim(filename))

      inquire(unit=unit, size=remaining)
      if (remaining < 0) then
         close(unit)
         _FAIL('Unable to determine size of file for CRC-32: ' // trim(filename))
      end if

      do while (remaining > 0)
         nbytes = min(remaining, int(CHUNK_SIZE, INT64))
         read(unit, iostat=iostat) buffer(1:nbytes)
         if (iostat /= 0) then
            close(unit)
            _FAIL('Error reading file for CRC-32: ' // trim(filename))
         end if
         state = update(table, state, buffer(1:nbytes))
         remaining = remaining - nbytes
      end do
      close(unit)

      crc = not(state)
      _RETURN(_SUCCESS)
   end function CRC32File

   ! The 32-bit pattern of crc as 8 upper-case hexadecimal digits.
   pure function CRC32Hex(crc) result(hex)
      integer(INT32), intent(in) :: crc
      character(len=8) :: hex

      character(len=*), parameter :: DIGITS = '0123456789ABCDEF'
      integer :: i, nibble

      do i = 1, 8
         nibble = int(iand(ishft(crc, -4*(8-i)), 15_INT32)) + 1
         hex(i:i) = DIGITS(nibble:nibble)
      end do
   end function CRC32Hex

end module mapl_CRC32_mod
