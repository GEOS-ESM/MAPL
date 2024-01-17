! Retrieve unit converter using udunits2, and use it to convert values.
! x and y are scalar or array variables of type(c_double) or type(c_float).

! The sequence is:
!     call InitializeFieldUnits(path, encoding, rc)
!     ...
!     call GetFieldUnitsConverter(from1, to1, conv1, rc)
!     call GetFieldUnitsConverter(from2, to2, conv2, rc)
!     ...
!     y1 = conv1 % convert(x1)
!     ...
!     y2 = conv2 % convert(x2)
!     ...
!     call conv1 % free()
!     ...
!     call conv2 % free()
!     ...
!     call FinalizeFieldUnits()

! InitializeFieldUnits must be called first, and FinalizeFieldUnits must be called last.
! InitializeFieldUnits and FinalizeFieldUnits are called once, before and after,
! respectively, all GetFieldUnitsConverter and conv % convert calls.

! For a given FieldUnitsConverter, GetFieldUnitsConverter and conv % convert
! cannot be called before InitializeFieldUnits or after FinalizeFieldUnits
! and conv % convert cannot be called before calling GetFieldUnitsConverter for conv.

#include "MAPL_Generic.h"
module mapl_FieldUnits

   use udunits2mod, FieldUnitsConverter => Converter, &
      initialize_udunits => initialize, finalize_udunits => finalize
   use udunits2encoding
   use ESMF
   use MAPL_ExceptionHandling

   implicit none

   public :: FieldUnitsConverter
   public :: GetFieldUnitsConverter
   public :: InitializeFieldUnits
   public :: FinalizeFieldUnits

   private 

contains
    
   ! Possible values for encoding are found in udunits2encoding.
   ! The default, UT_ENCODING_DEFAULT is used if encoding is not provided.
   ! If no path is given, the default path to the units database is used.
   subroutine InitializeFieldUnits(path, encoding, rc)
      character(len=*), optional, intent(in) :: path
      integer(ut_encoding), optional, intent(in) :: encoding
      integer, optional, intent(out) :: rc
      integer :: status

      call initialize_udunits(path, encoding, _RC)
      _RETURN(_SUCCESS)
      
   end subroutine InitializeFieldUnits

   ! from_identifier and to_identifier are strings for unit names or symbols
   ! in the udunits2 database.
   subroutine GetFieldUnitsConverter(from_identifier, to_identifier, conv, rc)
      character(len=*), intent(in) :: from_identifier, to_identifier
      type(FieldUnitsConverter), intent(out) :: conv
      integer, optional, intent(out) :: rc
      integer :: status

      call get_converter(conv, from_identifier, to_identifier, _RC)

   end subroutine GetFieldUnitsConverter

   subroutine FinalizeFieldUnits()

      call finalize_udunits()

   end subroutine FinalizeFieldUnits

 end module FieldUnits
