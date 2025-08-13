#include "MAPL.h"
module mapl3g_HConfigAs
   use gftl2_StringVector
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf
   implicit none(type,external)

   private

   public :: HConfigAsTime
   public :: HConfigAsTimeInterval
   public :: HConfigAsStringVector


   interface HConfigAsTime
      procedure :: as_time
   end interface HConfigAsTime

   interface HConfigAsTimeInterval
      procedure :: as_timeinterval
   end interface HConfigAsTimeInterval

   interface HConfigAsStringVector
      procedure :: as_stringvector
   end interface HConfigAsStringVector

contains

   function as_time(hconfig, unusable, keystring, index, rc) result(time)
      type(esmf_Time) :: time
      type(esmf_HConfig), intent(in) :: hconfig
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), intent(in), optional :: keystring
      integer, optional, intent(in) :: index
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: str

      str = esmf_HConfigAsString(hconfig, keystring=keystring, index=index, _RC)
      call esmf_TimeSet(time, str, _RC)

      _RETURN(_SUCCESS)
   end function as_time

   function as_timeinterval(hconfig, unusable, keystring, index, rc) result(interval)
      type(esmf_TimeInterval) :: interval
      type(esmf_HConfig), intent(in) :: hconfig
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), intent(in), optional :: keystring
      integer, optional, intent(in) :: index
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: str

      str = esmf_HConfigAsString(hconfig, keystring=keystring, index=index, _RC)
      call esmf_TimeIntervalSet(interval, str, _RC)

      _RETURN(_SUCCESS)
   end function as_timeinterval

   function as_stringvector(hconfig, unusable, keystring, index, rc) result(vector)
      type(StringVector) :: vector
      type(esmf_HConfig), intent(in) :: hconfig
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), intent(in), optional :: keystring
      integer, optional, intent(in) :: index
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: str
      integer :: i, n
      type(esmf_HConfig) :: subconfig

      n = esmf_HConfigGetSize(hconfig, keystring=keystring, index=index, _RC)
      subconfig = esmf_HConfigCreateAt(hconfig, keystring=keystring, index=index, _RC)

      do i = 1, n
         str = esmf_HConfigAsString(subconfig, index=i, _RC)
         call vector%push_back(str)
      end do

      call esmf_HConfigDestroy(subconfig, _RC)

      _RETURN(_SUCCESS)
   end function as_stringvector

end module mapl3g_HConfigAs
