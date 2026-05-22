#include "MAPL.h"
module mapl_HConfigAs_mod
   use gftl2_StringVector
   use mapl_KeywordEnforcer_mod
   use mapl_ErrorHandling_mod
   use esmf
   implicit none(type,external)

   private

   public :: HConfigAsItemType
   public :: HConfigAsStateIntent
   public :: HConfigAsTime
   public :: HConfigAsTimeInterval
   public :: HConfigAsTimeRange
   public :: HConfigAsStringVector


   interface HConfigAsItemType
      procedure :: as_itemtype
      procedure :: iter_as_itemtype
   end interface HConfigAsItemType

   interface HConfigAsStateIntent
      procedure :: as_stateintent
      procedure :: iter_as_stateintent
   end interface HConfigAsStateIntent

   interface HConfigAsTime
      procedure :: as_time
      procedure :: iter_as_time
   end interface HConfigAsTime

   interface HConfigAsTimeInterval
      procedure :: as_timeinterval
      procedure :: iter_as_timeinterval
   end interface HConfigAsTimeInterval

   interface HConfigAsTimeRange
      procedure :: as_timerange
      procedure :: iter_as_timerange
   end interface HConfigAsTimeRange

   interface HConfigAsStringVector
      procedure :: as_stringvector
      procedure :: iter_as_stringvector
   end interface HConfigAsStringVector

contains

   function as_stateintent(hconfig, unusable, keystring, index, rc) result(intent)
      type(esmf_StateIntent_flag) :: intent
      type(esmf_HConfig), intent(in) :: hconfig
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), intent(in), optional :: keystring
      integer, optional, intent(in) :: index
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: str

      str = esmf_HConfigAsString(hconfig, keystring=keystring, index=index, _RC)

      select case (esmf_UtilStringUpperCase(str))
      case ('ESMF_STATE_IMPORT')
         intent = ESMF_STATEINTENT_IMPORT
      case ('ESMF_STATE_EXPORT')
         intent = ESMF_STATEINTENT_EXPORT
      case ('ESMF_STATE_INTERNAL')
         intent = ESMF_STATEINTENT_INTERNAL
      case default
         intent = ESMF_STATEINTENT_UNSPECIFIED
         _FAIL('Unknown state intent: '//str)
      end select
      
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
    end function as_stateintent

   function iter_as_stateintent(hconfig_iter, unusable, keystring, index, rc) result(intent)
      type(esmf_StateIntent_flag) :: intent
      type(esmf_HConfigIter), intent(in) :: hconfig_iter
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), intent(in), optional :: keystring
      integer, optional, intent(in) :: index
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: str

      str = esmf_HConfigAsString(hconfig_iter, keystring=keystring, index=index, _RC)

      select case (esmf_UtilStringUpperCase(str))
      case ('ESMF_STATE_IMPORT')
         intent = ESMF_STATEINTENT_IMPORT
      case ('ESMF_STATE_EXPORT')
         intent = ESMF_STATEINTENT_EXPORT
      case ('ESMF_STATE_INTERNAL')
         intent = ESMF_STATEINTENT_INTERNAL
      case default
         intent = ESMF_STATEINTENT_UNSPECIFIED
         _FAIL('Unknown state intent: '//str)
      end select
      
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function iter_as_stateintent

   function as_itemtype(hconfig, unusable, keystring, index, rc) result(itemtype)
      type(esmf_StateItem_Flag) :: itemtype
      type(esmf_HConfig), intent(in) :: hconfig
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), intent(in), optional :: keystring
      integer, optional, intent(in) :: index
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: str

      str = esmf_HConfigAsString(hconfig, keystring=keystring, index=index, _RC)

      select case (ESMF_UtilStringUpperCase(str))
      case ('ESMF_STATEITEM_FIELD', 'FIELD')
         itemtype = ESMF_STATEITEM_FIELD
      case ('ESMF_STATEITEM_FIELDBUNDLE', 'FIELDBUNDLE', 'BUNDLE')
         itemtype = ESMF_STATEITEM_FIELDBUNDLE
      case ('ESMF_STATEITEM_STATE', 'STATE')
         itemtype = ESMF_STATEITEM_STATE
      case default
         itemtype = ESMF_STATEITEM_UNKNOWN
         _FAIL('Unknown item type: '//trim(str))
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function as_itemtype

function iter_as_itemtype(hconfig_iter, unusable, keystring, index, rc) result(itemtype)
      type(esmf_StateItem_Flag) :: itemtype
      type(esmf_HConfigIter), intent(in) :: hconfig_iter
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), intent(in), optional :: keystring
      integer, optional, intent(in) :: index
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: str

      str = esmf_HConfigAsString(hconfig_iter, keystring=keystring, index=index, _RC)

      select case (ESMF_UtilStringUpperCase(str))
      case ('ESMF_STATEITEM_FIELD')
         itemtype = ESMF_STATEITEM_FIELD
      case ('ESMF_STATEITEM_FIELDBUNDLE')
         itemtype = ESMF_STATEITEM_FIELDBUNDLE
      case ('ESMF_STATEITEM_STATE')
         itemtype = ESMF_STATEITEM_STATE
      case default
         itemtype = ESMF_STATEITEM_UNKNOWN
         _FAIL('Unknown item type: '//trim(str))
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function iter_as_itemtype

   
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
      call string_to_esmf_time(str, time, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
    end function as_time

   function iter_as_time(hconfig_iter, unusable, keystring, index, rc) result(time)
      type(esmf_Time) :: time
      type(esmf_HConfigIter), intent(in) :: hconfig_iter
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), intent(in), optional :: keystring
      integer, optional, intent(in) :: index
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: str

      str = esmf_HConfigAsString(hconfig_iter, keystring=keystring, index=index, _RC)
      call string_to_esmf_time(str, time, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function iter_as_time

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
      _UNUSED_DUMMY(unusable)
   end function as_timeinterval

   function iter_as_timeinterval(hconfig_iter, unusable, keystring, index, rc) result(interval)
      type(esmf_TimeInterval) :: interval
      type(esmf_HConfigIter), intent(in) :: hconfig_iter
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), intent(in), optional :: keystring
      integer, optional, intent(in) :: index
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: str

      str = esmf_HConfigAsString(hconfig_iter, keystring=keystring, index=index, _RC)
      call esmf_TimeIntervalSet(interval, str, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function iter_as_timeinterval

   function as_timerange(hconfig, unusable, keystring, index, rc) result(timerange)
      type(esmf_Time), allocatable :: timerange(:)
      type(esmf_HConfig), intent(in) :: hconfig
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), intent(in), optional :: keystring
      integer, optional, intent(in) :: index
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: str
      integer :: idx

       str = esmf_HConfigAsString(hconfig, keystring=keystring, index=index, _RC)
       idx = scan(str, '/')
       _ASSERT(idx /= 0, 'HConfigAsTimeRange: missing "/" separator in: '//str)
      allocate(timerange(2))
      call string_to_esmf_time(str(:idx-1), timerange(1), _RC)
      call string_to_esmf_time(str(idx+1:), timerange(2), _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function as_timerange

   function iter_as_timerange(hconfig_iter, unusable, keystring, index, rc) result(timerange)
      type(esmf_Time), allocatable :: timerange(:)
      type(esmf_HConfigIter), intent(in) :: hconfig_iter
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), intent(in), optional :: keystring
      integer, optional, intent(in) :: index
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: str
      integer :: idx

       str = esmf_HConfigAsString(hconfig_iter, keystring=keystring, index=index, _RC)
       idx = scan(str, '/')
       _ASSERT(idx /= 0, 'HConfigAsTimeRange: missing "/" separator in: '//str)
      allocate(timerange(2))
      call string_to_esmf_time(str(:idx-1), timerange(1), _RC)
      call string_to_esmf_time(str(idx+1:), timerange(2), _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function iter_as_timerange

   ! Private helper: robustly parse a date[T time] string into ESMF_Time.
   ! Handles both "YYYY-MM-DD" and "YYYY-MM-DDThh:mm:ss" formats.
   subroutine string_to_esmf_time(str, time, rc)
      character(len=*), intent(in) :: str
      type(esmf_Time), intent(out) :: time
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: tpos, firstdash, lastdash, firstcolon, lastcolon
      integer :: year, month, day, hour, min, sec
      character(:), allocatable :: date_str, time_str

      tpos = index(str, 'T')
      if (tpos > 0) then
         date_str = str(:tpos-1)
         time_str = str(tpos+1:)
      else
         date_str = trim(str)
         time_str = ''
      end if

      ! Parse date: YYYY-MM-DD
      firstdash = index(date_str, '-')
      lastdash  = index(date_str, '-', back=.true.)
      _ASSERT(firstdash > 0 .and. lastdash > firstdash, 'Invalid date string: '//date_str)
      read(date_str(:firstdash-1), *) year
      read(date_str(firstdash+1:lastdash-1), *) month
      read(date_str(lastdash+1:lastdash+2), *) day

      ! Parse time: hh:mm:ss (optional)
      hour = 0; min = 0; sec = 0
      if (len_trim(time_str) > 0) then
         firstcolon = index(time_str, ':')
         lastcolon  = index(time_str, ':', back=.true.)
         if (firstcolon > 0) then
            read(time_str(:firstcolon-1), *) hour
            if (lastcolon > firstcolon) then
               read(time_str(firstcolon+1:lastcolon-1), *) min
               read(time_str(lastcolon+1:lastcolon+2), *) sec
            else
               read(time_str(firstcolon+1:firstcolon+2), *) min
            end if
         else
            read(time_str, *) hour
         end if
      end if

      call esmf_TimeSet(time, yy=year, mm=month, dd=day, h=hour, m=min, s=sec, _RC)
      _RETURN(_SUCCESS)
   end subroutine string_to_esmf_time

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
      _UNUSED_DUMMY(unusable)
   end function as_stringvector

   function iter_as_stringvector(hconfig_iter, unusable, keystring, index, rc) result(vector)
      type(StringVector) :: vector
      type(esmf_HConfigIter), intent(in) :: hconfig_iter
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), intent(in), optional :: keystring
      integer, optional, intent(in) :: index
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: str
      integer :: i, n
      type(esmf_HConfig) :: subconfig

      n = esmf_HConfigGetSize(hconfig_iter, keystring=keystring, index=index, _RC)
      subconfig = esmf_HConfigCreateAt(hconfig_iter, keystring=keystring, index=index, _RC)

      do i = 1, n
         str = esmf_HConfigAsString(subconfig, index=i, _RC)
         call vector%push_back(str)
      end do

      call esmf_HConfigDestroy(subconfig, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function iter_as_stringvector

end module mapl_HConfigAs_mod
