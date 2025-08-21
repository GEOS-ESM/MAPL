#include "MAPL.h"
module mapl3g_HConfigAs
   use gftl2_StringVector
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf
   implicit none(type,external)

   private

   public :: HConfigAsItemType
   public :: HConfigAsStateIntent
   public :: HConfigAsTime
   public :: HConfigAsTimeInterval
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
      call esmf_TimeSet(time, str, _RC)

      _RETURN(_SUCCESS)
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
      call esmf_TimeSet(time, str, _RC)

      _RETURN(_SUCCESS)
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
   end function iter_as_timeinterval

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
   end function iter_as_stringvector

end module mapl3g_HConfigAs
