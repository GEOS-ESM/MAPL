#include "MAPL.h"

! This module is intended to manage user-level access to ESMF info
! objects and thereby ensure consistent support for namespace
! management and such.
!
! Known gap: MAPL_InfoGetValueKind reports an empty array as
! INFO_VALUE_OTHER, not as a list kind, because ESMF gives no
! element-kind information for a zero-length array.  A declared-list
! attribute whose value happens to be empty will wrongly fail a type
! check built on this query.
!
! Known gap: the string-list getter (MAPL_InfoGet/MAPL_InfoGetShared/
! MAPL_InfoGetPrivate with a StringVector values= argument) reads a
! scalar string value successfully (status 0) as a one-element list,
! rather than failing on the shape mismatch.  The numeric list
! getters (i4_1d, r4_1d, r8_1d) do not share this gap: they fail
! cleanly on a scalar value.  A declared-list attribute whose value
! happens to be a scalar string will wrongly pass a type check built
! on the string-list getter.

module mapl_InfoUtilities_mod

   use mapl_ErrorHandling_mod
   use mapl_KeywordEnforcer_mod

   use gftl2_StringVector
   use mapl_esmf_info_keys_mod
   use esmf, only: ESMF_StateItem_Flag
   use esmf, only: ESMF_STATEITEM_FIELD
   use esmf, only: ESMF_STATEITEM_FIELDBundle
   use esmf, only: operator(==), operator(/=)
   use esmf, only: ESMF_Info, ESMF_InfoPrint
   use esmf, only: ESMF_InfoCreate
   use esmf, only: ESMF_InfoIsPresent
   use esmf, only: ESMF_InfoGetFromHost
   use esmf, only: ESMF_InfoGet
   use esmf, only: ESMF_InfoGetAlloc
   use esmf, only: ESMF_InfoGetCharAlloc
   use esmf, only: MAPL_InfoSet => ESMF_InfoSet
   use esmf, only: ESMF_State
   use esmf, only: ESMF_StateGet
   use esmf, only: ESMF_Field
   use esmf, only: ESMF_FieldBundle
   use esmf, only: ESMF_KIND_I4
   use esmf, only: ESMF_KIND_R4
   use esmf, only: ESMF_KIND_R8
   use esmf, only: ESMF_MAXSTR
   use esmf, only: ESMF_TypeKind_Flag
   use esmf, only: ESMF_TYPEKIND_I4, ESMF_TYPEKIND_I8
   use esmf, only: ESMF_TYPEKIND_R4, ESMF_TYPEKIND_R8
   use esmf, only: ESMF_TYPEKIND_LOGICAL, ESMF_TYPEKIND_CHARACTER

   implicit none(type,external)
   private

   public :: MAPL_InfoGet
   public :: MAPL_InfoSet

   public :: MAPL_InfoCreateFromShared

   public :: MAPL_InfoGetShared
   public :: MAPL_InfoSetShared
   public :: MAPL_InfoGetPrivate
   public :: MAPL_InfoSetPrivate
   public :: MAPL_InfoSetNamespace

   public :: MAPL_InfoGetValueKind
   public :: MAPL_InfoGetKeys

   public :: MAPL_InfoHasCallbackInterface
   public :: MAPL_InfoGetCallbackInterface
   public :: MAPL_InfoSetCallbackInterface

   public :: InfoValueKind
   public :: operator(==)
   public :: operator(/=)
   public :: INFO_VALUE_ABSENT
   public :: INFO_VALUE_OTHER
   public :: INFO_VALUE_INTEGER
   public :: INFO_VALUE_REAL
   public :: INFO_VALUE_LOGICAL
   public :: INFO_VALUE_STRING
   public :: INFO_VALUE_INTEGER_LIST
   public :: INFO_VALUE_REAL_LIST
   public :: INFO_VALUE_LOGICAL_LIST
   public :: INFO_VALUE_STRING_LIST

   ! Stored type of a value in an ESMF_Info object, as reported by
   ! MAPL_InfoGetValueKind.  Integer covers i4 and i8; real covers r4
   ! and r8.  OTHER is any stored thing that is not one of these (for
   ! example a nested object or a null).
   type :: InfoValueKind
      private
      integer :: code = -1
   contains
      procedure :: is_list => infovaluekind_is_list
      procedure :: to_string => infovaluekind_to_string
   end type InfoValueKind

   type(InfoValueKind), parameter :: INFO_VALUE_ABSENT = InfoValueKind(0)
   type(InfoValueKind), parameter :: INFO_VALUE_OTHER = InfoValueKind(1)
   type(InfoValueKind), parameter :: INFO_VALUE_INTEGER = InfoValueKind(2)
   type(InfoValueKind), parameter :: INFO_VALUE_REAL = InfoValueKind(3)
   type(InfoValueKind), parameter :: INFO_VALUE_LOGICAL = InfoValueKind(4)
   type(InfoValueKind), parameter :: INFO_VALUE_STRING = InfoValueKind(5)
   type(InfoValueKind), parameter :: INFO_VALUE_INTEGER_LIST = InfoValueKind(6)
   type(InfoValueKind), parameter :: INFO_VALUE_REAL_LIST = InfoValueKind(7)
   type(InfoValueKind), parameter :: INFO_VALUE_LOGICAL_LIST = InfoValueKind(8)
   type(InfoValueKind), parameter :: INFO_VALUE_STRING_LIST = InfoValueKind(9)

   interface operator(==)
      procedure :: infovaluekind_equal
   end interface operator(==)

   interface operator(/=)
      procedure :: infovaluekind_not_equal
   end interface operator(/=)

   ! Stored-type query on an ESMF_Info object, or on a State's own Info
   ! or a named item's Info, both under the shared namespace.
   interface MAPL_InfoGetValueKind
      procedure :: info_get_value_kind
      procedure :: info_state_get_shared_value_kind
      procedure :: info_stateitem_get_shared_value_kind
   end interface MAPL_InfoGetValueKind

   ! Unqualified names of the children under a key, or under a prefix
   ! relative to a State's own Info or a named item's Info, both under
   ! the shared namespace.
   interface MAPL_InfoGetKeys
      procedure :: info_get_keys
      procedure :: info_state_get_shared_keys
      procedure :: info_stateitem_get_shared_keys
   end interface MAPL_InfoGetKeys

   interface MAPL_InfoCreateFromShared
      procedure :: info_field_create_from_shared
   end interface MAPL_InfoCreateFromShared

   ! State's own Info, callback namespace: the interface name a callback
   ! State's attribute and call scalar lookups resolve against.
   interface MAPL_InfoHasCallbackInterface
      procedure :: info_state_has_callback_interface
   end interface MAPL_InfoHasCallbackInterface

   interface MAPL_InfoGetCallbackInterface
      procedure :: info_state_get_callback_interface
   end interface MAPL_InfoGetCallbackInterface

   interface MAPL_InfoSetCallbackInterface
      procedure :: info_state_set_callback_interface
   end interface MAPL_InfoSetCallbackInterface

   ! Direct access through ESMF_Info object
   interface MAPL_InfoGet
      procedure :: info_get_string
      procedure :: info_get_logical
      procedure :: info_get_i4
      procedure :: info_get_r4
      procedure :: info_get_r8
      procedure :: info_get_i4_1d
      procedure :: info_get_r4_1d
      procedure :: info_get_r8_1d
      procedure :: info_get_string_list
   end interface MAPL_InfoGet

   ! Access info object from esmf stateitem
   interface MAPL_InfoGetShared
      procedure :: info_state_get_shared_string
      procedure :: info_state_get_shared_logical
      procedure :: info_state_get_shared_i4
      procedure :: info_state_get_shared_r4
      procedure :: info_state_get_shared_r8
      procedure :: info_state_get_shared_i4_1d
      procedure :: info_state_get_shared_r4_1d
      procedure :: info_state_get_shared_r8_1d
      procedure :: info_state_get_shared_string_list
      procedure :: info_stateitem_get_shared_string
      procedure :: info_stateitem_get_shared_logical
      procedure :: info_stateitem_get_shared_i4
      procedure :: info_stateitem_get_shared_r4
      procedure :: info_stateitem_get_shared_r8
      procedure :: info_stateitem_get_shared_i4_1d
      procedure :: info_stateitem_get_shared_r4_1d
      procedure :: info_stateitem_get_shared_r8_1d
      procedure :: info_stateitem_get_shared_string_list
   end interface MAPL_InfoGetShared

   interface MAPL_InfoSetShared
      procedure :: info_state_set_shared_string
      procedure :: info_state_set_shared_logical
      procedure :: info_state_set_shared_i4
      procedure :: info_state_set_shared_r4
      procedure :: info_state_set_shared_r8
      procedure :: info_state_set_shared_i4_1d
      procedure :: info_state_set_shared_r4_1d
      procedure :: info_state_set_shared_r8_1d
      procedure :: info_state_set_shared_string_list
      procedure :: info_stateitem_set_shared_string
      procedure :: info_stateitem_set_shared_logical
      procedure :: info_stateitem_set_shared_i4
      procedure :: info_stateitem_set_shared_r4
      procedure :: info_stateitem_set_shared_r8
      procedure :: info_stateitem_set_shared_i4_1d
      procedure :: info_stateitem_set_shared_r4_1d
      procedure :: info_stateitem_set_shared_r8_1d
      procedure :: info_stateitem_set_shared_string_list
   end interface MAPL_InfoSetShared

   interface MAPL_InfoGetPrivate
      procedure :: info_stateitem_get_private_string
      procedure :: info_stateitem_get_private_logical
      procedure :: info_stateitem_get_private_i4
      procedure :: info_stateitem_get_private_r4
      procedure :: info_stateitem_get_private_r8
      procedure :: info_stateitem_get_private_i4_1d
      procedure :: info_stateitem_get_private_r4_1d
      procedure :: info_stateitem_get_private_r8_1d
      procedure :: info_stateitem_get_private_string_list
   end interface MAPL_InfoGetPrivate

   interface MAPL_InfoSetPrivate
      procedure :: info_stateitem_set_private_string
      procedure :: info_stateitem_set_private_logical
      procedure :: info_stateitem_set_private_i4
      procedure :: info_stateitem_set_private_r4
      procedure :: info_stateitem_set_private_r8
      procedure :: info_stateitem_set_private_i4_1d
      procedure :: info_stateitem_set_private_r4_1d
      procedure :: info_stateitem_set_private_r8_1d
      procedure :: info_stateitem_set_private_string_list
   end interface MAPL_InfoSetPrivate

   ! Control namespace in state
   interface MAPL_InfoSetNamespace
      procedure :: set_namespace
   end interface MAPL_InfoSetNamespace

contains

   ! MAPL_InfoGet
   subroutine info_get_string(info, key, value, unusable, rc)
      type(ESMF_Info), intent(in) :: info
      character(*), intent(in) :: key
      character(:), allocatable, intent(out) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_present

      is_present = ESMF_InfoIsPresent(info, key=key, _RC)
      if (.not. is_present) call ESMF_InfoPrint(info)
      _ASSERT(is_present,  "Key not found in info object: " // key)

      call ESMF_InfoGetCharAlloc(info, key=key, value=value, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_get_string

   subroutine info_get_logical(info, key, value, unusable, rc)
      type(ESMF_Info), intent(in) :: info
      character(*), intent(in) :: key
      logical, intent(out) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_present

      is_present = ESMF_InfoIsPresent(info, key=key, _RC)
      _ASSERT(is_present,  "Key not found in info object: " // key)

      call ESMF_InfoGet(info, key=key, value=value, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_get_logical

   subroutine info_get_i4(info, key, value, unusable, rc)
      type(ESMF_Info), intent(in) :: info
      character(*), intent(in) :: key
      integer(kind=ESMF_KIND_I4), intent(out) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_present

      is_present = ESMF_InfoIsPresent(info, key=key, _RC)
      if (.not. is_present) call ESMF_InfoPrint(info)
      _ASSERT(is_present,  "Key not found in info object: " // key)

      call ESMF_InfoGet(info, key=key, value=value, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_get_i4

   subroutine info_get_r4(info, key, value, unusable, rc)
      type(ESMF_Info), intent(in) :: info
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R4), intent(out) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_present
      real(kind=ESMF_KIND_R8) :: value_r8

      is_present = ESMF_InfoIsPresent(info, key=key, _RC)
      _ASSERT(is_present,  "Key not found in info object: " // key)

      call ESMF_InfoGet(info, key=key, value=value_r8, _RC)
      value = real(value_r8, kind=ESMF_KIND_R4)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_get_r4

   subroutine info_get_r8(info, key, value, unusable, rc)
      type(ESMF_Info), intent(in) :: info
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R8), intent(out) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_present

      is_present = ESMF_InfoIsPresent(info, key=key, _RC)
      _ASSERT(is_present,  "Key not found in info object: " // key)

      call ESMF_InfoGet(info, key=key, value=value, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_get_r8

   subroutine info_get_i4_1d(info, key, values, unusable, rc)
      type(ESMF_Info), intent(in) :: info
      character(*), intent(in) :: key
      integer(kind=ESMF_KIND_I4), allocatable, intent(out) :: values(:)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_present

      is_present = ESMF_InfoIsPresent(info, key=key, _RC)
      _ASSERT(is_present,  "Key not found in info object: " // key)

      call ESMF_InfoGetAlloc(info, key=key, values=values, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_get_i4_1d

   subroutine info_get_r4_1d(info, key, values, unusable, rc)
      type(ESMF_Info), intent(in) :: info
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R4), allocatable, intent(out) :: values(:)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_present

      is_present = ESMF_InfoIsPresent(info, key=key, _RC)
      _ASSERT(is_present,  "Key not found in info object: " // key)

      call ESMF_InfoGetAlloc(info, key=key, values=values, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_get_r4_1d

   subroutine info_get_r8_1d(info, key, values, unusable, rc)
      type(ESMF_Info), intent(in) :: info
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R8), allocatable, intent(out) :: values(:)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_present

      is_present = ESMF_InfoIsPresent(info, key=key, _RC)
      _ASSERT(is_present,  "Key not found in info object: " // key)

      call ESMF_InfoGetAlloc(info, key=key, values=values, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_get_r8_1d

   ! ESMF_InfoGetAlloc has no character-array form, so each element is
   ! read with idx=.  Trailing blanks do not survive a round trip.
   subroutine info_get_string_list(info, key, values, unusable, rc)
      type(ESMF_Info), intent(in) :: info
      character(*), intent(in) :: key
      type(StringVector), intent(out) :: values
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_present
      integer :: i, n
      character(:), allocatable :: element

      is_present = ESMF_InfoIsPresent(info, key=key, _RC)
      _ASSERT(is_present,  "Key not found in info object: " // key)

      call ESMF_InfoGet(info, key=key, size=n, _RC)
      do i = 1, n
         call ESMF_InfoGetCharAlloc(info, key=key, value=element, idx=i, _RC)
         call values%push_back(element)
      end do

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_get_string_list

   ! Never fails for an absent key: reports INFO_VALUE_ABSENT.
   subroutine info_get_value_kind(info, key, value_kind, unusable, rc)
      type(ESMF_Info), intent(in) :: info
      character(*), intent(in) :: key
      type(InfoValueKind), intent(out) :: value_kind
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_present
      logical :: is_array
      type(ESMF_TypeKind_Flag) :: typekind

      is_present = ESMF_InfoIsPresent(info, key=key, _RC)
      if (.not. is_present) then
         value_kind = INFO_VALUE_ABSENT
         _RETURN(_SUCCESS)
      end if

      call ESMF_InfoGet(info, key=key, typekind=typekind, isArray=is_array, _RC)

      if (typekind == ESMF_TYPEKIND_I4 .or. typekind == ESMF_TYPEKIND_I8) then
         value_kind = merge(INFO_VALUE_INTEGER_LIST, INFO_VALUE_INTEGER, is_array)
      else if (typekind == ESMF_TYPEKIND_R4 .or. typekind == ESMF_TYPEKIND_R8) then
         value_kind = merge(INFO_VALUE_REAL_LIST, INFO_VALUE_REAL, is_array)
      else if (typekind == ESMF_TYPEKIND_LOGICAL) then
         value_kind = merge(INFO_VALUE_LOGICAL_LIST, INFO_VALUE_LOGICAL, is_array)
      else if (typekind == ESMF_TYPEKIND_CHARACTER) then
         value_kind = merge(INFO_VALUE_STRING_LIST, INFO_VALUE_STRING, is_array)
      else
         value_kind = INFO_VALUE_OTHER
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_get_value_kind

   ! Immediate children of prefix, unqualified.  An absent prefix, or a
   ! prefix that holds a value rather than an object, has no children
   ! (empty result, success).  An empty prefix names the top level.  A
   ! trailing '/' is stripped first: ESMF otherwise treats it as part
   ! of a child key name and the lookup silently returns no children.
   ! Child key names longer than ESMF_MAXSTR are truncated, because
   ! each name is read into a fixed-length ESMF_MAXSTR buffer below.
   subroutine info_get_keys(info, prefix, keys, unusable, rc)
      type(ESMF_Info), intent(in) :: info
      character(*), intent(in) :: prefix
      type(StringVector), intent(out) :: keys
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i, n
      type(InfoValueKind) :: value_kind
      character(len=ESMF_MAXSTR) :: child
      character(:), allocatable :: key

      key = prefix
      if (len(key) > 0) then
         if (key(len(key):len(key)) == '/') key = key(1:len(key)-1)
      end if

      if (len(key) > 0) then
         call MAPL_InfoGetValueKind(info, key, value_kind, _RC)
         if (value_kind /= INFO_VALUE_OTHER) then
            _RETURN(_SUCCESS)
         end if
      end if

      call ESMF_InfoGet(info, key=key, size=n, _RC)
      do i = 1, n
         call ESMF_InfoGet(info, key=key, idx=i, ikey=child, _RC)
         call keys%push_back(trim(child))
      end do

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_get_keys

   ! State's own Info, shared namespace
   subroutine info_state_get_shared_value_kind(state, key, value_kind, unusable, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: key
      type(InfoValueKind), intent(out) :: value_kind
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: state_info

      call ESMF_InfoGetFromHost(state, state_info, _RC)
      call MAPL_InfoGetValueKind(state_info, concat(INFO_SHARED_NAMESPACE,key), value_kind, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_state_get_shared_value_kind

   ! A named item's Info (a member of the State), shared namespace
   subroutine info_stateitem_get_shared_value_kind(state, short_name, key, value_kind, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      type(InfoValueKind), intent(out) :: value_kind
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call info_stateitem_get_info(state, short_name, info, _RC)
      call MAPL_InfoGetValueKind(info, concat(INFO_SHARED_NAMESPACE,key), value_kind, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_get_shared_value_kind

   ! State's own Info, shared namespace
   subroutine info_state_get_shared_keys(state, key, keys, unusable, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: key
      type(StringVector), intent(out) :: keys
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: state_info

      call ESMF_InfoGetFromHost(state, state_info, _RC)
      call MAPL_InfoGetKeys(state_info, concat(INFO_SHARED_NAMESPACE,key), keys, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_state_get_shared_keys

   ! A named item's Info (a member of the State), shared namespace
   subroutine info_stateitem_get_shared_keys(state, short_name, key, keys, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      type(StringVector), intent(out) :: keys
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call info_stateitem_get_info(state, short_name, info, _RC)
      call MAPL_InfoGetKeys(info, concat(INFO_SHARED_NAMESPACE,key), keys, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_get_shared_keys

   ! State's own Info, callback namespace
   function info_state_has_callback_interface(state, rc) result(is_present)
      type(ESMF_State), intent(in) :: state
      integer, optional, intent(out) :: rc
      logical :: is_present

      integer :: status
      type(ESMF_Info) :: state_info

      call ESMF_InfoGetFromHost(state, state_info, _RC)
      is_present = ESMF_InfoIsPresent(state_info, &
           key=INFO_CALLBACK_NAMESPACE // KEY_CALLBACK_INTERFACE, _RC)

      _RETURN(_SUCCESS)
   end function info_state_has_callback_interface

   subroutine info_state_get_callback_interface(state, interface_name, rc)
      type(ESMF_State), intent(in) :: state
      character(:), allocatable, intent(out) :: interface_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: state_info

      call ESMF_InfoGetFromHost(state, state_info, _RC)
      call MAPL_InfoGet(state_info, key=INFO_CALLBACK_NAMESPACE // KEY_CALLBACK_INTERFACE, &
           value=interface_name, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_state_get_callback_interface

   subroutine info_state_set_callback_interface(state, interface_name, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: interface_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: state_info

      call ESMF_InfoGetFromHost(state, state_info, _RC)
      call MAPL_InfoSet(state_info, key=INFO_CALLBACK_NAMESPACE // KEY_CALLBACK_INTERFACE, &
           value=interface_name, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_state_set_callback_interface

   pure logical function infovaluekind_equal(left, right) result(equal)
      type(InfoValueKind), intent(in) :: left, right

      equal = left%code == right%code
   end function infovaluekind_equal

   pure logical function infovaluekind_not_equal(left, right) result(not_equal)
      type(InfoValueKind), intent(in) :: left, right

      not_equal = .not. (left == right)
   end function infovaluekind_not_equal

   pure logical function infovaluekind_is_list(this) result(is_list)
      class(InfoValueKind), intent(in) :: this

      is_list = this == INFO_VALUE_INTEGER_LIST .or. this == INFO_VALUE_REAL_LIST &
           .or. this == INFO_VALUE_LOGICAL_LIST .or. this == INFO_VALUE_STRING_LIST
   end function infovaluekind_is_list

   pure function infovaluekind_to_string(this) result(name)
      class(InfoValueKind), intent(in) :: this
      character(:), allocatable :: name

      if (this == INFO_VALUE_ABSENT) then
         name = 'absent'
      else if (this == INFO_VALUE_OTHER) then
         name = 'other'
      else if (this == INFO_VALUE_INTEGER) then
         name = 'integer'
      else if (this == INFO_VALUE_REAL) then
         name = 'real'
      else if (this == INFO_VALUE_LOGICAL) then
         name = 'logical'
      else if (this == INFO_VALUE_STRING) then
         name = 'string'
      else if (this == INFO_VALUE_INTEGER_LIST) then
         name = 'integer list'
      else if (this == INFO_VALUE_REAL_LIST) then
         name = 'real list'
      else if (this == INFO_VALUE_LOGICAL_LIST) then
         name = 'logical list'
      else if (this == INFO_VALUE_STRING_LIST) then
         name = 'string list'
      else
         name = 'unknown'
      end if
   end function infovaluekind_to_string

   function info_field_create_from_shared(field, rc) result(info)
      type(ESMF_Info) :: info
      type(ESMF_Field), intent(in) :: field
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: host_info

      call ESMF_InfoGetFromHost(field, host_info, _RC)
      info = ESMF_InfoCreate(host_info, key=INFO_SHARED_NAMESPACE, _RC)

      _RETURN(_SUCCESS)
   end function info_field_create_from_shared

   ! MAPL_InfoGetShared
   subroutine info_state_get_shared_string(state, key, value, unusable, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: key
      character(:), allocatable, intent(out) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: state_info

      call ESMF_InfoGetFromHost(state, state_info, _RC)
      call MAPL_InfoGet(state_info, key=concat(INFO_SHARED_NAMESPACE,key), value=value, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_state_get_shared_string

   subroutine info_state_get_shared_logical(state, key, value, unusable, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: key
      logical, intent(out) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: state_info

      call ESMF_InfoGetFromHost(state, state_info, _RC)
      call MAPL_InfoGet(state_info, key=concat(INFO_SHARED_NAMESPACE,key), value=value, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_state_get_shared_logical

   subroutine info_state_get_shared_i4(state, key, value, unusable, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: key
      integer(kind=ESMF_KIND_I4), intent(out) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: state_info

      call ESMF_InfoGetFromHost(state, state_info, _RC)
      call MAPL_InfoGet(state_info, key=concat(INFO_SHARED_NAMESPACE,key), value=value, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_state_get_shared_i4

   subroutine info_state_get_shared_r4(state, key, value, unusable, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R4), intent(out) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: state_info

      call ESMF_InfoGetFromHost(state, state_info, _RC)
      call MAPL_InfoGet(state_info, key=concat(INFO_SHARED_NAMESPACE,key), value=value, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_state_get_shared_r4

   subroutine info_state_get_shared_r8(state, key, value, unusable, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R8), intent(out) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: state_info

      call ESMF_InfoGetFromHost(state, state_info, _RC)
      call MAPL_InfoGet(state_info, key=concat(INFO_SHARED_NAMESPACE,key), value=value, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_state_get_shared_r8

   subroutine info_state_get_shared_i4_1d(state, key, values, unusable, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: key
      integer(kind=ESMF_KIND_I4), allocatable, intent(out) :: values(:)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: state_info

      call ESMF_InfoGetFromHost(state, state_info, _RC)
      call MAPL_InfoGet(state_info, key=concat(INFO_SHARED_NAMESPACE,key), values=values, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_state_get_shared_i4_1d

   subroutine info_state_get_shared_r4_1d(state, key, values, unusable, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R4), allocatable, intent(out) :: values(:)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: state_info

      call ESMF_InfoGetFromHost(state, state_info, _RC)
      call MAPL_InfoGet(state_info, key=concat(INFO_SHARED_NAMESPACE,key), values=values, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_state_get_shared_r4_1d

   subroutine info_state_get_shared_r8_1d(state, key, values, unusable, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R8), allocatable, intent(out) :: values(:)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: state_info

      call ESMF_InfoGetFromHost(state, state_info, _RC)
      call MAPL_InfoGet(state_info, key=concat(INFO_SHARED_NAMESPACE,key), values=values, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_state_get_shared_r8_1d

   subroutine info_state_get_shared_string_list(state, key, values, unusable, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: key
      type(StringVector), intent(out) :: values
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: state_info

      call ESMF_InfoGetFromHost(state, state_info, _RC)
      call MAPL_InfoGet(state_info, key=concat(INFO_SHARED_NAMESPACE,key), values=values, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_state_get_shared_string_list

   subroutine info_stateitem_get_shared_string(state, short_name, key, value, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      character(:), allocatable, intent(out) :: value
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call info_stateitem_get_info(state, short_name, info, _RC)
      call MAPL_InfoGet(info, key=concat(INFO_SHARED_NAMESPACE,key), value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_get_shared_string

   subroutine info_stateitem_get_shared_logical(state, short_name, key, value, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      logical, intent(out) :: value
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call info_stateitem_get_info(state, short_name, info, _RC)
      call MAPL_InfoGet(info, key=concat(INFO_SHARED_NAMESPACE,key), value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_get_shared_logical

   subroutine info_stateitem_get_shared_i4(state, short_name, key, value, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      integer(kind=ESMF_KIND_I4), intent(out) :: value
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call info_stateitem_get_info(state, short_name, info, _RC)
      call MAPL_InfoGet(info, key=concat(INFO_SHARED_NAMESPACE,key), value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_get_shared_i4

   subroutine info_stateitem_get_shared_r4(state, short_name, key, value, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R4), intent(out) :: value
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call info_stateitem_get_info(state, short_name, info, _RC)
      call MAPL_InfoGet(info, key=concat(INFO_SHARED_NAMESPACE,key), value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_get_shared_r4

   subroutine info_stateitem_get_shared_r8(state, short_name, key, value, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R8), intent(out) :: value
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call info_stateitem_get_info(state, short_name, info, _RC)
      call MAPL_InfoGet(info, key=concat(INFO_SHARED_NAMESPACE,key), value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_get_shared_r8

   subroutine info_stateitem_get_shared_i4_1d(state, short_name, key, values, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      integer(kind=ESMF_KIND_I4), allocatable, intent(out) :: values(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call info_stateitem_get_info(state, short_name, info, _RC)
      call MAPL_InfoGet(info, key=concat(INFO_SHARED_NAMESPACE,key), values=values, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_get_shared_i4_1d

   subroutine info_stateitem_get_shared_r4_1d(state, short_name, key, values, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R4), allocatable, intent(out) :: values(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call info_stateitem_get_info(state, short_name, info, _RC)
      call MAPL_InfoGet(info, key=concat(INFO_SHARED_NAMESPACE,key), values=values, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_get_shared_r4_1d

   subroutine info_stateitem_get_shared_r8_1d(state, short_name, key, values, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R8), allocatable, intent(out) :: values(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call info_stateitem_get_info(state, short_name, info, _RC)
      call MAPL_InfoGet(info, key=concat(INFO_SHARED_NAMESPACE,key), values=values, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_get_shared_r8_1d

   subroutine info_stateitem_get_shared_string_list(state, short_name, key, values, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      type(StringVector), intent(out) :: values
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call info_stateitem_get_info(state, short_name, info, _RC)
      call MAPL_InfoGet(info, key=concat(INFO_SHARED_NAMESPACE,key), values=values, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_get_shared_string_list

   ! MAPL_InfoSetShared
   subroutine info_state_set_shared_string(state, key, value, unusable, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: key
      character(*), intent(in) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: state_info

      call ESMF_InfoGetFromHost(state, state_info, _RC)
      call MAPL_InfoSet(state_info, key=concat(INFO_SHARED_NAMESPACE,key), value=value, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_state_set_shared_string

   subroutine info_state_set_shared_logical(state, key, value, unusable, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: key
      logical, intent(in) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: state_info

      call ESMF_InfoGetFromHost(state, state_info, _RC)
      call MAPL_InfoSet(state_info, key=concat(INFO_SHARED_NAMESPACE,key), value=value, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_state_set_shared_logical

   subroutine info_state_set_shared_i4(state, key, value, unusable, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: key
      integer(kind=ESMF_KIND_I4), intent(in) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: state_info

      call ESMF_InfoGetFromHost(state, state_info, _RC)
      call MAPL_InfoSet(state_info, key=concat(INFO_SHARED_NAMESPACE,key), value=value, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_state_set_shared_i4

   subroutine info_state_set_shared_r4(state, key, value, unusable, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R4), intent(in) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: state_info

      call ESMF_InfoGetFromHost(state, state_info, _RC)
      call MAPL_InfoSet(state_info, key=concat(INFO_SHARED_NAMESPACE,key), value=value, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_state_set_shared_r4

   subroutine info_state_set_shared_r8(state, key, value, unusable, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R8), intent(in) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: state_info

      call ESMF_InfoGetFromHost(state, state_info, _RC)
      call MAPL_InfoSet(state_info, key=concat(INFO_SHARED_NAMESPACE,key), value=value, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_state_set_shared_r8

   subroutine info_state_set_shared_i4_1d(state, key, values, unusable, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: key
      integer(kind=ESMF_KIND_I4), intent(in) :: values(:)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: state_info

      call ESMF_InfoGetFromHost(state, state_info, _RC)
      call MAPL_InfoSet(state_info, key=concat(INFO_SHARED_NAMESPACE,key), values=values, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_state_set_shared_i4_1d

   subroutine info_state_set_shared_r4_1d(state, key, values, unusable, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R4), intent(in) :: values(:)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: state_info

      call ESMF_InfoGetFromHost(state, state_info, _RC)
      call MAPL_InfoSet(state_info, key=concat(INFO_SHARED_NAMESPACE,key), values=values, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_state_set_shared_r4_1d

   subroutine info_state_set_shared_r8_1d(state, key, values, unusable, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R8), intent(in) :: values(:)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: state_info

      call ESMF_InfoGetFromHost(state, state_info, _RC)
      call MAPL_InfoSet(state_info, key=concat(INFO_SHARED_NAMESPACE,key), values=values, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_state_set_shared_r8_1d

   subroutine info_state_set_shared_string_list(state, key, values, unusable, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: key
      character(*), intent(in) :: values(:)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: state_info

      call ESMF_InfoGetFromHost(state, state_info, _RC)
      call MAPL_InfoSet(state_info, key=concat(INFO_SHARED_NAMESPACE,key), values=values, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine info_state_set_shared_string_list

   subroutine info_stateitem_set_shared_string(state, short_name, key, value, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      character(*), intent(in) :: value
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call info_stateitem_get_info(state, short_name, info, _RC)
      call MAPL_InfoSet(info, key=concat(INFO_SHARED_NAMESPACE,key), value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_set_shared_string

   subroutine info_stateitem_set_shared_logical(state, short_name, key, value, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      logical, intent(in) :: value
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call info_stateitem_get_info(state, short_name, info, _RC)
      call MAPL_InfoSet(info, key=concat(INFO_SHARED_NAMESPACE,key), value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_set_shared_logical

   subroutine info_stateitem_set_shared_i4(state, short_name, key, value, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      integer(kind=ESMF_KIND_I4), intent(in) :: value
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call info_stateitem_get_info(state, short_name, info, _RC)
      call MAPL_InfoSet(info, key=concat(INFO_SHARED_NAMESPACE,key), value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_set_shared_i4

   subroutine info_stateitem_set_shared_r4(state, short_name, key, value, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R4), intent(in) :: value
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call info_stateitem_get_info(state, short_name, info, _RC)
      call MAPL_InfoSet(info, key=concat(INFO_SHARED_NAMESPACE,key), value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_set_shared_r4

   subroutine info_stateitem_set_shared_r8(state, short_name, key, value, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R8), intent(in) :: value
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call info_stateitem_get_info(state, short_name, info, _RC)
      call MAPL_InfoSet(info, key=concat(INFO_SHARED_NAMESPACE,key), value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_set_shared_r8

   subroutine info_stateitem_set_shared_i4_1d(state, short_name, key, values, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      integer(kind=ESMF_KIND_I4), intent(in) :: values(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call info_stateitem_get_info(state, short_name, info, _RC)
      call MAPL_InfoSet(info, key=concat(INFO_SHARED_NAMESPACE,key), values=values, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_set_shared_i4_1d

   subroutine info_stateitem_set_shared_r4_1d(state, short_name, key, values, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R4), intent(in) :: values(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call info_stateitem_get_info(state, short_name, info, _RC)
      call MAPL_InfoSet(info, key=concat(INFO_SHARED_NAMESPACE,key), values=values, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_set_shared_r4_1d

   subroutine info_stateitem_set_shared_r8_1d(state, short_name, key, values, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R8), intent(in) :: values(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call info_stateitem_get_info(state, short_name, info, _RC)
      call MAPL_InfoSet(info, key=concat(INFO_SHARED_NAMESPACE,key), values=values, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_set_shared_r8_1d

   subroutine info_stateitem_set_shared_string_list(state, short_name, key, values, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      character(*), intent(in) :: values(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call info_stateitem_get_info(state, short_name, info, _RC)
      call MAPL_InfoSet(info, key=concat(INFO_SHARED_NAMESPACE,key), values=values, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_set_shared_string_list

   ! MAPL_InfoGetPrivate

   subroutine info_stateitem_get_private_string(state, short_name, key, value, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      character(:), allocatable, intent(out) :: value
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: item_info
      character(:), allocatable :: namespace
      character(:), allocatable :: private_key

      call get_namespace(state, namespace, _RC)

      call info_stateitem_get_info(state, short_name, item_info, _RC)
      private_key = concat(INFO_PRIVATE_NAMESPACE // namespace, key)
      call MAPL_InfoGet(item_info, key=private_key, value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_get_private_string

   subroutine info_stateitem_get_private_logical(state, short_name, key, value, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      logical, intent(out) :: value
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: item_info
      character(:), allocatable :: namespace
      character(:), allocatable :: private_key

      call get_namespace(state, namespace, _RC)

      call info_stateitem_get_info(state, short_name, item_info, _RC)
      private_key = concat(INFO_PRIVATE_NAMESPACE // namespace, key)
      call MAPL_InfoGet(item_info, key=private_key, value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_get_private_logical

   subroutine info_stateitem_get_private_i4(state, short_name, key, value, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      integer(kind=ESMF_KIND_I4), intent(out) :: value
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: item_info
      character(:), allocatable :: namespace
      character(:), allocatable :: private_key

      call get_namespace(state, namespace, _RC)

      call info_stateitem_get_info(state, short_name, item_info, _RC)
      private_key = concat(INFO_PRIVATE_NAMESPACE // namespace, key)
      call MAPL_InfoGet(item_info, key=private_key, value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_get_private_i4

   subroutine info_stateitem_get_private_r4(state, short_name, key, value, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R4), intent(out) :: value
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: item_info
      character(:), allocatable :: namespace
      character(:), allocatable :: private_key

      call get_namespace(state, namespace, _RC)
      call info_stateitem_get_info(state, short_name, item_info, _RC)
      private_key = concat(INFO_PRIVATE_NAMESPACE // namespace, key)
      call MAPL_InfoGet(item_info, key=private_key, value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_get_private_r4

   subroutine info_stateitem_get_private_r8(state, short_name, key, value, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R8), intent(out) :: value
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: item_info
      character(:), allocatable :: namespace
      character(:), allocatable :: private_key

      call get_namespace(state, namespace, _RC)
      call info_stateitem_get_info(state, short_name, item_info, _RC)
      private_key = concat(INFO_PRIVATE_NAMESPACE // namespace, key)
      call MAPL_InfoGet(item_info, key=private_key, value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_get_private_r8

   subroutine info_stateitem_get_private_i4_1d(state, short_name, key, values, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      integer(kind=ESMF_KIND_I4), allocatable, intent(out) :: values(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: item_info
      character(:), allocatable :: namespace
      character(:), allocatable :: private_key

      call get_namespace(state, namespace, _RC)

      call info_stateitem_get_info(state, short_name, item_info, _RC)
      private_key = concat(INFO_PRIVATE_NAMESPACE // namespace, key)
      call MAPL_InfoGet(item_info, key=private_key, values=values, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_get_private_i4_1d

   subroutine info_stateitem_get_private_r4_1d(state, short_name, key, values, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R4), allocatable, dimension(:), intent(out) :: values
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: item_info
      character(:), allocatable :: namespace
      character(:), allocatable :: private_key

      call get_namespace(state, namespace, _RC)
      call info_stateitem_get_info(state, short_name, item_info, _RC)
      private_key = concat(INFO_PRIVATE_NAMESPACE // namespace, key)
      call MAPL_InfoGet(item_info, key=private_key, values=values, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_get_private_r4_1d

   subroutine info_stateitem_get_private_r8_1d(state, short_name, key, values, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R8), allocatable, intent(out) :: values(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: item_info
      character(:), allocatable :: namespace
      character(:), allocatable :: private_key

      call get_namespace(state, namespace, _RC)

      call info_stateitem_get_info(state, short_name, item_info, _RC)
      private_key = concat(INFO_PRIVATE_NAMESPACE // namespace, key)
      call MAPL_InfoGet(item_info, key=private_key, values=values, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_get_private_r8_1d

   subroutine info_stateitem_get_private_string_list(state, short_name, key, values, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      type(StringVector), intent(out) :: values
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: item_info
      character(:), allocatable :: namespace
      character(:), allocatable :: private_key

      call get_namespace(state, namespace, _RC)

      call info_stateitem_get_info(state, short_name, item_info, _RC)
      private_key = concat(INFO_PRIVATE_NAMESPACE // namespace, key)
      call MAPL_InfoGet(item_info, key=private_key, values=values, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_get_private_string_list

   ! MAPL_InfoGetPrivate

   subroutine info_stateitem_set_private_string(state, short_name, key, value, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      character(*), intent(in) :: value
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: item_info
      character(:), allocatable :: namespace
      character(:), allocatable :: private_key

      call get_namespace(state, namespace, _RC)

      call info_stateitem_get_info(state, short_name, item_info, _RC)
      private_key = concat(INFO_PRIVATE_NAMESPACE // namespace, key)
      call MAPL_InfoSet(item_info, key=private_key, value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_set_private_string


  subroutine info_stateitem_set_private_logical(state, short_name, key, value, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      logical, intent(in) :: value
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: item_info
      character(:), allocatable :: namespace
      character(:), allocatable :: private_key

      call get_namespace(state, namespace, _RC)

      call info_stateitem_get_info(state, short_name, item_info, _RC)
      private_key = concat(INFO_PRIVATE_NAMESPACE // namespace, key)
      call MAPL_InfoSet(item_info, key=private_key, value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_set_private_logical

   subroutine info_stateitem_set_private_i4(state, short_name, key, value, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      integer(kind=ESMF_KIND_I4), intent(in) :: value
      integer, optional, intent(out) :: rc

      integer :: status

      type(ESMF_Info) :: item_info
      character(:), allocatable :: namespace
      character(:), allocatable :: private_key

      call get_namespace(state, namespace, _RC)
      call info_stateitem_get_info(state, short_name, item_info, _RC)
      private_key = concat(INFO_PRIVATE_NAMESPACE // namespace, key)
      call MAPL_InfoSet(item_info, key=private_key, value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_set_private_i4

   subroutine info_stateitem_set_private_r4(state, short_name, key, value, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R4), intent(in) :: value
      integer, optional, intent(out) :: rc

      integer :: status

      type(ESMF_Info) :: item_info
      character(:), allocatable :: namespace
      character(:), allocatable :: private_key

      call get_namespace(state, namespace, _RC)
      call info_stateitem_get_info(state, short_name, item_info, _RC)
      private_key = concat(INFO_PRIVATE_NAMESPACE // namespace, key)
      call MAPL_InfoSet(item_info, key=private_key, value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_set_private_r4

   subroutine info_stateitem_set_private_r8(state, short_name, key, value, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R8), intent(in) :: value
      integer, optional, intent(out) :: rc

      integer :: status

      type(ESMF_Info) :: item_info
      character(:), allocatable :: namespace
      character(:), allocatable :: private_key

      call get_namespace(state, namespace, _RC)
      call info_stateitem_get_info(state, short_name, item_info, _RC)
      private_key = concat(INFO_PRIVATE_NAMESPACE // namespace, key)
      call MAPL_InfoSet(item_info, key=private_key, value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_set_private_r8

   subroutine info_stateitem_set_private_i4_1d(state, short_name, key, values, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      integer(kind=ESMF_KIND_I4), intent(in) :: values(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: item_info
      character(:), allocatable :: namespace
      character(:), allocatable :: private_key

      call get_namespace(state, namespace, _RC)

      call info_stateitem_get_info(state, short_name, item_info, _RC)
      private_key = concat(INFO_PRIVATE_NAMESPACE // namespace, key)
      call MAPL_InfoSet(item_info, key=private_key, values=values, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_set_private_i4_1d

   subroutine info_stateitem_set_private_r4_1d(state, short_name, key, values, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R4), intent(in) :: values(:)
      integer, optional, intent(out) :: rc

      integer :: status

      type(ESMF_Info) :: item_info
      character(:), allocatable :: namespace
      character(:), allocatable :: private_key

      call get_namespace(state, namespace, _RC)
      call info_stateitem_get_info(state, short_name, item_info, _RC)
      private_key = concat(INFO_PRIVATE_NAMESPACE // namespace, key)
      call MAPL_InfoSet(item_info, key=private_key, values=values, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_set_private_r4_1d

   subroutine info_stateitem_set_private_r8_1d(state, short_name, key, values, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R8), intent(in) :: values(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: item_info
      character(:), allocatable :: namespace
      character(:), allocatable :: private_key

      call get_namespace(state, namespace, _RC)

      call info_stateitem_get_info(state, short_name, item_info, _RC)
      private_key = concat(INFO_PRIVATE_NAMESPACE // namespace, key)
      call MAPL_InfoSet(item_info, key=private_key, values=values, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_set_private_r8_1d

   subroutine info_stateitem_set_private_string_list(state, short_name, key, values, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      character(*), intent(in) :: values(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: item_info
      character(:), allocatable :: namespace
      character(:), allocatable :: private_key

      call get_namespace(state, namespace, _RC)

      call info_stateitem_get_info(state, short_name, item_info, _RC)
      private_key = concat(INFO_PRIVATE_NAMESPACE // namespace, key)
      call MAPL_InfoSet(item_info, key=private_key, values=values, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_set_private_string_list


   ! private helper procedure
   subroutine info_stateitem_get_info(state, short_name, info, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      type(ESMF_Info), intent(out) :: info
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_StateItem_Flag) :: itemType
      type(ESMF_Field) :: field
      type(ESMF_FieldBundle) :: bundle

      call ESMF_StateGet(state, itemName=short_name, itemType=itemType, _RC)
      if (itemType == ESMF_STATEITEM_FIELD) then
         call ESMF_StateGet(state, itemName=short_name, field=field, _RC)
         call ESMF_InfoGetFromHost(field, info, _RC)
         _RETURN(_SUCCESS)
      end if
      if (itemType == ESMF_STATEITEM_FIELDBUNDLE) then
         call ESMF_StateGet(state, itemName=short_name, fieldbundle=bundle, _RC)
         call ESMF_InfoGetFromHost(bundle, info, _RC)
         _RETURN(_SUCCESS)
      end if
      _FAIL('Unsupported state item type.')

      _RETURN(_SUCCESS)
   end subroutine info_stateitem_get_info


   subroutine get_namespace(state, namespace, rc)
      type(ESMF_State), intent(in) :: state
      character(:), allocatable, intent(out) :: namespace
      integer, optional, intent(out) :: rc

      integer :: status

      call MAPL_InfoGetShared(state, key='namespace', value=namespace, _RC)

      _RETURN(_SUCCESS)
   end subroutine get_namespace

   subroutine set_namespace(state, namespace, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: namespace
      integer, optional, intent(out) :: rc

      integer :: status

      call MAPL_InfoSetShared(state, key='namespace', value=namespace, _RC)

      _RETURN(_SUCCESS)
   end subroutine set_namespace


   function concat(namespace, key) result(full_key)
      character(*), intent(in) :: namespace
      character(*), intent(in) :: key
      character(:), allocatable :: full_key

      if (len(key) == 0) then
         full_key = namespace
         return
      end if

      if (key(1:1) == '/') then
         full_key = namespace // key
         return
      end if
      full_key = namespace // '/' // key

   end function concat

end module mapl_InfoUtilities_mod

