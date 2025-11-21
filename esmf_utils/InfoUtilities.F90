#include "MAPL.h"

! This module is intended to manage user-level access to ESMF info
! objects and thereby ensure consistent support for namespace
! management and such.

module mapl3g_InfoUtilities
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer

   use mapl3g_esmf_info_keys
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

   interface MAPL_InfoCreateFromShared
      procedure :: info_field_create_from_shared
   end interface MAPL_InfoCreateFromShared

   ! Direct access through ESMF_Info object
   interface MAPL_InfoGet
      procedure :: info_get_string
      procedure :: info_get_logical
      procedure :: info_get_i4
      procedure :: info_get_r4
      procedure :: info_get_r8
      procedure :: info_get_i4_1d
      procedure :: info_get_r4_1d
   end interface MAPL_InfoGet

   ! Access info object from esmf stateitem
   interface MAPL_InfoGetShared
      procedure :: info_state_get_shared_string
      procedure :: info_stateitem_get_shared_string
      procedure :: info_stateitem_get_shared_logical
      procedure :: info_stateitem_get_shared_i4
      procedure :: info_stateitem_get_shared_r4
      procedure :: info_stateitem_get_shared_r8
      procedure :: info_stateitem_get_shared_i4_1d
      procedure :: info_stateitem_get_shared_r4_1d
   end interface MAPL_InfoGetShared

   interface MAPL_InfoSetShared
      procedure :: info_state_set_shared_string
      procedure :: info_stateitem_set_shared_string
      procedure :: info_stateitem_set_shared_logical
      procedure :: info_stateitem_set_shared_i4
      procedure :: info_stateitem_set_shared_r4
      procedure :: info_stateitem_set_shared_r8
      procedure :: info_stateitem_set_shared_i4_1d
      procedure :: info_stateitem_set_shared_r4_1d
   end interface MAPL_InfoSetShared


   interface MAPL_InfoGetPrivate
      procedure :: info_stateitem_get_private_string
      procedure :: info_stateitem_get_private_logical
      procedure :: info_stateitem_get_private_i4
      procedure :: info_stateitem_get_private_r4
      procedure :: info_stateitem_get_private_r8
      procedure :: info_stateitem_get_private_i4_1d
      procedure :: info_stateitem_get_private_r4_1d
   end interface MAPL_InfoGetPrivate

   interface MAPL_InfoSetPrivate
      procedure :: info_stateitem_set_private_string
      procedure :: info_stateitem_set_private_logical
      procedure :: info_stateitem_set_private_i4
      procedure :: info_stateitem_set_private_r4
      procedure :: info_stateitem_set_private_r8
      procedure :: info_stateitem_set_private_i4_1d
      procedure :: info_stateitem_set_private_r4_1d
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
   end subroutine info_get_i4

   subroutine info_get_r4(info, key, value, unusable, rc)
      type(ESMF_Info), intent(in) :: info
      character(*), intent(in) :: key
      real(kind=ESMF_KIND_R4), intent(out) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_present

      is_present = ESMF_InfoIsPresent(info, key=key, _RC)
      _ASSERT(is_present,  "Key not found in info object: " // key)

      call ESMF_InfoGet(info, key=key, value=value, _RC)

      _RETURN(_SUCCESS)
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
   end subroutine info_get_r4_1d


   
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
   end subroutine info_state_get_shared_string

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
   end subroutine info_state_set_shared_string

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
      character(len(namespace)+len(key)+1) :: full_key

      if (key(1:1) == '/') then
         full_key = namespace // key
         return
      end if
      full_key = namespace // '/' //key

   end function concat
   
end module mapl3g_InfoUtilities


