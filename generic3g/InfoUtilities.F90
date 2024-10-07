#include "MAPL_Generic.h"

! This module is intended to manage user-level access to ESMF info
! objects and thereby ensure consistent support for namespace
! management and such.

module mapl3g_InfoUtilities
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer

   use mapl3g_esmf_info_keys
   use esmf, only: ESMF_StateItem_Flag
   use esmf, only: ESMF_STATEITEM_FIELD
   use esmf, only: operator(==), operator(/=)
   use esmf, only: ESMF_Info
   use esmf, only: ESMF_InfoIsPresent
   use esmf, only: ESMF_InfoGetFromHost
   use esmf, only: ESMF_InfoGet
   use esmf, only: ESMF_InfoGetAlloc
   use esmf, only: ESMF_InfoGetCharAlloc
   use esmf, only: ESMF_InfoSet
   use esmf, only: ESMF_State
   use esmf, only: ESMF_StateGet
   use esmf, only: ESMF_Field
   use esmf, only: ESMF_KIND_I4

   implicit none
   private

   public :: MAPL_InfoGetShared
   public :: MAPL_InfoSetShared
   public :: MAPL_InfoGetPrivate
   public :: MAPL_InfoSetPrivate
   public :: MAPL_InfoGetInternal
   public :: MAPL_InfoSetInternal
   public :: MAPL_InfoSetNamespace

   interface MAPL_InfoGetShared
      procedure :: info_get_shared_string
      procedure :: info_get_shared_i4
      procedure :: info_get_state_shared_string
      procedure :: info_get_stateitem_shared_i4
   end interface MAPL_InfoGetShared

   interface MAPL_InfoSetShared
      procedure :: info_set_shared_string
      procedure :: info_set_shared_i4
      procedure :: info_set_state_shared_string
      procedure :: info_set_stateitem_shared_i4
   end interface MAPL_InfoSetShared
   
   interface MAPL_InfoGetPrivate
      procedure :: info_get_private_i4
      procedure :: info_get_stateitem_private_i4
   end interface MAPL_InfoGetPrivate
   
   interface MAPL_InfoSetPrivate
      procedure :: info_set_private_i4
       procedure :: info_set_stateitem_private_i4
   end interface MAPL_InfoSetPrivate

   interface MAPL_InfoGetInternal
      procedure :: info_get_internal_i4_1d
      procedure :: info_get_stateitem_internal_i4_1d
   end interface MAPL_InfoGetInternal

   interface MAPL_InfoSetInternal
      procedure :: info_set_internal_i4_1d
      procedure :: info_set_stateitem_internal_i4_1d
   end interface MAPL_InfoSetInternal

   interface MAPL_InfoSetNamespace
      procedure :: set_namespace
   end interface MAPL_InfoSetNamespace

contains

   ! Procedures that act directly on ESMF_Info object
   ! ------------------------------------------------

   ! Getters (namespace: shared)
   ! ---------------------------
   subroutine info_get_shared_string(info, key, value, unusable, rc)
      type(ESMF_Info), intent(in) :: info
      character(*), intent(in) :: key
      character(:), allocatable, intent(out) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_present

      is_present = ESMF_InfoIsPresent(info, key=KEY_SHARED//key, _RC)
      _ASSERT(is_present,  "Key not found in info object: " // key)

      call ESMF_InfoGetCharAlloc(info, key=KEY_SHARED//key, value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_get_shared_string

   subroutine info_get_shared_i4(info, key, value, rc)
      type(ESMF_Info), intent(in) :: info
      character(*), intent(in) :: key
      integer(kind=ESMF_KIND_I4), intent(out) :: value
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_present

      is_present = ESMF_InfoIsPresent(info, key=KEY_SHARED//key, _RC)
      _ASSERT(is_present,  "Key not found in info object: " // key)

      call ESMF_InfoGet(info, key=KEY_SHARED//key, value=value, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine info_get_shared_i4


   ! Setters (namespace: shared)
   ! ---------------------------
   subroutine info_set_shared_string(info, key, value, unusable, rc)
      type(ESMF_Info), intent(inout) :: info
      character(*), intent(in) :: key
      character(*), intent(in) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_InfoSet(info, key=KEY_SHARED // key, value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_set_shared_string

   subroutine info_set_shared_i4(info, key, value, unusable, rc)
      type(ESMF_Info), intent(inout) :: info
      character(*), intent(in) :: key
      integer(ESMF_KIND_I4), intent(in) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_InfoSet(info, key=KEY_SHARED // key, value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_set_shared_i4


   ! Getters (namespace: private)
   ! ----------------------------

   subroutine info_get_private_i4(info, key, value, unusable, rc)
      type(ESMF_Info), intent(in) :: info
      character(*), intent(in) :: key
      integer(kind=ESMF_KIND_I4), intent(out) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_InfoGet(info, key=KEY_PRIVATE//key, value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_get_private_i4

   ! Setters (namespace: private)
   ! ----------------------------
   subroutine info_set_private_i4(info, key, value, unusable, rc)
      type(ESMF_Info), intent(inout) :: info
      character(*), intent(in) :: key
      integer(ESMF_KIND_I4), intent(in) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_InfoSet(info, key=KEY_PRIVATE//key, value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_set_private_i4


   ! Getters (namespace: internal)
   ! -----------------------------
      subroutine info_get_internal_i4_1d(info, key, values, unusable, rc)
      type(ESMF_Info), intent(in) :: info
      character(*), intent(in) :: key
      integer, allocatable, intent(out) :: values(:)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_present

      is_present = ESMF_InfoIsPresent(info, key=KEY_INTERNAL//key, _RC)
      _ASSERT(is_present,  "Key not found in info object: " // KEY_INTERNAL // key)

      call ESMF_InfoGetAlloc(info, key=KEY_INTERNAL//key, values=values, scalarToArray=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine info_get_internal_i4_1d

   ! Setters (namespace: internal)
   ! ----------------------------

   subroutine info_set_internal_i4_1d(info, key, values, unusable, rc)
      type(ESMF_Info), intent(inout) :: info
      character(*), intent(in) :: key
      integer, intent(in) :: values(:)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_InfoSet(info, key=KEY_INTERNAL//key, values=values, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_set_internal_i4_1d


   ! Accessors on ESMF_State objects
   ! ------------------------------

   subroutine info_get_state_shared_string(state, key, value, unusable, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: key
      character(:), allocatable, intent(out) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: state_info

      call ESMF_InfoGetFromHost(state, state_info, _RC)
      call MAPL_InfoGetShared(state_info, key=key, value=value, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine info_get_state_shared_string

   subroutine info_set_state_shared_string(state, key, value, unusable, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: key
      character(*), intent(in) :: value
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: state_info

      call ESMF_InfoGetFromHost(state, state_info, _RC)
      call MAPL_InfoSetShared(state_info, key=key, value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_set_state_shared_string


   ! Accessors for state items (extra arg for name)
   ! ----------------------------------------------

   subroutine info_get_stateitem_shared_i4(state, short_name, key, value, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      integer(kind=ESMF_KIND_I4), intent(out) :: value
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call info_get_stateitem_info(state, short_name, info, _RC)
      call MAPL_InfoGetShared(info, key=key, value=value, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine info_get_stateitem_shared_i4

   subroutine info_set_stateitem_shared_i4(state, short_name, key, value, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      integer(kind=ESMF_KIND_I4), intent(in) :: value
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call info_get_stateitem_info(state, short_name, info, _RC)
      call MAPL_InfoSetShared(info, key=key, value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_set_stateitem_shared_i4

   subroutine info_get_stateitem_private_i4(state, short_name, key, value, rc)
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
      
      call info_get_stateitem_info(state, short_name, item_info, _RC)
      private_key = namespace // '/' // key 
      call MAPL_InfoGetPrivate(item_info, key=private_key, value=value, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine info_get_stateitem_private_i4


   subroutine info_set_stateitem_private_i4(state, short_name, key, value, rc)
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
      
      call info_get_stateitem_info(state, short_name, item_info, _RC)
      private_key = namespace // '/' // key
      call MAPL_InfoSetPrivate(item_info, key=private_key, value=value, _RC)

      _RETURN(_SUCCESS)
   end subroutine info_set_stateitem_private_i4


   ! Internal 

   subroutine info_get_stateitem_internal_i4_1d(state, short_name, key, values, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      integer(kind=ESMF_KIND_I4), allocatable, intent(out) :: values(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call info_get_stateitem_info(state, short_name, info, _RC)
      call MAPL_InfoGetInternal(info, key=key, values=values, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine info_get_stateitem_internal_i4_1d



  subroutine info_set_stateitem_internal_i4_1d(state, short_name, key, values, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      character(*), intent(in) :: key
      integer(kind=ESMF_KIND_I4), intent(in) :: values(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call info_get_stateitem_info(state, short_name, info, _RC)
      call MAPL_InfoSetInternal(info, key=key, values=values, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine info_set_stateitem_internal_i4_1d


   ! private helper procedure
   subroutine info_get_stateitem_info(state, short_name, info, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: short_name
      type(ESMF_Info), intent(out) :: info
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_StateItem_Flag) :: itemType
      type(ESMF_Field) :: field

      call ESMF_StateGet(state, itemName=short_name, itemType=itemType, _RC)
      if (itemType == ESMF_STATEITEM_FIELD) then
         call ESMF_StateGet(state, itemName=short_name, field=field, _RC)
         call ESMF_InfoGetFromHost(field, info, _RC)
      else
         _FAIL('unsupported state item type')
      end if

      _RETURN(_SUCCESS)
   end subroutine info_get_stateitem_info


   subroutine get_namespace(state, namespace, rc)
      type(ESMF_State), intent(in) :: state
      character(:), allocatable, intent(out) :: namespace
      integer, optional, intent(out) :: rc

      type(ESMF_Info) :: state_info
      integer :: status

      call MAPL_InfoGetShared(state, key='namespace', value=namespace, _RC)

      _RETURN(_SUCCESS)
   end subroutine get_namespace

   subroutine set_namespace(state, namespace, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: namespace
      integer, optional, intent(out) :: rc

      type(ESMF_Info) :: state_info
      integer :: status

      call MAPL_InfoSetShared(state, key='namespace', value=namespace, _RC)

      _RETURN(_SUCCESS)
   end subroutine set_namespace

end module mapl3g_InfoUtilities
