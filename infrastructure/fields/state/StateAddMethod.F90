#include "MAPL.h"

module mapl3g_CallbackMap_private
   use mapl3g_ESMF_Interfaces, only: I_CallBackMethod
   implicit none(type,external)
   private

   public :: CallbackMethodWrapper

   type :: CallbackMethodWrapper
      procedure(I_CallBackMethod), pointer, nopass :: userRoutine
   end type CallbackMethodWrapper

end module mapl3g_CallbackMap_private

module mapl3g_CallbackMap
   use mapl3g_CallbackMap_private, only: CallbackMethodWrapper

#define Key __CHARACTER_DEFERRED
#define T CallbackMethodWrapper
#define Map CallbackMap
#define Pair CallbackPair
#define MapIterator CallbackMapIterator

#include "map/template.inc"

#undef MapIterator
#undef Pair
#undef Map
#undef T
#undef Key

end module mapl3g_CallbackMap

module mapl3g_StateAddMethod
   use ESMF, only: ESMF_State, ESMF_MethodAdd
   use ESMF, only: ESMF_Info, ESMF_InfoGetFromHost, ESMF_InfoIsPresent, ESMF_InfoSet, ESMF_InfoGet
   use ESMF, only: ESMF_KIND_I4
   use mapl3g_CallbackMap
   use mapl3g_ESMF_Interfaces, only: I_CallBackMethod
   use mapl3g_CallbackMap_private, only: CallbackMethodWrapper
   use mapl_ErrorHandling
   implicit none(type,external)
   private

   public :: mapl_StateAddMethod
   public :: CallbackMap
   public :: CallbackMapIterator
   public :: CallbackMethodWrapper
   public :: get_callbacks
   public :: operator(/=)

   interface mapl_StateAddMethod
      procedure :: state_add_method
   end interface mapl_StateAddMethod

contains

   subroutine state_add_method(state, label, userRoutine, rc)
      type(ESMF_State), intent(inout) :: state
      character(len=*), intent(in) :: label
      procedure(I_CallBackMethod) :: userRoutine
      integer, optional, intent(out) :: rc

      integer :: status
      type(CallbackMap), pointer :: callbacks
      type(CallbackMethodWrapper) :: wrapper

      call get_callbacks(state, callbacks, _RC)
      wrapper%userRoutine => userRoutine
      call callbacks%insert(label, wrapper)
      call ESMF_MethodAdd(state, label=label, userRoutine=userRoutine, _RC)

      _RETURN(_SUCCESS)
   end subroutine state_add_method

   subroutine get_callbacks(state, callbacks, rc)
      type(ESMF_State), intent(inout) :: state
      type(CallbackMap), pointer, intent(out) :: callbacks
      integer, optional, intent(out) :: rc

      integer :: status
      integer(kind=ESMF_KIND_I4), allocatable :: valueList(:)
      logical :: isPresent
      type(ESMF_Info) :: infoh

      type :: CallbackMapWrapper
         type(CallbackMap), pointer :: map
      end type CallbackMapWrapper
      type(CallbackMapWrapper) :: wrapper

      call ESMF_InfoGetFromHost(state, infoh, _RC)
      isPresent = ESMF_InfoIsPresent(infoh, 'MAPL_CALLBACK_MAP', _RC)
      if (.not. isPresent) then ! create callback map for this state
         allocate(callbacks)
         wrapper%map => callbacks
         valueList = transfer(wrapper, [1])
         call ESMF_InfoSet(infoh, key='MAPL_CALLBACK_MAP', values=valueList, _RC)
      end if

      ! Ugly hack to decode ESMF attribute as a gFTL map
      valueList = transfer(wrapper, [1])
      call ESMF_InfoGet(infoh, key='MAPL_CALLBACK_MAP', values=valueList, _RC)
      wrapper = transfer(valueList, wrapper)
      callbacks => wrapper%map

      _RETURN(_SUCCESS)
   end subroutine get_callbacks

end module mapl3g_StateAddMethod
