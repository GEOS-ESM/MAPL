#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module NUOPC_HistoryCapMod
   use ESMF
   use NUOPC
    use NUOPC_Model, &
    modelSS    => SetServices

   use FieldRegistryMod
   use HistoryCapMod
   use NUOPCmapMod

   implicit none
   private

   public SetServices
   public initialize_HistoryCap_wrapper

   character(*), parameter :: internal_name = 'NUOPC_HistoryCap'

   type :: HistoryCap_wrapper
      type(HistoryCap), pointer :: ptr
   end type HistoryCap_wrapper
contains
   subroutine SetServices(model, rc)
      type(ESMF_GridComp)  :: model
      integer, intent(out) :: rc

      integer :: i

      rc = ESMF_SUCCESS

      call NUOPC_CompDerive(model, ModelSS, rc=rc)
      VERIFY_NUOPC_(rc)

      ! attach specializing method(s)
      call NUOPC_CompSpecialize(model, specLabel=label_Advertise, &
         specRoutine=advertise, rc=rc)
      VERIFY_NUOPC_(rc)
      call NUOPC_CompSpecialize(model, specLabel=label_RealizeProvided, &
         specRoutine=realize, rc=rc)
      VERIFY_NUOPC_(rc)
      call NUOPC_CompSpecialize(model, specLabel=label_DataInitialize, &
         specRoutine=initialize_data, rc=rc)
      VERIFY_NUOPC_(rc)
      call NUOPC_CompSpecialize(model, specLabel=label_Advance, &
         specRoutine=advance, rc=rc)
      VERIFY_NUOPC_(rc)
      call ESMF_MethodRemove(model, label=label_CheckImport, rc=rc)
      VERIFY_NUOPC_(rc)
      call NUOPC_CompSpecialize(model, specLabel=label_CheckImport, &
         specRoutine=check_import, rc=rc)
      VERIFY_NUOPC_(rc)

      call NUOPC_CompSpecialize(model, specLabel=label_SetClock, &
         specRoutine=set_clock, rc=rc)
      VERIFY_NUOPC_(rc)
      call NUOPC_CompSpecialize(model, specLabel=label_Finalize, &
         specRoutine=finalize, rc=rc)
      VERIFY_NUOPC_(rc)
   end subroutine SetServices

   subroutine initialize_p0(model, rc)
      type(ESMF_GridComp)  :: model
      !type(ESMF_State)     :: import_state
      !type(ESMF_State)     :: export_state
      !type(ESMF_Clock)     :: clock
      integer, intent(out) :: rc

      type(HistoryCap), pointer :: cap

      rc = ESMF_SUCCESS

      cap => get_HistoryCap(model, rc)
      VERIFY_NUOPC_(rc)

      call cap%init_p0(model, rc)
      VERIFY_NUOPC_(rc)
   end subroutine initialize_p0

   subroutine advertise(model, rc)
      type(ESMF_GridComp)  :: model
      integer, intent(out) :: rc

      type(HistoryCap), pointer :: cap

      rc = ESMF_SUCCESS

      call initialize_p0(model,rc=rc)
      VERIFY_NUOPC_(rc)
      cap => get_HistoryCap(model, rc)
      VERIFY_NUOPC_(rc)

      call cap%advertise(model, rc)
      VERIFY_NUOPC_(rc)
   end subroutine advertise

   subroutine realize(model, rc)
      type(ESMF_GridComp)  :: model
      integer, intent(out) :: rc

      type(HistoryCap), pointer :: cap

      rc = ESMF_SUCCESS

      cap => get_HistoryCap(model, rc)
      VERIFY_NUOPC_(rc)

      call cap%realize(model, rc)
      VERIFY_NUOPC_(rc)
   end subroutine realize

   subroutine initialize_data(model, rc)
      type(ESMF_GridComp)  :: model
      integer, intent(out) :: rc

      type(HistoryCap), pointer :: cap

      rc = ESMF_SUCCESS

      cap => get_HistoryCap(model, rc)
      VERIFY_NUOPC_(rc)

      call cap%data_init(model, rc)
      VERIFY_NUOPC_(rc)
   end subroutine initialize_data

   subroutine advance(model, rc)
      type(ESMF_GridComp)  :: model
      integer, intent(out) :: rc

      type(HistoryCap), pointer :: cap

      rc = ESMF_SUCCESS

      cap => get_HistoryCap(model, rc)
      VERIFY_NUOPC_(rc)

      call cap%advance(rc)
      VERIFY_NUOPC_(rc)
   end subroutine advance

   subroutine check_import(model, rc)
      type(ESMF_GridComp)  :: model
      integer, intent(out) :: rc

      type(HistoryCap), pointer :: cap

      rc = ESMF_SUCCESS

      cap => get_HistoryCap(model, rc)
      VERIFY_NUOPC_(rc)

      call cap%check_import(rc)
      VERIFY_NUOPC_(rc)
   end subroutine check_import

   subroutine set_clock(model, rc)
      type(ESMF_GridComp)  :: model
      integer, intent(out) :: rc

      type(HistoryCap), pointer :: cap

      rc = ESMF_SUCCESS

      cap => get_HistoryCap(model, rc)
      VERIFY_NUOPC_(rc)

      call cap%set_clock(model, rc)
      VERIFY_NUOPC_(rc)
   end subroutine set_clock

   subroutine finalize(model, rc)
      type(ESMF_GridComp)  :: model
      integer, intent(out) :: rc

      type(HistoryCap), pointer :: cap

      rc = ESMF_SUCCESS

      cap => get_HistoryCap(model, rc)
      VERIFY_NUOPC_(rc)

      call cap%finalize(rc)
      VERIFY_NUOPC_(rc)
   end subroutine finalize

   function get_HistoryCap(gc, rc) result(cap)
      type(ESMF_GridComp), intent(inout) :: gc
      integer,             intent(  out) :: rc
      type(HistoryCap),    pointer       :: cap

      type(HistoryCap_wrapper) :: wrapper

      rc = ESMF_SUCCESS

      call ESMF_UserCompGetInternalState(gc, internal_name, wrapper, rc)
      VERIFY_NUOPC_(rc)

      cap => wrapper%ptr
   end function get_HistoryCap

   subroutine initialize_HistoryCap_wrapper(gc, name, root_rc, set_services, registry, disable_throughput, rc)
      type(ESMF_GridComp), target,        intent(inout) :: gc
      character(*),                       intent(in   ) :: name
      character(*),                       intent(in   ) :: root_rc
      procedure(i_set_services), pointer, intent(in   ) :: set_services
      type(FieldRegistry),                intent(in   ) :: registry
      logical,                            intent(in   ) :: disable_throughput
      integer,                            intent(  out) :: rc

      type(HistoryCap)         :: cap
      type(HistoryCap_wrapper) :: wrapper

      rc = ESMF_SUCCESS

      call cap%initialize(name, root_rc, set_services, registry, disable_throughput)

      allocate(wrapper%ptr)
      wrapper%ptr = cap

      call ESMF_UserCompSetInternalState(gc, internal_name, wrapper, rc)
      VERIFY_NUOPC_(rc)
   end subroutine initialize_HistoryCap_wrapper
end module NUOPC_HistoryCapMod
