#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module HistoryWriter_GridComp
   use ESMF
   use NUOPC
   !use NUOPC_ModelBase
   use NUOPC_Model, modelSS    => SetServices
   use MAPL_LatLonGridFactoryMod
   use MAPL_BaseMod
   use HistoryWriterMod
   use CollectionMod
  
  implicit none
  
  private
  
  public :: SetServices
  public :: initialize_WriterGC_wrapper

  character(*), parameter :: internal_name = 'NUOPC_HistoryWriter_GridComp'

   type :: HistoryWriter_wrapper
      type(HistoryWriter), pointer :: ptr
   end type HistoryWriter_wrapper

  contains

  subroutine SetServices(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS
    
    call NUOPC_CompDerive(model, modelSS, rc=rc)
    VERIFY_NUOPC_(rc)
   
    ! specialize model
    call NUOPC_CompSpecialize(model, specLabel=label_Advertise, &
      specRoutine=Advertise, rc=rc)
    VERIFY_NUOPC_(rc)
    call NUOPC_CompSpecialize(model, specLabel=label_RealizeAccepted, &
      specRoutine=RealizeAccepted, rc=rc)
    VERIFY_NUOPC_(rc)
    call NUOPC_CompSpecialize(model, specLabel=label_RealizeProvided, &
      specRoutine=RealizeProvided, rc=rc)
    VERIFY_NUOPC_(rc)
    call NUOPC_CompSpecialize(model, specLabel=label_AcceptTransfer, &
      specRoutine=acceptTransfer, rc=rc)
    VERIFY_NUOPC_(rc)

    !call NUOPC_CompSpecialize(model, specLabel=label_SetClock, &
       !specRoutine=set_clock, rc=rc)
    !VERIFY_NUOPC_(rc)
    call ESMF_MethodRemove(model, label=label_CheckImport, rc=rc)
    VERIFY_NUOPC_(rc)
     call NUOPC_CompSpecialize(model, specLabel=label_CheckImport, &
       specRoutine=writer_CheckImport, rc=rc)
    VERIFY_NUOPC_(rc)



    call NUOPC_CompSpecialize(model, specLabel=label_Advance, &
      specRoutine=write_collection, rc=rc)
    VERIFY_NUOPC_(rc)
    
  end subroutine

  subroutine initialize_WriterGC_Wrapper(gc,hist_collection,rc)
      type(ESMF_GridComp), target,        intent(inout) :: gc
      type(Collection),                   intent(in   ) :: hist_collection
      integer,                            intent(  out) :: rc

      type(HistoryWriter) :: writer
      type(HistoryWriter_wrapper) :: wrapper

      rc = ESMF_SUCCESS
      call writer%initialize(hist_collection)

      allocate(wrapper%ptr)
      wrapper%ptr = writer

      call ESMF_UserCompSetInternalState(gc, internal_name, wrapper, rc)
      VERIFY_NUOPC_(rc)

  end subroutine 

   function get_HistoryWriter(gc, rc) result(writer)
      type(ESMF_GridComp), intent(inout) :: gc
      integer,             intent(  out) :: rc
      type(HistoryWriter), pointer       :: writer

      type(HistoryWriter_wrapper) :: wrapper

      rc = ESMF_SUCCESS

      call ESMF_UserCompGetInternalState(gc, internal_name, wrapper, rc)
      VERIFY_NUOPC_(rc)

      writer => wrapper%ptr

   end function get_HistoryWriter

    subroutine writer_CheckImport(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

        ! This is the routine that enforces the implicit time dependence on the
        ! import fields. This simply means that the timestamps on the Fields in the
        ! importState are checked against the stopTime on the Component's
        ! internalClock. Consequenty, this model starts out with forcing fields
        ! at the future stopTime, as it does its forward stepping from currentTime
        ! to stopTime.


        rc=ESMF_SUCCESS
       VERIFY_NUOPC_(rc)
    end subroutine writer_CheckImport

   !subroutine set_clock(model, rc)
      !type(ESMF_GridComp)  :: model
      !integer, intent(out) :: rc
!
      !type(ESMF_TimeInterval) :: time_step
      !type(ESMF_Clock) :: model_clock
      !type(ESMF_Time) :: ctime
!
      !rc = ESMF_SUCCESS
!
      !call ESMF_TimeIntervalSet(time_step, s=900, rc=rc)
      !VERIFY_NUOPC_(rc)
!
      !! set clock with time interval
      !call NUOPC_ModelGet(model, modelClock=model_clock, rc=rc)
      !VERIFY_NUOPC_(rc)
      !call ESMF_ClockSet(model_clock, timeStep=time_step, rc=rc)
      !VERIFY_NUOPC_(rc)
      !call ESMF_ClockGet(model_clock,currtime=ctime)
      !call ESMF_TimePrint(ctime,options='string')
      !VERIFY_NUOPC_(rc)
   !end subroutine set_clock

 subroutine Advertise(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    type(HistoryWriter), pointer :: writer

    rc = ESMF_SUCCESS
  
    writer => get_HistoryWriter(model,rc)
    VERIFY_NUOPC_(rc)

    call writer%advertise(model,rc)
    VERIFY_NUOPC_(rc)
    
  end subroutine

  subroutine realizeAccepted(model,rc)
      type(ESMF_GridComp)               :: model
      integer,            intent(  out) :: rc
      type(HistoryWriter), pointer :: writer

      rc = ESMF_SUCCESS
     
      writer => get_HistoryWriter(model,rc)
      VERIFY_NUOPC_(rc)

      call writer%realizeAccepted(model,rc)
      VERIFY_NUOPC_(rc)

   end subroutine realizeAccepted

  subroutine realizeProvided(model,rc)
      type(ESMF_GridComp)               :: model
      integer,            intent(  out) :: rc

      rc = ESMF_SUCCESS
     
   end subroutine realizeProvided

  subroutine acceptTransfer(model,rc)
      type(ESMF_GridComp)               :: model
      integer,            intent(  out) :: rc
      type(HistoryWriter), pointer :: writer

      rc = ESMF_SUCCESS
     
      writer => get_HistoryWriter(model,rc)
      VERIFY_NUOPC_(rc)

      call writer%acceptTransfer(model,rc)
      VERIFY_NUOPC_(rc)

   end subroutine acceptTransfer

  subroutine write_collection(model,rc)
      type(ESMF_GridComp)               :: model
      integer,            intent(  out) :: rc
      type(HistoryWriter), pointer :: writer

      rc = ESMF_SUCCESS
     
      writer => get_HistoryWriter(model,rc)
      VERIFY_NUOPC_(rc)

      call writer%write_collection(model,rc)
      VERIFY_NUOPC_(rc)

   end subroutine write_collection

end module HistoryWriter_GridComp
