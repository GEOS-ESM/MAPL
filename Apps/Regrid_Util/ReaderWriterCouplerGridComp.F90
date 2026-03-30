#include "MAPL.h"

module mapl3g_ReaderWriterCouplerGridComp

   use mapl3
   use mapl_ErrorHandling
   use mapl3g_Generic, only: MAPL_GridCompSetEntryPoint
   use mapl3g_ConfigurableReaderGridComp, only: reader_setServices => setServices
   use mapl3g_ConfigurableWriterGridComp, only: writer_setServices => setServices
   use esmf
   use gFTL2_StringVector

   implicit none
   private

   public :: setServices

   !private state
   type :: ReaderWriteCouplerGridComp
     type(StringVector) :: input_files
     type(StringVector) :: output_files
   end type
   character(*), parameter :: PRIVATE_STATE = "ReaderWriterCoupler"

contains

   subroutine setServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      type(ChildSpec) :: writer_spec, reader_spec
      integer :: status,i 
      type(ESMF_HConfig) :: hconfig, child_hconfig
      character(len=4) :: str
      type(ReaderWriteCouplerGridComp), pointer :: readerwritercoupler_gridcomp

      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init, _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name="run", _RC)
      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)

      _SET_NAMED_PRIVATE_STATE(gridcomp, ReaderWriteCouplerGridComp, PRIVATE_STATE)
      _GET_NAMED_PRIVATE_STATE(gridcomp, ReaderWriteCouplerGridComp, PRIVATE_STATE, readerwritercoupler_gridcomp)

      readerwritercoupler_gridcomp%input_files = mapl_HConfigAsStringVector(hconfig,keyString='input_files', _RC)
      readerwritercoupler_gridcomp%output_files = mapl_HConfigAsStringVector(hconfig,keyString='output_files', _RC)
      do i=1,readerwritercoupler_gridcomp%input_files%size()
         write(str, '(I4.4)')i
         child_hconfig = ESMF_HConfigCreate(hconfig, _RC)
         call ESMF_HConfigAdd(child_hconfig, readerwritercoupler_gridcomp%input_files%at(i), addKeyString='input_file', _RC)
         reader_spec = ChildSpec(user_setServices(reader_setServices),hconfig=child_hconfig)
         call MAPL_GridCompAddChild(gridcomp, 'reader_'//str, reader_spec, _RC)
      enddo
      do i=1,readerwritercoupler_gridcomp%output_files%size()
         write(str, '(I4.4)')i
         child_hconfig = ESMF_HConfigCreate(hconfig, _RC)
         call ESMF_HConfigAdd(child_hconfig, readerwritercoupler_gridcomp%input_files%at(i), addKeyString='input_file', _RC)
         call ESMF_HConfigAdd(child_hconfig, readerwritercoupler_gridcomp%output_files%at(i), addKeyString='output_file', _RC)
         writer_spec = ChildSpec(user_setServices(writer_setServices),hconfig=child_hconfig)
         call MAPL_GridCompAddChild(gridcomp, 'writer_'//str, writer_spec, _RC)
      enddo

      _RETURN(_SUCCESS)
   end subroutine setServices

   subroutine init(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(gridcomp)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine init

   recursive subroutine run(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status, i
      type(ReaderWriteCouplerGridComp), pointer :: readerwritercoupler_gridcomp

      _HERE,' bmaa coupler run'
      _GET_NAMED_PRIVATE_STATE(gridcomp, ReaderWriteCouplerGridComp, PRIVATE_STATE, readerwritercoupler_gridcomp)
      ! you need to run each reader/writer for each time in the file and set the clock I guess
      ! so that the clock is the time you want to write I guess
      ! can we do that?
      do i=1, readerwritercoupler_gridcomp%input_files%size()
         call MAPL_GridCompRunChildren(gridcomp,phase_name='run', _RC)
         call MAPL_GridCompRunChildren(gridcomp,phase_name='run', _RC)
      enddo
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(gridcomp)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine run

end module mapl3g_ReaderWriterCouplerGridComp
