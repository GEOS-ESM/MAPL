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

contains

   subroutine setServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      type(ChildSpec) :: writer_spec, reader_spec
      integer :: status,i 
      type(ESMF_HConfig) :: hconfig, child_hconfig
      type(StringVector) :: input_files, output_files
      character(len=4) :: str

      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init, _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name="run", _RC)
      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)
      input_files = mapl_HConfigAsStringVector(hconfig,keyString='input_files', _RC)
      output_files = mapl_HConfigAsStringVector(hconfig,keyString='output_files', _RC)
      do i=1,input_files%size()
         write(str, '(I4.4)')i
         child_hconfig = ESMF_HConfigCreate(hconfig, _RC)
         call ESMF_HConfigAdd(child_hconfig, input_files%at(i), addKeyString='input_file', _RC)
         reader_spec = ChildSpec(user_setServices(reader_setServices),hconfig=child_hconfig)
         call MAPL_GridCompAddChild(gridcomp, 'reader_'//str, reader_spec, _RC)
      enddo
      do i=1,output_files%size()
         write(str, '(I4.4)')i
         child_hconfig = ESMF_HConfigCreate(hconfig, _RC)
         call ESMF_HConfigAdd(child_hconfig, input_files%at(i), addKeyString='input_file', _RC)
         call ESMF_HConfigAdd(child_hconfig, output_files%at(i), addKeyString='output_file', _RC)
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

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(gridcomp)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine run

end module mapl3g_ReaderWriterCouplerGridComp
