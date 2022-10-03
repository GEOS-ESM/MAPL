#include "MAPL_ErrLog.h"

module mapl3g_GenericCouplerComponent
   use :: esmf, only: ESMF_CplComp
   use :: esmf, only: ESMF_CplCompRun
   use :: esmf, only: ESMF_State
   use :: esmf, only: ESMF_Clock
   use :: esmf, only: ESMF_SUCCESS
   use :: mapl3g_ChildComponent
   use :: mapl_ErrorHandling
   implicit none
   private

   public :: GenericCouplerComponent

   type :: CouplerMeta
      type(CouplerTaskVector) :: tasks
   contains
      procedure :: initialize
      procedure :: run
      procedure :: finalize
      procedure :: add_task
   end type CouplerMeta

   type :: GenericCouplerComponent
      type(ESMF_CplComp) :: cplcomp
      type(ESMF_State) :: importState ! export of child I
      type(ESMF_State) :: exportState ! import of child J

      type(CouplerItemVector) :: actions

   contains
      procedure, private :: run_self
      generic :: run => run_self
   end type GenericCouplerComponent

   generic :: CouplerMeta => new_CouplerMeta

contains

   subroutine SetServices(cplcomp, rc)
      type(ESMF_CplComp) :: cplcomp
      integer, intent(out) :: rc

      call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_INITIALIZE, initialize, _RC)
      call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_INITIALIZE, run, _RC)
      call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_INITIALIZE, finalize, _RC)

   end subroutine SetServices

   subroutine initialize(cplcomp, import_state, export_state, clock, rc)

      meta => get_meta(cplcomp, _RC)
      do i = 1, meta%tasks%size()
         task => meta%tasks%of(i)
         call task%initialize(import_state, export_state, _RC)
      end do

      _RETURN(_ESMF_SUCCESS)
   end subroutine initialize

   subroutine run(cplcomp, import_state, export_state, clock, rc)

      meta => get_meta(cplcomp, _RC)
      do i = 1, meta%tasks%size()
         task => meta%tasks%of(i)
         call task%run(import_state, export_state, _RC)
      end do

      _RETURN(_ESMF_SUCCESS)
   end subroutine run

   function new_CouplerMeta(tasks) result(meta)
      type(CouplerMeta) :: meta
      type(CouplerTask), intent(in) :: tasks

      meta%tasks = tasks

   end function new_CouplerMeta

   subroutine add_task(this, task)
      class(CouplerMeta), intent(inout) :: this
      call this%tasks%push_back(task)
   end subroutine add_task


end module mapl3g_GenericCouplerComponent
