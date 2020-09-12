#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module NUOPC_MAPLfields_Mod
   use ESMF
   use NUOPC
   use NUOPC_Model
   use MAPL_Mod
   use MAPL_CapGridCompMod
   use MAPL_Profiler, only: BaseProfiler, get_global_time_profiler

   implicit none
   private

   type :: FieldAttributes
      type(ESMF_Field)              :: field
      character(len=:), allocatable :: name
      character(len=:), allocatable :: long_name
      character(len=:), allocatable :: units
   contains
      procedure :: advertise_to_state
      procedure :: realize_to_import_state
      procedure :: realize_to_export_state
   end type FieldAttributes
contains
   subroutine advertise_to_state(this, state, rc)
      class(FieldAttributes), intent(inout) :: this
      type(ESMF_State),       intent(inout) :: state
      integer,                intent(  out) :: rc

      rc = ESMF_SUCCESS

      if (.not. NUOPC_FieldDictionaryHasEntry(this%name)) then
         call NUOPC_FieldDictionaryAddEntry(standardName=trim(this%name), &
               canonicalUnits=trim(this%units), rc=rc)
         VERIFY_NUOPC_(rc)
      end if

      call NUOPC_Advertise(state, standardName=trim(this%name), &
            TransferOfferGeomObject="will provide", rc=rc)
      VERIFY_NUOPC_(rc)
   end subroutine advertise_to_state

   subroutine realize_to_import_state(this, state, rc)
      class(FieldAttributes), intent(inout) :: this
      type(ESMF_State),       intent(inout) :: state
      integer,                intent(  out) :: rc

      rc = ESMF_SUCCESS

      call ESMF_FieldValidate(this%field, rc=rc)
      VERIFY_NUOPC_(rc)
      call NUOPC_Realize(state, field=this%field, rc=rc)
      VERIFY_NUOPC_(rc)
   end subroutine realize_to_import_state

   subroutine realize_to_export_state(this, state, rc)
      class(FieldAttributes), intent(inout) :: this
      type(ESMF_State),       intent(inout) :: state
      integer,                intent(  out) :: rc

      rc = ESMF_SUCCESS

      call MAPL_AllocateCoupling(this%field, rc=rc)
      VERIFY_NUOPC_(rc)
      call NUOPC_Realize(state, field=this%field, rc=rc)
      VERIFY_NUOPC_(rc)
   end subroutine realize_to_export_state
end module NUOPC_MAPLfields_Mod
