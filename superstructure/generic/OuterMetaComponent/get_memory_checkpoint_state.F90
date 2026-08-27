#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) get_memory_checkpoint_state_smod

   use mapl_ErrorHandling_mod

   implicit none(type,external)

contains

   ! Retrieve the nested ESMF_State within this%memory_checkpoint
   ! corresponding to state_intent. Caller must ensure
   ! this%memory_checkpoint has already been created (e.g. via
   ! this%has_memory_checkpoint) before calling.
   module subroutine get_memory_checkpoint_state_(this, state_intent, state, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      type(ESMF_State), intent(out) :: state
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: item_name

      if (state_intent == ESMF_STATEINTENT_IMPORT) then
         item_name = "import"
      else if (state_intent == ESMF_STATEINTENT_EXPORT) then
         item_name = "export"
      else if (state_intent == ESMF_STATEINTENT_INTERNAL) then
         item_name = "internal"
      else
         _FAIL('Unsupported state intent for memory checkpoint.')
      end if

      call ESMF_StateGet(this%memory_checkpoint, itemName=item_name, nestedState=state, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine get_memory_checkpoint_state_

end submodule get_memory_checkpoint_state_smod
