#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) ensure_memory_checkpoint_smod

   use mapl_ErrorHandling_mod

   implicit none(type,external)

contains

   ! Lazily create this%memory_checkpoint as an ESMF_State containing
   ! three nested ESMF_States named "import", "export", "internal",
   ! mirroring MultiState's naming convention. No-op if already created.
   module subroutine ensure_memory_checkpoint_(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_State) :: import_state, export_state, internal_state
      logical :: is_created

      is_created = ESMF_StateIsCreated(this%memory_checkpoint)
      if (is_created) then
         _RETURN(ESMF_SUCCESS)
      end if

      this%memory_checkpoint = ESMF_StateCreate(name="memory_checkpoint", _RC)

      import_state = ESMF_StateCreate(name="import", _RC)
      export_state = ESMF_StateCreate(name="export", _RC)
      internal_state = ESMF_StateCreate(name="internal", _RC)

      call ESMF_StateAdd(this%memory_checkpoint, [import_state], _RC)
      call ESMF_StateAdd(this%memory_checkpoint, [export_state], _RC)
      call ESMF_StateAdd(this%memory_checkpoint, [internal_state], _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine ensure_memory_checkpoint_

end submodule ensure_memory_checkpoint_smod
