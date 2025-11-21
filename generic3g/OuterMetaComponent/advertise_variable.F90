#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) advertise_var_spec_smod
   use mapl3g_Field_API
   use mapl3g_VariableSpec
   use mapl3g_StateItemSpec
   use mapl3g_StateItemExtension
   use mapl3g_VirtualConnectionPt
   use mapl_ErrorHandling
   implicit none(type,external)

contains

   module subroutine advertise_variable(this, var_spec, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      type(VariableSpec), intent(in) :: var_spec
      integer, optional, intent(out) :: rc
      
      integer :: status
      type(StateItemSpec), target :: item_spec
      type(StateItemSpec), pointer :: item_spec_ptr
      type(StateItemExtension), pointer :: item_extension
      type(VirtualConnectionPt) :: virtual_pt
      
      item_spec = var_spec%make_StateItemSpec(this%registry, &
           this%geom, this%vertical_grid, timestep=this%user_timestep, offset=this%user_offset, _RC)
      virtual_pt = var_spec%make_virtualPt()
      call this%registry%add_primary_spec(virtual_pt, item_spec)
      
      item_extension => this%registry%get_primary_extension(virtual_pt, _RC)
      item_spec_ptr => item_extension%get_spec() 

      call item_spec_ptr%create(_RC)
      call set_default_activation(item_spec_ptr, var_spec%state_intent, _RC)

      _RETURN(_SUCCESS)

   contains

      subroutine set_default_activation(item_spec, state_intent, rc)
         type(StateItemSpec), intent(inout) :: item_spec
         type(esmf_StateIntent_Flag), intent(in) :: state_intent
         integer, optional, intent(out) :: rc
         
         integer :: status

         if (state_intent == ESMF_STATEINTENT_EXPORT) then
            if (this%component_spec%misc%activate_all_exports) then
               call item_spec%activate(_RC)
            end if
         end if
         
         if (state_intent == ESMF_STATEINTENT_IMPORT) then
            if (this%component_spec%misc%activate_all_imports) then
               call item_spec%activate(_RC)
            end if
         end if
         
         if (state_intent == ESMF_STATEINTENT_INTERNAL) then
            call item_spec%activate(_RC)
         end if
         
         _RETURN(_SUCCESS)
      end subroutine set_default_activation

   end subroutine advertise_variable

end submodule advertise_var_spec_smod
