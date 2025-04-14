#include "MAPL_Generic.h"

module mapl3g_StateGet

   use mapl_ErrorHandling
   use esmf

   implicit none
   private

   public :: StateGet

   interface StateGet
      procedure :: state_get
   end interface StateGet

   ! Submodule interfaces
   interface
      module function get_bundle_from_state(state, rc) result(bundle)
         type(ESMF_State), intent(in) :: state
         integer, optional, intent(out) :: rc
         type(ESMF_FieldBundle) :: bundle ! result
      end function get_bundle_from_state
   end interface

contains

   subroutine state_get(state, bundle, rc)
      type(ESMF_State), intent(in) :: state
      type(ESMF_FieldBundle), optional, intent(out) :: bundle
      integer, optional, intent(out) :: rc

      integer :: status

      if (present(bundle)) bundle = get_bundle_from_state(state, _RC)

      _RETURN(_SUCCESS)
   end subroutine state_get

end module mapl3g_StateGet
