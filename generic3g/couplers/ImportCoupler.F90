module mapl3g_ImportCoupler
   use mapl3g_GenericCoupler
   implicit none (type, external)
   private

   public :: ImportCoupler

   type, extends :: GenericCoupler
   contains
      procedure :: update
   end type GenericCoupler

contains

   subroutine update(this)
      class(ImportCoupler), intent(in) :: this

      alarm = ESMF_ClockGetAlarm(..., _RC)
      is_ringing = ESMF_AlarmIsRinging(alarm, _RC)
      _RETURN_UNLESS(is_ringing)

      call this%update_dependecies()
      
      
end module mapl3g_ImportCoupler
