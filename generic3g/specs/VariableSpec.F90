module mapl3g_VariableSpec
   use mapl_KeywordEnforcerMod
   use esmf, only: ESMF_StateIntent_Flag
   implicit none
   private

   public :: VariableSpec

   ! This type is a "struct" not a class.  It has no functionality and
   ! is only used to hold a collection of user-specified options for
   ! state items.

   type VariableSpec
      ! Mandatory values:
      type(ESMF_StateIntent_Flag) :: state_intent
      character(:), allocatable :: short_name
      ! Optional values:
   end type VariableSpec

   interface VariableSpec
      module procedure :: new_VariableSpec
   end interface VariableSpec

contains

   function new_VariableSpec(short_name, unusable) result(spec)
      type(VariableSpec) :: spec
      character(*), intent(in) :: short_name
      class(KeywordEnforcer), optional, intent(in) :: unusable

      spec%short_name = short_name
   end function new_VariableSpec
      

end module mapl3g_VariableSpec
