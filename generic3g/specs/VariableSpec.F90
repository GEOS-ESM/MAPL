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
      character(:), allocatable :: standard_name

      character(:), allocatable :: units
      ! Optional values:
   end type VariableSpec

   interface VariableSpec
      module procedure :: new_VariableSpec
   end interface VariableSpec

contains

   function new_VariableSpec(state_intent, unusable, short_name, standard_name, units) result(spec)
      type(VariableSpec) :: spec
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      class(KeywordEnforcer), optional, intent(in) :: unusable
      ! Note: short_name and standard_name are not optional, but
      ! require keywords to prevent confusion.
      character(*), intent(in) :: short_name
      character(*), intent(in) :: standard_name
      ! Optional args:
      character(*), optional, intent(in) :: units

      spec%state_intent = state_intent
      spec%short_name = short_name
      spec%standard_name = standard_name

      if (present(units)) spec%units = units
      
   end function new_VariableSpec
      

end module mapl3g_VariableSpec
