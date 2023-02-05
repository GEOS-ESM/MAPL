#include "MAPL_Generic.h"

module mapl3g_VariableSpec
   use mapl3g_StateItemSpecTypeId
   use mapl_KeywordEnforcerMod
   use esmf, only: ESMF_StateIntent_Flag
   implicit none
   private

   public :: VariableSpec

   type VariableSpec
      ! Mandatory values:
      type(ESMF_StateIntent_Flag) :: state_intent
      character(:), allocatable :: short_name
      character(:), allocatable :: standard_name

      ! Optional values
      !   - either not mandatory, or have sensibe defaults
      type(StateItemSpecTypeId) :: type_id = MAPL_TYPE_ID_FIELD
      character(:), allocatable :: units
   contains
      procedure :: initialize
   end type VariableSpec

   interface VariableSpec
      module procedure :: new_VariableSpec
   end interface VariableSpec

contains

   function new_VariableSpec( &
        state_intent, short_name, unusable, standard_name, &
        type_id, units) result(var_spec)
      type(VariableSpec) :: var_spec
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      class(KeywordEnforcer), optional, intent(in) :: unusable
      ! Optional args:
      character(*), optional, intent(in) :: standard_name
      type(StateItemSpecTypeId), optional, intent(in) :: type_id
      character(*), optional, intent(in) :: units

      var_spec%state_intent = state_intent
      var_spec%short_name = short_name

#if defined(SET_OPTIONAL)
#  undef SET_OPTIONAL
#endif
#define SET_OPTIONAL(attr) if (present(attr)) var_spec% attr = attr

      SET_OPTIONAL(standard_name)
      SET_OPTIONAL(type_id)
      SET_OPTIONAL(units)
      
   end function new_VariableSpec


   ! Failing to find attributes in config is ok - they are
   ! left uninitialized. Constistency and sufficiency checks are
   ! relegated to the various StateItemSpec subclasses.
   subroutine initialize(this, config)
      use yaFyaml
      class(VariableSpec), intent(out) :: this
      class(YAML_Node), intent(in) :: config

      call config%get(this%standard_name, 'standard_name')
      this%type_id = get_type_id(config)
      call config%get(this%units, 'units')

   contains

      
      function get_type_id(config) result(type_id)
         type(StateItemSpecTypeId) :: type_id
         class(YAML_Node), intent(in) :: config

         character(:), allocatable :: type_id_as_string
         integer :: status

         type_id = MAPL_TYPE_ID_FIELD ! default
         if (.not. config%has('type_id')) return
         
         call config%get(type_id_as_string, 'type_id', rc=status)
         if (status /= 0) then
            type_id = MAPL_TYPE_ID_INVALID
            return
         end if
         
         select case (type_id_as_string)
         case ('field')
            type_id = MAPL_TYPE_ID_FIELD
         case ('bundle')
            type_id = MAPL_TYPE_ID_BUNDLE
         case ('state')
            type_id = MAPL_TYPE_ID_STATE
         case ('service_provider')
            type_id = MAPL_TYPE_ID_SERVICE_PROVIDER
         case ('service_subcriber')
            type_id = MAPL_TYPE_ID_SERVICE_SUBSCRIBER
         case default
            type_id = MAPL_TYPE_ID_INVALID
         end select
         
      end function get_type_id
      
   end subroutine initialize

end module mapl3g_VariableSpec
