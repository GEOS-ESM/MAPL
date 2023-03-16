#include "MAPL_Generic.h"

module mapl3g_VariableSpec
   use mapl3g_AbstractStateItemSpec
   use mapl3g_StateItemSpecTypeId
   use mapl3g_ExtraDimsSpec
   use mapl3g_FieldSpec
   use mapl3g_InvalidSpec
   use mapl3g_VirtualConnectionPt
   use mapl_KeywordEnforcerMod
   use mapl_ErrorHandling
   use esmf
   use nuopc
   implicit none
   private

   public :: VariableSpec

   ! This type provides components that might be needed for _any_
   ! state item.  This is largely to support legacy interfaces, but it
   ! also allows us to defer interpretation until after user
   ! setservices() have run.
   type VariableSpec
      ! Mandatory values:
      type(ESMF_StateIntent_Flag) :: state_intent
      character(:), allocatable :: short_name
      type(ESMF_TypeKind_Flag) :: typekind = ESMF_TYPEKIND_R4

      ! Optional values
      character(:), allocatable :: standard_name
      type(ESMF_StateItem_Flag) :: type_id = MAPL_STATEITEM_FIELD
      character(:), allocatable :: units
      type(ExtraDimsSpec) :: extra_dims
   contains
      procedure :: make_virtualPt
      procedure :: make_ItemSpec
      procedure :: make_FieldSpec
!!$      procedure :: make_StateSpec
!!$      procedure :: make_BundleSpec
!!$      procedure :: initialize
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
      type(ESMF_StateItem_Flag), optional, intent(in) :: type_id
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
         type(ESMF_StateItem_Flag) :: type_id
         class(YAML_Node), intent(in) :: config

         character(:), allocatable :: type_id_as_string
         integer :: status

         type_id = MAPL_STATEITEM_FIELD ! default
         if (.not. config%has('type_id')) return
         
         call config%get(type_id_as_string, 'type_id', rc=status)
         if (status /= 0) then
            type_id = MAPL_STATEITEM_UNKNOWN
            return
         end if
         
         select case (type_id_as_string)
         case ('field')
            type_id = MAPL_STATEITEM_FIELD
         case ('bundle')
            type_id = MAPL_STATEITEM_FIELDBUNDLE
         case ('state')
            type_id = MAPL_STATEITEM_STATE
         case ('service_provider')
            type_id = MAPL_STATEITEM_SERVICE_PROVIDER
         case ('service_subcriber')
            type_id = MAPL_STATEITEM_SERVICE_SUBSCRIBER
         case default
            type_id = MAPL_STATEITEM_UNKNOWN
         end select
         
      end function get_type_id
      
   end subroutine initialize

   function make_virtualPt(this) result(v_pt)
      type(VirtualConnectionPt) :: v_pt
      class(VariableSpec), intent(in) :: this
      v_pt = VirtualConnectionPt(this%state_intent, this%short_name)
   end function make_virtualPt


   ! This implementation ensures that an object is at least created
   ! even if failures are encountered.  This is necessary for
   ! robust error handling upstream.
   function make_ItemSpec(this, geom, rc) result(item_spec)
      class(AbstractStateItemSpec), allocatable :: item_spec
      class(VariableSpec), intent(in) :: this
      type(ESMF_Geom), intent(in) :: geom
      integer, optional, intent(out) :: rc

      integer :: status
      select case (this%type_id%ot)
      case (MAPL_STATEITEM_FIELD%ot)
         allocate(FieldSpec::item_spec)
         item_spec = this%make_FieldSpec(geom, _RC)
!!$      case (MAPL_STATEITEM_FIELDBUNDLE)
!!$         allocate(FieldBundleSpec::item_spec)
!!$         item_spec = this%make_FieldBundleSpec(geom, _RC)
      case default
         ! Fail, but still need to allocate a result.
         allocate(InvalidSpec::item_spec)
         _FAIL('Unsupported type.')
      end select

      _RETURN(_SUCCESS)
   end function make_ItemSpec

   
   function make_FieldSpec(this, geom, rc) result(field_spec)
      type(FieldSpec) :: field_spec
      class(VariableSpec), intent(in) :: this
      type(ESMF_Geom), intent(in) :: geom
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: units

      if (.not. valid(this)) then
         _RETURN(_FAILURE)
      end if

      units = get_units(this, _RC)

      field_spec = new_FieldSpec_geom(geom=geom, typekind=this%typekind, extra_dims=this%extra_dims, &
           standard_name=this%standard_name, long_name=' ', units=units)

      _RETURN(_SUCCESS)

   contains

      logical function valid(this) result(is_valid)
         class(VariableSpec), intent(in) :: this

         is_valid = .false. ! unless

         if (.not. this%type_id == MAPL_STATEITEM_FIELD) return
         if (.not. allocated(this%standard_name)) return

         is_valid = .true.
         
      end function valid

      function get_units(this, rc) result(units)
         character(:), allocatable :: units
         class(VariableSpec), intent(in) :: this
         integer, optional, intent(out) :: rc

         character(len=ESMF_MAXSTR) :: canonical_units
         integer :: status
         
         if (allocated(this%units)) then ! user override of canonical
            units = this%units
            _RETURN(_SUCCESS)
         end if

         call NUOPC_FieldDictionaryGetEntry(this%standard_name, canonical_units, status)
         _ASSERT(status == ESMF_SUCCESS,'Units not found for standard name: <'//this%standard_name//'>')
         units = trim(canonical_units)

         _RETURN(_SUCCESS)
      end function get_units
      
   end function make_FieldSpec

end module mapl3g_VariableSpec
