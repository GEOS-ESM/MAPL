#include "MAPL_Generic.h"

module mapl3g_VariableSpec
   use mapl3g_AbstractStateItemSpec
   use mapl3g_StateItem
   use mapl3g_UngriddedDimsSpec
   use mapl3g_VerticalDimSpec
   use mapl3g_HorizontalDimsSpec
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

      ! Metadata
      character(:), allocatable :: standard_name
      type(ESMF_StateItem_Flag) :: state_item = MAPL_STATEITEM_FIELD
      character(:), allocatable :: units
      character(:), allocatable :: substate

      real, allocatable :: default_value

      ! Geometry
      type(VerticalDimSpec) :: vertical_dim_spec ! none, center, edge
      type(HorizontalDimsSpec) :: horizontal_dims_spec ! none, geom
      type(UngriddedDimsSpec) :: ungridded_dims
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
        state_item, units, substate, typekind, default_value) result(var_spec)
      type(VariableSpec) :: var_spec
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      class(KeywordEnforcer), optional, intent(in) :: unusable
      ! Optional args:
      character(*), optional, intent(in) :: standard_name
      type(ESMF_StateItem_Flag), optional, intent(in) :: state_item
      character(*), optional, intent(in) :: units
      character(*), optional, intent(in) :: substate
      type(ESMF_TypeKind_Flag), optional, intent(in) :: typekind
      real, optional, intent(in) :: default_value

      var_spec%state_intent = state_intent
      var_spec%short_name = short_name

#if defined(_SET_OPTIONAL)
#  undef _SET_OPTIONAL
#endif
#define _SET_OPTIONAL(attr) if (present(attr)) var_spec% attr = attr

      _SET_OPTIONAL(standard_name)
      _SET_OPTIONAL(state_item)
      _SET_OPTIONAL(units)
      _SET_OPTIONAL(substate)
      _SET_OPTIONAL(typekind)
      _SET_OPTIONAL(default_value)

   end function new_VariableSpec


   ! Failing to find attributes in config is ok - they are
   ! left uninitialized. Constistency and sufficiency checks are
   ! relegated to the various StateItemSpec subclasses.
   subroutine initialize(this, config)
      use yaFyaml
      class(VariableSpec), intent(out) :: this
      class(YAML_Node), intent(in) :: config

      call config%get(this%standard_name, 'standard_name')
      this%state_item = get_state_item(config)
      call config%get(this%units, 'units')

   contains

      
      function get_state_item(config) result(state_item)
         type(ESMF_StateItem_Flag) :: state_item
         class(YAML_Node), intent(in) :: config

         character(:), allocatable :: state_item_as_string
         integer :: status

         state_item = MAPL_STATEITEM_FIELD ! default
         if (.not. config%has('state_item')) return
         
         call config%get(state_item_as_string, 'state_item', rc=status)
         if (status /= 0) then
            state_item = MAPL_STATEITEM_UNKNOWN
            return
         end if
         
         select case (state_item_as_string)
         case ('field')
            state_item = MAPL_STATEITEM_FIELD
         case ('bundle')
            state_item = MAPL_STATEITEM_FIELDBUNDLE
         case ('state')
            state_item = MAPL_STATEITEM_STATE
         case ('service_provider')
            state_item = MAPL_STATEITEM_SERVICE_PROVIDER
         case ('service_subcriber')
            state_item = MAPL_STATEITEM_SERVICE_SUBSCRIBER
         case default
            state_item = MAPL_STATEITEM_UNKNOWN
         end select
         
      end function get_state_item
      
   end subroutine initialize

   function make_virtualPt(this) result(v_pt)
      type(VirtualConnectionPt) :: v_pt
      class(VariableSpec), intent(in) :: this
      v_pt = VirtualConnectionPt(this%state_intent, this%short_name)
      if (allocated(this%substate)) then
         v_pt = v_pt%add_comp_name(this%substate)

      end if
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

      select case (this%state_item%ot)
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

      field_spec = new_FieldSpec_geom(geom=geom, typekind=this%typekind, ungridded_dims=this%ungridded_dims, &
           standard_name=this%standard_name, long_name=' ', units=units, default_value=this%default_value)

      _RETURN(_SUCCESS)

   contains

      logical function valid(this) result(is_valid)
         class(VariableSpec), intent(in) :: this

         is_valid = .false. ! unless

         if (.not. this%state_item == MAPL_STATEITEM_FIELD) return
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
