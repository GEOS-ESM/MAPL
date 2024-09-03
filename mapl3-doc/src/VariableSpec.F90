
#include "MAPL_Generic.h"

module mapl3g_VariableSpec

   use mapl3g_UngriddedDims
   use mapl3g_VerticalDimSpec
   use mapl3g_HorizontalDimsSpec
   use mapl3g_VirtualConnectionPt
   use mapl3g_ActualConnectionPt
   use mapl3g_VerticalGrid
   use mapl_KeywordEnforcerMod
   use mapl3g_ActualPtVector
   use mapl_ErrorHandling
   use mapl3g_StateRegistry
   use mapl3g_StateItem
   use esmf
   use gFTL2_StringVector
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
      type(ESMF_StateItem_Flag) :: itemtype = MAPL_STATEITEM_FIELD
      type(StringVector), allocatable :: service_items
      character(:), allocatable :: units
      character(:), allocatable :: substate
      real, allocatable :: default_value
      type(StringVector) :: attributes
      integer, allocatable :: bracket_size

      ! Geometry
      type(ESMF_Geom), allocatable :: geom
      type(VerticalDimSpec) :: vertical_dim_spec = VERTICAL_DIM_UNKNOWN ! none, center, edge
      type(HorizontalDimsSpec) :: horizontal_dims_spec = HORIZONTAL_DIMS_GEOM ! none, geom
      type(UngriddedDims) :: ungridded_dims
      type(StringVector) :: dependencies
   contains
      procedure :: make_virtualPt

      procedure :: make_dependencies
      procedure, private :: pick_geom_
      procedure :: initialize
   end type VariableSpec

   interface VariableSpec
      module procedure :: new_VariableSpec
   end interface VariableSpec

contains

   function new_VariableSpec( &
        state_intent, short_name, unusable, standard_name, geom, &
        units, substate, itemtype, typekind, vertical_dim_spec, ungridded_dims, default_value, &
        service_items, attributes, &
        bracket_size, &
        dependencies) result(var_spec)

      type(VariableSpec) :: var_spec
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      ! Optional args:
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: standard_name
      type(ESMF_Geom), optional, intent(in) :: geom
      type(ESMF_StateItem_Flag), optional, intent(in) :: itemtype
      type(StringVector), optional :: service_items
      character(*), optional, intent(in) :: units
      character(*), optional, intent(in) :: substate
      type(ESMF_TypeKind_Flag), optional, intent(in) :: typekind
      type(VerticalDimSpec), optional, intent(in) :: vertical_dim_spec
      type(UngriddedDims), optional, intent(in) :: ungridded_dims
      real, optional, intent(in) :: default_value
      type(StringVector), optional, intent(in) :: attributes
      integer, optional, intent(in) :: bracket_size
      type(StringVector), optional, intent(in) :: dependencies

      var_spec%state_intent = state_intent
      var_spec%short_name = short_name

#if defined(_SET_OPTIONAL)
#  undef _SET_OPTIONAL
#endif
#define _SET_OPTIONAL(attr) if (present(attr)) var_spec%attr = attr

      _SET_OPTIONAL(standard_name)
      _SET_OPTIONAL(geom)
      _SET_OPTIONAL(itemtype)
      _SET_OPTIONAL(units)
      _SET_OPTIONAL(substate)
      _SET_OPTIONAL(typekind)
      _SET_OPTIONAL(service_items)
      _SET_OPTIONAL(default_value)
      _SET_OPTIONAL(vertical_dim_spec)
      _SET_OPTIONAL(ungridded_dims)
      _SET_OPTIONAL(attributes)
      _SET_OPTIONAL(bracket_size)
      _SET_OPTIONAL(dependencies)

      _UNUSED_DUMMY(unusable)
   end function new_VariableSpec


   ! Failing to find attributes in config is ok - they are
   ! left uninitialized. Constistency and sufficiency checks are
   ! relegated to the various StateItemSpec subclasses.
   subroutine initialize(this, config)
      class(VariableSpec), intent(out) :: this
      type(ESMF_HConfig), intent(in) :: config

      this%standard_name = ESMF_HConfigAsString(config,keyString='standard_name')
      this%itemtype = get_itemtype(config)
      this%units = ESMF_HConfigAsString(config,keyString='units')

   contains
      
      function get_itemtype(config) result(itemtype)
         type(ESMF_StateItem_Flag) :: itemtype
         type(ESMF_HConfig), intent(in) :: config

         character(:), allocatable :: itemtype_as_string
         integer :: status

         itemtype = MAPL_STATEITEM_FIELD ! default
         if (.not. ESMF_HConfigIsDefined(config,keyString='itemtype')) return
        
         itemtype_as_string = ESMF_HConfigAsString(config,keyString='itemtype',rc=status) 
         if (status /= 0) then
            itemtype = MAPL_STATEITEM_UNKNOWN
            return
         end if
         
         select case (itemtype_as_string)
         case ('field')
            itemtype = MAPL_STATEITEM_FIELD
         case ('bundle')
            itemtype = MAPL_STATEITEM_FIELDBUNDLE
         case ('state')
            itemtype = MAPL_STATEITEM_STATE
         case ('service_provider')
            itemtype = MAPL_STATEITEM_SERVICE_PROVIDER
         case ('service_subcriber')
            itemtype = MAPL_STATEITEM_SERVICE_SUBSCRIBER
         case ('wildcard')
            itemtype = MAPL_STATEITEM_WILDCARD
         case ('bracket')
            itemtype = MAPL_STATEITEM_BRACKET
         case default
            itemtype = MAPL_STATEITEM_UNKNOWN
         end select
         
      end function get_itemtype
      
   end subroutine initialize

   function make_virtualPt(this) result(v_pt)
      type(VirtualConnectionPt) :: v_pt
      class(VariableSpec), intent(in) :: this
      v_pt = VirtualConnectionPt(this%state_intent, this%short_name)
      if (allocated(this%substate)) then
         v_pt = v_pt%add_comp_name(this%substate)
      end if
   end function make_virtualPt

   subroutine pick_geom_(this, that_geom, geom, rc)
      class(VariableSpec), intent(in) :: this
      type(ESMF_Geom), optional, intent(in) :: that_geom
      type(ESMF_Geom), allocatable, intent(out) :: geom
      integer, optional, intent(out) :: rc

      integer :: status

      if (present(that_geom) .and. allocated(this%geom)) then
         _FAIL("Cannot have both this and that geom :-(")
      end if
      if (present(that_geom)) geom = that_geom
      if (allocated(this%geom)) geom = this%geom

      _RETURN(_SUCCESS)
   end subroutine pick_geom_

   subroutine fill_units(this, units, rc)
      class(VariableSpec), intent(in) :: this
      character(:), allocatable, intent(out) :: units
      integer, optional, intent(out) :: rc
      
      character(len=ESMF_MAXSTR) :: canonical_units
      integer :: status

      ! Only fill if not already specified
      if (allocated(this%units)) then
         units = this%units
         _RETURN(_SUCCESS)
      end if

      ! Only fill if standard name is provided
      _RETURN_UNLESS(allocated(this%standard_name))

      call NUOPC_FieldDictionaryGetEntry(this%standard_name, canonical_units, status)
      _ASSERT(status == ESMF_SUCCESS,'Units not found for standard name: <'//this%standard_name//'>')
      units = trim(canonical_units)
      
      _RETURN(_SUCCESS)
   end subroutine fill_units

   function make_dependencies(this, rc) result(dependencies)
      type(ActualPtVector) :: dependencies
      class(VariableSpec), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      type(ActualConnectionPt) :: a_pt

      dependencies = ActualPtVector()
      do i = 1, this%dependencies%size()
         a_pt = ActualConnectionPt(VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, this%dependencies%of(i)))
         call dependencies%push_back(a_pt)
      end do

      _RETURN(_SUCCESS)
   end function make_dependencies

end module mapl3g_VariableSpec
