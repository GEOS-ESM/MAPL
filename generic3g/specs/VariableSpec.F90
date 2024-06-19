
#include "MAPL_Generic.h"

module mapl3g_VariableSpec
   use mapl3g_StateItemSpec
   use mapl3g_StateItem
   use mapl3g_UngriddedDims
   use mapl3g_VerticalDimSpec
   use mapl3g_HorizontalDimsSpec
   use mapl3g_FieldSpec
   use mapl3g_WildcardSpec
   use mapl3g_BracketSpec
   use mapl3g_ServiceSpec
   use mapl3g_InvalidSpec
   use mapl3g_VirtualConnectionPt
   use mapl3g_ActualConnectionPt
   use mapl3g_VerticalGeom
   use mapl_KeywordEnforcerMod
   use mapl3g_ActualPtVector
   use mapl_ErrorHandling
   use mapl3g_HierarchicalRegistry
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
      type(VerticalDimSpec) :: vertical_dim_spec = VERTICAL_DIM_UNKNOWN ! none, center, edge
      type(HorizontalDimsSpec) :: horizontal_dims_spec = HORIZONTAL_DIMS_GEOM ! none, geom
      type(UngriddedDims) :: ungridded_dims
      type(StringVector) :: dependencies
   contains
      procedure :: make_virtualPt
      procedure :: make_ItemSpec
      procedure :: make_BracketSpec
      procedure :: make_FieldSpec
      procedure :: make_ServiceSpec
      procedure :: make_WildcardSpec

      procedure :: make_dependencies
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
        units, substate, itemtype, typekind, vertical_dim_spec, ungridded_dims, default_value, &
        service_items, attributes, &
        bracket_size, &
        dependencies) result(var_spec)

      type(VariableSpec) :: var_spec
      type(ESMF_StateIntent_Flag), intent(in) :: state_intent
      character(*), intent(in) :: short_name
      class(KeywordEnforcer), optional, intent(in) :: unusable
      ! Optional args:
      character(*), optional, intent(in) :: standard_name
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


   ! This implementation ensures that an object is at least created
   ! even if failures are encountered.  This is necessary for
   ! robust error handling upstream.
   function make_ItemSpec(this, geom, vertical_geom, registry, rc) result(item_spec)
      class(StateItemSpec), allocatable :: item_spec
      class(VariableSpec), intent(in) :: this
      type(ESMF_Geom), optional, intent(in) :: geom
      type(VerticalGeom), intent(in) :: vertical_geom
      type(HierarchicalRegistry), intent(in) :: registry
      integer, optional, intent(out) :: rc

      integer :: status
      type(ActualPtVector) :: dependencies

      select case (this%itemtype%ot)
      case (MAPL_STATEITEM_FIELD%ot)
         allocate(FieldSpec::item_spec)
         item_spec = this%make_FieldSpec(geom, vertical_geom,  _RC)
!!$      case (MAPL_STATEITEM_FIELDBUNDLE)
!!$         allocate(FieldBundleSpec::item_spec)
!!$         item_spec = this%make_FieldBundleSpec(geom, _RC)
      case (MAPL_STATEITEM_SERVICE%ot)
         allocate(ServiceSpec::item_spec)
         item_spec = this%make_ServiceSpec(registry, _RC)
      case (MAPL_STATEITEM_WILDCARD%ot)
         allocate(WildcardSpec::item_spec)
         item_spec = this%make_WildcardSpec(geom, vertical_geom,  _RC)
      case (MAPL_STATEITEM_BRACKET%ot)
         allocate(BracketSpec::item_spec)
         item_spec = this%make_BracketSpec(geom, vertical_geom,  _RC)
      case default
         ! Fail, but still need to allocate a result.
         allocate(InvalidSpec::item_spec)
         _FAIL('Unsupported type.')
      end select

      dependencies = this%make_dependencies(_RC)
      call item_spec%set_dependencies(dependencies)
      call item_spec%set_raw_dependencies(this%dependencies)

      _RETURN(_SUCCESS)
   end function make_ItemSpec
   
   function make_BracketSpec(this, geom, vertical_geom, rc) result(bracket_spec)
      type(BracketSpec) :: bracket_spec
      class(VariableSpec), intent(in) :: this
      type(ESMF_Geom), optional, intent(in) :: geom
      type(VerticalGeom), intent(in) :: vertical_geom
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: units
      type(FieldSpec) :: field_spec

      if (.not. valid(this)) then
         _RETURN(_FAILURE)
      end if

      call fill_units(this, units, _RC)

      field_spec = FieldSpec(geom=geom, vertical_geom=vertical_geom, vertical_dim_spec=this%vertical_dim_spec, ungridded_dims=this%ungridded_dims, &
           typekind=this%typekind, &
           standard_name=this%standard_name, long_name=' ', units=units, attributes=this%attributes, default_value=this%default_value)

      
      bracket_spec = BracketSpec(field_spec, this%bracket_size)

      _RETURN(_SUCCESS)

   contains

      logical function valid(this) result(is_valid)
         class(VariableSpec), intent(in) :: this

         is_valid = .false. ! unless

         if (.not. this%itemtype == MAPL_STATEITEM_BRACKET) return
         if (.not. allocated(this%standard_name)) return
         if (.not. allocated(this%bracket_size)) return

         is_valid = .true.

      end function valid

   end function make_BracketSpec

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

   function make_FieldSpec(this, geom, vertical_geom, rc) result(field_spec)
      type(FieldSpec) :: field_spec
      class(VariableSpec), intent(in) :: this
      type(ESMF_Geom), optional, intent(in) :: geom
      type(VerticalGeom), intent(in) :: vertical_geom
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: units

      if (.not. valid(this)) then
         _RETURN(_FAILURE)
      end if

      _ASSERT(this%vertical_dim_spec /= VERTICAL_DIM_UNKNOWN, 'must provide a vertical dim spec')
      call fill_units(this, units, _RC)

      field_spec = FieldSpec(geom=geom, vertical_geom=vertical_geom, vertical_dim_spec=this%vertical_dim_spec, ungridded_dims=this%ungridded_dims, &
           typekind=this%typekind, &
           standard_name=this%standard_name, long_name=' ', units=units, attributes=this%attributes, default_value=this%default_value)

      _RETURN(_SUCCESS)

   contains

      logical function valid(this) result(is_valid)
         class(VariableSpec), intent(in) :: this

         is_valid = .false. ! unless

         if (.not. this%itemtype == MAPL_STATEITEM_FIELD) return
!#         if (.not. allocated(this%standard_name)) return

         is_valid = .true.

      end function valid


   end function make_FieldSpec

   ! ------
   ! ServiceSpec needs reference to the specs of the fields that are to be
   ! handled by the service.   Shallow copy of these will appear in the FieldBundle in the
   ! import state of the requesting gridcomp.
   ! ------
   function make_ServiceSpec(this, registry, rc) result(service_spec)
      type(ServiceSpec) :: service_spec
      class(VariableSpec), intent(in) :: this
      type(HierarchicalRegistry), intent(in) :: registry
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i, n
      type(StateItemSpecPtr), allocatable :: specs(:)
      type(ActualConnectionPt) :: a_pt

      if (.not. valid(this)) then
         _RETURN(_FAILURE)
      end if

      n = this%service_items%size()
      allocate(specs(n))

      do i = 1, n
         a_pt = ActualConnectionPt(VirtualConnectionPt(ESMF_STATEINTENT_INTERNAL, this%service_items%of(i)))
         specs(i)%ptr => registry%get_item_spec(a_pt, _RC)
      end do
      service_spec = ServiceSpec(specs)

      _RETURN(_SUCCESS)

   contains

      logical function valid(this) result(is_valid)
         class(VariableSpec), intent(in) :: this

         is_valid = .false. ! unless
         if (.not. this%itemtype == MAPL_STATEITEM_SERVICE) return
         is_valid = .true.
         
      end function valid

   end function make_ServiceSpec

   function make_WildcardSpec(this, geom, vertical_geom, rc) result(wildcard_spec)
      type(WildcardSpec) :: wildcard_spec
      class(VariableSpec), intent(in) :: this
      type(ESMF_Geom), intent(in) :: geom
      type(VerticalGeom), intent(in) :: vertical_geom
      integer, optional, intent(out) :: rc

      integer :: status
      type(FieldSpec) :: field_spec

      field_spec = new_FieldSpec_geom(geom=geom, vertical_geom=vertical_geom, &
           vertical_dim_spec=this%vertical_dim_spec, typekind=this%typekind, ungridded_dims=this%ungridded_dims, &
           attributes=this%attributes, default_value=this%default_value)
      wildcard_spec = WildCardSpec(field_spec)

      _RETURN(_SUCCESS)
   contains

      logical function valid(this) result(is_valid)
         class(VariableSpec), intent(in) :: this
         
         is_valid = .false. ! unless
         if (allocated(this%standard_name)) return
         if (allocated(this%units)) return ! maybe this can be relaxed - match only thisgs that have same units?
         if (this%attributes%size() > 0) return
         if (allocated(this%default_value)) return
         is_valid = .true.
         
      end function valid
   end function make_WildcardSpec

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
