#include "MAPL_ErrLog.h"

submodule (mapl3g_ComponentSpecParser) parse_var_specs_smod
   implicit none
   
contains

   ! A component is not required to have var_specs.   E.g, in theory GCM gridcomp will not
   ! have var specs in MAPL3, as it does not really have a preferred geom on which to declare
   ! imports and exports.
   module function parse_var_specs(hconfig, timestep, refTime, rc) result(var_specs)
      type(VariableSpecVector) :: var_specs
      type(ESMF_HConfig), intent(in) :: hconfig
      type(ESMF_TimeInterval), optional, intent(in) :: timestep
      type(ESMF_Time), optional, intent(in) :: refTime
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_states_section
      type(ESMF_HConfig) :: subcfg

      has_states_section = ESMF_HConfigIsDefined(hconfig,keyString=COMPONENT_STATES_SECTION, _RC)
      _RETURN_UNLESS(has_states_section)

      subcfg = ESMF_HConfigCreateAt(hconfig,keyString=COMPONENT_STATES_SECTION, _RC)

      call parse_state_specs(var_specs, subcfg, COMPONENT_INTERNAL_STATE_SECTION,  timestep, refTime, _RC)
      call parse_state_specs(var_specs, subcfg, COMPONENT_EXPORT_STATE_SECTION, timestep, refTime, _RC)
      call parse_state_specs(var_specs, subcfg, COMPONENT_IMPORT_STATE_SECTION, timestep, refTime, _RC)

      call ESMF_HConfigDestroy(subcfg, _RC)

      _RETURN(_SUCCESS)
   contains

      subroutine parse_state_specs(var_specs, hconfig, state_intent, timestep, refTime, rc)
         type(VariableSpecVector), intent(inout) :: var_specs
         type(ESMF_HConfig), target, intent(in) :: hconfig
         character(*), intent(in) :: state_intent
         type(ESMF_TimeInterval), optional, intent(in) :: timestep
         type(ESMF_Time), optional, intent(in) :: refTime
         integer, optional, intent(out) :: rc

         type(VariableSpec) :: var_spec
         type(ESMF_HConfigIter) :: iter,e,b
         character(:), allocatable :: name
         character(:), allocatable :: short_name
         type(ESMF_HConfig) :: attributes
         type(ESMF_TypeKind_Flag) :: typekind
         real, allocatable :: default_value
         type(VerticalDimSpec) :: vertical_dim_spec
         type(UngriddedDims) :: ungridded_dims
         character(:), allocatable :: standard_name
         character(:), allocatable :: units
         character(len=:), allocatable :: accumulation_type
         type(ESMF_StateItem_Flag), allocatable :: itemtype
         type(ESMF_StateIntent_Flag) :: esmf_state_intent

         type(StringVector) :: service_items
         integer :: status
         logical :: has_state
         logical :: has_standard_name
         logical :: has_units
         logical :: has_accumulation_type
         type(ESMF_HConfig) :: subcfg
         type(StringVector) :: dependencies

         has_state = ESMF_HConfigIsDefined(hconfig,keyString=state_intent, _RC)
         _RETURN_UNLESS(has_state)

         subcfg = ESMF_HConfigCreateAt(hconfig,keyString=state_intent, _RC)

         b = ESMF_HConfigIterBegin(subcfg, _RC)
         e = ESMF_HConfigIterEnd(subcfg, _RC)
         iter = b
         do while (ESMF_HConfigIterLoop(iter,b,e))
            name = ESMF_HConfigAsStringMapKey(iter, _RC)
            attributes = ESMF_HConfigCreateAtMapVal(iter,_RC)

            short_name = name
            typekind = to_typekind(attributes, _RC)
            call val_to_float(default_value, attributes, KEY_DEFAULT_VALUE, _RC)
            vertical_dim_spec = to_VerticalDimSpec(attributes,_RC)
            ungridded_dims = to_UngriddedDims(attributes, _RC)

            has_standard_name = ESMF_HConfigIsDefined(attributes,keyString='standard_name', _RC)
            if (has_standard_name) then
               standard_name = ESMF_HConfigAsString(attributes,keyString='standard_name', _RC)
            end if

            has_units = ESMF_HConfigIsDefined(attributes,keyString='units', _RC)
            if (has_units) then
               units = ESMF_HConfigAsString(attributes,keyString='units', _RC)
            end if

            has_accumulation_type = ESMF_HConfigIsDefined(attributes, keyString=KEY_ACCUMULATION_TYPE, _RC)
            if(has_accumulation_type) then
               accumulation_type = ESMF_HConfigAsString(attributes, keyString=KEY_ACCUMULATION_TYPE, _RC)
            end if

            call to_itemtype(itemtype, attributes, _RC)
            call to_service_items(service_items, attributes, _RC)

            dependencies = to_dependencies(attributes, _RC)

            esmf_state_intent = to_esmf_state_intent(state_intent)
            var_spec = VariableSpec(esmf_state_intent, short_name=short_name, &
                 itemtype=itemtype, &
                 service_items=service_items, &
                 standard_name=standard_name, &
                 units=units, &
                 typekind=typekind, &
                 default_value=default_value, &
                 vertical_dim_spec=vertical_dim_spec, &
                 ungridded_dims=ungridded_dims, &
                 dependencies=dependencies, &
                 accumulation_type=accumulation_type, &
                 timestep=timestep, &
                 refTime=refTime)

            if (allocated(units)) deallocate(units)
            if (allocated(standard_name)) deallocate(standard_name)
            if (allocated(accumulation_type)) deallocate(accumulation_type)
            call var_specs%push_back(var_spec)

            call ESMF_HConfigDestroy(attributes, _RC)

         end do

         call ESMF_HConfigDestroy(subcfg, _RC)

         _RETURN(_SUCCESS)
      end subroutine parse_state_specs

      subroutine val_to_float(x, attributes, key, rc)
         real, allocatable, intent(out) :: x
         type(ESMF_HConfig), intent(in) :: attributes
         character(*), intent(in) :: key
         integer, optional, intent(out) :: rc

         integer :: status
         logical :: has_default_value

         has_default_value = ESMF_HConfigIsDefined(attributes, keyString=key, _RC)
         _RETURN_UNLESS(has_default_value)

         allocate(x)
         x = ESMF_HConfigAsR4(attributes, keyString=KEY_DEFAULT_VALUE, _RC)

         _RETURN(_SUCCESS)
      end subroutine val_to_float

      function to_typekind(attributes, rc) result(typekind)
         use :: mapl3g_ESMF_Utilities, only: MAPL_TYPEKIND_MIRROR
         type(ESMF_TypeKind_Flag) :: typekind
         type(ESMF_HConfig), intent(in) :: attributes
         integer, optional, intent(out) :: rc

         integer :: status
         logical :: typekind_is_specified
         character(:), allocatable :: typekind_str

         typekind = ESMF_TYPEKIND_R4 ! GEOS defaults

         typekind_is_specified = ESMF_HConfigIsDefined(attributes, keyString='typekind', _RC)
         _RETURN_UNLESS(typekind_is_specified)

         typekind_str= ESMF_HConfigAsString(attributes,keyString='typekind',_RC)
         select case (ESMF_UtilStringLowerCase(typekind_str))
         case ('r4')
            typekind = ESMF_TYPEKIND_R4
         case ('r8')
            typekind = ESMF_TYPEKIND_R8
         case ('i4')
            typekind = ESMF_TYPEKIND_I4
         case ('i8')
            typekind = ESMF_TYPEKIND_I8
         case ('mirror')
            typekind = MAPL_TYPEKIND_MIRROR
         case default
            _FAIL('Unsupported typekind: <'//typekind_str//'>')
         end select

         _RETURN(_SUCCESS)
      end function to_typekind

      function to_VerticalDimSpec(attributes, rc) result(vertical_dim_spec)
         type(VerticalDimSpec) :: vertical_dim_spec
         type(ESMF_HConfig), intent(in) :: attributes
         integer, optional, intent(out) :: rc

         integer :: status
         character(:), allocatable :: vertical_str
         logical :: has_dim_spec

         vertical_dim_spec = VERTICAL_DIM_UNKNOWN
         has_dim_spec = ESMF_HConfigIsDefined(attributes,keyString=KEY_VERTICAL_DIM_SPEC, _RC)
         _RETURN_UNLESS(has_dim_spec)

         vertical_str = ESMF_HConfigAsString(attributes,keyString=KEY_VERTICAL_DIM_SPEC,_RC)

         select case (ESMF_UtilStringLowerCase(vertical_str))
         case ('vertical_dim_none', 'n', 'none')
            vertical_dim_spec = VERTICAL_DIM_NONE
         case ('vertical_dim_center', 'c', 'center')
            vertical_dim_spec = VERTICAL_DIM_CENTER
         case ('vertical_dim_edge', 'e', 'edge')
            vertical_dim_spec = VERTICAL_DIM_EDGE
         case ('vertical_dim_mirror', 'm', 'mirror')
            vertical_dim_spec = VERTICAL_DIM_MIRROR
         case default
            _FAIL('Unsupported vertical_dim_spec')
         end select

         _RETURN(_SUCCESS)
      end function to_VerticalDimSpec

      function to_UngriddedDims(attributes,rc) result(ungridded_dims)
         type(UngriddedDims) :: ungridded_dims
         type(ESMF_HConfig), intent(in) :: attributes
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_HConfig) :: dim_specs, dim_spec
         character(len=:), allocatable :: dim_name, dim_units
         real, allocatable :: coordinates(:)
         integer :: dim_size,i
         type(UngriddedDim) :: temp_dim

         logical :: has_ungridded_dims, has_name, has_units, has_extent, has_coordinates
         integer :: n_specs

         has_ungridded_dims = ESMF_HConfigIsDefined(attributes, keyString=KEY_UNGRIDDED_DIMS, _RC)
         _RETURN_UNLESS(has_ungridded_dims)

         dim_specs = ESMF_HConfigCreateAt(attributes, keyString=KEY_UNGRIDDED_DIMS, _RC)

         n_specs = ESMF_HConfigGetSize(dim_specs, _RC)
         do i = 1, n_specs
            dim_spec = ESMF_HConfigCreateAt(dim_specs, index=i, _RC)
            has_name = ESMF_HConfigIsDefined(dim_spec,keyString=KEY_UNGRIDDED_DIM_NAME)
            has_units = ESMF_HConfigIsDefined(dim_spec,keyString=KEY_UNGRIDDED_DIM_UNITS)
            has_extent = ESMF_HConfigIsDefined(dim_spec,keyString=KEY_UNGRIDDED_DIM_EXTENT)
            has_coordinates = ESMF_HConfigIsDefined(dim_spec,keyString=KEY_UNGRIDDED_DIM_COORDINATES)
            _ASSERT(.not.(has_extent .and. has_coordinates), "Both extent and coordinates specified")
            if (has_name) then
               dim_name = ESMF_HConfigAsString(dim_spec, keyString=KEY_UNGRIDDED_DIM_NAME, _RC)
            end if
            if (has_units) then
               dim_units = ESMF_HConfigAsString(dim_spec, keyString=KEY_UNGRIDDED_DIM_UNITS, _RC)
            end if
            if (has_extent) then
               dim_size = ESMF_HConfigAsI4(dim_spec, keyString=KEY_UNGRIDDED_DIM_EXTENT, _RC)
               temp_dim = UngriddedDim(dim_size, name=dim_name, units=dim_units)
            end if
            if (has_coordinates) then
               coordinates = ESMF_HConfigAsR4Seq(dim_spec, keyString=KEY_UNGRIDDED_DIM_COORDINATES, _RC)
               temp_dim = UngriddedDim(coordinates, name=dim_name, units=dim_units)
            end if
            call ungridded_dims%add_dim(temp_dim, _RC)
            call ESMF_HConfigDestroy(dim_spec, _RC)
         end do

         call ESMF_HConfigDestroy(dim_specs, _RC)

         _RETURN(_SUCCESS)
      end function to_UngriddedDims


      subroutine to_itemtype(itemtype, attributes, rc)
         type(ESMF_StateItem_Flag), allocatable, intent(out) :: itemtype
         type(ESMF_HConfig), target, intent(in) :: attributes
         integer, optional, intent(out) :: rc

         integer :: status
         character(:), allocatable :: subclass
         logical :: has_itemtype

         has_itemtype = ESMF_HConfigIsDefined(attributes,keyString='class',_RC)
         _RETURN_UNLESS(has_itemtype)

         subclass= ESMF_HConfigAsString(attributes, keyString='class',_RC)

         select case (ESMF_UtilStringLowerCase(subclass))
         case ('field')
            itemtype = MAPL_STATEITEM_FIELD
         case ('service')
            itemtype = MAPL_STATEITEM_SERVICE
         case ('wildcard')
            itemtype = MAPL_STATEITEM_WILDCARD
         case default
            _FAIL('unknown subclass for state item: '//subclass)
         end select

         _RETURN(_SUCCESS)
      end subroutine to_itemtype

      subroutine to_service_items(service_items, attributes, rc)
         type(StringVector), intent(out) :: service_items
         type(ESMF_HConfig), target, intent(in) :: attributes
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_HConfig) :: seq
         integer :: num_items, i
         character(:), allocatable :: item_name
         logical :: has_service_items

         has_service_items = ESMF_HConfigIsDefined(attributes,keyString='items',_RC)
         _RETURN_UNLESS(has_service_items)

         seq = ESMF_HConfigCreateAt(attributes,keyString='items',_RC)
         _ASSERT(ESMF_HConfigIsSequence(seq),"items must be a sequence")
         num_items = ESMF_HConfigGetSize(seq,_RC)
         do i = 1,num_items
            item_name = ESMF_HConfigAsString(seq,index = i, _RC)
            call service_items%push_back(item_name)
         end do

         _RETURN(_SUCCESS)
      end subroutine to_service_items

      function to_dependencies(attributes, rc) result(dependencies)
         type(StringVector) :: dependencies
         type(ESMF_HConfig), intent(in) :: attributes
         integer, optional, intent(out) :: rc

         integer :: status
         logical :: has_dependencies
         type(ESMF_HConfig) :: dependencies_hconfig
         integer :: i, n_dependencies
         character(:), allocatable :: name

         dependencies = StringVector()
         has_dependencies = ESMF_HConfigIsDefined(attributes, keyString='dependencies', _RC)
         _RETURN_UNLESS(has_dependencies)

         dependencies_hconfig = ESMF_HConfigCreateAt(attributes, keyString='dependencies', _RC)
         _ASSERT(ESMF_HConfigIsSequence(dependencies_hconfig), 'expected sequence for attribute <dependencies>')
         n_dependencies = ESMF_HConfigGetSize(dependencies_hconfig, _RC)

         do i = 1, n_dependencies
            name = ESMF_HConfigAsString(dependencies_hconfig, index=i, _RC)
            call dependencies%push_back(name)
         end do

         _RETURN(_SUCCESS)
      end function to_dependencies

   end function parse_var_specs

end submodule parse_var_specs_smod


