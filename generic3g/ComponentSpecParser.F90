#include "MAPL_ErrLog.h"

module mapl3g_ComponentSpecParser
   use mapl3g_ComponentSpec
   use mapl3g_ChildSpec
   use mapl3g_ChildSpecMap
   use mapl3g_UserSetServices
   use mapl_ErrorHandling
   use mapl3g_VariableSpec
   use mapl3g_ConnectionPt
   use mapl3g_VirtualConnectionPt
   use mapl3g_VariableSpecVector
   use mapl3g_HierarchicalRegistry, only: Connection
   use mapl3g_SimpleConnection
   use mapl3g_MatchConnection
   use mapl3g_ReexportConnection
   use mapl3g_ConnectionVector
   use mapl3g_VerticalDimSpec
   use mapl3g_UngriddedDimsSpec
   use mapl3g_UngriddedDimSpec
   use mapl3g_Stateitem
   use mapl3g_ESMF_Utilities
   use mapl3g_UserSetServices
   use gftl2_StringVector, only: StringVector
   use esmf
   implicit none
   private

   ! 
   public :: parse_component_spec

   ! The following interfaces are public only for testing purposes.
   public :: parse_children
   public :: parse_child
   public :: parse_SetServices
!!$   public :: parse_ChildSpecMap
!!$   public :: parse_ChildSpec

   character(*), parameter :: MAPL_SECTION = 'mapl'
   character(*), parameter :: COMPONENT_GEOM_SECTION = 'geom'
   character(*), parameter :: COMPONENT_STATES_SECTION = 'states'
   character(*), parameter :: COMPONENT_IMPORT_STATE_SECTION = 'import'
   character(*), parameter :: COMPONENT_EXPORT_STATE_SECTION = 'export'
   character(*), parameter :: COMPONENT_INTERNAL_STATE_SECTION = 'internal'
   character(*), parameter :: COMPONENT_CONNECTIONS_SECTION = 'connections'
   character(*), parameter :: COMPONENT_CHILDREN_SECTION = 'children'

   character(*), parameter :: KEY_DEFAULT_VALUE = 'default_value'
   character(*), parameter :: KEY_UNGRIDDED_DIM_SPECS = 'ungridded_dim_specs'
   character(*), parameter :: KEY_UNGRIDDED_DIM_NAME = 'dim_name'
   character(*), parameter :: KEY_UNGRIDDED_DIM_EXTENT = 'extent'
   character(*), parameter :: KEY_VERTICAL_DIM_SPEC = 'vertical_dim_spec'
   
contains

   type(ComponentSpec) function parse_component_spec(hconfig, rc) result(spec)
      type(ESMF_HConfig), target, intent(inout) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_mapl_section
      logical :: has_geom_section
      type(ESMF_HConfig) :: subcfg

      has_mapl_section = ESMF_HConfigIsDefined(hconfig, keyString=MAPL_SECTION, _RC)
      _RETURN_UNLESS(has_mapl_section)
      subcfg = ESMF_HConfigCreateAt(hconfig, keyString=MAPL_SECTION, _RC)

      has_geom_section = ESMF_HConfigIsDefined(subcfg,keyString=COMPONENT_GEOM_SECTION, _RC)
      if (has_geom_section) then
         spec%geom_hconfig = parse_geom_spec(subcfg, _RC)
      end if

      spec%var_specs = parse_var_specs(subcfg, _RC)
      spec%connections = parse_connections(subcfg, _RC)
      spec%children = parse_children(subcfg, _RC)

      call ESMF_HConfigDestroy(subcfg, _RC)

      _RETURN(_SUCCESS)
   end function parse_component_spec


   ! Geom subcfg is passed raw to the GeomManager layer.  So little
   ! processing is needed here.
   function parse_geom_spec(hconfig, rc) result(geom_hconfig)
      type(ESMF_HConfig) :: geom_hconfig
      type(ESMF_HConfig), optional, intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status

      geom_hconfig = ESMF_HConfigCreateAt(hconfig,keyString=COMPONENT_GEOM_SECTION, _RC)

      _RETURN(_SUCCESS)
   end function parse_geom_spec

   ! A component is not required to have var_specs.   E.g, in theory GCM gridcomp will not
   ! have var specs in MAPL3, as it does not really have a preferred geom on which to declare
   ! imports and exports.
   function parse_var_specs(hconfig, rc) result(var_specs)
      type(VariableSpecVector) :: var_specs
      type(ESMF_HConfig), optional, intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_states_section
      type(ESMF_HConfig) :: subcfg

      has_states_section = ESMF_HConfigIsDefined(hconfig,keyString=COMPONENT_STATES_SECTION, _RC)
      _RETURN_UNLESS(has_states_section)

      subcfg = ESMF_HConfigCreateAt(hconfig,keyString=COMPONENT_STATES_SECTION, _RC)

      call parse_state_specs(var_specs, subcfg, COMPONENT_INTERNAL_STATE_SECTION,  _RC)
      call parse_state_specs(var_specs, subcfg, COMPONENT_EXPORT_STATE_SECTION, _RC)
      call parse_state_specs(var_specs, subcfg, COMPONENT_IMPORT_STATE_SECTION, _RC)

      call ESMF_HConfigDestroy(subcfg, _RC)

      _RETURN(_SUCCESS)
   contains

      subroutine parse_state_specs(var_specs, hconfig, state_intent, rc)
         type(VariableSpecVector), intent(inout) :: var_specs
         type(ESMF_HConfig), target, intent(in) :: hconfig
         character(*), intent(in) :: state_intent
         integer, optional, intent(out) :: rc

         type(VariableSpec) :: var_spec
         type(ESMF_HConfigIter) :: iter,e,b
         character(:), allocatable :: name
         character(:), allocatable :: short_name
         character(:), allocatable :: substate
         type(ESMF_HConfig) :: attributes
         type(ESMF_TypeKind_Flag) :: typekind
         real, allocatable :: default_value
         type(VerticalDimSpec) :: vertical_dim_spec
         type(UngriddedDimsSpec) :: ungridded_dim_specs
         character(:), allocatable :: standard_name
         character(:), allocatable :: units
         type(ESMF_StateItem_Flag), allocatable :: itemtype
         type(ESMF_StateIntent_Flag) :: esmf_state_intent

         type(StringVector) :: service_items
         integer :: status
         logical :: has_state
         logical :: has_standard_name
         logical :: has_units
         type(ESMF_HConfig) :: subcfg

         has_state = ESMF_HConfigIsDefined(hconfig,keyString=state_intent, _RC)
         _RETURN_UNLESS(has_state)

         subcfg = ESMF_HConfigCreateAt(hconfig,keyString=state_intent, _RC)

         b = ESMF_HConfigIterBegin(subcfg, _RC) 
         e = ESMF_HConfigIterEnd(subcfg, _RC) 
         iter = ESMF_HConfigIterBegin(subcfg, _RC)
         do while (ESMF_HConfigIterLoop(iter,b,e))
            name = ESMF_HConfigAsStringMapKey(iter, _RC)
            attributes = ESMF_HConfigCreateAtMapVal(iter,_RC)

            call split(name, short_name, substate)

            typekind = to_typekind(attributes, _RC)
            call val_to_float(default_value, attributes, 'default_value', _RC)
            vertical_dim_spec = to_VerticalDimSpec(attributes,_RC)
            ungridded_dim_specs = to_UngriddedDimsSpec(attributes, _RC)

            has_standard_name = ESMF_HConfigIsDefined(attributes,keyString='standard_name', _RC)
            if (has_standard_name) then
               standard_name = ESMF_HConfigAsString(attributes,keyString='standard_name', _RC)
            end if

            has_units = ESMF_HConfigIsDefined(attributes,keyString='units', _RC)
            if (has_units) then
               units = ESMF_HConfigAsString(attributes,keyString='units', _RC)
            end if

            call to_itemtype(itemtype, attributes, _RC)
            call to_service_items(service_items, attributes, _RC)

            esmf_state_intent = to_esmf_state_intent(state_intent)
             
            var_spec = VariableSpec(esmf_state_intent, short_name=short_name, &
                 itemtype=itemtype, &
                 service_items=service_items, &
                 standard_name=standard_name, &
                 units=units, &
                 typekind=typekind, &
                 substate=substate, &
                 default_value=default_value, &
                 vertical_dim_spec=vertical_dim_spec, &
                 ungridded_dims=ungridded_dim_specs &
                 )

            call var_specs%push_back(var_spec)

            call ESMF_HConfigDestroy(attributes, _RC)
            
         end do

         call ESMF_HConfigDestroy(subcfg, _RC)

         _RETURN(_SUCCESS)
      end subroutine parse_state_specs

      subroutine split(name, short_name, substate)
         character(*), intent(in) :: name
         character(:), allocatable, intent(out) :: short_name
         character(:), allocatable, intent(out) :: substate

         integer :: idx

         idx = index(name, '/')
         if (idx == 0) then
            short_name = name
            return
         end if

         short_name = name(idx+1:)
         substate = name(:idx-1)
      end subroutine split

      subroutine val_to_float(x, attributes, key, rc)
         real, allocatable, intent(out) :: x
         type(ESMF_HConfig), intent(in) :: attributes
         character(*), intent(in) :: key
         integer, optional, intent(out) :: rc

         integer :: status
         logical :: has_default_value

         has_default_value = ESMF_HConfigIsDefined(attributes,keyString=KEY_DEFAULT_VALUE, _RC)
         _RETURN_UNLESS(has_default_value)

         allocate(x)
         x = ESMF_HConfigAsR4(attributes,keyString=KEY_DEFAULT_VALUE,_RC)

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
         select case (typekind_str)
         case ('R4')
            typekind = ESMF_TYPEKIND_R4
         case ('R8')
            typekind = ESMF_TYPEKIND_R8
         case ('I4')
            typekind = ESMF_TYPEKIND_I4
         case ('I8')
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

         vertical_dim_spec = VERTICAL_DIM_NONE ! GEOS default
         has_dim_spec = ESMF_HConfigIsDefined(attributes,keyString=KEY_VERTICAL_DIM_SPEC, _RC)
         _RETURN_UNLESS(has_dim_spec)
         
         vertical_str= ESMF_HConfigAsString(attributes,keyString=KEY_VERTICAL_DIM_SPEC,_RC)

         select case (vertical_str)
         case ('vertical_dim_none', 'N')
            vertical_dim_spec = VERTICAL_DIM_NONE
         case ('vertical_dim_center', 'C')
            vertical_dim_spec = VERTICAL_DIM_CENTER
         case ('vertical_dim_edge', 'E')
            vertical_dim_spec = VERTICAL_DIM_EDGE
         case default
            _FAIL('Unsupported vertical_dim_spec')
         end select

         _RETURN(_SUCCESS)
      end function to_VerticalDimSpec

      function to_UngriddedDimsSpec(attributes,rc) result(ungridded_dims_spec)
         type(UngriddedDimsSpec) :: ungridded_dims_spec
         type(ESMF_HConfig), intent(in) :: attributes
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_HConfig) :: dim_specs, dim_spec
         character(len=:), allocatable :: dim_name
         integer :: dim_size,i
         type(UngriddedDimSpec) :: temp_dim_spec

         logical :: has_ungridded_dim_specs
         integer :: n_specs

         has_ungridded_dim_specs = ESMF_HConfigIsDefined(attributes, keyString=KEY_UNGRIDDED_DIM_SPECS, _RC)
         _RETURN_UNLESS(has_ungridded_dim_specs)

         dim_specs = ESMF_HConfigCreateAt(attributes, keyString=KEY_UNGRIDDED_DIM_SPECS, _RC)

         n_specs = ESMF_HConfigGetSize(dim_specs, _RC)
         do i = 1, n_specs
            dim_spec = ESMF_HConfigCreateAt(dim_specs, index=i, _RC)
            dim_name = ESMF_HConfigAsString(dim_spec, keyString=KEY_UNGRIDDED_DIM_NAME, _RC)
            dim_size = ESMF_HConfigAsI4(dim_spec, keyString=KEY_UNGRIDDED_DIM_EXTENT, _RC)
            temp_dim_spec = UngriddedDimSpec(dim_size)
            call ungridded_dims_spec%add_dim_spec(temp_dim_spec, _RC)
            call ESMF_HConfigDestroy(dim_spec, _RC)
         end do 

         call ESMF_HConfigDestroy(dim_specs, _RC)
         
         _RETURN(_SUCCESS)
      end function to_UngriddedDimsSpec


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

         select case (subclass)
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
      
   end function parse_var_specs


   type(ConnectionVector) function parse_connections(hconfig, rc) result(connections)
      type(ESMF_HConfig), optional, intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      type(ESMF_HConfig) :: conn_specs, conn_spec
      class(Connection), allocatable :: conn
      integer :: status, i, num_specs
      logical :: has_connections

      has_connections = ESMF_HConfigIsDefined(hconfig,keyString=COMPONENT_CONNECTIONS_SECTION,_RC)
      _RETURN_UNLESS(has_connections)

      conn_specs = ESMF_HConfigCreateAt(hconfig, keyString=COMPONENT_CONNECTIONS_SECTION, _RC)

      num_specs = ESMF_HConfigGetSize(conn_specs, _RC)
      do i = 1, num_specs
         conn_spec = ESMF_HConfigCreateAt(conn_specs, index=i, _RC)
         conn = parse_connection(conn_spec, _RC)
         call connections%push_back(conn)
      enddo 

      _RETURN(_SUCCESS)

   contains

      function parse_connection(config, rc) result(conn)
         class(Connection), allocatable :: conn
         type(ESMF_HConfig), optional, intent(in) :: config
         integer, optional, intent(out) :: rc

         integer :: status
         character(:), allocatable :: src_name, dst_name
         character(:), allocatable :: src_comp, dst_comp
         character(:), allocatable :: src_intent, dst_intent

         call get_comps(config, src_comp, dst_comp, _RC)

         if (ESMF_HConfigIsDefined(config,keyString='all_unsatisfied')) then
            conn = MatchConnection( &
                 ConnectionPt(src_comp, VirtualConnectionPt(state_intent='export', short_name='^.*$')), &
                 ConnectionPt(dst_comp, VirtualConnectionPt(state_intent='import', short_name='^.*$'))  &
                 )
            _RETURN(_SUCCESS)
         end if
            
         call get_names(config, src_name, dst_name, _RC)
         call get_intents(config, src_intent, dst_intent, _RC)

         associate ( &
              src_pt => VirtualConnectionPt(state_intent=src_intent, short_name=src_name), &
              dst_pt => VirtualConnectionPt(state_intent=dst_intent, short_name=dst_name) )

           if (dst_intent == 'export') then
              conn = ReexportConnection( &
                ConnectionPt(src_comp, src_pt), &
                ConnectionPt(dst_comp, dst_pt))
           else
              conn = SimpleConnection( &
                   ConnectionPt(src_comp, src_pt), &
                   ConnectionPt(dst_comp, dst_pt))
           end if

         end associate

         _RETURN(_SUCCESS)
      end function parse_connection

      subroutine get_names(config, src_name, dst_name, rc)
         type(ESMF_HConfig), intent(in) :: config
         character(:), allocatable :: src_name
         character(:), allocatable :: dst_name
         integer, optional, intent(out) :: rc

         integer :: status

         associate (provides_names => &
              ESMF_HConfigIsDefined(config,keyString='name') .or. &
              (ESMF_HConfigIsDefined(config,keyString='src_name') .and. ESMF_HConfigIsDefined(config,keyString='dst_name')) &
              )
           _ASSERT(provides_names, "Must specify 'name' or 'src_name' .and. 'dst_name' in connection.")
         end associate

         if (ESMF_HConfigIsDefined(Config,keystring='name')) then ! replicate for src and dst
            src_name = ESMF_HConfigAsString(config,keyString='name',_RC)
            dst_name = src_name
            _RETURN(_SUCCESS)
         end if

         src_name = ESMF_HConfigAsString(config,keyString='src_name',_RC)
         dst_name = ESMF_HConfigAsString(config,keyString='dst_name',_RC)

         _RETURN(_SUCCESS)
      end subroutine get_names

      subroutine get_comps(config, src_comp, dst_comp, rc)
         type(ESMF_HConfig), intent(in) :: config
         character(:), allocatable :: src_comp
         character(:), allocatable :: dst_comp
         integer, optional, intent(out) :: rc

         integer :: status
         
         _ASSERT(ESMF_HConfigIsDefined(config,keyString='src_comp'), 'Connection must specify a src component')
         _ASSERT(ESMF_HConfigIsDefined(config,keyString='dst_comp'), 'Connection must specify a dst component')
         src_comp = ESMF_HConfigAsString(config,keyString='src_comp',_RC)
         dst_comp = ESMF_HConfigAsString(config,keyString='dst_comp',_RC)
         _RETURN(_SUCCESS)
      end subroutine get_comps

      subroutine get_intents(config, src_intent, dst_intent, rc)
         type(ESMF_HConfig), intent(in) :: config
         character(:), allocatable :: src_intent
         character(:), allocatable :: dst_intent
         integer, optional, intent(out) :: rc

         integer :: status

         ! defaults
         src_intent = 'export'
         dst_intent = 'import'

         if (ESMF_HConfigIsDefined(config,keyString='src_intent')) then
            src_intent = ESMF_HConfigAsString(config,keyString='src_intent',_RC)
         end if
         if (ESMF_HConfigIsDefined(config,keyString='dst_intent')) then
            dst_intent = ESMF_HConfigAsString(config,keyString='dst_intent',_RC)
         end if

         _RETURN(_SUCCESS)
      end subroutine get_intents

   end function parse_connections

   
   type(DSOSetServices) function parse_setservices(config, rc) result(user_ss)
      type(ESMF_HConfig), target, intent(in) :: config
      integer, optional, intent(out) :: rc

      character(:), allocatable :: sharedObj, userRoutine
      integer :: status

      sharedObj = ESMF_HConfigAsString(config,keyString='sharedObj',rc=status)
      _ASSERT(status == 0, 'setServices spec does not specify sharedObj')

      if (ESMF_HConfigIsDefined(config,keyString='userRoutine')) then
         userRoutine = ESMF_HConfigAsString(config,keyString='userRoutine',_RC)
      else
         userRoutine = 'setservices_'
      end if

      user_ss = user_setservices(sharedObj, userRoutine)
      
      _RETURN(_SUCCESS)
   end function parse_setservices


   function parse_children(hconfig, rc) result(children)
      type(ChildSpecMap) :: children
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_children
      logical :: is_map
      type(ESMF_HConfig) :: children_cfg, child_cfg
      type(ESMF_HConfigIter) :: iter, iter_begin, iter_end
      type(ChildSpec) :: child_spec
      character(:), allocatable :: child_name


      has_children = ESMF_HConfigIsDefined(hconfig, keyString=COMPONENT_CHILDREN_SECTION, _RC)
      _RETURN_UNLESS(has_children)

      children_cfg = ESMF_HConfigCreateAt(hconfig, keyString=COMPONENT_CHILDREN_SECTION, _RC)
      is_map = ESMF_HConfigIsMap(children_cfg, _RC)

      _ASSERT(is_map, 'children spec must be mapping')

      iter_begin = ESMF_HCOnfigIterBegin(children_cfg, _RC)
      iter_end = ESMF_HConfigIterEnd(children_cfg, _RC)
      iter = ESMF_HConfigIterBegin(children_cfg, _RC)
      do while (ESMF_HConfigIterLoop(iter, iter_begin, iter_end))
         child_name = ESMF_HConfigAsStringMapKey(iter, _RC)
         child_cfg = ESMF_HConfigCreateAtMapVal(iter, _RC)
         child_spec = parse_child(child_cfg, _RC)
         call children%insert(child_name, child_spec)
         call ESMF_HConfigDestroy(child_cfg, _RC)
      end do

      call ESMF_HConfigDestroy(children_cfg, _RC)

      _RETURN(_SUCCESS)
   end function parse_children


   function parse_child(hconfig, rc) result(child)
      type(ChildSpec) :: child
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      class(AbstractUserSetServices), allocatable :: setservices

      character(*), parameter :: dso_keys(*) = [character(len=9) :: 'dso', 'DSO', 'sharedObj', 'sharedobj']
      character(*), parameter :: userProcedure_keys(*) = [character(len=10) :: 'SetServices', 'setServices', 'setservices']
      integer :: i
      character(:), allocatable :: dso_key, userProcedure_key, try_key
      logical :: dso_found, userProcedure_found
      logical :: has_key
      logical :: has_config_file
      character(:), allocatable :: sharedObj, userProcedure, config_file


      dso_found = .false.
      ! Ensure precisely one name is used for dso
      do i = 1, size(dso_keys)
         try_key = trim(dso_keys(i))
         has_key = ESMF_HconfigIsDefined(hconfig, keyString=try_key, _RC)
         if (has_key) then
            _ASSERT(.not. dso_found, 'multiple specifications for dso in hconfig for child')
            dso_found = .true.
            dso_key = try_key
         end if
      end do
      _ASSERT(dso_found, 'Must specify a dso for hconfig of child')
      sharedObj = ESMF_HconfigAsString(hconfig, keyString=dso_key, _RC)

      userProcedure_found = .false.
      do i = 1, size(userProcedure_keys)
         try_key = userProcedure_keys(i)
         if (ESMF_HconfigIsDefined(hconfig, keyString=try_key)) then
            _ASSERT(.not. userProcedure_found, 'multiple specifications for dso in hconfig for child')
            userProcedure_found = .true.
            userProcedure_key = try_key
         end if
      end do
      userProcedure = 'setservices_'         
      if (userProcedure_found) then
         userProcedure = ESMF_HconfigAsString(hconfig, keyString=userProcedure_key,_RC)
      end if

      has_config_file = ESMF_HconfigIsDefined(hconfig, keyString='config_file', _RC)
      if (has_config_file) then
         config_file = ESMF_HconfigAsString(hconfig, keyString='config_file',_RC)
      end if

      setservices = user_setservices(sharedObj, userProcedure)
      child = ChildSpec(setservices, config_file=config_file)

      _RETURN(_SUCCESS)
   end function parse_child

end module mapl3g_ComponentSpecParser
