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
   use yaFyaml
   use gftl2_StringVector, only: StringVector
   use esmf
   implicit none
   private

   ! 
   public :: parse_component_spec

   ! The following interfaces are public only for testing purposes.
   public :: parse_ChildSpecMap
   public :: parse_ChildSpec
   public :: parse_SetServices
   public :: var_parse_ChildSpecMap

   !public :: parse_UngriddedDimsSpec
   
contains
   type(ComponentSpec) function parse_component_spec(config, rc) result(spec)
      type(ESMF_HConfig), target, intent(inout) :: config
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_HConfig) :: subcfg

      if (ESMF_HConfigIsDefined(config,keyString='states')) then
         subcfg = ESMF_HConfigCreateAt(config,keyString='states',_RC)
         spec%var_specs = process_var_specs(subcfg)
      end if

      if (ESMF_HConfigIsDefined(config,keyString='connections')) then
         subcfg = ESMF_HConfigCreateAt(config,keyString='connections',_RC)
         spec%connections = process_connections(subcfg)
      end if
!!$      spec%grid_spec = process_grid_spec(config%of('grid', _RC)
!!$      spec%services_spec = process_grid_spec(config%of('serviceservices', _RC)

      _RETURN(_SUCCESS)
   end function parse_component_spec


   function process_var_specs(config, rc) result(var_specs)
      type(VariableSpecVector) :: var_specs
      type(ESMF_HConfig), optional, intent(in) :: config
      integer, optional, intent(out) :: rc

      integer :: status

      if (.not. present(config)) then
         _RETURN(_SUCCESS)
      end if

      if (ESMF_HConfigIsDefined(config,keyString='internal')) then
         call process_state_specs(var_specs, ESMF_HConfigCreateAt(config,keyString='internal'), ESMF_STATEINTENT_INTERNAL, _RC)
      end if
      if (ESMF_HConfigIsDefined(config,keyString='import')) then
         call process_state_specs(var_specs, ESMF_HConfigCreateAt(config,keyString='import'), ESMF_STATEINTENT_IMPORT, _RC)
      end if
      if (ESMF_HConfigIsDefined(config,keyString='export')) then
         call process_state_specs(var_specs, ESMF_HConfigCreateAt(config,keyString='export'), ESMF_STATEINTENT_EXPORT, _RC)
      end if

      _RETURN(_SUCCESS)
   contains

      subroutine process_state_specs(var_specs, config, state_intent, rc)
         type(VariableSpecVector), intent(inout) :: var_specs
         type(ESMF_HConfig), target, intent(in) :: config
         type(Esmf_StateIntent_Flag), intent(in) :: state_intent
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
         type(UngriddedDimsSpec) :: ungridded_dims_spec
         character(:), allocatable :: standard_name
         character(:), allocatable :: units
         type(ESMF_StateItem_Flag), allocatable :: itemtype

         type(StringVector), allocatable :: service_items

         b = ESMF_HConfigIterBegin(config) 
         e = ESMF_HConfigIterEnd(config) 
         iter = ESMF_HConfigIterBegin(config)
         do while (ESMF_HConfigIterLoop(iter,b,e))
            name = ESMF_HConfigAsStringMapKey(iter,_RC)
            attributes = ESMF_HConfigCreateAtMapVal(iter,_RC)

            call split(name, short_name, substate)
            call to_typekind(typekind, attributes, _RC)
            call val_to_float(default_value, attributes, 'default_value', _RC)

            call to_VerticalDimSpec(vertical_dim_spec,attributes,_RC)

            call to_UngriddedDimsSpec(ungridded_dims_spec,attributes,_RC)

            if (ESMF_HConfigIsDefined(attributes,keyString='standard_name')) then
               standard_name = ESMF_HConfigAsString(attributes,keyString='standard_name',_RC)
            end if
            
            if (ESMF_HConfigIsDefined(attributes,keyString='units')) then
               standard_name = ESMF_HConfigAsString(attributes,keyString='units',_RC)
            end if

            call to_itemtype(itemtype, attributes, _RC)
            call to_service_items(service_items, attributes, _RC)
            
            var_spec = VariableSpec(state_intent, short_name=short_name, &
                 itemtype=itemtype, &
                 service_items=service_items, &
                 standard_name=standard_name, &
                 units=units, &
                 typekind=typekind, &
                 substate=substate, &
                 default_value=default_value, &
                 vertical_dim_spec = vertical_dim_spec, &
                 ungridded_dims = ungridded_dims_spec &
                 )

            call var_specs%push_back(var_spec)
         end do

         _RETURN(_SUCCESS)
      end subroutine process_state_specs

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

         _RETURN_UNLESS(ESMF_HConfigIsDefined(attributes,keyString='default_value'))
         allocate(x)
         x = ESMF_HConfigAsR4(attributes,keyString='default_vale',_RC)

         _RETURN(_SUCCESS)
      end subroutine val_to_float

      subroutine to_typekind(typekind, attributes, rc)
         type(ESMF_TypeKind_Flag) :: typekind
         type(ESMF_HConfig), intent(in) :: attributes
         integer, optional, intent(out) :: rc

         integer :: status
         character(:), allocatable :: typekind_str

         typekind = ESMF_TYPEKIND_R4 ! GEOS default
         if (.not. ESMF_HConfigIsDefined(attributes,keyString='typekind')) then
            _RETURN(_SUCCESS)
         end if
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
         case default
            _FAIL('Unsupported typekind')
         end select

         _RETURN(_SUCCESS)
      end subroutine to_typekind

      subroutine to_VerticalDimSpec(vertical_dim_spec, attributes, rc)
         type(VerticalDimSpec) :: vertical_dim_spec
         type(ESMF_HConfig), intent(in) :: attributes
         integer, optional, intent(out) :: rc

         integer :: status
         character(:), allocatable :: vertical_str

         vertical_dim_spec = VERTICAL_DIM_NONE ! GEOS default
         
         if (.not. ESMF_HConfigIsDefined(attributes,keyString='vertical_dim_spec')) then
            _RETURN(_SUCCESS)
         end if
         vertical_str= ESMF_HConfigAsString(attributes,keyString='vertical_dim_spec',_RC)

         select case (vertical_str)
         case ('vertical_dim_none', 'N')
            vertical_dim_spec = VERTICAL_DIM_NONE
         case ('vertical_dim_center', 'C')
            vertical_dim_spec = VERTICAL_DIM_CENTER
         case ('vertical_dim_edge', 'E')
            vertical_dim_spec = VERTICAL_DIM_EDGE
         case default
            _FAIL('Unsupported typekind')
         end select

         _RETURN(_SUCCESS)
      end subroutine to_VerticalDimSpec

      subroutine to_UngriddedDimsSpec(ungridded_dims_spec,attributes,rc)
         type(UngriddedDimsSpec) :: ungridded_dims_spec
         type(ESMF_HConfig), intent(in) :: attributes
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_HConfig) :: dim_specs, dim_spec
         character(len=:), allocatable :: dim_name
         integer :: dim_size,i
         type(UngriddedDimSpec) :: temp_dim_spec

         if (.not. ESMF_HConfigIsDefined(config,keyString='ungridded_dim_specs')) then
            _RETURN(_SUCCESS)
         end if
         dim_specs = ESMF_HConfigCreateAt(config,keyString='ungridded_dim_specs',_RC)
         
         do i=1,ESMF_HConfigGetSize(dim_specs)
            dim_spec = ESMF_HConfigCreateAt(dim_specs,index=i,_RC)
            dim_name = ESMF_HConfigAsString(dim_spec,keyString='dim_name',_RC)
            dim_size = ESMF_HConfigAsI4(dim_spec,keyString='extent',_RC)
            temp_dim_spec = UngriddedDimSpec(dim_size)
            call ungridded_dims_spec%add_dim_spec(temp_dim_spec,_RC)
         end do 

         _RETURN(_SUCCESS)
      end subroutine to_UngriddedDimsSpec


      subroutine to_itemtype(itemtype, attributes, rc)
         type(ESMF_StateItem_Flag), allocatable, intent(out) :: itemtype
         type(ESMF_HConfig), target, intent(in) :: attributes
         integer, optional, intent(out) :: rc

         integer :: status
         character(:), allocatable :: subclass

         if (.not. ESMF_HConfigIsDefined(config,keyString='class')) then
            _RETURN(_SUCCESS)
         end if

         subclass= ESMF_HConfigAsString(config,keyString='class',_RC) 

         select case (subclass)
         case ('field')
            itemtype = MAPL_STATEITEM_FIELD
         case ('service')
            itemtype = MAPL_STATEITEM_SERVICE
         case default
            _FAIL('unknown subclass for state item: '//subclass)
         end select

         _RETURN(_SUCCESS)
      end subroutine to_itemtype
      
      subroutine to_service_items(service_items, attributes, rc)
         type(StringVector), allocatable, intent(out) :: service_items
         type(ESMF_HConfig), target, intent(in) :: attributes
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_HConfig) :: seq, item 
         integer :: num_items, i
         character(:), allocatable :: item_name

         if (.not. ESMF_HConfigIsDefined(attributes,keyString='items')) then
            _RETURN(_SUCCESS)
         end if

         allocate(service_items)
       
         seq = ESMF_HConfigCreateAt(attributes,keyString='items',_RC)
         _ASSERT(ESMF_HConfigIsSequence(seq),"items must be a sequence")
         num_items = ESMF_HConfigGetSize(seq,_RC) 
         do i = 1,num_items
            item_name = ESMF_HConfigAsString(seq,index = i, _RC)
            call service_items%push_back(item_name)
         end do

         _RETURN(_SUCCESS)
      end subroutine to_service_items
      
   end function process_var_specs


   type(ConnectionVector) function process_connections(config, rc) result(connections)
      type(ESMF_HConfig), optional, intent(in) :: config
      integer, optional, intent(out) :: rc

      type(ESMF_HConfig) :: conn_spec
      class(Connection), allocatable :: conn
      integer :: status, i, num_specs

      if (.not. present(config)) then
         _RETURN(_SUCCESS)
      end if

      num_specs = ESMF_HConfigGetSize(config,_RC)
      do i =1,num_specs
         conn_spec = ESMF_HConfigCreateAt(config,index=i,_RC)
         conn = process_connection(conn_spec, _RC)
         call connections%push_back(conn)
      enddo 

      _RETURN(_SUCCESS)
   contains

      function process_connection(config, rc) result(conn)
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
                 ConnectionPt(src_comp, VirtualConnectionPt(state_intent='export', short_name='*')), &
                 ConnectionPt(dst_comp, VirtualConnectionPt(state_intent='import', short_name='*'))  &
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
      end function process_connection

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

   end function process_connections

   
   type(ChildSpec) function parse_ChildSpec(config, rc) result(child_spec)
      type(ESMF_HConfig), intent(in) :: config
      integer, optional, intent(out) :: rc

      type(ESMF_HConfig) :: subcfg
      integer :: status

      _ASSERT(ESMF_HConfigIsDefined(config,keyString='setServices'),"child spec must specify a 'setServices' spec")
      subcfg = ESMF_HConfigCreateAt(config,keyString='setServices',_RC)
      child_spec%user_setservices = parse_setservices(subcfg, _RC)

      if (ESMF_HConfigIsDefined(config,keyString='esmf_config')) then
         child_spec%esmf_config_file = ESMF_HConfigAsString(config,keyString='esmf_config',_RC)
      end if
      if (ESMF_HConfigIsDefined(config,keyString='yaml_config')) then
         child_spec%yaml_config_file = ESMF_HConfigAsString(config,keyString='yaml_config',_RC)
      end if

      _RETURN(_SUCCESS)
   end function parse_ChildSpec

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


   ! Note: It is convenient to allow a null pointer for the config in
   ! the case of no child specs.  It spares the higher level procedure
   ! making the relevant check.

   type(ChildSpecMap) function parse_ChildSpecMap(config, rc) result(specs)
      type(ESMF_HConfig), pointer, intent(in) :: config
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_HConfigIter) :: hconfigIter,hconfigIterBegin,hconfigIterEnd

      character(:), allocatable :: child_name
      type(ChildSpec) :: child_spec
      class(NodeIterator), allocatable :: iter
      type(ESMF_HConfig) :: subcfg

      if (.not. associated(config)) then
         specs = ChildSpecMap()
         _RETURN(_SUCCESS)
      end if
      _ASSERT(ESMF_HConfigIsMap(config), 'children spec must be mapping of names to child specs')
      

      hconfigIter = ESMF_HConfigIterBegin(config,_RC)
      hconfigIterBegin = ESMF_HConfigIterBegin(config,_RC)
      hconfigIterEnd = ESMF_HConfigIterEnd(config,_RC)
      do while (ESMF_HConfigIterLoop(hconfigIter,hconfigIterBegin,hconfigIterEnd))
         child_name = ESMF_HConfigAsStringMapKey(hconfigIter)
         subcfg = ESMF_HConfigCreateAtMapVal(hconfigIter)
         child_spec = parse_ChildSpec(subcfg)
         call specs%insert(child_name, child_spec)
      end do

      _RETURN(_SUCCESS)
   end function parse_ChildSpecMap

   type(ChildSpecMap) function var_parse_ChildSpecMap(config, rc) result(specs)
      type(ESMF_HConfig), pointer, intent(in) :: config
      integer, optional, intent(out) :: rc

      integer :: status

      type(ESMF_HConfigIter) :: hconfigIter,hconfigIterBegin,hconfigIterEnd
      character(:), allocatable :: child_name
      type(ESMF_HConfig) :: subcfg
      type(ChildSpec) :: child_spec

      type(ChildSpecMap) :: kludge
      integer :: counter
      
      counter = 0
!!$      specs = ChildSpecMap()
      
      if (.not. associated(config)) then
         specs = ChildSpecMap()
         _RETURN(_SUCCESS)
      end if
      _ASSERT(ESMF_HConfigIsMap(config), 'children spec must be mapping of names to child specs')
      hconfigIter = ESMF_HConfigIterBegin(config,_RC)
      hconfigIterBegin = ESMF_HConfigIterBegin(config,_RC)
      hconfigIterEnd = ESMF_HConfigIterEnd(config,_RC)
      do while (ESMF_HConfigIterLoop(hconfigIter,hconfigIterBegin,hconfigIterEnd)) 
        counter = counter + 1
        child_name = ESMF_HConfigAsStringMapKey(hconfigIter,_RC)
        subcfg = ESMF_HConfigCreateAtMapVal(hconfigIter)
        child_spec = parse_ChildSpec(subcfg, _RC)
        call specs%insert(child_name, child_spec)
      end do

!!$      call specs%deep_copy(kludge)
      specs = kludge
      _RETURN(_SUCCESS)
   end function var_parse_ChildSpecMap

      

   function parse_UngriddedDimsSpec(config, rc) result(dims_spec)
      use mapl3g_UngriddedDimsSpec
      type(UngriddedDimsSpec) :: dims_spec
      type(ESMF_HConfig), pointer, intent(in) :: config
      integer, optional, intent(out) :: rc

!!$      dims_spec = UngriddedDimsSpec()
      
   end function parse_UngriddedDimsSpec
   
end module mapl3g_ComponentSpecParser
