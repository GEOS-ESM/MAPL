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

   public :: parse_UngriddedDimsSpec
   
contains

   type(ComponentSpec) function parse_component_spec(config, rc) result(spec)
      class(YAML_Node), target, intent(inout) :: config
      integer, optional, intent(out) :: rc

      integer :: status

      if (config%has('states')) then
         spec%var_specs = process_var_specs(config%of('states'), _RC)
      end if

      if (config%has('connections')) then
         spec%connections = process_connections(config%of('connections'), _RC)
      end if
!!$      spec%grid_spec = process_grid_spec(config%of('grid', _RC)
!!$      spec%services_spec = process_grid_spec(config%of('serviceservices', _RC)

      _RETURN(_SUCCESS)
   end function parse_component_spec


   function process_var_specs(config, rc) result(var_specs)
      type(VariableSpecVector) :: var_specs
      class(YAML_Node), optional, intent(in) :: config
      integer, optional, intent(out) :: rc

      integer :: status

      if (.not. present(config)) then
         _RETURN(_SUCCESS)
      end if

      if (config%has('internal')) then
         call process_state_specs(var_specs, config%of('internal'), ESMF_STATEINTENT_INTERNAL, _RC)
      end if
      if (config%has('import')) then
         call process_state_specs(var_specs, config%of('import'), ESMF_STATEINTENT_IMPORT, _RC)
      end if
      if (config%has('export')) then
         call process_state_specs(var_specs, config%of('export'), ESMF_STATEINTENT_EXPORT, _RC)
      end if

      _RETURN(_SUCCESS)
   contains

      subroutine process_state_specs(var_specs, config, state_intent, rc)
         type(VariableSpecVector), intent(inout) :: var_specs
         class(YAML_Node), target, intent(in) :: config
         type(Esmf_StateIntent_Flag), intent(in) :: state_intent
         integer, optional, intent(out) :: rc

         type(VariableSpec) :: var_spec
         class(NodeIterator), allocatable :: iter, e
         character(:), pointer :: name
         character(:), allocatable :: short_name
         character(:), allocatable :: substate
         class(YAML_Node), pointer :: attributes
         type(ESMF_TypeKind_Flag) :: typekind
         real, allocatable :: default_value
         type(VerticalDimSpec) :: vertical_dim_spec
         type(UngriddedDimsSpec) :: ungridded_dims_spec
         character(:), allocatable :: standard_name
         character(:), allocatable :: units
         type(ESMF_StateItem_Flag), allocatable :: itemtype

         type(StringVector), allocatable :: service_items

         allocate(e, source=config%end())
         allocate(iter, source=config%begin())
         do while (iter /= e)
            name => to_string(iter%first())
            attributes => iter%second()

            call split(name, short_name, substate)
            call to_typekind(typekind, attributes, _RC)
            call val_to_float(default_value, attributes, 'default_value', _RC)

            call to_VerticalDimSpec(vertical_dim_spec,attributes,_RC)

            call to_UngriddedDimsSpec(ungridded_dims_spec,attributes,_RC)

            if (attributes%has('standard_name')) then
               standard_name = to_string(attributes%of('standard_name'))
            end if
            
            if (attributes%has('units')) then
               units = to_string(attributes%of('units'))
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
            call iter%next()
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
         class(YAML_Node), intent(in) :: attributes
         character(*), intent(in) :: key
         integer, optional, intent(out) :: rc

         integer :: status

         _RETURN_UNLESS(attributes%has('default_value'))
         allocate(x)
         call attributes%get(x, 'default_value', _RC)

         _RETURN(_SUCCESS)
      end subroutine val_to_float

      subroutine to_typekind(typekind, attributes, rc)
         type(ESMF_TypeKind_Flag) :: typekind
         class(YAML_Node), intent(in) :: attributes
         integer, optional, intent(out) :: rc

         integer :: status
         character(:), allocatable :: typekind_str

         typekind = ESMF_TYPEKIND_R4 ! GEOS default
         if (.not. attributes%has('typekind')) then
            _RETURN(_SUCCESS)
         end if
         call attributes%get(typekind_str, 'typekind', _RC)

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
         class(YAML_Node), intent(in) :: attributes
         integer, optional, intent(out) :: rc

         integer :: status
         character(:), allocatable :: vertical_str

         vertical_dim_spec = VERTICAL_DIM_NONE ! GEOS default
         
         if (.not. attributes%has('vertical_dim_spec')) then
            _RETURN(_SUCCESS)
         end if
         call attributes%get(vertical_str, 'vertical_dim_spec', _RC)

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
         class(YAML_Node), intent(in) :: attributes
         integer, optional, intent(out) :: rc

         integer :: status
         class(YAML_Node), pointer :: dim_specs, dim_spec
         character(len=:), allocatable :: dim_name
         integer :: dim_size,i
         type(UngriddedDimSpec) :: temp_dim_spec

         if (.not.attributes%has('ungridded_dim_specs')) then
            _RETURN(_SUCCESS)
         end if
         dim_specs => attributes%of('ungridded_dim_specs')
         do i=1,dim_specs%size()
            dim_spec => dim_specs%of(i) 
            call dim_spec%get(dim_name,'dim_name',_RC)
            call dim_spec%get(dim_size,'extent',_RC)            
            temp_dim_spec = UngriddedDimSpec(dim_size)
            call ungridded_dims_spec%add_dim_spec(temp_dim_spec,_RC)
         end do 

         _RETURN(_SUCCESS)
      end subroutine to_UngriddedDimsSpec


      subroutine to_itemtype(itemtype, attributes, rc)
         type(ESMF_StateItem_Flag), allocatable, intent(out) :: itemtype
         class(YAML_Node), target, intent(in) :: attributes
         integer, optional, intent(out) :: rc

         integer :: status
         character(:), allocatable :: subclass

         if (.not. attributes%has('class')) then
            _RETURN(_SUCCESS)
         end if

         call attributes%get(subclass, 'class', _RC)

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
         class(YAML_Node), target, intent(in) :: attributes
         integer, optional, intent(out) :: rc

         integer :: status
         class(YAML_Node), pointer :: seq
         class(YAML_Node), pointer :: item
         class(NodeIterator), allocatable :: seq_iter
         character(:), pointer :: item_name

         if (.not. attributes%has('items')) then
            _RETURN(_SUCCESS)
         end if

         allocate(service_items)
         seq => attributes%of('items')
         associate (e => seq%end())
           seq_iter = seq%begin()
           do while (seq_iter /= e)
              item => seq_iter%at(_RC)
              item_name => to_string(item, _RC)
              call service_items%push_back(item_name)
              call seq_iter%next()
           end do
         end associate

         _RETURN(_SUCCESS)
      end subroutine to_service_items
      
   end function process_var_specs


   type(ConnectionVector) function process_connections(config, rc) result(connections)
      class(YAML_Node), optional, intent(in) :: config
      integer, optional, intent(out) :: rc

      class(NodeIterator), allocatable :: iter, e
      class(Connection), allocatable :: conn
      class(YAML_Node), pointer :: conn_spec
      integer :: status

      if (.not. present(config)) then
         _RETURN(_SUCCESS)
      end if

      allocate(e, source=config%end())
      allocate(iter, source=config%begin())
      do while (iter /= e)
         conn_spec => iter%at(_RC)
         conn = process_connection(conn_spec, _RC)
         call connections%push_back(conn)
         call iter%next()
      end do

      _RETURN(_SUCCESS)
   contains

      function process_connection(config, rc) result(conn)
         class(Connection), allocatable :: conn
         class(YAML_Node), optional, intent(in) :: config
         integer, optional, intent(out) :: rc

         integer :: status
         character(:), allocatable :: src_name, dst_name
         character(:), allocatable :: src_comp, dst_comp
         character(:), allocatable :: src_intent, dst_intent

         call get_comps(config, src_comp, dst_comp, _RC)

         if (config%has('all_unsatisfied')) then
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
         class(YAML_Node), intent(in) :: config
         character(:), allocatable :: src_name
         character(:), allocatable :: dst_name
         integer, optional, intent(out) :: rc

         integer :: status

         associate (provides_names => &
              config%has('name') .or. &
              (config%has('src_name') .and. config%has('dst_name')) &
              )
           _ASSERT(provides_names, "Must specify 'name' or 'src_name' .and. 'dst_name' in connection.")
         end associate

         if (config%has('name')) then ! replicate for src and dst
            call config%get(src_name, 'name', _RC)
            dst_name = src_name
            _RETURN(_SUCCESS)
         end if

         call config%get(src_name, 'src_name', _RC)
         call config%get(dst_name, 'dst_name', _RC)

         _RETURN(_SUCCESS)
      end subroutine get_names

      subroutine get_comps(config, src_comp, dst_comp, rc)
         class(YAML_Node), intent(in) :: config
         character(:), allocatable :: src_comp
         character(:), allocatable :: dst_comp
         integer, optional, intent(out) :: rc

         integer :: status
         
         _ASSERT(config%has('src_comp'), 'Connection must specify a src component')
         _ASSERT(config%has('dst_comp'), 'Connection must specify a dst component')
         call config%get(src_comp, 'src_comp', _RC)
         call config%get(dst_comp, 'dst_comp', _RC)
         _RETURN(_SUCCESS)
      end subroutine get_comps

      subroutine get_intents(config, src_intent, dst_intent, rc)
         class(YAML_Node), intent(in) :: config
         character(:), allocatable :: src_intent
         character(:), allocatable :: dst_intent
         integer, optional, intent(out) :: rc

         integer :: status

         ! defaults
         src_intent = 'export'
         dst_intent = 'import'

         if (config%has('src_intent')) then
            call config%get(src_intent,'src_intent', _RC)
         end if
         if (config%has('dst_intent')) then
            call config%get(dst_intent,'dst_intent', _RC)
         end if

         _RETURN(_SUCCESS)
      end subroutine get_intents

   end function process_connections

   
   type(ChildSpec) function parse_ChildSpec(config, rc) result(child_spec)
      class(YAML_Node), intent(in) :: config
      integer, optional, intent(out) :: rc

      integer :: status

      _ASSERT(config%has('setServices'),"child spec must specify a 'setServices' spec")
      child_spec%user_setservices = parse_setservices(config%of('setServices'), _RC)

      if (config%has('esmf_config')) then
         call config%get(child_spec%esmf_config_file, 'esmf_config', _RC)
      end if

      if (config%has('yaml_config')) then
         call config%get(child_spec%yaml_config_file, 'yaml_config', _RC)
      end if

      _RETURN(_SUCCESS)
   end function parse_ChildSpec

   type(DSOSetServices) function parse_setservices(config, rc) result(user_ss)
      class(YAML_Node), target, intent(in) :: config
      integer, optional, intent(out) :: rc

      character(:), allocatable :: sharedObj, userRoutine
      integer :: status

      call config%get(sharedObj, 'sharedObj', rc=status)
      _ASSERT(status == 0, 'setServices spec does not specify sharedObj')

      if (config%has('userRoutine')) then
         call config%get(userRoutine, 'userRoutine', _RC)
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
      class(YAML_Node), pointer, intent(in) :: config
      integer, optional, intent(out) :: rc

      integer :: status

      character(:), pointer :: child_name
      type(ChildSpec) :: child_spec
      class(NodeIterator), allocatable :: iter
      class(YAML_Node), pointer :: subcfg

      if (.not. associated(config)) then
         specs = ChildSpecMap()
         _RETURN(_SUCCESS)
      end if
      _ASSERT(config%is_mapping(), 'children spec must be mapping of names to child specs')

      associate (e => config%end())
        allocate(iter, source=config%begin())
        do while (iter /= e)
           child_name => to_string(iter%first(), _RC)
           subcfg => iter%second()
           child_spec = parse_ChildSpec(subcfg)
           call specs%insert(child_name, child_spec)
           call iter%next()
        end do
      end associate

      _RETURN(_SUCCESS)
   end function parse_ChildSpecMap

   type(ChildSpecMap) function var_parse_ChildSpecMap(config, rc) result(specs)
      class(YAML_Node), pointer, intent(in) :: config
      integer, optional, intent(out) :: rc

      integer :: status

      character(:), pointer :: child_name
      type(ChildSpec) :: child_spec
      class(NodeIterator), allocatable :: iter

      type(ChildSpecMap) :: kludge
      integer :: counter
      
      counter = 0
!!$      specs = ChildSpecMap()
      
      if (.not. associated(config)) then
         specs = ChildSpecMap()
         _RETURN(_SUCCESS)
      end if
      _ASSERT(config%is_mapping(), 'children spec must be mapping of names to child specs')

      associate (e => config%end())
        allocate(iter, source=config%begin())
        do while (iter /= e)
           counter = counter + 1
           child_name => to_string(iter%first(), _RC)
           child_spec = parse_ChildSpec(iter%second(), _RC)
           call specs%insert(child_name, child_spec)
           call iter%next()
        end do
      end associate

!!$      call specs%deep_copy(kludge)
      specs = kludge
      _RETURN(_SUCCESS)
   end function var_parse_ChildSpecMap

      

   function parse_UngriddedDimsSpec(config, rc) result(dims_spec)
      use mapl3g_UngriddedDimsSpec
      type(UngriddedDimsSpec) :: dims_spec
      class(YAML_Node), pointer, intent(in) :: config
      integer, optional, intent(out) :: rc

!!$      dims_spec = UngriddedDimsSpec()
      
   end function parse_UngriddedDimsSpec
   
end module mapl3g_ComponentSpecParser
