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
   use mapl3g_ConnectionSpec
   use mapl3g_ConnectionSpecVector
   use yaFyaml
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

   public :: parse_ExtraDimsSpec
   
contains

   type(ComponentSpec) function parse_component_spec(config, rc) result(spec)
      class(YAML_Node), target, intent(inout) :: config
      integer, optional, intent(out) :: rc

      integer :: status

      if (config%has('states')) then
         spec%var_specs = process_var_specs(config%of('states'), _RC)
      end if

      if (config%has('connections')) then
         spec%connections = process_connections_spec(config%of('connections'), _RC)
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

      if (config%has('import')) then
         call process_state_specs(var_specs, config%of('import'), ESMF_STATEINTENT_IMPORT, _RC)
      end if
      if (config%has('export')) then
         call process_state_specs(var_specs, config%of('export'), ESMF_STATEINTENT_EXPORT, _RC)
      end if
      if (config%has('internal')) then
         call process_state_specs(var_specs, config%of('internal'), ESMF_STATEINTENT_INTERNAL, _RC)
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
         character(:), pointer :: short_name
         class(YAML_Node), pointer :: attributes

         allocate(e, source=config%end())
         allocate(iter, source=config%begin())
         do while (iter /= e)
            short_name => to_string(iter%first())
            attributes => iter%second()
            var_spec = VariableSpec(state_intent, short_name=short_name, &
                 standard_name=to_string(attributes%of('standard_name')), &
                 units=to_string(attributes%of('units')))
            call var_specs%push_back(var_spec)
            call iter%next()
         end do

         _RETURN(_SUCCESS)
      end subroutine process_state_specs
   end function process_var_specs


   function process_connections_spec(config, rc) result(connections)
      type(ConnectionSpecVector) :: connections
      class(YAML_Node), optional, intent(in) :: config
      integer, optional, intent(out) :: rc

      class(NodeIterator), allocatable :: iter, e
      type(ConnectionSpec) :: connection
      class(YAML_Node), pointer :: conn_spec
      integer :: status

      if (.not. present(config)) then
         _RETURN(_SUCCESS)
      end if

      allocate(e, source=config%end())
      allocate(iter, source=config%begin())
      do while (iter /= e)
         conn_spec => iter%at(_RC)
         connection = process_connection(conn_spec, _RC)
         call connections%push_back(connection)
         call iter%next()
      end do

      _RETURN(_SUCCESS)
   contains

      function process_connection(config, rc) result(connection)
         type(ConnectionSpec) :: connection
         class(YAML_Node), optional, intent(in) :: config
         integer, optional, intent(out) :: rc

         integer :: status
         character(:), allocatable :: short_name
         character(:), allocatable :: src_comp
         character(:), allocatable :: dst_comp
         type(VirtualConnectionPt) :: src_pt, dst_pt
         
         _ASSERT(config%has('name'),'Connection must specify a name.')
         _ASSERT(config%has('src_comp'), 'Connection must specify a src component')
         _ASSERT(config%has('dst_comp'), 'Connection must specify a dst component')

         call config%get(short_name, 'name', _RC)
         call config%get(src_comp, 'src_comp', _RC)
         call config%get(dst_comp, 'dst_comp', _RC)
         
         src_pt = VirtualConnectionPt(state_intent='export', short_name=short_name)
         dst_pt = VirtualConnectionPt(state_intent='import', short_name=short_name)

         connection = ConnectionSpec( &
              ConnectionPt(src_comp, src_pt), &
              ConnectionPt(dst_comp, dst_pt))

         _RETURN(_SUCCESS)
      end function process_connection

   end function process_connections_spec

   
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

      

   function parse_ExtraDimsSpec(config, rc) result(dims_spec)
      use mapl3g_ExtraDimsSpec
      type(ExtraDimsSpec) :: dims_spec
      class(YAML_Node), pointer, intent(in) :: config
      integer, optional, intent(out) :: rc

!!$      dims_spec = ExtraDimsSpec()
      
   end function parse_ExtraDimsSpec
   
end module mapl3g_ComponentSpecParser
