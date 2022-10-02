#include "MAPL_ErrLog.h"

module mapl3g_ComponentSpecParser
   use mapl3g_ComponentSpec
   use mapl3g_ChildSpec
   use mapl3g_ChildSpecMap
   use mapl3g_UserSetServices
   use mapl_ErrorHandling
   use yaFyaml
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
      class(YAML_Node), intent(inout) :: config
      integer, optional, intent(out) :: rc

      integer :: status

!!$      spec%states_spec = process_states_spec(config%of('states'), _RC)
!!$      spec%connections_spec = process_connections_spec(config%of('connections'), _RC)
!!$      spec%children_spec = process_children_spec(config%of('children'), _RC)
!!$      spec%grid_spec = process_grid_spec(config%of('grid', _RC)
!!$      spec%services_spec = process_grid_spec(config%of('serviceservices', _RC)

      _RETURN(_SUCCESS)
   end function parse_component_spec


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
      class(YAML_Node), intent(in) :: config
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

      associate (b => config%begin(), e => config%end())
        iter = b
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

      associate (b => config%begin(), e => config%end())
        iter = b
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
