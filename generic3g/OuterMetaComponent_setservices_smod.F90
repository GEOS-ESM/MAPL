#include "MAPL_ErrLog.h"

submodule (mapl3g_OuterMetaComponent) OuterMetaComponent_setservices_smod
   use esmf
   use gFTL2_StringVector
   use mapl3g_ESMF_Interfaces, only: I_Run
   ! Kludge to work around Intel 2021 namespace bug that exposes
   ! private names from other modules in unrelated submodules.
   ! Report filed 2022-03-14 (T. Clune)
   use mapl_keywordenforcer, only: KE => KeywordEnforcer
   use yafyaml
   implicit none

contains

   module subroutine SetServices(this, rc)
      use mapl3g_GenericGridComp, only: generic_setservices => setservices
      class(OuterMetaComponent), intent(inout) :: this
      integer, intent(out) :: rc

      integer :: status
      class(NodeIterator), allocatable :: iter_child_config
      type(ChildComponentMapIterator), allocatable :: iter_child
      class(YAML_Node), pointer :: child_config
      character(:), pointer :: name

!!$      call before(this, _RC)
!!$
!!$      if (this%config%has_yaml()) then
!!$         associate( config => this%config%yaml_cfg )
!!$           call this%set_component_spec(build_component_spec(config, _RC))
!!$         end associate
!!$      end if


      _HERE
      this%user_gc = create_user_gridcomp(this, _RC)

      if (this%config%has_yaml()) then
         associate ( config => this%config%yaml_cfg )
           _HERE, config
           _HERE, 'has children?' ,config%has('children')
           if (config%has('children')) then
              associate ( children => config%of('children') )
                associate (b => children%begin(), e => children%end() )
                  iter_child_config = b
                  do while (iter_child_config /= e)
                     name => to_string(iter_child_config%first(), _RC)
                     _HERE, 'child: ', name
                     child_config => iter_child_config%second()
                     call this%add_child(name, child_config, _RC)
                     call iter_child_config%next()
                  end do
                end associate
              end associate
           end if
         end associate
      end if

      _HERE,'run user sets services'
      block
        character(ESMF_MAXSTR) :: name
        call ESMF_GridCompGet(this%self_gc, name=name, _RC)
        _HERE, 'run user setservices for <',trim(name),'>'
      end block
      call this%user_setservices%run_setservices(this%user_gc, _RC)

      _HERE,'num children: ', this%children%size()
      associate ( b => this%children%begin(), e => this%children%end() )
        iter_child = b
        do while (iter_child /= e)
           associate (child_comp => iter_child%second())
             block
               character(ESMF_MAXSTR) :: name
               call ESMF_GridCompGet(this%self_gc, name=name, _RC)
               _HERE, 'run child setservices for <',trim(name),'> ', iter_child%first()
             end block

             call ESMF_GridCompSetServices(child_comp%gridcomp, generic_setservices, _RC)
             block
               character(ESMF_MAXSTR) :: name
               call ESMF_GridCompGet(this%self_gc, name=name, _RC)
               _HERE, '... completed child setservices for <',trim(name),'> ', iter_child%first()
             end block

           end associate
           call iter_child%next()
        end do
      end associate

!!$      call <messy stuff>
!!$
!!$      ...
      
      _RETURN(ESMF_SUCCESS)
   end subroutine SetServices

   function create_user_gridcomp(this, unusable, rc) result(user_gc)
      type(ESMF_GridComp) :: user_gc
      class(OuterMetaComponent), intent(in) :: this
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      character(ESMF_MAXSTR) :: name
      integer :: status

      _HERE
      call ESMF_GridCompGet(this%self_gc, name=name, _RC)
      user_gc = ESMF_GridCompCreate(name=name, _RC)
      call attach_inner_meta(user_gc, this%self_gc, _RC)
      _HERE

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function create_user_gridcomp


   module subroutine set_entry_point(this, method_flag, userProcedure, unusable, phase_name, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_Method_Flag), intent(in) :: method_flag
      procedure(I_Run) :: userProcedure
      class(KE), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: phase_name
      integer, optional, intent(out) ::rc

      integer :: status

      call add_phase(this%phases_map, method_flag=method_flag, phase_name=phase_name, _RC)

      associate(phase_idx => get_phase_index(this%phases_map%of(method_flag), phase_name=phase_name))
        call ESMF_GridCompSetEntryPoint(this%user_gc, method_flag, userProcedure, phase=phase_idx, _RC)
      end associate

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine set_entry_point


   ! This should move to a separate module.
!!$   function build_component_spec(config, rc) result(component_spec)
!!$      type(ComponentSpec) :: component_spec
!!$
!!$      component_spec%setservices_spec = process_setservices_spec(config%of('setservices'), _RC)
!!$      component_spec%states_spec = process_states_spec(config%of('states'), _RC)
!!$      component_spec%connections_spec = process_connections_spec(config%of('connections'), _RC)
!!$      component_spec%children_spec = process_children_spec(config%of('children'), _RC)
!!$      component_spec%grid_spec = process_grid_spec(config%of('grid', _RC)
!!$      component_spec%services_spec = process_grid_spec(config%of('serviceservices', _RC)
!!$
!!$      _RETURN(_SUCCESS)
!!$   end function build_component_spec

end submodule OuterMetaComponent_setservices_smod
