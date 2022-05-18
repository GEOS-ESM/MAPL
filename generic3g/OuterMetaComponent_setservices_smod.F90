#include "MAPL_ErrLog.h"

submodule (mapl3g_OuterMetaComponent) OuterMetaComponent_setservices_smod
   use esmf
   use gFTL2_StringVector
   use mapl3g_ESMF_Interfaces, only: I_Run
   use mapl3g_UserSetServices, only: user_setservices
   use mapl3g_ComponentSpecBuilder
   ! Kludge to work around Intel 2021 namespace bug that exposes
   ! private names from other modules in unrelated submodules.
   ! Report filed 2022-03-14 (T. Clune)
   use mapl_keywordenforcer, only: KE => KeywordEnforcer
   use yafyaml
   implicit none

contains

   !========================================================================
   ! Generic SetServices order of operations:
   !
   ! 1) Parse any generic aspects of the config.
   ! 2) Create inner user gridcomp and call its setservices.
   ! 3) Process children
   ! 4) Process specs
   !
   ! Note that specs are processed depth first, but that this may
   ! reverse when step (3) is moved to a new generic initialization phase.
   !=========================================================================
   
   recursive module subroutine SetServices(this, rc)
      use mapl3g_GenericGridComp, only: generic_setservices => setservices
      class(OuterMetaComponent), intent(inout) :: this
      integer, intent(out) :: rc

      integer :: status
      class(YAML_Node), pointer :: child_config, children_config
      character(:), pointer :: name

!!$      call before(this, _RC)
!!$

      if (this%config%has_yaml()) then
         this%component_spec = build_component_spec(this%config%yaml_cfg, _RC)
!!$         call parse_config(this, this%config%yaml_cfg, _RC)
      end if

      call process_user_gridcomp(this, _RC)
      call process_children(this, _RC)

      ! 4) Process generic specs
      call process_generic_specs(this, _RC)

!!$    call after(this, _RC)
      
      _RETURN(ESMF_SUCCESS)

   contains


      subroutine add_children_from_config(children_config, rc)
         class(YAML_Node), intent(in) :: children_config
         integer, optional, intent(out) :: rc

         class(NodeIterator), allocatable :: iter
         integer :: status

         associate (b => children_config%begin(), e => children_config%end() )

           ! ifort 2022.0 polymorphic assign fails for the line below.
           allocate(iter, source=b)

           do while (iter /= e)
              name => to_string(iter%first(), _RC)
              child_config => iter%second()
              call this%add_child(name, child_config, _RC)
              call iter%next()
           end do

         end associate

         _RETURN(ESMF_SUCCESS)
      end subroutine add_children_from_config

      ! Step 2.
      subroutine process_user_gridcomp(this, rc)
         class(OuterMetaComponent), intent(inout) :: this
         integer, optional, intent(out) :: rc
         
         integer :: status

         this%user_gc = create_user_gridcomp(this, _RC)
         call this%component_spec%user_setServices%run(this%user_gc, _RC)

         _RETURN(ESMF_SUCCESS)
      end subroutine process_user_gridcomp
      
      ! Step 3.
      recursive subroutine process_children(this, rc)
         class(OuterMetaComponent), intent(inout) :: this
         integer, optional, intent(out) :: rc
         
         type(ChildComponentMapIterator), allocatable :: iter
         integer :: status
         type(ChildComponent), pointer :: child_comp

         associate ( b => this%children%begin(), e => this%children%end() )
           iter = b
           do while (iter /= e)
              child_comp => iter%second()
              call ESMF_GridCompSetServices(child_comp%gridcomp, generic_setservices, _RC)
              call iter%next()
           end do
         end associate

         _RETURN(ESMF_SUCCESS)
      end subroutine process_children

      ! Step 4.
      ! Note that setservices is processed at an earlier step.
      subroutine process_generic_specs(this, rc)
         class(OuterMetaComponent), intent(inout) :: this
         integer, optional, intent(out) :: rc
         
         integer :: status


         _RETURN(ESMF_SUCCESS)
      end subroutine process_generic_specs

   end subroutine SetServices

   function create_user_gridcomp(this, unusable, rc) result(user_gc)
      type(ESMF_GridComp) :: user_gc
      class(OuterMetaComponent), intent(in) :: this
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      character(ESMF_MAXSTR) :: name
      integer :: status

      call ESMF_GridCompGet(this%self_gc, name=name, _RC)
      user_gc = ESMF_GridCompCreate(name=name, _RC)
      call attach_inner_meta(user_gc, this%self_gc, _RC)

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
