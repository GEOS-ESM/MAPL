#include "MAPL_ErrLog.h"

submodule (mapl3g_OuterMetaComponent) OuterMetaComponent_setservices_smod
   use esmf
   use gFTL2_StringVector
   use mapl3g_ESMF_Interfaces, only: I_Run
   use mapl3g_UserSetServices, only: user_setservices
   use mapl3g_ComponentSpecParser
   use mapl3g_HierarchicalRegistry
   ! Kludge to work around Intel 2021 namespace bug that exposes
   ! private names from other modules in unrelated submodules.
   ! Report filed 2022-03-14 (T. Clune)
   use mapl_keywordenforcer, only: KE => KeywordEnforcer
   implicit none


contains

   ! Note we spell the following routine with trailing underscore as a workaround
   ! for a bug in gfortran-12 that "leaks" private names into client code.
   !========================================================================
   ! Generic SetServices order of operations:
   !
   ! 1) Parse any generic aspects of the hconfig.
   ! 2) Create inner user gridcomp and call its setservices.
   ! 3) Process children
   ! 4) Process specs
   !
   ! Note that specs are processed depth first, but that this may
   ! reverse when step (3) is moved to a new generic initialization phase.
   !=========================================================================
   
   recursive module subroutine SetServices_(this, rc)
      use mapl3g_GenericGridComp, only: generic_setservices => setservices
      class(OuterMetaComponent), intent(inout) :: this
      integer, intent(out) :: rc

      integer :: status

!!$      call before(this, _RC)
!!$


      this%component_spec = parse_component_spec(this%hconfig, _RC)

      call process_user_gridcomp(this, _RC)
      call add_children_from_hconfig(this, _RC)

      call process_children(this, _RC)

      ! 4) Process generic specs
      call process_generic_specs(this, _RC)

!!$    call after(this, _RC)
      
      _RETURN(ESMF_SUCCESS)

   contains


      subroutine add_children_from_hconfig(this, rc)
         type(OuterMetaComponent), target, intent(inout) :: this
         integer, optional, intent(out) :: rc

         type(ESMF_Hconfig) :: child_spec
         type(ESMF_Hconfig) :: children_spec
         logical :: return

         integer :: status, num_children, i
         logical :: found
         
         found = ESMF_HconfigIsDefined(this%hconfig,keyString='children')
         if (.not. found) then
            _RETURN(_SUCCESS)
         end if

         children_spec = ESMF_HconfigCreateAt(this%hconfig,keyString='children',_RC)
         _ASSERT(ESMF_HconfigIsSequence(children_spec), 'Children in hconfig should be specified as a sequence.')
         num_children = ESMF_HconfigGetSize(children_spec,_RC)
         do i = 1,num_children
            child_spec = ESMF_HconfigCreateAt(children_spec,index=i,_RC)
            call add_child_from_hconfig(this, child_spec, _RC)
         end do

         _RETURN(_SUCCESS)
      end subroutine add_children_from_hconfig

      subroutine add_child_from_hconfig(this, child_spec, rc)
         type(OuterMetaComponent), target, intent(inout) :: this
         type(ESMF_Hconfig), intent(in) :: child_spec
         integer, optional, intent(out) :: rc
         
         integer :: status
         class(AbstractUserSetServices), allocatable :: setservices
         character(:), allocatable :: name

         character(*), parameter :: dso_keys(*) = [character(len=9) :: 'dso', 'DSO', 'sharedObj', 'sharedobj']
         character(*), parameter :: userProcedure_keys(*) = [character(len=10) :: 'SetServices', 'setServices', 'setservices']
         integer :: i
         character(:), allocatable :: dso_key, userProcedure_key, try_key
         logical :: dso_found, userProcedure_found
         character(:), allocatable :: sharedObj, userProcedure, hconfig_file
         type(ESMF_Hconfig) :: new_hconfig

         name = ESMF_HconfigAsString(child_spec,keyString='name',_RC)

         dso_found = .false.
         ! Ensure precisely one name is used for dso
         do i = 1, size(dso_keys)
            try_key = trim(dso_keys(i))
            if (ESMF_HconfigIsDefined(child_spec,keyString=try_key)) then
               _ASSERT(.not. dso_found, 'multiple specifications for dso in hconfig for child <'//name//'>.')
               dso_found = .true.
               dso_key = try_key
            end if
         end do
         _ASSERT(dso_found, 'Must specify a dso for hconfig of child <'//name//'>.')
         sharedObj = ESMF_HconfigAsString(child_spec,keyString=dso_key,_RC)

         userProcedure_found = .false.
         do i = 1, size(userProcedure_keys)
            try_key = userProcedure_keys(i)
            if (ESMF_HconfigIsDefined(child_spec,keyString=try_key)) then
               _ASSERT(.not. userProcedure_found, 'multiple specifications for dso in hconfig for child <'//name//'>.')
               userProcedure_found = .true.
               userProcedure_key = try_key
            end if
         end do
         userProcedure = 'setservices_'         
         if (userProcedure_found) then
            userProcedure = ESMF_HconfigAsString(child_spec,keyString=userProcedure_key,_RC)
         end if

         if (ESMF_HconfigIsDefined(child_spec,keyString='config_file')) then
            hconfig_file = ESMF_HconfigAsString(child_spec,keyString='config_file',_RC)
            new_hconfig = ESMF_HconfigCreate(filename=hconfig_file,_RC)
         end if

         call this%add_child(name, user_setservices(sharedObj, userProcedure), new_hconfig, _RC)

         _RETURN(ESMF_SUCCESS)
      end subroutine add_child_from_hconfig

      ! Step 2.
      subroutine process_user_gridcomp(this, rc)
         class(OuterMetaComponent), intent(inout) :: this
         integer, optional, intent(out) :: rc
         
         integer :: status

         call attach_inner_meta(this%user_gridcomp, this%self_gridcomp, _RC)
         call this%user_setServices%run(this%user_gridcomp, _RC)

         _RETURN(ESMF_SUCCESS)
      end subroutine process_user_gridcomp
      
      ! Step 3.
      recursive subroutine process_children(this, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         integer, optional, intent(out) :: rc
         
         type(ChildComponentMapIterator), allocatable :: iter
         integer :: status
         type(ChildComponent), pointer :: child_comp
         type(ESMF_GridComp) :: child_outer_gc

         associate ( b => this%children%begin(), e => this%children%end() )
           iter = b
           do while (iter /= e)
              child_comp => iter%second()
              child_outer_gc = child_comp%get_outer_gridcomp()
              call ESMF_GridCompSetServices(child_outer_gc, generic_setservices, _RC)
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

   end subroutine SetServices_


   module subroutine set_entry_point(this, method_flag, userProcedure, unusable, phase_name, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_Method_Flag), intent(in) :: method_flag
      procedure(I_Run) :: userProcedure
      class(KE), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: phase_name
      integer, optional, intent(out) ::rc

      integer :: status
      character(:), allocatable :: phase_name_

      if (present(phase_name)) then
         phase_name_ = phase_name
      else
         phase_name_ = get_default_phase_name(method_flag)
      end if

      call add_phase(this%phases_map, method_flag=method_flag, phase_name=phase_name_, _RC)

      associate(phase_idx => get_phase_index(this%phases_map%of(method_flag), phase_name=phase_name_))
        call ESMF_GridCompSetEntryPoint(this%user_gridcomp, method_flag, userProcedure, phase=phase_idx, _RC)
      end associate

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine set_entry_point


   ! This should move to a separate module.
!!$   function parse_component_spec(hconfig, rc) result(component_spec)
!!$      type(ComponentSpec) :: component_spec
!!$
!!$      component_spec%setservices_spec = process_setservices_spec(hconfig%of('setservices'), _RC)
!!$      component_spec%states_spec = process_states_spec(hconfig%of('states'), _RC)
!!$      component_spec%connections_spec = process_connections_spec(hconfig%of('connections'), _RC)
!!$      component_spec%children_spec = process_children_spec(hconfig%of('children'), _RC)
!!$      component_spec%grid_spec = process_grid_spec(hconfig%of('grid', _RC)
!!$      component_spec%services_spec = process_grid_spec(hconfig%of('serviceservices', _RC)
!!$
!!$      _RETURN(_SUCCESS)
!!$   end function parse_component_spec

end submodule OuterMetaComponent_setservices_smod
