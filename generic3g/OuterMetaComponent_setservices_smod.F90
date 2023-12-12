#include "MAPL_ErrLog.h"

submodule (mapl3g_OuterMetaComponent) OuterMetaComponent_setservices_smod
   use esmf
   use gFTL2_StringVector
   use mapl3g_ESMF_Interfaces, only: I_Run
   use mapl3g_ComponentSpecParser
   use mapl3g_HierarchicalRegistry
   use mapl3g_ChildSpec
   use mapl3g_ChildSpecMap
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
      type(GeomManager), pointer :: geom_mgr

      geom_mgr => get_geom_manager()
      _ASSERT(associated(geom_mgr), 'uh oh - cannot acces global geom_manager.')

      this%component_spec = parse_component_spec(this%hconfig, _RC)
      call this%user_component%setservices(this%self_gridcomp, _RC)
      call process_children(this, _RC)

      _RETURN(ESMF_SUCCESS)

   contains

      recursive subroutine process_children(this, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         integer, optional, intent(out) :: rc

         integer :: status

         call add_children(this, _RC)
         call run_children_setservices(this, _RC)

         _RETURN(_SUCCESS)
      end subroutine process_children

      recursive subroutine add_children(this, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         integer, optional, intent(out) :: rc
         
         integer :: status
         type(ChildSpecMapIterator) :: iter
         type(ChildSpec), pointer :: child_spec
         type(ESMF_HConfig), allocatable :: child_hconfig
         character(:), allocatable :: child_name

         associate ( e => this%component_spec%children%ftn_end() )
           iter = this%component_spec%children%ftn_begin()
           do while (iter /= e)
              call iter%next()
              child_name = iter%first()
              child_spec => iter%second()

              if (allocated(child_spec%config_file)) then
                 child_hconfig = ESMF_HConfigCreate(filename=child_spec%config_file, rc=status)
                 _ASSERT(status==0,'problem with config file: '//child_spec%config_file)
              end if
              call this%add_child(child_name, child_spec%user_setservices, child_hconfig, _RC)
           end do
         end associate

         _RETURN(_SUCCESS)
      end subroutine add_children

      ! By now children have either been added by specs or by direct
      ! calls in the parent gc's setservices.
      recursive subroutine run_children_setservices(this, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         integer, optional, intent(out) :: rc

         integer :: status
         type(ChildComponent), pointer :: child_comp
         type(ESMF_GridComp) :: child_outer_gc
         type(ChildComponentMapIterator) :: iter

          associate ( e => this%children%ftn_end() )
            iter = this%children%ftn_begin()
            do while (iter /= e)
               call iter%next()
               child_comp => iter%second()
               child_outer_gc = child_comp%get_outer_gridcomp()
               call ESMF_GridCompSetServices(child_outer_gc, generic_setservices, _RC)
            end do
         end associate

         _RETURN(ESMF_SUCCESS)
      end subroutine run_children_setservices

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
      type(ESMF_GridComp) :: user_gridcomp

      if (present(phase_name)) then
         phase_name_ = phase_name
      else
         phase_name_ = get_default_phase_name(method_flag)
      end if

!#      call add_phase(this%phases_map, method_flag=method_flag, phase_name=phase_name_, _RC)
      call add_phase(this%user_component%phases_map, method_flag=method_flag, phase_name=phase_name_, _RC)

!#      associate(phase_idx => get_phase_index(this%phases_map%of(method_flag), phase_name=phase_name_))
      associate(phase_idx => get_phase_index(this%user_component%phases_map%of(method_flag), phase_name=phase_name_))
        user_gridcomp = this%user_component%get_gridcomp()
        call ESMF_GridCompSetEntryPoint(user_gridcomp, method_flag, userProcedure, phase=phase_idx, _RC)
      end associate

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine set_entry_point

end submodule OuterMetaComponent_setservices_smod
