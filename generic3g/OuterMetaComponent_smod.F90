#include "MAPL_ErrLog.h"

submodule (mapl3g_OuterMetaComponent) OuterMetaComponent_setservices_smod
   use esmf
   use gFTL2_StringVector
   use mapl3g_ESMF_Interfaces, only: I_Run
   use mapl3g_ComponentSpecParser
   use mapl3g_HierarchicalRegistry
   use mapl3g_ChildSpec
   use mapl3g_ChildSpecMap
   use mapl3g_GenericGridComp
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
   ! 2) Add children from config
   ! 3) Create inner (user) gridcomp and call its setservices.
   !
   ! Note that specs are processed depth first, but that this may
   ! reverse when step (3) is moved to a new generic initialization phase.
   !=========================================================================
   
   recursive module subroutine SetServices_(this, user_setservices, rc)
      use mapl3g_GenericGridComp, only: generic_setservices => setservices
      class(AbstractUserSetServices), intent(in) :: user_setservices
      class(OuterMetaComponent), intent(inout) :: this
      integer, intent(out) :: rc

      integer :: status
      type(ESMF_GridComp) :: user_gridcomp

      this%component_spec = parse_component_spec(this%hconfig, _RC)
      user_gridcomp = this%user_component%get_gridcomp()
      call attach_inner_meta(user_gridcomp, this%self_gridcomp, _RC)
      call add_children(this, _RC)
      call user_setservices%run(user_gridcomp, _RC)

      _RETURN(ESMF_SUCCESS)

   contains

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
         type(ComponentHandler), pointer :: child_comp
         type(ESMF_GridComp) :: child_outer_gc
         type(ComponentHandlerMapIterator) :: iter

          associate ( e => this%children%ftn_end() )
            iter = this%children%ftn_begin()
            do while (iter /= e)
               call iter%next()
               child_comp => iter%second()
               child_outer_gc = child_comp%get_gridcomp()
               call ESMF_GridCompSetServices(child_outer_gc, generic_setservices, _RC)
            end do
         end associate

         _RETURN(ESMF_SUCCESS)
      end subroutine run_children_setservices

   end subroutine SetServices_

   module subroutine add_child_by_name(this, child_name, setservices, hconfig, rc)
      use mapl3g_GenericGridComp, only: generic_setservices => setservices
      class(OuterMetaComponent), intent(inout) :: this
      character(len=*), intent(in) :: child_name
      class(AbstractUserSetServices), intent(in) :: setservices
      type(ESMF_Hconfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_GridComp) :: child_gc
      type(ESMF_State) :: importState, exportState
      type(ComponentHandler) :: child_comp

      _ASSERT(is_valid_name(child_name), 'Child name <' // child_name //'> does not conform to GEOS standards.')

      child_gc = create_grid_comp(child_name, setservices, hconfig, _RC)
      call ESMF_GridCompSetServices(child_gc, generic_setservices, _RC)
      importState = ESMF_StateCreate(stateIntent=ESMF_STATEINTENT_IMPORT, name=child_name, _RC)
      exportState = ESMF_StateCreate(stateIntent=ESMF_STATEINTENT_EXPORT, name=child_name,  _RC)
      child_comp = ComponentHandler(child_gc, MultiState(importState=importState, exportState=exportState))

      _ASSERT(this%children%count(child_name) == 0, 'duplicate child name: <'//child_name//'>.')
      call this%children%insert(child_name, child_comp)

      _RETURN(ESMF_SUCCESS)
   end subroutine add_child_by_name


end submodule OuterMetaComponent_setservices_smod
