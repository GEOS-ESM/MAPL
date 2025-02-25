#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) SetServices_smod
   use mapl3g_ComponentSpecParser
   use mapl3g_ChildSpec
   use mapl3g_ChildSpecMap
   use mapl3g_GenericGridComp
   use mapl3g_BasicVerticalGrid
   use mapl3g_GriddedComponentDriverMap
   use mapl_ErrorHandling
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
   
   recursive module subroutine SetServices_(this, rc)
      use mapl3g_GenericGridComp, only: generic_setservices => setservices
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, intent(out) :: rc

      integer :: status
      type(ESMF_GridComp) :: user_gridcomp

      ! Note that Parent component should set timestep and refTime in outer meta before calling SetServices.
      this%component_spec = parse_component_spec(this%hconfig, this%registry, this%user_timeStep, this%user_runTime, _RC)

      user_gridcomp = this%user_gc_driver%get_gridcomp()
      call attach_inner_meta(user_gridcomp, this%self_gridcomp, _RC)
      call this%user_setservices%run(user_gridcomp, _RC)
      call add_children(this, _RC)
      call run_children_setservices(this, _RC)

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
              call this%add_child(child_name, child_spec, _RC)
           end do
         end associate

         _RETURN(_SUCCESS)
      end subroutine add_children

      ! By now children have either been added by specs or by direct
      ! calls in the parent gc's setservices.
      recursive subroutine run_children_setservices(this, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         integer, optional, intent(out) :: rc

         integer :: status, user_status
         type(GriddedComponentDriver), pointer :: child_comp
         type(ESMF_GridComp) :: child_outer_gc
         type(GriddedComponentDriverMapIterator) :: iter

         associate ( e => this%children%ftn_end() )
            iter = this%children%ftn_begin()
            do while (iter /= e)
               call iter%next()
               child_comp => iter%second()
               child_outer_gc = child_comp%get_gridcomp()
               call ESMF_GridCompSetServices(child_outer_gc, generic_setservices, _USERRC)
            end do
         end associate

         _RETURN(ESMF_SUCCESS)
      end subroutine run_children_setservices

   end subroutine SetServices_

end submodule SetServices_smod
