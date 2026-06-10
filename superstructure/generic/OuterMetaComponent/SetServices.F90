#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) SetServices_smod
   use mapl_ComponentSpecParser_mod
   use mapl_ChildSpec_mod
   use mapl_ChildSpecMap_mod
   use mapl_Generic_api
   use mapl_vertical_grid_api
   use mapl_GriddedComponentDriverMap_mod
   use mapl_ErrorHandling_mod
   use pflogger, only: logger_t => logger
   implicit none(type,external)

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
      use mapl_Generic_api, only: mapl_GenericSetServices
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, intent(out) :: rc

      integer :: status
      type(ESMF_GridComp) :: user_gridcomp
      class(logger_t), pointer :: logger

      ! Note that Parent component should set timestep and offset in outer meta before calling SetServices.
      this%component_spec = parse_component_spec(this%hconfig, this%registry, this%user_gc_driver%get_name(), _RC)

      user_gridcomp = this%user_gc_driver%get_gridcomp()
      call attach_inner_meta(user_gridcomp, this%self_gridcomp, _RC)
      logger => this%get_logger()
      call logger%info("SetServices:: starting...", _RC)
      call this%user_setservices%run(user_gridcomp, _RC)
      call logger%info("SetServices:: ...completed", _RC)
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
               call ESMF_GridCompSetServices(child_outer_gc, mapl_GenericSetServices, _USERRC)
            end do
         end associate

         _RETURN(ESMF_SUCCESS)
      end subroutine run_children_setservices

   end subroutine SetServices_

end submodule SetServices_smod
