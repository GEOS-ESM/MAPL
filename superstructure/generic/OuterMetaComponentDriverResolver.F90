#include "MAPL.h"

!------------------------------------------------------------------------------
! OuterMetaComponentDriverResolver: the real DriverResolver
! (superstructure/generic/graph/MethodInvocationAdapter.F90)
! implementation, resolving a REQ-MTH-009 driver_key against a real
! OuterMetaComponent's own driver ownership (REQ-MTH-008:
! user_gc_driver + one driver per child) -
! griddedcomponentdriver-integration-lifecycle design.md Decisions
! ("The concrete DriverResolver implementation lives in
! superstructure/generic/, alongside GraphBuilder.F90").
!
! Deliberately a plain module (not an OuterMetaComponent submodule):
! resolves entirely through OuterMetaComponent's existing public
! accessors (get_user_gc_driver/get_child_driver), so `children` itself
! stays private to mapl_OuterMetaComponent_mod - no wider audience gains
! access to it than already has (GraphBuilder.F90's own similar
! integration-tier position).
!
! driver_key convention: `SELF_COMPONENT_NAME` ('<self>',
! GraphBuilder.F90's existing sentinel, reused here rather than
! inventing a second one) or an empty string both resolve to the
! component's own driver; any other value is looked up as a child name.
! An unresolvable key is an explicit failure (get_child_driver's own
! _ASSERT), never a silent null return.
!------------------------------------------------------------------------------
module mapl_OuterMetaComponentDriverResolver_mod
   use mapl_MethodInvocationAdapter_mod, only: DriverResolver
   use mapl_OuterMetaComponent_mod, only: OuterMetaComponent
   use mapl_GriddedComponentDriver_mod, only: GriddedComponentDriver
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: OuterMetaComponentDriverResolver

   ! Matches GraphBuilder.F90's own SELF_COMPONENT_NAME - kept as an
   ! independent parameter (not `use`d from GraphBuilder.F90) to avoid
   ! introducing a new dependency edge from this module onto
   ! mapl_GraphBuilder_mod for a single string literal; both modules
   ! already independently document that they follow the same
   ! StateRegistry_Hierarchy_smod SELF-sentinel convention.
   character(*), parameter :: SELF_DRIVER_KEY = '<self>'

   type, extends(DriverResolver) :: OuterMetaComponentDriverResolver
      private
      class(OuterMetaComponent), pointer :: owner => null()
   contains
      procedure :: resolve => resolver_resolve
   end type OuterMetaComponentDriverResolver

   interface OuterMetaComponentDriverResolver
      module procedure new_OuterMetaComponentDriverResolver
   end interface OuterMetaComponentDriverResolver

contains

   function new_OuterMetaComponentDriverResolver(owner) result(resolver)
      class(OuterMetaComponent), target, intent(in) :: owner
      type(OuterMetaComponentDriverResolver) :: resolver

      resolver%owner => owner
   end function new_OuterMetaComponentDriverResolver

   function resolver_resolve(this, driver_key, rc) result(driver)
      class(OuterMetaComponentDriverResolver), intent(in) :: this
      character(*), intent(in) :: driver_key
      integer, optional, intent(out) :: rc
      class(GriddedComponentDriver), pointer :: driver

      integer :: status

      _ASSERT(associated(this%owner), 'OuterMetaComponentDriverResolver: resolve called with no owner attached')

      if (driver_key == SELF_DRIVER_KEY .or. driver_key == '') then
         driver => this%owner%get_user_gc_driver()
      else
         driver => this%owner%get_child_driver(driver_key, _RC)
      end if

      _RETURN(_SUCCESS)
   end function resolver_resolve

end module mapl_OuterMetaComponentDriverResolver_mod
