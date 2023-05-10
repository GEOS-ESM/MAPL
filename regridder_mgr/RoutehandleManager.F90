#include "MAPL_Generic.h"

module mapl_RoutehandleManager
   use esmf
   use mapl_RoutehandleSpec
   use mapl_RoutehandleSpecVector
   use mapl_RoutehandleVector
   use mapl_GeomManager
   use mapl_ErrorHandlingMod
   implicit none

   public :: RoutehandleManager
   public :: routehandle_manager
   

   type :: RoutehandleManager
      private
      type(RoutehandleSpecVector) :: specs
      type(RoutehandleVector) :: routehandles
   contains
      procedure :: get_routehandle
      procedure :: add_routehandle
      procedure :: delete_routehandle
   end type RoutehandleManager

   ! Singleton
   type(RoutehandleManager) :: routehandle_manager
   
contains


   function get_routehandle(this, spec, rc) result(routehandle)
      type(ESMF_Routehandle) :: routehandle
      class(RoutehandleManager), target, intent(inout) :: this
      type(RoutehandleSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status

      ! Note - find() ignores the rh component of MaplRoutehandle
      associate (b => this%specs%begin(), e => this%specs%end())
        associate ( iter => find(b, e, spec))
          if (iter /= this%specs%end()) then
             routehandle = this%routehandles%of(iter - this%specs%begin() + 1)
             _RETURN(_SUCCESS)
          end if
        end associate
      end associate

      call this%add_routehandle(spec, _RC)
      routehandle = this%routehandles%back()

      _RETURN(_SUCCESS)
   end function get_routehandle


   subroutine add_routehandle(this, spec, rc)
      class(RoutehandleManager), target, intent(inout) :: this
      type(RoutehandleSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      type(ESMF_Routehandle) :: routehandle
      integer :: status

      associate (b => this%specs%begin(), e => this%specs%end())
          _ASSERT(find(b, e, spec) /= e, "Spec not found in registry.")
      end associate

      routehandle = make_RouteHandle(spec, _RC)
      call this%specs%push_back(spec)
      call this%routehandles%push_back(routehandle)

      _RETURN(_SUCCESS)
   end subroutine add_routehandle

   
   subroutine delete_routehandle(this, spec, rc)
      class(RoutehandleManager), intent(inout) :: this
      type(RoutehandleSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      type(RoutehandleSpecVectorIterator) :: iter
      type(RoutehandleVectorIterator) :: rh_iter
      associate (b => this%specs%begin(), e => this%specs%end())
        iter = find(b, e, spec)
        _ASSERT(iter /= e, "Spec not found in registry.")

        iter = this%specs%erase(iter)
        rh_iter = this%routehandles%begin() + (iter - b)
        rh_iter = this%routehandles%erase(rh_iter)

      end associate

      _RETURN(_SUCCESS)
   end subroutine delete_routehandle

end module mapl_RoutehandleManager
