#include "MAPL.h"

! This purpose of this class is to provide a caching mechanism for
! ESMF Routehandle objects and thereby minimize the creation of
! distinct ESMF Routehandle objects during execution.  The creation of
! these objects can be expensive in terms of time and memory, so it is
! best to recognize when the objects can be used in new contexts.

! A Routehandle can be reused in any regridding scenario with the same
! in/out geometries.  At the same time there are options to
! FieldRegrid() that are independent of Routehandle which in turn
! results in the situation that distinct EsmfRegidder objects may
! utilize identical Routehandles due to the additional arguments.

! One nice thing is that since MAPL/GEOS only need a single
! EsmfRegridderFactory object, it is sensible to put a RH Manager
! object in that derived type rather than use a global object.


module mapl3g_RoutehandleManager
   use esmf
   use mapl3g_RoutehandleSpec
   use mapl3g_RoutehandleSpecVector
   use mapl3g_RoutehandleVector
   use mapl_ErrorHandlingMod
   implicit none

   public :: RoutehandleManager

   type :: RoutehandleManager
      private
      type(RoutehandleSpecVector) :: specs
      type(RoutehandleVector) :: routehandles
   contains
      procedure :: get_routehandle
      procedure :: add_routehandle
      procedure :: delete_routehandle
   end type RoutehandleManager

   interface RoutehandleManager
      module procedure :: new_RoutehandleManager
   end interface RoutehandleManager

contains

   function new_RoutehandleManager() result(mgr)
      type(RoutehandleManager) :: mgr

      mgr%specs = RoutehandleSpecVector()
      mgr%routehandles = RoutehandleVector()

   end function new_RoutehandleManager

   function get_routehandle(this, spec, rc) result(routehandle)
      type(ESMF_Routehandle) :: routehandle
      class(RoutehandleManager), target, intent(inout) :: this
      type(RoutehandleSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status

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
          _ASSERT(find(b, e, spec) == e, "Spec already exists in registry.")
      end associate

      routehandle = make_routehandle(spec, _RC)

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

end module mapl3g_RoutehandleManager
