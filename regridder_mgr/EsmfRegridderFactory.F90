#include "MAPL_Generic.h"

module mapl_EsmfRegridderFactory
   use mapl_Regridder
   use mapl_EsmfRegridder
   use mapl_RegridderFactory
   implicit none
   private
   
   public :: EsmfRegridderFactory

   type, extends(RegridderFactory) :: EsmfRegridderFactory
      private
      type(RoutehandleManager) :: routehandle_manager
   contains
      procedure :: supports
      procedure :: make_regridder
   end type EsmfRegridderFactory

contains

   logical function supports(this, spec)
      class(EsmfRegridderFactory), intent(in) :: this
      class(RegridderSpec), intent(in) :: spec

      type(EsmfRegridderSpec) :: reference

      supports = same_type_as(spec, reference)
      
   end function supports

   ! Inputs?
   !   - F_in, f_out vs  geom_in, geom_out
   !   - regrid method
   !   - misc with defaults

   function make_regridder(this, f_in, f_out, method, rc)
   end function make_regridder

   function make_regridder(this, spec, rc) result(regriddr)
      class(Regridder), allocatable  :: regriddr
      class(EsmfRegridderFactory), intent(in) :: this
      class(RegridderSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(RoutehandleSpec) :: routehandle_spec

      routehandle_spec = spec%make_routehandle_spec(_RC)
      routehandle => this%routehandle_manager%get_routehandle(routehandle_spec, _RC)

      regridder = EsmfRegridder(spec, routehandle)
      
      _RETURN(_SUCCESS)
   end function make_regridder
   
end module mapl_EsmfRegridderFactory
