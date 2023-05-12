#include "MAPL_Generic.h"

module mapl_EsmfRegridderFactory
   use mapl_RegridderFactory
   use mapl_Regridder
   use mapl_RoutehandleParam
   use mapl_RoutehandleManager
   use mapl_EsmfRegridder
   use mapl_RegridderParam
   use mapl_RegridderSpec
   use mapl_ErrorHandlingMod
   implicit none
   private
   
   public :: EsmfRegridderFactory

   type, extends(RegridderFactory) :: EsmfRegridderFactory
      private
      type(RoutehandleManager) :: routehandle_manager
   contains
      procedure :: supports
      procedure :: make_regridder_typesafe
   end type EsmfRegridderFactory

   interface EsmfRegridderFactory
      procedure :: new_EsmfRegridderFactory
   end interface EsmfRegridderFactory

contains

   function new_EsmfRegridderFactory() result(factory)
      type(EsmfRegridderFactory) :: factory

      factory%routehandle_manager = RoutehandleManager()

   end function new_EsmfRegridderFactory

   logical function supports(this, param)
      class(EsmfRegridderFactory), intent(in) :: this
      class(RegridderParam), intent(in) :: param

      type(EsmfRegridderParam) :: reference

      supports = same_type_as(param, reference)
      
   end function supports

   function make_regridder_typesafe(this, spec, rc) result(regriddr)
      class(Regridder), allocatable  :: regriddr
      class(EsmfRegridderFactory), intent(in) :: this
      type(RegridderSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Routehandle) :: routehandle

      associate (p => spec%get_param())
        select type (p)
        type is (EsmfRegridderParam)
           routehandle = make_routehandle(spec%get_geom_in(), spec%get_geom_out(), p%get_routehandle_param(), _RC)
        class default
           _FAIL('Wrong RegridderParam subclass passed to EsmfRegridderFactory.')
        end select
      end associate

      regriddr = EsmfRegridder(routehandle=routehandle, regridder_spec=spec)
      
      _RETURN(_SUCCESS)
   end function make_regridder_typesafe
   
end module mapl_EsmfRegridderFactory
