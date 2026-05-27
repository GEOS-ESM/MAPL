#include "MAPL.h"

module mapl_EsmfRegridderFactory_mod
   use mapl_RegridderFactory_mod
   use mapl_Regridder_mod
   use mapl_RoutehandleParam_mod
   use mapl_RoutehandleSpec_mod
   use mapl_RoutehandleManager_mod
   use mapl_EsmfRegridder_mod
   use mapl_RegridderParam_mod
   use mapl_RegridderSpec_mod
   use mapl_NullRegridder_mod
   use mapl_ErrorHandling_mod
   implicit none
   private
   
   public :: EsmfRegridderFactory

   type, extends(RegridderFactory) :: EsmfRegridderFactory
      private
      type(RoutehandleManager) :: rh_manager
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

      factory%rh_manager = RoutehandleManager()

   end function new_EsmfRegridderFactory

   logical function supports(this, param)
      class(EsmfRegridderFactory), intent(in) :: this
      class(RegridderParam), intent(in) :: param

      type(EsmfRegridderParam) :: reference

      supports = same_type_as(param, reference)
      _UNUSED_DUMMY(this)
      
   end function supports

   function make_regridder_typesafe(this, spec, rc) result(regriddr)
      class(Regridder), allocatable  :: regriddr
      class(EsmfRegridderFactory), intent(inout) :: this
      type(RegridderSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Routehandle) :: routehandle
      type(RoutehandleSpec) :: rh_spec

      regriddr = NULL_REGRIDDER
      associate (p => spec%get_param())
        select type (p)
         type is (EsmfRegridderParam)
            rh_spec = RoutehandleSpec(p%get_routehandle_param(), &
                 spec%get_geom_in(), spec%get_geom_out(), &
                 typekind_in=spec%get_typekind_in(), &
                 typekind_out=spec%get_typekind_out())
           routehandle = this%rh_manager%get_routehandle(rh_spec, _RC)
           deallocate(regriddr) ! workaround for gfortran 12.3
            regriddr = EsmfRegridder(p, routehandle, &
                 typekind_in=spec%get_typekind_in(), &
                 typekind_out=spec%get_typekind_out())
           call regriddr%set_spec(spec)
        class default
           _FAIL('Wrong RegridderParam subclass passed to EsmfRegridderFactory.')
        end select
      end associate


      _RETURN(_SUCCESS)
   end function make_regridder_typesafe
   
end module mapl_EsmfRegridderFactory_mod
