#include "MAPL_Generic.h"

module mapl3g_RoutehandleSpec
   use esmf
   use mapl3g_RoutehandleParam
   use mapl_ErrorHandlingMod
   use mapl3g_Geom_API, only: MAPL_SameGeom
   implicit none
   private

   public :: RoutehandleSpec
   public :: make_routehandle
   public :: operator(==)

   ! If an argument to FieldRegridStore is optional _and_ has no default
   ! value, then we use the ALLOCATABLE attribute.  This allows us to
   ! treate the optional argument as not present in the call.
   type :: RoutehandleSpec
      private
      type(ESMF_Geom) :: geom_in
      type(ESMF_Geom) :: geom_out
      type(RoutehandleParam) :: rh_param
   end type RoutehandleSpec


   interface make_routehandle
      module procedure make_routehandle_from_spec
   end interface make_routehandle

   interface operator(==)
      module procedure equal_to
   end interface operator(==)

   interface RoutehandleSpec
      module procedure new_RoutehandleSpec
   end interface RoutehandleSpec

contains

   function new_RoutehandleSpec( geom_in, geom_out, rh_param) result(spec)
      type(RoutehandleSpec) :: spec
      type(ESMF_Geom), intent(in) :: geom_in
      type(ESMF_Geom), intent(in) :: geom_out
      type(RoutehandleParam), intent(in) :: rh_param

      spec%geom_in = geom_in
      spec%geom_out = geom_out
      spec%rh_param = rh_param

   end function new_RoutehandleSpec

   function make_routehandle_from_spec(spec, rc) result(routehandle)
      type(ESMF_Routehandle) :: routehandle
      type(RoutehandleSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      
      routehandle = make_routehandle(spec%geom_in, spec%geom_out, spec%rh_param, _RC)
      
      _RETURN(_SUCCESS)
   end function make_routehandle_from_spec

   logical function equal_to(a, b) result(eq)
      type(RoutehandleSpec), intent(in) :: a
      type(RoutehandleSpec), intent(in) :: b

      eq = a%rh_param == b%rh_param
      if (.not. eq) return

      eq = MAPL_SameGeom(a%geom_in, b%geom_in)
      if (.not. eq) return
      
      eq = MAPL_SameGeom(a%geom_out, b%geom_out)
      if (.not. eq) return

   end function equal_to


end module mapl3g_RoutehandleSpec
