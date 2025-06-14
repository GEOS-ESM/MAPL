#include "MAPL_Generic.h"

module mapl3g_RegridderSpec
   use esmf
   use mapl3g_RegridderParam
   use mapl3g_Geom_API, only: MAPL_SameGeom
   implicit none
   private

   public :: RegridderSpec
   public :: operator(==)

   type :: RegridderSpec
      private
      class(RegridderParam), allocatable :: param
      type(ESMF_Geom) :: geom_in
      type(ESMF_Geom) :: geom_out
   contains
      procedure :: get_param
      procedure :: get_geom_in
      procedure :: get_geom_out
   end type RegridderSpec

   interface operator(==)
      module procedure equal_to
   end interface operator(==)

   interface RegridderSpec
      procedure new_RegridderSpec
   end interface RegridderSpec

contains

   function new_RegridderSpec(param, geom_in, geom_out) result(spec)
      type(RegridderSpec) :: spec
      class(RegridderParam), intent(in) :: param
      type(ESMF_Geom), intent(in) :: geom_in
      type(ESMF_Geom), intent(in) :: geom_out

      spec%param = param
      spec%geom_in = geom_in
      spec%geom_out = geom_out
   end function new_RegridderSpec

   function get_param(this) result(param)
      class(RegridderParam), allocatable :: param
      class(RegridderSpec), intent(in) :: this
      param = this%param
   end function get_param

   function get_geom_in(this) result(geom)
      type(ESMF_Geom) :: geom
      class(RegridderSpec), intent(in) :: this
      geom = this%geom_in
   end function get_geom_in

   function get_geom_out(this) result(geom)
      type(ESMF_Geom) :: geom
      class(RegridderSpec), intent(in) :: this
      geom = this%geom_out
   end function get_geom_out
   
   logical function equal_to(this, other) result(eq)
      type(RegridderSpec), intent(in) :: this
      type(RegridderSpec), intent(in) :: other

      eq = this%param == other%param
      if (.not. eq) return

      eq = MAPL_SameGeom(this%geom_in, other%geom_in)
      if (.not. eq) return

      eq = MAPL_SameGeom(this%geom_out, other%geom_out)
      if (.not. eq) return
      
   end function equal_to

   
end module mapl3g_RegridderSpec
