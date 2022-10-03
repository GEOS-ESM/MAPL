#include "MAPL_ErrLog.h"

module mapl3g_ComponentBuilder
   use esmf
   use mapl3g_FieldSpec
   use mapl_ErrorHandling
   implicit none
   private

   public :: ComponentBuilder

   type :: ComponentBuilder
   contains
      procedure :: make_field
   end type ComponentBuilder

contains

   function make_field(this, name, field_spec, rc) result(field)
      type(ESMF_Field) :: field
      class(ComponentBuilder), intent(in) :: this
      character(len=*), intent(in) :: name
      type(FieldSpec), intent(in) :: field_spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Grid) :: grid
      type(ESMF_DistGrid) :: dist_grid

      dist_grid = ESMF_DistGridCreate([1,1],[1,1], _RC)
      grid = ESMF_GridCreate(dist_grid, _RC)
      field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R4, name=name, _RC)

      _RETURN(ESMF_SUCCESS)
   end function make_field

end module mapl3g_ComponentBuilder
