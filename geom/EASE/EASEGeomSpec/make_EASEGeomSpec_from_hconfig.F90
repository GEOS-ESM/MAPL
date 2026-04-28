#include "MAPL.h"

submodule (mapl3g_EASEGeomSpec) make_EASEGeomSpec_from_hconfig_smod
   use mapl3g_GeomSpec
   use mapl3g_EASEConversion
   use mapl3g_EASEDecomposition
   use mapl_ErrorHandling
   use esmf
   implicit none (type, external)

contains

   module function make_EASEGeomSpec_from_hconfig(hconfig, rc) result(spec)
      type(EASEGeomSpec) :: spec
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status, cols, rows
      character(:), allocatable :: grid_name
      type(EASEDecomposition) :: decomposition

      grid_name = ESMF_HConfigAsString(hconfig, keyString='grid_name', _RC)
      call ease_extent(grid_name, cols, rows, _RC)
      decomposition = make_EASEDecomposition([cols, rows], _RC)
      spec = EASEGeomSpec(grid_name, decomposition, _RC)

      _RETURN(_SUCCESS)
   end function make_EASEGeomSpec_from_hconfig

end submodule make_EASEGeomSpec_from_hconfig_smod
