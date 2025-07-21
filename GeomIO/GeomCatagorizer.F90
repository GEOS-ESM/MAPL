#include "MAPL.h"

module mapl3g_GeomCatagorizer
   use mapl_ErrorHandling
   use mapl3g_GridPFIO
   use mapl3g_GeomPFIO
   use pfio

   implicit none
   private

   public make_geom_pfio

   contains 

   function make_geom_pfio(metadata, rc) result(geom_pfio)
      class(GeomPFIO), allocatable :: geom_pfio
      type(FileMetadata), intent(in) :: metadata
      integer, intent(out), optional :: rc

      type(GridPFIO) :: grid_pfio

      allocate(geom_pfio, source=grid_pfio)
      _RETURN(_SUCCESS)
   end function make_geom_pfio

end module mapl3g_GeomCatagorizer
