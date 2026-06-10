#include "MAPL.h"

module mapl_GeomCategorizer_mod

   use mapl_ErrorHandling_mod
   use mapl_GridPFIO_mod
   use mapl_GeomPFIO_mod
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


#ifdef IFX_RELEASE_BUG
      geom_pfio = grid_pfio
#else
      allocate(geom_pfio, source=grid_pfio)
#endif

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(metadata)
   end function make_geom_pfio

 end module mapl_GeomCategorizer_mod
