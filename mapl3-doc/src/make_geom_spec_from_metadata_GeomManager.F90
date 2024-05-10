#include "MAPL_Generic.h"

submodule (mapl3g_GeomManager) make_geom_spec_from_metadata_smod
   use mapl3g_GeomSpec
   use mapl3g_NullGeomSpec
   use mapl3g_MaplGeom
   use mapl3g_GeomFactory
   use mapl3g_GeomFactoryVector
   use mapl3g_GeomSpecVector
   use mapl3g_IntegerMaplGeomMap
   use mapl_ErrorHandlingMod
   use pfio_FileMetadataMod
   use esmf
   use gftl2_IntegerVector
   implicit none

   abstract interface
      logical function I_FactoryPredicate(factory)
         import GeomFactory
         class(GeomFactory), intent(in) :: factory
      end function I_FactoryPredicate
   end interface
      
contains
   
   ! If factory not found, return a null pointer _and_ a nonzero rc.
   function find_factory(factories, predicate, rc) result(factory)
      class(GeomFactory), pointer :: factory
      type(GeomFactoryVector), pointer, intent(in) :: factories ! Force TARGET attr on actual
      procedure(I_FactoryPredicate) :: predicate
      integer, optional, intent(out) :: rc

      integer :: status
      type(GeomFactoryVectorIterator) :: iter

      factory => null()
      iter = find_if(factories%begin(), factories%end(), predicate)
      _ASSERT(iter /= factories%end(), "No factory found satisfying given predicate.")
      factory => iter%of()

      _RETURN(_SUCCESS)
   end function find_factory

   module function make_geom_spec_from_metadata(this, file_metadata, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(GeomManager), target, intent(inout) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      class(GeomFactory), pointer :: factory
      integer :: status

      geom_spec = NullGeomSpec()
      factory => find_factory(this%factories, supports_metadata, _RC)
      geom_spec = factory%make_spec(file_metadata, _RC)
      
      _RETURN(_SUCCESS)
   contains
      logical function supports_metadata(factory)
         class(GeomFactory), intent(in) :: factory
         supports_metadata = factory%supports(file_metadata)
      end function supports_metadata
   end function make_geom_spec_from_metadata

end submodule make_geom_spec_from_metadata_smod
