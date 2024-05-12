#include "MAPL_Generic.h"

submodule (mapl3g_GeomManager) add_factory_smod
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
   
   module subroutine add_factory(this, factory)
      class(GeomManager), intent(inout) :: this
      class(GeomFactory), intent(in) :: factory

      call this%factories%push_back(factory)
   end subroutine add_factory

end submodule add_factory_smod
