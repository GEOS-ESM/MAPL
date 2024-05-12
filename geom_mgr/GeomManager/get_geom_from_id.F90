#include "MAPL_Generic.h"

submodule (mapl3g_GeomManager) get_geom_from_id_smod
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

contains
   
   module function get_geom_from_id(this, id, rc) result(geom)
      type(ESMF_Geom) :: geom
      class(GeomManager), target, intent(inout) :: this
      integer, intent(in) :: id
      integer, optional, intent(out) :: rc

      integer :: status
      type(MaplGeom), pointer :: mapl_geom

      mapl_geom => this%mapl_geoms%at(id, _RC)
      geom = mapl_geom%get_geom()

      _RETURN(_SUCCESS)
   end function get_geom_from_id

end submodule get_geom_from_id_smod
