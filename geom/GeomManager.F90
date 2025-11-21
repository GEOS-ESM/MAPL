#include "MAPL.h"

module mapl3g_GeomManager
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
   private

   public :: GeomManager
   public :: geom_manager ! singleton
   public :: get_geom_manager

   type GeomManager
!#      private
      type(GeomFactoryVector) :: factories

      ! A GeomSpecId map would be more elegant here, but imposing an ordering
      ! on GeomSpec subclasses is tedious at best.    If gFTL ever has an
      ! unordered map template (i.e., based on a hash instead of ordering), then
      ! this decision could be revisited.
      type(IntegerVector)      :: geom_ids
      type(GeomSpecVector)     :: geom_specs
      type(IntegerMaplGeomMap) :: mapl_geoms

      ! A counter (id_counter) is used to assign each new geom
      ! a unique label.  This allows other classes to support
      ! time-varying geoms by detecting when the ID has changed.
      integer :: id_counter = 0

   contains

      ! Public API
      ! ----------
      procedure :: initialize
      procedure :: add_factory
      procedure :: get_mapl_geom_from_hconfig
      procedure :: get_mapl_geom_from_metadata
      procedure :: get_mapl_geom_from_spec
      procedure :: get_mapl_geom_from_id
      generic :: get_mapl_geom => &
           get_mapl_geom_from_hconfig, &
           get_mapl_geom_from_metadata, &
           get_mapl_geom_from_spec, &
           get_mapl_geom_from_id

      ! Internal API
      ! ------------
      procedure :: delete_mapl_geom

      procedure :: make_geom_spec_from_hconfig
      procedure :: make_geom_spec_from_metadata
      generic :: make_geom_spec => &
           make_geom_spec_from_hconfig, &
           make_geom_spec_from_metadata

      procedure :: make_mapl_geom_from_spec
      generic :: make_mapl_geom => make_mapl_geom_from_spec

      procedure :: add_mapl_geom

   end type GeomManager

   integer, parameter :: MAX_ID = 10000

   ! Singleton - must be initialized in mapl_init()
   type(GeomManager), target, protected :: geom_manager

   interface GeomManager
      procedure new_GeomManager
   end interface GeomManager

   abstract interface
      logical function I_FactoryPredicate(factory)
         import GeomFactory
         class(GeomFactory), intent(in) :: factory
      end function I_FactoryPredicate
   end interface

   interface
      module function new_GeomManager() result(mgr)
         type(GeomManager) :: mgr
      end function new_GeomManager

      module subroutine initialize(this)
         class(GeomManager), intent(inout) :: this
      end subroutine

      module subroutine add_factory(this, factory)
         class(GeomManager), intent(inout) :: this
         class(GeomFactory), intent(in) :: factory
      end subroutine add_factory

      module subroutine delete_mapl_geom(this, geom_spec, rc)
         class(GeomManager), intent(inout) :: this
         class(GeomSpec), intent(in) :: geom_spec
         integer, optional, intent(out) :: rc
      end subroutine delete_mapl_geom


      module function get_mapl_geom_from_hconfig(this, hconfig, rc) result(mapl_geom)
         type(MaplGeom), pointer :: mapl_geom
         class(GeomManager), target, intent(inout) :: this
         type(ESMF_HConfig), intent(inout) :: hconfig
         integer, optional, intent(out) :: rc
      end function get_mapl_geom_from_hconfig

      module function get_mapl_geom_from_metadata(this, metadata, rc) result(mapl_geom)
         type(MaplGeom), pointer :: mapl_geom
         class(GeomManager), target, intent(inout) :: this
         type(FileMetadata), intent(in) :: metadata
         integer, optional, intent(out) :: rc
      end function get_mapl_geom_from_metadata

      module function get_mapl_geom_from_id(this, id, rc) result(mapl_geom)
         type(MaplGeom), pointer :: mapl_geom
         class(GeomManager), target, intent(inout) :: this
         integer, intent(in) :: id
         integer, optional, intent(out) :: rc
      end function get_mapl_geom_from_id


      module function get_mapl_geom_from_spec(this, geom_spec, rc) result(mapl_geom)
         type(MaplGeom), pointer :: mapl_geom
         class(GeomManager), target, intent(inout) :: this
         class(GeomSpec), intent(in) :: geom_spec
         integer, optional, intent(out) :: rc
      end function get_mapl_geom_from_spec


      ! Add a new mapl_geom given a geom_spec.
      ! This also labels the geom with a unique id using ESMF_Info.
      module function add_mapl_geom(this, geom_spec, rc) result(mapl_geom)
         type(MaplGeom), pointer :: mapl_geom
         class(GeomManager), target, intent(inout) :: this
         class(GeomSpec), intent(in) :: geom_spec
         integer, optional, intent(out) :: rc
      end function add_mapl_geom


      module function make_geom_spec_from_metadata(this, file_metadata, rc) result(geom_spec)
         class(GeomSpec), allocatable :: geom_spec
         class(GeomManager), target, intent(inout) :: this
         type(FileMetadata), intent(in) :: file_metadata
         integer, optional, intent(out) :: rc
      end function make_geom_spec_from_metadata

      module function make_geom_spec_from_hconfig(this, hconfig, rc) result(geom_spec)
         class(GeomSpec), allocatable :: geom_spec
         class(GeomManager), target, intent(inout) :: this
         type(ESMF_HConfig), intent(inout) :: hconfig
         integer, optional, intent(out) :: rc
      end function make_geom_spec_from_hconfig


      module function make_mapl_geom_from_spec(this, spec, rc) result(mapl_geom)
         use gftl2_StringVector
         use mapl3g_StringDictionary
         type(MaplGeom) :: mapl_geom
         class(GeomManager), target, intent(inout) :: this
         class(GeomSpec), intent(in) :: spec
         integer, optional, intent(out) :: rc
      end function make_mapl_geom_from_spec

      module function get_geom_from_id(this, id, rc) result(geom)
         type(ESMF_Geom) :: geom
         class(GeomManager), target, intent(inout) :: this
         integer, intent(in) :: id
         integer, optional, intent(out) :: rc
      end function get_geom_from_id

      module function get_geom_manager() result(geom_mgr)
         type(GeomManager), pointer :: geom_mgr
      end function get_geom_manager

      module function find_factory(factories, predicate, rc) result(factory)
         class(GeomFactory), pointer :: factory
         type(GeomFactoryVector), pointer, intent(in) :: factories ! Force TARGET attr on actual
         procedure(I_FactoryPredicate) :: predicate
         integer, optional, intent(out) :: rc
      end function find_factory
   end interface

end module mapl3g_GeomManager
