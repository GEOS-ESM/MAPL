#include "MAPL_Generic.h"

module mapl_GeomManager
   use mapl_GeomSpec
   use mapl_NullGeomSpec
   use mapl_MaplGeom
   use mapl_GeomFactory
   use mapl_GeomFactoryVector
   use mapl_GeomSpecVector
   use mapl_IntegerMaplGeomMap
   use mapl_GeomUtilities, only: MAPL_GeomSetId
   use mapl_ErrorHandlingMod
   use pfio_FileMetadataMod
   use esmf
   use gftl2_IntegerVector
   implicit none
   private

   public :: GeomManager
   public :: geom_manager ! singleton

   type GeomManager
      private
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
      procedure :: get_mapl_geom_from_config
      procedure :: get_mapl_geom_from_metadata
      procedure :: get_mapl_geom_from_spec
      procedure :: get_mapl_geom_from_id
      generic :: get_mapl_geom => &
           get_mapl_geom_from_config, &
           get_mapl_geom_from_metadata, &
           get_mapl_geom_from_spec, &
           get_mapl_geom_from_id

      ! Internal API
      ! ------------
      procedure :: delete_mapl_geom
      procedure :: set_id

      procedure :: make_geom_spec_from_config
      procedure :: make_geom_spec_from_metadata
      generic :: make_geom_spec => &
           make_geom_spec_from_config, &
           make_geom_spec_from_metadata
      procedure :: make_mapl_geom_from_spec
      generic :: make_mapl_geom => make_mapl_geom_from_spec

      procedure :: add_mapl_geom

   end type GeomManager

   integer, parameter :: MAX_ID = 10000

   ! Singleton - must be initialized in mapl_init()
   type(GeomManager) :: geom_manager

contains

   function new_GeomManager() result(mgr)
!!$      use mapl_LatLonGeomFactory
!!$      use mapl_CubedSphereGeomFactory
      type(GeomManager) :: mgr

!!$      ! Load default factories
!!$      type(LatLonGeomFactory) :: latlon_factory
!!$      type(CubedSphereGeomFactory) :: cs_factory
!!$      type(FakeCubedSphereGeomFactory) :: fake_cs_factory 
!!$      type(TripolarGeomFactory) :: tripolar_factory
!!$      type(CustomGeomFactory) :: custom_geom_factory
!!$
!!$      call mgr%factories%push_back(latlon_factory)
!!$      call mgr%factories%push_back(cs_factory)
!!$      call mgr%factories%push_back(fake_cs_factory)
!!$      call mgr%factories%push_back(tripolar_factory)
!!$      call mgr%factories%push_back(custom_geom_factory)

!!$      ! Output only samplers.  These cannot be created from metadata.
!!$      ! And likely have a time dependence.
!!$      call mgr%factories%push_back(StationSampler_factory)
!!$      call mgr%factories%push_back(TrajectorySampler_factory)
!!$      call mgr%factories%push_back(SwathSampler_factory)

   end function new_GeomManager


   subroutine delete_mapl_geom(this, geom_spec, rc)
      class(GeomManager), intent(inout) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: id, idx
      type(GeomSpecVectorIterator) :: spec_iter
      integer :: n

      associate (specs => this%geom_specs)

        associate (spec_iter => find(specs%begin(), specs%end(), geom_spec))
          if (spec_iter /= specs%end()) then

             idx = 1 + (spec_iter - specs%begin())
             id = this%geom_ids%of(idx)

             n = this%mapl_geoms%erase(id) ! num deleted
             _ASSERT(n == 1, "Inconsistent status in GeomManager.")

             _RETURN(_SUCCESS)
          end if
        end associate
      end associate

      _FAIL('GeomSpec not found.')

   end subroutine delete_mapl_geom


   function get_mapl_geom_from_config(this, config, rc) result(mapl_geom)
      type(MaplGeom), pointer :: mapl_geom
      class(GeomManager), target, intent(inout) :: this
      type(ESMF_Config), intent(inout) :: config
      integer, optional, intent(out) :: rc

      class(GeomSpec), allocatable :: geom_spec
      integer :: status

      geom_spec = this%make_geom_spec(config, _RC)
      mapl_geom => this%get_mapl_geom(geom_spec, _RC)

      _RETURN(_SUCCESS)
   end function get_mapl_geom_from_config

   function get_mapl_geom_from_metadata(this, metadata, rc) result(mapl_geom)
      type(MaplGeom), pointer :: mapl_geom
      class(GeomManager), target, intent(inout) :: this
      type(FileMetadata), intent(in) :: metadata
      integer, optional, intent(out) :: rc

      class(GeomSpec), allocatable :: geom_spec
      type(MaplGeom), allocatable :: tmp_mapl_geom
      integer :: status

      geom_spec = this%make_geom_spec(metadata, _RC)
      mapl_geom => this%get_mapl_geom(geom_spec, _RC)

      _RETURN(_SUCCESS)
   end function get_mapl_geom_from_metadata

   function get_mapl_geom_from_id(this, id, rc) result(mapl_geom)
      type(MaplGeom), pointer :: mapl_geom
      class(GeomManager), target, intent(inout) :: this
      integer, intent(in) :: id
      integer, optional, intent(out) :: rc

      integer :: status

      mapl_geom => this%mapl_geoms%at(id, _RC)

      _RETURN(_SUCCESS)
   end function get_mapl_geom_from_id


   function get_mapl_geom_from_spec(this, geom_spec, rc) result(mapl_geom)
      type(MaplGeom), pointer :: mapl_geom
      class(GeomManager), target, intent(inout) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      type(MaplGeom) :: tmp_mapl_geom
      integer :: status

!!$      iter = find(this%geom_ids, geom_spec)
!!$      if (iter /= this%geom_ids%end()) then
!!$         mapl_geom => this%mapl_geoms%at(iter - this%geom_ids%begin(), _RC)
!!$         _RETURN(_SUCCESS)
!!$      end if
!!$
!!$      ! Otherwise build a new geom and store it.
!!$      mapl_geom => this%add_mapl_geom(geom_spec, _RC)

      _RETURN(_SUCCESS)
   end function get_mapl_geom_from_spec


   ! Add a new mapl_geom given a geom_spec.
   ! This also labels the geom with a unique id using ESMF_Info.
   function add_mapl_geom(this, geom_spec, rc) result(mapl_geom)
      type(MaplGeom), pointer :: mapl_geom
      class(GeomManager), target, intent(inout) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(MaplGeom) :: tmp_geom

      mapl_geom => null() ! unless
      
!!$      iter = find(this%mapl_geoms, geom_spec)
!!$      _ASSERT(iter /= this%mapl_geoms%end(), "Requested geom_spec already exists.")
!!$
!!$      tmp_geom = this%make_mapl_geom(geom_spec, _RC)
!!$      associate(id => this%global_id)
!!$        id = id + 1
!!$        _ASSERT(id <= MAX_ID, "Too many geoms created.")
!!$
!!$        call tmp_geom%set_id(id, _RC)
!!$        call this%geom_ids%insert(geom_spec, id)
!!$        call this%mapl_geoms%insert(id, tmp_geom)
!!$        mapl_geom => this%mapl_geoms%of(id)
!!$      end associate

      _RETURN(_SUCCESS)
   end function add_mapl_geom


   function make_geom_spec_from_metadata(this, metadata, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(GeomManager), target, intent(inout) :: this
      type(FileMetadata), intent(in) :: metadata
      integer, optional, intent(out) :: rc

      class(GeomFactory), pointer :: factory
      integer :: i
      integer :: status
      logical :: supports

      geom_spec = NullGeomSpec()
      do i = 1, this%factories%size()
         factory => this%factories%of(i)
         geom_spec = factory%make_spec(metadata, supports=supports, _RC)
         _RETURN_IF(supports)
      end do

      _FAIL("No factory found to interpret metadata")
   end function make_geom_spec_from_metadata

   function make_geom_spec_from_config(this, config, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(GeomManager), target, intent(inout) :: this
      type(ESMF_Config), intent(inout) :: config
      integer, optional, intent(out) :: rc

      class(GeomFactory), pointer :: factory
      integer :: i
      integer :: status
      logical :: supports

      do i = 1, this%factories%size()
         factory => this%factories%of(i)
         geom_spec = factory%make_spec(config, supports=supports, _RC)
         _RETURN_IF(supports)
      end do

      _FAIL("No factory found to interpret config")
   end function make_geom_spec_from_config


   function make_mapl_geom_from_spec(this, spec, rc) result(mapl_geom)
      use gftl2_StringVector
      type(MaplGeom) :: mapl_geom
      class(GeomManager), target, intent(inout) :: this
      class(GeomSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      class(GeomFactory), pointer :: factory
      integer :: status
      integer :: i
      type(ESMF_Geom) :: geom
      type(FileMetadata) :: file_metadata
      type(StringVector) :: gridded_dims

      do i = 1, this%factories%size()
         factory => this%factories%of(i)
         if (.not. factory%supports(spec)) cycle

         geom = factory%make_geom(spec, _RC)
         file_metadata = factory%make_file_metadata(spec, _RC)
         gridded_dims = factory%make_gridded_dims(spec, _RC)
         call this%set_id(geom, _RC)
         
         mapl_geom = MaplGeom(spec, geom, file_metadata, gridded_dims)
         _RETURN(_SUCCESS)
      end do

      _FAIL("No factory found to interpret geom spec")
   end function make_mapl_geom_from_spec

   subroutine set_id(this, geom, rc)
      class(GeomManager), target, intent(inout) :: this
      type(ESMF_Geom), intent(inout) :: geom
      integer, optional, intent(out) :: rc

      type(ESMF_Info) :: info
      integer :: status

      associate (id => this%id_counter)
        id = id + 1
        call MAPL_GeomSetId(geom, id, _RC)
      end associate

      _RETURN(_SUCCESS)
   end subroutine set_id

   function get_geom_from_id(this, id, rc) result(geom)
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

end module mapl_GeomManager
