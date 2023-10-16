
#include "MAPL_Generic.h"

submodule (mapl3g_GeomManager) GeomManager_smod
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

contains
   
   module function new_GeomManager() result(mgr)
      use mapl3g_LatLonGeomFactory
!#      use mapl_CubedSphereGeomFactory
      type(GeomManager) :: mgr

      ! Load default factories
      type(LatLonGeomFactory) :: latlon_factory
!#      type(CubedSphereGeomFactory) :: cs_factory
!#      type(FakeCubedSphereGeomFactory) :: fake_cs_factory 
!#      type(TripolarGeomFactory) :: tripolar_factory
!#      type(CustomGeomFactory) :: custom_geom_factory
!#
!#      call mgr%factories%push_back(latlon_factory)
!#      call mgr%factories%push_back(cs_factory)
!#      call mgr%factories%push_back(fake_cs_factory)
!#      call mgr%factories%push_back(tripolar_factory)
!#      call mgr%factories%push_back(custom_geom_factory)
!#
!#      ! Output only samplers.  These cannot be created from metadata.
!#      ! And likely have a time dependence.
!#      call mgr%factories%push_back(StationSampler_factory)
!#      call mgr%factories%push_back(TrajectorySampler_factory)
!#      call mgr%factories%push_back(SwathSampler_factory)

      call mgr%add_factory(latlon_factory)

   end function new_GeomManager


   module subroutine add_factory(this, factory)
      class(GeomManager), intent(inout) :: this
      class(GeomFactory), intent(in) :: factory

      call this%factories%push_back(factory)
   end subroutine add_factory

   module subroutine delete_mapl_geom(this, geom_spec, rc)
      class(GeomManager), intent(inout) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      integer :: id, idx
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


   module function get_mapl_geom_from_hconfig(this, hconfig, rc) result(mapl_geom)
      type(MaplGeom), pointer :: mapl_geom
      class(GeomManager), target, intent(inout) :: this
      type(ESMF_HConfig), intent(inout) :: hconfig
      integer, optional, intent(out) :: rc

      class(GeomSpec), allocatable :: geom_spec
      integer :: status

      geom_spec = this%make_geom_spec(hconfig, _RC)
      mapl_geom => this%get_mapl_geom(geom_spec, _RC)

      _RETURN(_SUCCESS)
   end function get_mapl_geom_from_hconfig

   module function get_mapl_geom_from_metadata(this, metadata, rc) result(mapl_geom)
      type(MaplGeom), pointer :: mapl_geom
      class(GeomManager), target, intent(inout) :: this
      type(FileMetadata), intent(in) :: metadata
      integer, optional, intent(out) :: rc

      class(GeomSpec), allocatable :: geom_spec
      integer :: status

      geom_spec = this%make_geom_spec(metadata, _RC)
      mapl_geom => this%get_mapl_geom(geom_spec, _RC)

      _RETURN(_SUCCESS)
   end function get_mapl_geom_from_metadata

   module function get_mapl_geom_from_id(this, id, rc) result(mapl_geom)
      type(MaplGeom), pointer :: mapl_geom
      class(GeomManager), target, intent(inout) :: this
      integer, intent(in) :: id
      integer, optional, intent(out) :: rc

      integer :: status

      mapl_geom => this%mapl_geoms%at(id, _RC)

      _RETURN(_SUCCESS)
   end function get_mapl_geom_from_id


   module function get_mapl_geom_from_spec(this, geom_spec, rc) result(mapl_geom)
      type(MaplGeom), pointer :: mapl_geom
      class(GeomManager), target, intent(inout) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(GeomSpecVectorIterator) :: iter
      integer :: idx

      associate (b => this%geom_specs%begin(), e => this%geom_specs%end())
        iter = find(first=b, last=e, value=geom_spec)
        if (iter /= this%geom_specs%end()) then
           idx = iter - b + 1  ! Fortran index starts at 1
           mapl_geom => this%mapl_geoms%at(idx, _RC)
           _RETURN(_SUCCESS)
        end if
      end associate

      ! Otherwise build a new geom and store it.
      mapl_geom => this%add_mapl_geom(geom_spec, _RC)
      _RETURN(_SUCCESS)
   end function get_mapl_geom_from_spec


   ! Add a new mapl_geom given a geom_spec.
   ! This also labels the geom with a unique id using ESMF_Info.
   module function add_mapl_geom(this, geom_spec, rc) result(mapl_geom)
      type(MaplGeom), pointer :: mapl_geom
      class(GeomManager), target, intent(inout) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(MaplGeom) :: tmp_mapl_geom
      type(GeomSpecVectorIterator) :: iter

      mapl_geom => null() ! unless

      associate (b => this%geom_specs%begin(), e => this%geom_specs%end())
        iter = find(b, e, geom_spec)
        _ASSERT(iter == e, "Requested geom_spec already exists.")
      end associate

      tmp_mapl_geom = this%make_mapl_geom(geom_spec, _RC)

      associate (id => this%id_counter)
        id = id + 1
        _ASSERT(id <= MAX_ID, "Too many geoms created.")

        call tmp_mapl_geom%set_id(id, _RC)
        call this%geom_ids%push_back(id)
        call this%geom_specs%push_back(geom_spec)
        call this%mapl_geoms%insert(id, tmp_mapl_geom)

        mapl_geom => this%mapl_geoms%of(id)
      end associate

      _RETURN(_SUCCESS)
   end function add_mapl_geom


   module function make_geom_spec_from_metadata(this, file_metadata, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(GeomManager), target, intent(inout) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      class(GeomFactory), pointer :: factory
      integer :: i
      integer :: status
      logical :: supports

      geom_spec = NullGeomSpec()
      do i = 1, this%factories%size()
         factory => this%factories%of(i)
         supports = factory%supports(file_metadata)
         if (supports) then
            geom_spec = factory%make_spec(file_metadata, _RC)
            _RETURN(_SUCCESS)
         end if
      end do

      _FAIL("No factory found to interpret metadata")
   end function make_geom_spec_from_metadata

   module function make_geom_spec_from_hconfig(this, hconfig, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(GeomManager), target, intent(inout) :: this
      type(ESMF_HConfig), intent(inout) :: hconfig
      integer, optional, intent(out) :: rc

      class(GeomFactory), pointer :: factory
      integer :: i
      integer :: status
      logical :: supports

      geom_spec = NULL_GEOM_SPEC ! in case construction fails
      do i = 1, this%factories%size()
         factory => this%factories%of(i)
         supports = factory%supports(hconfig, _RC)
         if (supports) then
            geom_spec = factory%make_spec(hconfig, _RC)
            _RETURN(_SUCCESS)
         end if
      end do

      _FAIL("No factory found to interpret hconfig")
   end function make_geom_spec_from_hconfig


   module function make_mapl_geom_from_spec(this, spec, rc) result(mapl_geom)
      use gftl_StringVector
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
      logical :: found

      found = .false.
      do i = 1, this%factories%size()
         factory => this%factories%of(i)
         if (.not. factory%supports(spec)) cycle
         found = .true.
         exit
      end do
      _ASSERT(found, 'No factory supports spec.')

      geom = factory%make_geom(spec, _RC)
      file_metadata = factory%make_file_metadata(spec, _RC)
      gridded_dims = factory%make_gridded_dims(spec, _RC)
      mapl_geom = MaplGeom(spec, geom, file_metadata, gridded_dims)

      _RETURN(_SUCCESS)
   end function make_mapl_geom_from_spec

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

end submodule GeomManager_smod
