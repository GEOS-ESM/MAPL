module mapl_RegridderManager
   use mapl_RegridderFactoryVector
   use mapl_SpecRegridderVector
   implicit none
   private

   public :: RegridderManager

contains

   type RegridderManager
      private
      class(RegridderFactoryVector) :: factories
      type(RegridderSpecVector) :: regridder_specs
      class(MaplRegridderVector) :: regridders
   contains
      procedure :: add_factory
      procedure :: add_mapl_regridder

      ! Retrieve stored mapl_geom, or produce a new one if not found
      procedure :: get_regridder_from_spec
      procedure :: get_mapl_geom_from_metadata
      generic :: get_mapl_geom => get_mapl_geom_from_config, get_mapl_geom_from_metadata

      ! Produce a new mapl_geom from config or metadata
      procedure :: make_geom_spec_from_config
      procedure :: make_geom_spec_from_metadata
      generic :: make_geom_spec => make_geom_spec_from_config, make_geom_spec_from_metadata

      ! Find pointer to mapl_geom corresponding to specified geom_spec.
      ! Return null() if not found.
      procedure :: find_mapl_geom_from_spec
      generic :: find => find_mapl_geom_from_spec

   end type RegridderManager

contains

   function new_RegridderManager() result(mgr)
      use mapl_LatLonRegridderFactory
      use mapl_CubedSphereRegridderFactory
      type(RegridderManager) :: mgr

      ! Load default factories
      type(LatLonRegridderFactory) :: latlon_factory
      type(CubedSphereRegridderFactory) :: cs_factory
      type(FakeCubedSphereRegridderFactory) :: fake_cs_factory 
      type(TripolarRegridderFactory) :: tripolar_factory
      type(CustomRegridderFactory) :: custom_geom_factory

      call mgr%factories%push_back(latlon_factory)
      call mgr%factories%push_back(cs_factory)
      call mgr%factories%push_back(fake_cs_factory)
      call mgr%factories%push_back(tripolar_factory)
      call mgr%factories%push_back(custom_geom_factory)

      ! Output only samplers.  These cannot be created from metadata.
      ! And likely have a time dependence.
      call mgr%factories%push_back(StationSampler_factory)
      call mgr%factories%push_back(TrajectorySampler_factory)
      call mgr%factories%push_back(SwathSampler_factory)

   end function new_RegridderManager


   ! TODO - do we need an RC here for duplicate name?
   subroutine add_factory(this, type_name, factory)
      class(RegridderManager), intent(inout) :: this
      character(*), intent(in) :: type_name
      class(RegridderFactory), intent(in) :: factory
      call this%factories%insert(type_name, factory)
   end subroutine add_factory

   ! TODO - do we need an RC here for duplicate spec?
   subroutine add_mapl_geom(this, geom_spec, mapl_geom)
      class(RegridderManager), intent(inout) :: this
      class(RegridderSpec), intent(in) :: geom_spec
      type(MaplRegridder), intent(in) :: mapl_geom

      call this%geoms-specs%push_back(geom_spec)
      call this%mapl_geoms%push_back(mapl_geom)

   end subroutine add_mapl_geom


   function get_mapl_geom_from_config(this, config, rc) result(mapl_geom)
      type(MaplRegridder), pointer :: mapl_geom
      class(RegridderManager), target, intent(inout) :: this
      type(ESMF_Config), intent(in) :: config
      integer, optional, intent(out) :: rc

      class(RegridderSpec), allocatable :: geom_spec
      type(MaplRegridder), allocatable :: tmp_mapl_geom
      integer :: status

      geom_spec = this%make_geom_spec(config, _RC)
      mapl_geom => this%find(geom_spec)
      _RETURN_IF(associated(mapl_geom))

      ! Otherwise build a new geom and store it.
      tmp_mapl_geom = spec%make_mapl_geom(_RC)
      call this%make_mapl_geom(geom_tmp_mapl_geom)
      mapl_geom => this%mapl_geoms%back()

      _RETURN(_SUCCESS)
   end function GET_mapl_geom_from_config


   function get_mapl_geom_from_metadata(this, metadata, rc) result(mapl_geom)
      type(MaplRegridder), pointer :: mapl_geom
      class(RegridderManager), target, intent(inout) :: this
      character(*), intent(in) :: class
      type(ESMF_Config), intent(in) :: metadata
      integer, optional, intent(out) :: rc

      class(RegridderSpec), allocatable :: geom_spec
      type(MaplRegridder), allocatable :: tmp_mapl_geom
      integer :: status

      geom_spec = this%make_geom_spec(metadata, _RC)
      mapl_geom => this%find(geom_spec)
      _RETURN_IF(associated(mapl_geom))
      
      ! Otherwise build a new geom and store it.
      tmp_mapl_geom = spec%make_mapl_geom(_RC)
      call this%make_mapl_geom(geom_spec, tmp_mapl_geom)

      _RETURN(_SUCCESS)
   end function get_mapl_geom_from_metadata


   function make_geom_spec_from_metadata(this, metadata, rc) return(geom_spec)
      class(RegridderSpec), allocatable :: geom_spec
      class(RegridderManager), target, intent(inout) :: this
      type(FileMetadata), intent(in) :: metadata
      integer, optional, intent(out) :: rc

      class(RegridderFactory), pointer :: factory
      integer :: i
      integer :: status

      factory => null()

      do i = 1, this%factories%size()
         factory => this%factories%of(i)
         geom_spec = factory%make_geom_spec(metadata, rc=status)
         if (status == _SUCCESS) exit
      end do
      _ASSERT(associated(factory), "No factory found to interpret metadata")

      _RETURN(_FAILURE)
   end function make_geom_spec_from_metadata

   function make_geom_spec_from_config(this, config, rc) return(geom_spec)
      class(RegridderSpec), allocatable :: geom_spec
      class(RegridderManager), target, intent(inout) :: this
      type(ESMF_Config), intent(in) :: config
      integer, optional, intent(out) :: rc

      class(RegridderFactory), pointer :: factory
      integer :: i
      integer :: status

      factory => null()

      do i = 1, this%factories%size()
         factory => this%factories%of(i)
         geom_spec = factory%make_geom_spec(config, rc=status)
         if (status == _SUCCESS) exit
      end do
      _ASSERT(associated(factory), "No factory found to interpret config")

      _RETURN(_FAILURE)
   end function make_geom_spec_from_config



   function find_mapl_geom_from_spec(this, geom_spec) result(mapl_geom)
      type(MaplRegridder), pointer :: mapl_geom
      class(RegridderManager), target, intent(in) :: this
      class(RegridderSpec), intent(in) :: geom_spec

      mapl_geom => null()

      ! Check if we already have a geom for this spec
      associate ( iter => find(this%geom_specs, geom_spec) )
        associate ( idx =>  1 + distance(iter,this%geom_specs%begin()) )
          if (idx /= 0) then
             mapl_geom => this%mapl_geoms%of(idx)
             _RETURN(_SUCCESS)
          end if
        end associate
      end associate

   end function find_mapl_geom_from_spec


end module mapl_RegridderManager
