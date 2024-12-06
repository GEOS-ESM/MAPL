module oomph_UngriddedDimSpec
   implicit none
   private

   public :: UngriddedDimSpec
   public :: UNKNOWN_DIM_NAME
   public :: UNKNOWN_DIM_UNITS

   type :: UngriddedDimSpec
      private
      character(:), allocatable :: name
      character(:), allocatable :: units
      real, allocatable :: coordinates(:)
   contains
      procedure :: get_extent
      procedure :: get_name
      procedure :: get_units
      procedure :: get_coordinates
   end type UngriddedDimSpec

   interface UngriddedDimSpec
      module procedure new_UngriddedDimSpec_extent
      module procedure new_UngriddedDimSpec_name_and_coords
      module procedure new_UngriddedDimSpec_name_units_and_coords
   end interface UngriddedDimSpec

   character(*), parameter :: UNKNOWN_DIM_NAME = 'unknown dim name'
   character(*), parameter :: UNKNOWN_DIM_UNITS = 'unknown_dim_units'

contains

   pure function new_UngriddedDimSpec_extent(extent) result(spec)
      integer, intent(in) :: extent
      type(UngriddedDimSpec) :: spec

      spec = UngriddedDimSpec(UNKNOWN_DIM_NAME, UNKNOWN_DIM_UNITS, default_coords(extent))
   end function new_UngriddedDimSpec_extent


   pure function default_coords(extent) result(coords)
      real, allocatable :: coords(:)
      integer, intent(in) :: extent

      integer :: i
      coords = [(i, i=1, extent)]

   end function default_coords
     

   pure function new_UngriddedDimSpec_name_and_coords(name, coordinates) result(spec)
      type(UngriddedDimSpec) :: spec
      character(*), intent(in) :: name
      real, intent(in) :: coordinates(:)

      spec = UngriddedDimSpec(name, UNKNOWN_DIM_UNITS, coordinates)

   end function new_UngriddedDimSpec_name_and_coords

   pure function new_UngriddedDimSpec_name_units_and_coords(name, units, coordinates) result(spec)
      type(UngriddedDimSpec) :: spec
      character(*), intent(in) :: name
      character(*), intent(in) :: units
      real, intent(in) :: coordinates(:)

      spec%name = name
      spec%units = units
      spec%coordinates = coordinates

   end function new_UngriddedDimSpec_name_units_and_coords

   pure integer function get_extent(this) result(extent)
      class(UngriddedDimSpec), intent(in) :: this
      extent = size(this%coordinates)
   end function get_extent

   pure function get_name(this) result(name)
      character(:), allocatable :: name
      class(UngriddedDimSpec), intent(in) :: this
      name = this%name
   end function get_name

   pure function get_units(this) result(units)
      character(:), allocatable :: units
      class(UngriddedDimSpec), intent(in) :: this
      units = this%units
   end function get_units

   ! Default coordinates are:  [1., 2., ...]
   pure function get_coordinates(this) result(coordinates)
      real, allocatable :: coordinates(:)
      class(UngriddedDimSpec), intent(in) :: this
      coordinates = this%coordinates
   end function get_coordinates

end module oomph_UngriddedDimSpec
