#include "MAPL_Generic.h"
module mapl3g_UngriddedDimSpec
   use mapl3g_LU_Bound
   use mapl_ErrorHandling
   use esmf, only: ESMF_Info
   use esmf, only: ESMF_InfoCreate
   use esmf, only: ESMF_InfoSet
   implicit none
   private

   public :: UngriddedDimSpec
   public :: operator(==)
   public :: operator(/=)

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
      procedure :: get_bounds
      procedure :: make_info
   end type UngriddedDimSpec

   interface UngriddedDimSpec
      module procedure new_UngriddedDimSpec_extent
      module procedure new_UngriddedDimSpec_name_and_coords
      module procedure new_UngriddedDimSpec_name_units_and_coords
   end interface UngriddedDimSpec

   interface operator(==)
      module procedure equal_to
   end interface operator(==)

   interface operator(/=)
      module procedure not_equal_to
   end interface operator(/=)

   character(*), parameter :: UNKNOWN_DIM_NAME = 'NONE'
   character(*), parameter :: UNKNOWN_DIM_UNITS = 'NONE'
   

contains

   pure function new_UngriddedDimSpec_name_units_and_coords(name, units, coordinates) result(spec)
      type(UngriddedDimSpec) :: spec
      character(*), intent(in) :: name
      character(*), intent(in) :: units
      real, intent(in) :: coordinates(:)

      spec%name = name
      spec%units = units
      spec%coordinates = coordinates

   end function new_UngriddedDimSpec_name_units_and_coords

   pure function new_UngriddedDimSpec_name_and_coords(name, coordinates) result(spec)
      type(UngriddedDimSpec) :: spec
      character(*), intent(in) :: name
      real, intent(in) :: coordinates(:)
      spec = UngriddedDimSpec(name, UNKNOWN_DIM_UNITS, coordinates)
   end function new_UngriddedDimSpec_name_and_coords


   pure function new_UngriddedDimSpec_extent(extent) result(spec)
      integer, intent(in) :: extent
      type(UngriddedDimSpec) :: spec
      spec = UngriddedDimSpec(UNKNOWN_DIM_NAME, default_coords(extent))
   end function new_UngriddedDimSpec_extent


   pure function default_coords(extent, lbound) result(coords)
      real, allocatable :: coords(:)
      integer, intent(in) :: extent
      integer, optional, intent(in) :: lbound

      integer :: i
      integer :: lbound_

      lbound_ = 1
      if (present(lbound)) lbound_ = lbound

      ! 10 levels lbound of 1:  [1,...,10]
      ! 10 levels lbound of 0:  [0,..., 9]
      coords = [(i, i=lbound_, lbound_ + extent - 1)]

   end function default_coords


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


   pure function get_coordinates(this) result(coordinates)
      real, allocatable :: coordinates(:)
      class(UngriddedDimSpec), intent(in) :: this
      coordinates = this%coordinates
   end function get_coordinates


   pure function get_bounds(this) result(bound)
      type(LU_Bound) :: bound
      class(UngriddedDimSpec), intent(in) :: this
      bound%lower = 1
      bound%upper = size(this%coordinates)
   end function get_bounds


   pure logical function equal_to(a, b)
      class(UngriddedDimSpec), intent(in) :: a
      class(UngriddedDimSpec), intent(in) :: b

      equal_to = &
           same_type_as(a, b) .and. &
           (a%name == b%name) .and. &
           (a%units == b%units) .and. &
           all(a%coordinates == b%coordinates)

   end function equal_to


   pure logical function not_equal_to(a, b)
      type(UngriddedDimSpec), intent(in) :: a
      type(UngriddedDimSpec), intent(in) :: b

      not_equal_to = .not. (a == b)

   end function not_equal_to

   function make_info(this, rc) result(info)
      type(ESMF_Info) :: info
      class(UngriddedDimSpec), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      info = ESMF_InfoCreate(_RC)
      if (allocated(this%name)) then
         call ESMF_InfoSet(info, key='name', value=this%name, _RC)
      end if
      if (allocated(this%units)) then
         call ESMF_InfoSet(info, key='units', value=this%units, _RC)
      end if
      call ESMF_InfoSet(info, key='coordinates', values=this%coordinates, _RC)

      _RETURN(_SUCCESS)
   end function make_info

end module mapl3g_UngriddedDimSpec
