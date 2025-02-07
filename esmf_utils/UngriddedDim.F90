#include "MAPL_Generic.h"
module mapl3g_UngriddedDim
   use mapl3g_InfoUtilities
   use mapl3g_LU_Bound
   use mapl_ErrorHandling
   use esmf, only: ESMF_Info
   use esmf, only: ESMF_InfoCreate
   use esmf, only: ESMF_InfoSet
   implicit none
   private

   public :: UngriddedDim
   public :: make_ungriddedDim
   public :: operator(==)
   public :: operator(/=)

   type :: UngriddedDim
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
   end type UngriddedDim

   interface UngriddedDim
      module procedure new_UngriddedDim_extent
      module procedure new_UngriddedDim_coordinates
   end interface UngriddedDim

   interface operator(==)
      module procedure equal_to
   end interface operator(==)

   interface operator(/=)
      module procedure not_equal_to
   end interface operator(/=)

   character(*), parameter :: UNKNOWN_DIM_NAME = 'NONE'
   character(*), parameter :: UNKNOWN_DIM_UNITS = 'NONE'

contains


   pure function new_UngriddedDim_extent(extent, name, units) result(spec)
      integer, intent(in) :: extent
      character(len=*), optional, intent(in) :: name
      character(len=*), optional, intent(in) :: units
      type(UngriddedDim) :: spec

      spec%name = UNKNOWN_DIM_NAME
      if (present(name)) spec%name = name
      spec%units = UNKNOWN_DIM_UNITS
      if (present(units)) spec%units = units
      spec%coordinates = default_coords(extent)

   end function new_UngriddedDim_extent

   pure function new_UngriddedDim_coordinates(coordinates, name, units) result(spec)
      real, intent(in) :: coordinates(:)
      character(len=*), optional, intent(in) :: name
      character(len=*), optional, intent(in) :: units
      type(UngriddedDim) :: spec

      spec%name = UNKNOWN_DIM_NAME
      if (present(name)) spec%name = name
      spec%units = UNKNOWN_DIM_UNITS
      if (present(units)) spec%units = units
      spec%coordinates = coordinates

   end function new_UngriddedDim_coordinates

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
      class(UngriddedDim), intent(in) :: this
      extent = size(this%coordinates)
   end function get_extent


   pure function get_name(this) result(name)
      character(:), allocatable :: name
      class(UngriddedDim), intent(in) :: this
      name = this%name
   end function get_name


   pure function get_units(this) result(units)
      character(:), allocatable :: units
      class(UngriddedDim), intent(in) :: this
      units = this%units
   end function get_units


   pure function get_coordinates(this) result(coordinates)
      real, allocatable :: coordinates(:)
      class(UngriddedDim), intent(in) :: this
      coordinates = this%coordinates
   end function get_coordinates


   pure function get_bounds(this) result(bound)
      type(LU_Bound) :: bound
      class(UngriddedDim), intent(in) :: this
      bound%lower = 1
      bound%upper = size(this%coordinates)
   end function get_bounds


   pure logical function equal_to(a, b)
      class(UngriddedDim), intent(in) :: a
      class(UngriddedDim), intent(in) :: b

      equal_to = &
           same_type_as(a, b) .and. &
           (a%name == b%name) .and. &
           (a%units == b%units) .and. &
           a%get_extent() == b%get_extent()

      if (equal_to) then
         equal_to = all(a%coordinates == b%coordinates)
      end if

   end function equal_to


   pure logical function not_equal_to(a, b)
      type(UngriddedDim), intent(in) :: a
      type(UngriddedDim), intent(in) :: b

      not_equal_to = .not. (a == b)

   end function not_equal_to

   function make_info(this, rc) result(info)
      type(ESMF_Info) :: info
      class(UngriddedDim), intent(in) :: this
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

   function make_ungriddedDim(info, rc) result(dim)
      type(UngriddedDim) :: dim
      type(ESMF_Info), intent(in) :: info
      integer, optional, intent(out) :: rc
      integer :: status

      call MAPL_InfoGet(info, key='name', value=dim%name, _RC)
      call MAPL_InfoGet(info, key='units', value=dim%units, _RC)
      call MAPL_InfoGet(info, key='coordinates', values=dim%coordinates, _RC)

      _RETURN(_SUCCESS)
   end function make_ungriddedDim

end module mapl3g_UngriddedDim

