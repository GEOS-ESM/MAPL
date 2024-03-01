#include "MAPL_ErrLog.h"

submodule (mapl3g_CoordinateAxis) CoordinateAxis_smod
   use esmf, only: ESMF_UtilStringLowerCase
   use mapl_ErrorHandling
   use gftl_StringVector
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64

contains
   
   pure module function new_CoordinateAxis(centers, corners) result(axis)
      type(CoordinateAxis) :: axis
      real(kind=R8), intent(in) :: centers(:)
      real(kind=R8), intent(in) :: corners(:)

      axis%centers = centers
      axis%corners = corners
   end function new_CoordinateAxis


   elemental logical module function equal_to(a, b)
      type(CoordinateAxis), intent(in) :: a, b

      ! Do the fast checks first
      equal_to = size(a%centers) == size(b%centers)
      if (.not. equal_to) return
      equal_to = size(a%corners) == size(b%corners)
      if (.not. equal_to) return

      equal_to = all(a%centers == b%centers)
      if (.not. equal_to) return
      equal_to = all(a%corners == b%corners)
   end function equal_to

   elemental logical module function not_equal_to(a, b)
      type(CoordinateAxis), intent(in) :: a, b

      not_equal_to = .not. (a == b)
   end function not_equal_to

   ! Accessors
   !----------
   ! Note that size(this%corners) might be one larger for non-periodic
   pure module function get_extent(this) result(extent)
      class(CoordinateAxis), intent(in) :: this
      integer :: extent
      extent = size(this%centers)
   end function get_extent

   pure module function get_centers(this) result(centers)
      real(kind=R8), allocatable :: centers(:)
      class(CoordinateAxis), intent(in) :: this

      centers = this%centers
      
   end function get_centers

      
   pure module function get_corners(this) result(corners)
      real(kind=R8), allocatable :: corners(:)
      class(CoordinateAxis), intent(in) :: this

      corners = this%corners
      
   end function get_corners

   pure logical module function is_periodic(this)
      class(CoordinateAxis), intent(in) :: this

      real(kind=R8) :: span, spacing
      real(kind=R8), parameter :: tolerance = 0.01
      
      associate (corners => this%corners)
        associate (n => size(corners))

          if (n == 1) then
             is_periodic = .false.
             return
          end if
        
          span = corners(n) - corners(1)
          spacing = corners(2) - corners(1)

          if (abs(span - 360) < (tolerance * spacing)) then
             is_periodic = .true.
          else
             is_periodic = .false.
          end if

        end associate
      end associate
      
   end function is_periodic

 
   module function get_dim_name(file_metadata, units, rc) result(dim_name)
      character(:), allocatable :: dim_name
      type(FileMetadata), target, intent(in) :: file_metadata
      character(*), intent(in) :: units
      integer, optional, intent(out) :: rc

      integer :: status
      type(StringVariableMap), pointer :: vars
      type(Variable), pointer :: var
      type(StringVariableMapIterator) :: iter
      type(StringVector), pointer :: dims
      character(:), allocatable :: units_lower_case
      character(:), allocatable :: units_found
      logical :: has_units
      type(Attribute), pointer :: attr
      logical :: found
      integer :: counter

      dim_name = ''
      units_lower_case = ESMF_UtilStringLowerCase(units, _RC)
      found = .false.
      counter = 0

      vars => file_metadata%get_variables(_RC)
      associate ( e => vars%ftn_end() )
        iter = vars%ftn_begin()
        do while (iter /= e)
           call iter%next()

           var => iter%second()
           has_units = var%is_attribute_present('units', _RC)
           if (.not. has_units) cycle

           attr => var%get_attribute('units', _RC)
           units_found = attr%get_string(_RC)
           units_found = ESMF_UtilStringLowerCase(units_found, _RC)
           if (units_found /= units_lower_case) cycle
           
           dims => var%get_dimensions()
           if (dims%size() /= 1) cycle

           found = .true.
           counter = counter + 1
           _ASSERT(counter == 1, 'Too many variables match requested units: ' // units)
           dim_name = dims%of(1)
           
        end do
      end associate
      _ASSERT(found, "No variable found with units: " // units//".")

      _RETURN(_SUCCESS)
   end function get_dim_name

   module function get_coordinates_dim(file_metadata, dim_name, rc) result(coordinates)
      real(kind=R8), dimension(:), allocatable :: coordinates
      type(FileMetadata), intent(in) :: file_metadata
      character(len=*), intent(in) :: dim_name
      integer, optional, intent(out) :: rc

      integer :: status
      class (CoordinateVariable), pointer :: v
      class (*), pointer :: ptr(:)

      v => file_metadata%get_coordinate_variable(dim_name, _RC)
      ptr => v%get_coordinate_data()
      _ASSERT(associated(ptr),'coordinate data not allocated')

      select type (ptr)
      type is (real(kind=REAL64))
         coordinates = ptr
      type is (real(kind=REAL32))
         coordinates = ptr
      class default
         _FAIL('unsuppoted kind for coordinate data -- must be REAL32 or REAL64')
      end select

      _RETURN(_SUCCESS)
   end function get_coordinates_dim


end submodule CoordinateAxis_smod
