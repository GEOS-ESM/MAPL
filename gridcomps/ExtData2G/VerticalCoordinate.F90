#include "MAPL_Exceptions.h"
module VerticalCoordinateMod
   use PFIO
   use MAPL_ExceptionHandling
   use MAPL_FileMetadataUtilsMod
   use gFTL_StringVector
   use udunits2f, UDUNITS_are_convertible => are_convertible, &
      initialize_udunits => initialize, finalize_udunits => finalize

   public VerticalCoordinate
   public fixed_level
   public model_pressure
   public model_height
   public no_coord

   enum, bind(C)
      enumerator :: no_coord
      enumerator :: simple_coord
      enumerator :: fixed_pressure
      enumerator :: fixed_height
      enumerator :: model_pressure
   end enum

   enum, bind(C)
      enumerator :: vertical_stagger_center
      enumerator :: vertical_stagger_edge
   end enum

   type VerticalCoordinate
      integer :: num_levels
      real, allocatable :: bk(:)
      real, allocatable :: abk(:)
      real, allocatable :: levels(:)
      character(len=:), allocatable :: surf_name
      character(len=:), allocatable :: surf_units
      character(len=:), allocatable :: positive
      character(len=:), allocatable :: level_units
      integer :: stagger
      integer :: vertical_type
      !contains 
         !procedure detect_verticalCoordinate
   end type

   interface VerticalCoordinate
      procedure :: new_VerticalCoordinate
   end interface verticalCoordinate

contains

   function new_verticalCoordinate(metadata, var_name, rc) result(vertical_coord)
      type(VerticalCoordinate) :: vertical_coord
      type(FileMetaDataUtils), intent(in) :: metadata
      character(len=*), intent(in) :: var_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(StringVector), pointer :: dimensions
      type(StringVectorIterator) :: iter
      type(Variable), pointer :: var, dim_var
      class(CoordinateVariable), pointer :: coord_var
      character(len=:), pointer :: dim_name
      logical :: is_vertical_coord_var, has_pressure_units, has_height_units
      character(len=:), allocatable :: lev_name, temp_units, formula_terms, standard_name
   
      var => metadata%get_variable(var_name, _RC)
      dimensions => var%get_dimensions()
      lev_name = ''
      iter = dimensions%begin()
      dimensions => var%get_dimensions()
      do while(iter /= dimensions%end())
         dim_name => iter%get()
         if (metadata%has_variable(dim_name)) then
            dim_var => metadata%get_variable(dim_name)
            is_vertical_coord_var = detect_cf_vertical_coord_var(dim_var, _RC)
            if (is_vertical_coord_var) lev_name = dim_name
            exit 
         end if
         call iter%next()
      end do
      ! if not blank, we found something that "looks" like a vertical coordinate according to cf, now lets fill it out
      if (lev_name /= '') then
         coord_var => metadata%get_coordinate_variable(lev_name, _RC)
         vertical_coord%levels = get_coords(coord_var,_RC) 
         vertical_coord%num_levels = size(vertical_coord%levels)

         if (coord_var%is_attribute_present("positive")) vertical_coord%positive = coord_var%get_attribute_string("positive")
         if (coord_var%is_attribute_present("units"))  temp_units = coord_var%get_attribute_string("units")

         ! now test if this is a "fixed" pressure level, if has units of pressure, then CF says is pressure dimensional coordinate
         has_pressure_units = UDUNITS_are_convertible(temp_units, 'hPa', _RC)
         if (has_pressure_units) then
            vertical_coord%level_units = temp_units
            vertical_coord%vertical_type = fixed_pressure
            _RETURN(_SUCCESS)
         end if
         ! now test if this is a "fixed" height level, if has height units, then dimensioanl coordinate, but must have positive 
         has_height_units = UDUNITS_are_convertible(temp_units, 'M', _RC)
         if (has_height_units) then
            _ASSERT(allocated(vertical_coord%positive),"non pressure veritcal dimensional coordinates must have positive attribute")
            vertical_coord%level_units = temp_units
            vertical_coord%vertical_type = fixed_height
            _RETURN(_SUCCESS)
         end if
         ! now test if this is a model pressure, the positive says is vertical and formula_terms says, this is a parametric quantity
         if (coord_var%is_attribute_present("positive") .and. coord_var%is_attribute_present("formula_terms")) then
            standard_name = coord_Var%get_attribute_string("standard_name") 
            formula_terms = coord_var%get_attribute_string("formula_terms")
            if (standard_name == "atmosphere_hybrid_sigma_pressure_coordinate" then

            else
               _FAIL("unsupported hybrid vertical coordinate")
            end if
            _RETURN(_SUCCESS)
         end if
         ! if this is none of those, then a simple coordinate
         vertical_coord%vertical_type = simple_coord
      end if
      _RETURN(_SUCCESS)
      
    end function new_VerticalCoordinate

    ! this is what CF says makes a vertical coordinate
    ! see 2nd paragraph of section 4.3
    ! but, either the coordinate variable is dimensional  has units of pressure, in this case positive is optional
    ! or it still dimensional and has some other units of height and positive is not optional
    ! or it dimensionless in which case must have positive
    function detect_cf_vertical_coord_var(var, rc) result(is_vertical_coord_var)
       logical :: is_vertical_coord_var
       type(Variable), intent(in) :: var
       integer, optional, intent(out) :: rc

       integer :: status

       logical :: has_positive, has_pressure_units, has_units
       character(len=:), allocatable :: units
       character(len=3) :: pressure_hpa

       pressure_hpa = "Pa"
       has_positive = var%is_attribute_present("positive", _RC)
       has_units = var%is_attribute_present("units", _RC)
       if (has_units) then
          units = var%get_attribute_string("units", _RC) 
          has_pressure_units = UDUNITS_are_convertible(units, pressure_hpa, _RC)
       end if
       is_vertical_coord_var = has_pressure_units .or. has_positive
       _RETURN(_SUCCESS)
    end function detect_cf_vertical_coord_var

    function get_coords(coord_var, rc) result(coords)
       real, allocatable :: coords(:)
       class(CoordinateVariable), intent(in) :: coord_var
       integer, intent(out), optional :: rc

       class(*), pointer :: ptr(:)

       ptr => coord_var%get_coordinate_data()
       _ASSERT(associated(ptr),"coord variable coordinate data not found")
       select type (ptr)
       type is (real(kind=REAL64))
          coords=ptr
       type is (real(kind=REAL32))
          coords=ptr
       type is (integer(kind=INT64))
          coords=ptr
       type is (integer(kind=INT32))
          coords=ptr
       class default
          _FAIL("unsupported coordinate variable type in ")
       end select
       _RETURN(_SUCCESS)
    end function get_coords

end module VerticalCoordinateMod   
