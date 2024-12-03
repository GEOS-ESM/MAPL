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
   public model_level
   public no_coord

   enum, bind(C)
      enumerator :: no_coord
      enumerator :: fixed_level
      enumerator :: model_level
   end enum

   type VerticalCoordinate
      real, allocatable :: bk(:)
      real, allocatable :: abk(:)
      real, allocatable :: levels(:)
      character(len=:), allocatable :: ps_name
      character(len=:), allocatable :: ps_units
      character(len=:), allocatable :: positive
      integer :: vertical_type
      contains 
         procedure detect_verticalCoordinate
   end type

contains

   subroutine detect_verticalCoordinate(this, metadata, var_name, rc)
      class(VerticalCoordinate), intent(inout) :: this
      type(FileMetaDataUtils), intent(in) :: metadata
      character(len=*), intent(in) :: var_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(StringVector), pointer :: dimensions
      type(StringVectorIterator) :: iter
      type(Variable), pointer :: var, dim_var
      character(len=:), pointer :: dim_name
      logical :: is_vertical_coord_var
   
      call initialize_udunits(_RC)
      var => metadata%get_variable(var_name, _RC)
      dimensions => var%get_dimensions()

      iter = dimensions%begin()
      dimensions => var%get_dimensions()    
      do while(iter /= dimensions%end())
         dim_name => iter%get() 
         if (metadata%has_variable(dim_name)) then
            dim_var => metadata%get_variable(dim_name)
            is_vertical_coord_var = detect_vertical_coord_var(dim_var, _RC) 
         end if
         call iter%next()
      end do
      call finalize_udunits()
      
    end subroutine

    function detect_vertical_coord_var(var, rc) result(is_vertical_coord_var)
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
          write(*,*)'bmaa units ',units
          has_pressure_units = UDUNITS_are_convertible(units, pressure_hpa, _RC)
          write(*,*)'bmaa can ',has_pressure_units
       end if
       is_vertical_coord_var = .false.
       _RETURN(_SUCCESS)
    end function

end module VerticalCoordinateMod   
