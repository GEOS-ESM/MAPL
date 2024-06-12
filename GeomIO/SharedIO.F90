#include "MAPL_Generic.h"
module mapl3g_SharedIO
   use mapl_ErrorHandlingMod
   use esmf
   use pfio
   use gFTL2_StringVector
   use mapl3g_geom_mgr
   use MAPL_BaseMod
   use mapl3g_output_info
   use mapl3g_UngriddedDims

   implicit none

   public add_variables
   public add_variable
   public get_mapl_geom
   public create_time_variable
   public bundle_to_metadata
   public esmf_to_pfio_type

contains

   function bundle_to_metadata(bundle, geom, rc) result(metadata)
      type(FileMetaData) :: metadata
      type(ESMF_FieldBundle), intent(in) :: bundle
      type(ESMF_Geom), intent(in) :: geom
      integer, optional, intent(out) :: rc

      integer:: status
      type(MaplGeom), pointer :: mapl_geom
      type(Variable) :: time_var
      type(ESMF_Time) :: fake_time

      mapl_geom => get_mapl_geom(geom, _RC)
      metadata = mapl_geom%get_file_metadata()
      ! Add metadata for vertical geom, note could be both center and edge
      call add_vertical_dimensions(bundle, metadata, _RC)
      ! Add metadata for all unique ungridded dimensions the set of fields has
      call add_ungridded_dimensions(bundle, metadata, _RC)

      ! Add time metadata
      call ESMF_TimeSet(fake_time, timeString="1900-04-03T21:00:00", _RC)
      call metadata%add_dimension('time', pFIO_UNLIMITED)
      time_var = create_time_variable(fake_time, _RC)
      call metadata%add_variable('time', time_var, _RC)

      ! Variables
      call add_variables(metadata, bundle, _RC)

      _RETURN(_SUCCESS)
   end function bundle_to_metadata

   subroutine add_variables(metadata, bundle, rc)
      type(ESMF_FieldBundle), intent(in) :: bundle
      type(FileMetaData), intent(inout) :: metadata
      integer, intent(out), optional :: rc

      integer :: status, num_fields, i
      character(len=ESMF_MAXSTR), allocatable :: field_names(:)
      type(ESMF_Field) :: field

      call ESMF_FieldBundleGet(bundle, fieldCount=num_fields, _RC)
      allocate(field_names(num_fields))
      call ESMF_FieldBundleGet(bundle, fieldNameList=field_names, _RC)
      do i=1,num_fields
         call ESMF_FieldBundleGet(bundle, field_names(i), field=field, _RC)
         call add_variable(metadata, field, _RC)
      enddo
      _RETURN(_SUCCESS)

   end subroutine add_variables

   subroutine add_variable(metadata, field,  rc)
      type(ESMF_Field), intent(in) :: field
      type(FileMetaData), intent(inout) :: metadata
      integer, intent(out), optional :: rc

      type(Variable) :: v
      integer :: status
      character(len=:), allocatable :: dims
      type(ESMF_TYPEKIND_FLAG) :: typekind
      integer :: pfio_type
      type(ESMF_Info) :: info
      character(len=:), allocatable :: char
      character(len=ESMF_MAXSTR) :: fname
      type(MAPLGeom), pointer :: mapl_geom
      type(StringVector) :: grid_variables
      type(ESMF_Geom) :: esmfgeom

      call ESMF_FieldGet(field, geom=esmfgeom, _RC)
      mapl_geom => get_mapl_geom(esmfgeom, _RC)
      grid_variables = mapl_geom%get_gridded_dims()
      dims = string_vec_to_comma_sep(grid_variables)
      call ESMF_FieldGet(field, name=fname, typekind = typekind, _RC)
      ! add vertical dimension
      vert_dim_name = get_vertical_dimension_name_from_field(field, _RC)
      dims = dims//","//vert_dim_name
      ! add any ungridded dimensions
      dims = dims // ungridded_dim_names(field, _RC)
      ! add time dimension
      dims = dims//",time"
      pfio_type = esmf_to_pfio_type(typekind ,_RC)
      v = Variable(type=pfio_type, dimensions=dims)
      call ESMF_InfoGetFromHost(field, info, _RC)
      call ESMF_InfoGetCharAlloc(info, 'MAPL/units', char, _RC)
      call v%add_attribute('units',char)
      call ESMF_InfoGetCharAlloc(info, 'MAPL/standard_name', char, _RC)
      call v%add_attribute('long_name',char)
      call metadata%add_variable(trim(fname), v, _RC)
      _RETURN(_SUCCESS)

   end subroutine add_variable

   function get_mapl_geom(geom, rc) result(mapl_geom)
      type(MAPLGeom), pointer :: mapl_geom
      type(ESMF_Geom), intent(in) :: geom 
      integer, optional, intent(out) :: rc

      integer :: status, id
      type(GeomManager), pointer :: geom_mgr

      geom_mgr => get_geom_manager()
      id = MAPL_GeomGetId(geom, _RC)
      mapl_geom => geom_mgr%get_mapl_geom_from_id(id, _RC)
      _RETURN(_SUCCESS)

   end function get_mapl_geom

   function esmf_to_pfio_type(esmf_type, rc) result(pfio_type)
      integer :: pfio_type
      type(ESMF_TYPEKIND_FLAG), intent(in) :: esmf_type
      integer, intent(out), optional :: rc
      if (esmf_type == ESMF_TYPEKIND_R4) then
         pfio_type = pFIO_REAL32
      else if (esmf_type == ESMF_TYPEKIND_R8) then
         pfio_type = pFIO_REAL64
      else
         _FAIL("Unsupported ESMF field typekind for output")
      end if
      _RETURN(_SUCCESS)
   end function

   function string_vec_to_comma_sep(string_vec) result(comma_sep)
      character(len=:), allocatable :: comma_sep
      type(StringVector), intent(in) :: string_vec
      type(stringVectorIterator) :: iter
      character(len=:), pointer :: var

      iter = string_vec%begin()
      var => iter%of()
      comma_sep = var
      call iter%next()
      do while (iter /= string_vec%end())
         var => iter%of()
         comma_sep = comma_sep//","//var
         call iter%next()
      enddo
   end function  

   function create_time_variable(current_time, rc) result(time_var)
      type(Variable) :: time_var
      type(ESMF_Time), intent(in) :: current_time
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: iso_time_string

      call ESMF_TimeGet(current_time, timeString=iso_time_string, _RC)
      iso_time_string = "minutes since "//trim(iso_time_string)
      time_var = Variable(type=PFIO_REAL32, dimensions='time')
      call time_var%add_attribute('long_name', 'time')
      call time_var%add_attribute('units', iso_time_string, _RC)

      _RETURN(_SUCCESS)
   end function create_time_variable

   subroutine add_vertical_dimensions(bundle, metadata, rc)
      type(ESMF_FieldBundle), intent(in) :: bundle
      type(FileMetaData), intent(inout) :: metadata
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: num_levels
      type(StringVector) :: vertical_names
      type(StringVectorIterator) :: iter
      character(len=:), allocatable :: name
      
      num_levels = get_num_levels(bundle, _RC)
      if(num_levels == 0) return
      vertical_names = get_vertical_dim_spec_names(bundle, _RC)
      iter = vertical_names%begin()
      do while(iter /= vertical_names%end())
         name = iter%of()
         num_levels = get_vertical_dimension_num_levels(name, num_levels)
         name = get_vertical_dimension_name(name)
         call metadata%add_dimension(name, num_levels)
         call iter%next()
      end do
      _RETURN(_SUCCESS)

   end subroutine add_vertical_dimensions

   function get_vertical_dimension_name(dim_spec_name) result(dim_name)
      character(len=:), allocatable :: dim_name
      character(len=*), intent(in) :: dim_spec_name
      character(len=*), parameter :: VERTICAL_CENTER_NAME = 'lev'
      character(len=*), parameter :: VERTICAL_EDGE_NAME = 'edge'

      dim_name = VERTICAL_CENTER_NAME
      if(dim_spec_name == 'VERTICAL_DIM_EDGE') dim_name = VERTICAL_EDGE_NAME

   end function get_vertical_dimension_name

   integer function get_vertical_dimension_num_levels(dim_spec_name, num_levels) result(num)
      character(len=*), intent(in) :: dim_spec_name
      integer, intent(in) :: num_levels

      num = num_levels
      if(dim_spec_name == 'VERTICAL_DIM_EDGE') num = num_levels + 1
      
   end function get_vertical_dimension_num_levels

   function get_vertical_dimension_name_from_field(field, rc) result(dim_name)
      character(len=:), allocatable, intent(out) :: dim_name
      type(ESMF_Field), intent(in) :: field
      integer, intent(out), optional :: rc
      integer :: status
      character(len=:), allocatable :: dim_spec_name

      dim_spec_name = get_vertical_dim_spec_name(field, _RC)
      dim_name = get_vertical_dimension_name(dim_spec_name)
      _RETURN(_SUCCESS)

   end function get_vertical_dimension_name_from_field

   subroutine add_ungridded_dimensions(bundle, metadata, rc)
      type(ESMF_FieldBundle), intent(in) :: bundle
      type(FileMetaData), intent(inout) :: metadata
      integer, optional, intent(out) :: rc
      integer :: status
      type(UngriddedDims) :: ungridded_dims
      type(UngriddedDim) :: ungridded_dim
      integer :: i

      ungridded_dims = get_ungridded_dims(bundle, _RC)
      do i = 1, ungridded_dims%get_num_ungridded()
         ungridded_dim = ungridded_dims%get_ith_dim_spec(i)
         call metadata%add_dimension(ungridded_dim%get_name(), ungridded_dim%get_extent())
      end do
      _RETURN(_SUCCESS)

   end subroutine add_ungridded_dimensions

   function ungridded_dim_names(field, rc) result(dim_names)
      character(len=:), allocatable :: dim_names
      type(ESMF_Field), intent(in) :: field
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i
      character, parameter :: JOIN = ','

      dim_names = ''
      ungridded_dims = get_ungridded_dims(field, _RC)
      do i = 1, ungridded_dims%get_num_ungridded()
         dim_names = JOIN // ungridded_dims%get_ith_dim_spec(i)%get_name()
      end do
      _RETURN(_SUCCESS)
      
   end function ungridded_dim_names

end module mapl3g_SharedIO
