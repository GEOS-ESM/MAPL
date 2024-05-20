#include "MAPL_Generic.h"
module mapl3g_SharedIO
   use mapl_ErrorHandlingMod
   use esmf
   use pfio
   use gFTL2_StringVector
   use mapl3g_geom_mgr
   use MAPL_BaseMod

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

      ! Add metadata for all unique ungridded dimensions the set of fields has

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

      ! add any ungridded dimensions

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

end module mapl3g_SharedIO

