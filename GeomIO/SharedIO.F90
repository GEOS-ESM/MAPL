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
   public create_local_start
   public create_global_count
   public create_global_start
   public create_file_shape

   contains

   function create_file_shape(grid, field_shape, rc) result(file_shape)
      integer, allocatable :: file_shape(:)
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: field_shape(:)
      integer, intent(out), optional :: rc

      integer :: status, sz, ungr, tile_count
      call ESMF_GridGet(grid, tileCount=tile_count, _RC)
      sz = size(field_shape)
      ungr = sz - 2
      if (tile_count == 6) then
         allocate(file_shape(sz+1))
         file_shape(1:3) = [field_shape(1), field_shape(2), 1]
         file_shape(4:4+ungr-1) = [field_shape(2+ungr:sz)]
      else if (tile_count == 1) then
         file_shape = field_shape
      else 
         _FAIL("unsupported grid")
      end if

      _RETURN(_SUCCESS)
   end function create_file_shape

   function create_global_start(grid, field_shape, time_index, rc) result(global_start)
      integer, allocatable :: global_start(:)
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: field_shape(:)
      integer, optional,  intent(in) :: time_index
      integer, intent(out), optional :: rc

      integer :: status, sz, tile_count, tm
      call ESMF_GridGet(grid, tileCount=tile_count, _RC)
      sz = size(field_shape)

      tm = 0
      if (present(time_index)) tm=1
      if (tile_count == 6) then
         allocate(global_start(sz+1+tm))
         global_start(1:sz+1) = 1
         if (present(time_index)) global_start(sz+2) = time_index
      else if (tile_count == 1) then
         allocate(global_start(sz+tm))
         global_start(1:sz) = 1
         if (present(time_index)) global_start(sz+1) = time_index
      else 
         _FAIL("unsupported grid")
      end if

      _RETURN(_SUCCESS)
   end function create_global_start

   function create_global_count(grid, field_shape, have_time, rc) result(global_count)
      integer, allocatable :: global_count(:)
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: field_shape(:)
      logical, optional, intent(in) :: have_time
      integer, intent(out), optional :: rc

      integer :: status, sz, ungr, tile_count, global_dim(3), tm
      if (present(have_time)) tm=1
      call ESMF_GridGet(grid, tileCount=tile_count, _RC)
      call MAPL_GridGet(grid, globalCellCountPerDim=global_dim, _RC)
      sz = size(field_shape)
      ungr = sz - 2

      if (tile_count == 6) then
         allocate(global_count(sz+1+tm))
         global_count(1:3) =[global_dim(1),global_dim(1),6]
         global_count(4:4+ungr-1) = field_shape(3:sz)
         if (have_time) global_count(sz+2) = 1
      else if (tile_count == 1) then
         allocate(global_count(sz+tm))
         global_count(1:2) =[global_dim(1),global_dim(2)]
         global_count(3:3+ungr-1) = field_shape(3:sz)
         if (have_time) global_count(sz+1) = 1
      else 
         _FAIL("unsupported grid")
      end if
      

      _RETURN(_SUCCESS)
   end function create_global_count

   function create_local_start(grid, field_shape, have_time, rc) result(local_start)
      integer, allocatable :: local_start(:)
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: field_shape(:)
      logical, optional, intent(in) :: have_time
      integer, intent(out), optional :: rc

      integer :: status, sz, ungr, tile_count, i1, in, j1, jn, tile, global_dim(3), tm
      call ESMF_GridGet(grid, tileCount=tile_count, _RC)
      call MAPL_GridGetInterior(grid, i1,in, j1, jn)
      call MAPL_GridGet(grid, globalCellCountPerDim=global_dim, _RC)
      tm=0
      if (present(have_time)) tm=1
      sz = size(field_shape)
      ungr = sz - 2
      if (tile_count == 6) then
         tile = 1 + (j1-1)/global_dim(1)
         allocate(local_start(sz+1+tm))
         local_start(1:3) = [i1, j1-(tile-1)*global_dim(1),tile]
         if (have_time) local_start(4:4+ungr) = 1
      else if (tile_count == 1) then
         allocate(local_start(sz+tm))
         local_start(1:2) = [i1,j1]
         if (have_time) local_start(3:3+ungr) = 1 
      else 
         _FAIL("unsupported grid")
      end if

      _RETURN(_SUCCESS)
   end function create_local_start

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

