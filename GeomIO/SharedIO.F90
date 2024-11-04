#include "MAPL_Generic.h"
module mapl3g_SharedIO
   use mapl_ErrorHandlingMod
   use mapl3g_InfoUtilities
   use mapl3g_FieldBundleGet
   use mapl3g_FieldGet
   use mapl3g_VerticalStaggerLoc
   use pfio
   use gFTL2_StringVector
   use gFTL2_StringSet
   use mapl3g_geom_mgr
   use MAPL_BaseMod
   use mapl3g_UngriddedDims
   use mapl3g_UngriddedDim
!#   use mapl3g_FieldDimensionInfo
   use esmf

   implicit none(type,external)

   public add_variables
   public add_variable
   public get_mapl_geom
   public create_time_variable
   public bundle_to_metadata
   public esmf_to_pfio_type

   public :: add_vertical_dimensions
   public :: get_vertical_dimension_name
   public :: get_vertical_dimension_num_levels
   public :: get_vertical_dimension_name_from_field
   public :: add_ungridded_dimensions
   public :: ungridded_dim_names

   character(len=*), parameter :: EMPTY = ''
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

      integer :: status, i
      type(ESMF_Field) :: field
      type(ESMF_Field), allocatable :: fieldList(:)

      call MAPL_FieldBundleGet(bundle, fieldList=fieldList, _RC)
      do i=1,size(fieldList)
         call add_variable(metadata, fieldList(i), _RC)
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
      character(len=:), allocatable :: char
      character(len=ESMF_MAXSTR) :: fname
      type(MAPLGeom), pointer :: mapl_geom
      type(StringVector) :: grid_variables
      type(ESMF_Geom) :: esmfgeom
      character(len=:), allocatable :: vert_dim_name, ungridded_names

      call ESMF_FieldGet(field, geom=esmfgeom, _RC)
      mapl_geom => get_mapl_geom(esmfgeom, _RC)
      grid_variables = mapl_geom%get_gridded_dims()
      dims = string_vec_to_comma_sep(grid_variables)
      call ESMF_FieldGet(field, name=fname, typekind=typekind, _RC)
      ! add vertical dimension
      vert_dim_name = get_vertical_dimension_name_from_field(field, _RC)
      if(vert_dim_name /= EMPTY) dims = dims//","//vert_dim_name
      ! add any ungridded dimensions
      ungridded_names = ungridded_dim_names(field, _RC)
      if(ungridded_names /= EMPTY) dims = dims // ungridded_names
      ! add time dimension
      dims = dims//",time"
      pfio_type = esmf_to_pfio_type(typekind ,_RC)
      v = Variable(type=pfio_type, dimensions=dims)
      call MAPL_FieldGet(field, units=char, _RC)
      call v%add_attribute('units',char)
      call MAPL_FieldGet(field, standard_name=char, _RC)
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
      character(len=:), allocatable :: dim_name
      type(VerticalStaggerLoc) :: vert_staggerloc
      integer :: i, num_vgrid_levels
      type(ESMF_Field), allocatable :: fieldList(:)


      call MAPL_FieldBundleGet(bundle, fieldList=fieldList, _RC)

      vertical_names = StringVector()
      do i = 1, size(fieldList)
         _HERE, i, size(fieldList)
         call MAPL_FieldGet(fieldList(i), vert_staggerloc=vert_staggerloc, _RC)
         dim_name = vert_staggerloc%get_dimension_name()
         if (dim_name == "") cycle
         
         call MAPL_FieldGet(fieldList(i), num_vgrid_levels=num_vgrid_levels, _RC)
         call vertical_names%push_back(dim_name)
         _HERE, i, size(fieldList)
      end do

      associate (e => vertical_names%ftn_end())
        iter = vertical_names%ftn_begin()
        do while(iter /= e)
           call iter%next()
           dim_name = iter%of()
           num_levels = vert_staggerloc%get_num_levels(num_vgrid_levels)
           call metadata%add_dimension(dim_name, num_levels)
        end do
      end associate

      _RETURN(_SUCCESS)

   end subroutine add_vertical_dimensions

   function get_vertical_dimension_name(dim_spec_name) result(dim_name)
      character(len=:), allocatable :: dim_name
      character(len=*), intent(in) :: dim_spec_name
      character(len=*), parameter :: VERTICAL_CENTER_NAME = 'lev'
      character(len=*), parameter :: VERTICAL_EDGE_NAME = 'edge'
      character(len=*), parameter :: VERTICAL_UNKNOWN_NAME = EMPTY

      dim_name = VERTICAL_UNKNOWN_NAME

      if(dim_spec_name == 'VERTICAL_DIM_EDGE') then
         dim_name = VERTICAL_EDGE_NAME
         return
      end if

      if(dim_spec_name == 'VERTICAL_DIM_CENTER') then
         dim_name = VERTICAL_CENTER_NAME
         return
      end if

   end function get_vertical_dimension_name

   integer function get_vertical_dimension_num_levels(dim_spec_name, num_levels) result(num)
      character(len=*), intent(in) :: dim_spec_name
      integer, intent(in) :: num_levels

      num = num_levels
      if(dim_spec_name == 'VERTICAL_DIM_EDGE') num = num_levels + 1
      
   end function get_vertical_dimension_num_levels

   function get_vertical_dimension_name_from_field(field, rc) result(dim_name)
      character(len=:), allocatable :: dim_name
      type(ESMF_Field), intent(in) :: field
      integer, intent(out), optional :: rc

      integer :: status
      type(VerticalStaggerLoc) :: vert_staggerloc

      call MAPL_FieldGet(field, vert_staggerLoc=vert_staggerLoc, _RC)
      dim_name = vert_staggerLoc%get_dimension_name()
      _RETURN(_SUCCESS)

   end function get_vertical_dimension_name_from_field

   subroutine add_ungridded_dimensions(bundle, metadata, rc)
      type(ESMF_FieldBundle), intent(in) :: bundle
      type(FileMetaData), intent(inout) :: metadata
      integer, optional, intent(out) :: rc
      integer :: status
      type(UngriddedDims) :: field_ungridded_dims, ungridded_dims
      type(UngriddedDim) :: u
      integer :: i, j
      type(ESMF_Field) :: field
      type(ESMF_Field), allocatable :: fieldList(:)
      type(StringSet) :: dim_names
      character(:), allocatable :: dim_name
      logical :: is_new

      call MAPL_FieldBundleGet(bundle, fieldList=fieldList, _RC)
      do i = 1, size(fieldList)
         call MAPL_FieldGet(fieldList(i), ungridded_dims=field_ungridded_dims, _RC)
         
         do j = 1, field_ungridded_dims%get_num_ungridded()
            u = ungridded_dims%get_ith_dim_spec(i)
            dim_name = u%get_name()
            call dim_names%insert(dim_name, is_new=is_new)
            if (is_new) then
               call metadata%add_dimension(u%get_name(), u%get_extent())
            end if
         end do
      end do

      _RETURN(_SUCCESS)
   end subroutine add_ungridded_dimensions

   function ungridded_dim_names(field, rc) result(dim_names)
      character(len=:), allocatable :: dim_names
      type(ESMF_Field), intent(in) :: field
      integer, optional, intent(out) :: rc
      integer :: status
      type(UngriddedDims) :: ungridded_dims

      call MAPL_FieldGet(field, ungridded_dims=ungridded_dims, _RC)
      dim_names = cat_ungridded_dim_names(ungridded_dims)
      _RETURN(_SUCCESS)
      
   end function ungridded_dim_names

   function cat_ungridded_dim_names(dims) result(dim_names)
      character(len=:), allocatable :: dim_names
      class(UngriddedDims), intent(in) :: dims
      type(UngriddedDim) :: u
      integer :: i
      character, parameter :: JOIN = ','

      dim_names = EMPTY
      do i = 1, dims%get_num_ungridded()
         u = dims%get_ith_dim_spec(i)
         dim_names = JOIN // u%get_name()
      end do

   end function cat_ungridded_dim_names

end module mapl3g_SharedIO
