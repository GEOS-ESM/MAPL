#include "MAPL.h"

module mapl3g_SharedIO

   use mapl_ErrorHandlingMod
   use mapl3g_FieldBundle_API
   use mapl3g_Field_API
   use mapl3g_VerticalStaggerLoc
   use pfio, only: FileMetaData, Variable, UnlimitedEntity
   use pfio, only: PFIO_UNLIMITED, PFIO_REAL32, PFIO_REAL64
   use gFTL2_StringVector
   use gFTL2_StringSet
   use mapl3g_Geom_API
   use MAPL_BaseMod
   use mapl3g_UngriddedDims
   use mapl3g_UngriddedDim
   use esmf

   implicit none(type,external)

   public add_variables
   public add_variable
   public get_mapl_geom
   public create_time_variable
   public bundle_to_metadata
   public esmf_to_pfio_type

   public :: add_vertical_dimensions
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
      call metadata%add_dimension('time', PFIO_UNLIMITED)

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
      do i = 1, size(fieldList)
         call add_variable(metadata, fieldList(i), _RC)
      enddo

      _RETURN(_SUCCESS)
   end subroutine add_variables

   subroutine add_variable(metadata, field,  rc)
      type(ESMF_Field), intent(in) :: field
      type(FileMetaData), intent(inout) :: metadata
      integer, intent(out), optional :: rc

      integer :: status
      type(Variable) :: v
      character(len=:), allocatable :: variable_dim_names
      type(ESMF_TYPEKIND_FLAG) :: typekind
      character(len=:), allocatable :: short_name
      character(len=:), allocatable :: units
      character(len=:), allocatable :: long_name
      character(len=:), allocatable :: standard_name

      type(ESMF_Geom) :: geom
      integer :: pfio_type

      variable_dim_names = get_variable_dim_names(field, geom, _RC)
      call MAPL_FieldGet(field, short_name=short_name, typekind=typekind, _RC)
      pfio_type = esmf_to_pfio_type(typekind ,_RC)
      v = Variable(type=pfio_type, dimensions=variable_dim_names)

      ! Attributes
      call MAPL_FieldGet(field, units=units, long_name=long_name, standard_name=standard_name, _RC)
      if (allocated(units))then
         call v%add_attribute('units', units)
      end if
      if (allocated(long_name)) then
         call v%add_attribute('long_name', long_name)
      end if
      if (allocated(standard_name)) then
         call v%add_attribute('standard_name', standard_name)
      end if

      call metadata%add_variable(short_name, v, _RC)

      _RETURN(_SUCCESS)
   end subroutine add_variable

   function get_variable_dim_names(field, geom, rc) result(dim_names)
      character(len=:), allocatable :: dim_names
      type(ESMF_Field), intent(in) :: field
      type(ESMF_Geom), intent(in) :: geom
      integer, optional, intent(out) :: rc

      type(MAPLGeom), pointer :: mapl_geom
      type(StringVector) :: grid_variables
      type(ESMF_Geom) :: esmfgeom
      type(ESMF_Info) :: field_info
      character(len=:), allocatable :: vert_dim_name, ungridded_names
      logical :: vert_only
      integer :: grid_to_field_map(2), status

      ! horizontal dimension
      call ESMF_FieldGet(field, geom=esmfgeom, _RC)
      mapl_geom => get_mapl_geom(esmfgeom, _RC)
      grid_variables = mapl_geom%get_gridded_dims()
      call ESMF_FieldGet(field, gridToFieldMap=grid_to_field_map, _RC)
      vert_only = all(grid_to_field_map==0)
      dim_names = EMPTY
      if (.not. vert_only) dim_names = string_vec_to_comma_sep(grid_variables) // ","

      ! add vertical dimension
      vert_dim_name = get_vertical_dimension_name_from_field(field, _RC)
      if(vert_dim_name /= EMPTY) dim_names = dim_names // vert_dim_name // ","
      ! add any ungridded dimensions
      ungridded_names = ungridded_dim_names(field, _RC)
      if(ungridded_names /= EMPTY) dim_names = dim_names // ungridded_names // ","
      ! add time dimension
      dim_names = dim_names // "time"

      _RETURN(_SUCCESS)
   end function get_variable_dim_names

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
         pfio_type = PFIO_REAL32
      else if (esmf_type == ESMF_TYPEKIND_R8) then
         pfio_type = PFIO_REAL64
      else
         _FAIL("Unsupported ESMF field typekind for output")
      end if

      _RETURN(_SUCCESS)
   end function esmf_to_pfio_type

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
         comma_sep = comma_sep // "," // var
         call iter%next()
      enddo
   end function string_vec_to_comma_sep

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

      character(len=:), allocatable :: dim_name
      type(VerticalStaggerLoc) :: vertical_stagger
      type(ESMF_Field), allocatable :: fieldList(:)
      integer :: i, j, num_field_levels, status
      type(Variable) :: level_var
      real(kind=REAL64), allocatable :: temp_coords(:)
      logical :: lev_added, edge_added

      call MAPL_FieldBundleGet(bundle, fieldList=fieldList, _RC)
      lev_added = .false.
      edge_added = .false.
      do i = 1, size(fieldList)
         call MAPL_FieldGet(fieldList(i), vert_staggerloc=vertical_stagger, _RC)
         if (vertical_stagger == VERTICAL_STAGGER_NONE) cycle
         call MAPL_FieldGet(fieldList(i), num_levels=num_field_levels, _RC)
         dim_name = vertical_stagger%get_dimension_name()
         call metadata%add_dimension(dim_name, num_field_levels)
         if ((dim_name == "lev") .and. (.not. lev_added)) then
            call add_lev_or_edge_(dim_name, num_field_levels, metadata, _RC)
            lev_added = .true.
         end if
         if ((dim_name == "edge") .and. (.not. edge_added)) then
            call add_lev_or_edge_(dim_name, num_field_levels, metadata, _RC)
            edge_added = .true.
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine add_vertical_dimensions

   subroutine add_lev_or_edge_(dim_name, num_levels, metadata, rc)
      character(len=*), intent(in) :: dim_name
      integer, intent(in) :: num_levels
      type(FileMetaData), intent(inout) :: metadata
      integer, optional, intent(out) :: rc

      type(Variable) :: level_var
      real(kind=REAL64), allocatable :: temp_coords(:)
      integer :: j, status

      level_var = Variable(type=PFIO_REAL64, dimensions=dim_name)
      call level_var%add_attribute('long_name','vertical level')
      call level_var%add_attribute('units','layer')
      call level_var%add_attribute('positive','down')
      call level_var%add_attribute('coordinate','eta')
      call level_var%add_attribute('standard_name','model_layers')
      allocate(temp_coords(num_levels), _STAT)
      temp_coords = [(j, j=1,num_levels)]
      call level_var%add_const_value(UnlimitedEntity(temp_coords))
      call metadata%add_variable(dim_name, level_var, _RC)

      _RETURN(_SUCCESS)
   end subroutine add_lev_or_edge_

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
      type(UngriddedDims) :: field_ungridded_dims
      type(UngriddedDim) :: u
      integer :: ifield, jdim
      type(ESMF_Field) :: field
      type(ESMF_Field), allocatable :: fieldList(:)
      type(StringSet) :: dim_names
      character(:), allocatable :: dim_name
      logical :: is_new

      call MAPL_FieldBundleGet(bundle, fieldList=fieldList, _RC)
      do ifield = 1, size(fieldList)
         call MAPL_FieldGet(fieldList(ifield), ungridded_dims=field_ungridded_dims, _RC)

         do jdim = 1, field_ungridded_dims%get_num_ungridded()
            u = field_ungridded_dims%get_ith_dim_spec(jdim)
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

      integer :: i

#define JOIN(a,b) a // ',' // b
      dim_names = EMPTY
      do i = 1, dims%get_num_ungridded()
         associate (u => dims%get_ith_dim_spec(i))
           if (dim_names /= EMPTY) then
              dim_names = JOIN(dim_names, u%get_name())
           else
              dim_names = u%get_name()
           end if
         end associate
      end do
#undef JOIN
   end function cat_ungridded_dim_names

end module mapl3g_SharedIO
