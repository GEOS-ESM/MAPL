#include "MAPL.h"

module mapl3g_FieldBundleRead

   use ESMF
   use mapl_ErrorHandling
   use mapl3g_GeomPFIO
   use mapl3g_GeomCatagorizer
   use mapl3g_Geom_API, only: GeomManager, MaplGeom, get_geom_manager, get_mapl_geom, MAPL_SameGeom
   use mapl3g_Field_API, only: MAPL_FieldCreate, MAPL_FieldGet
   use mapl3g_FieldBundle_API
   use mapl3g_VerticalStaggerLoc
   use mapl3g_VerticalGrid_API
   use mapl3g_RegridderManager, only: get_regridder_manager, RegridderManager
   use mapl3g_RegridderSpec
   use mapl3g_RegridderMethods
   use mapl3g_Regridder, only: Regridder
   use MAPL_FileMetadataUtilsMod
   use MAPL_StringTemplate, only: fill_grads_template
   use pFIO
   use gFTL2_StringVector

   implicit none(type, external)
   private

   public :: MAPL_read_bundle
   public :: FieldBundlePopulate

contains

   !---------------------------------------------------------------------------
   ! FieldBundlePopulate
   !
   ! Given an EMPTY bundle and file metadata, populate the bundle with
   ! MAPL3-compatible fields allocated on file_geom.  Only variables that are
   ! actual data fields (not coordinate/time vars) are added.
   !
   ! Arguments:
   !   bundle          - Empty bundle to populate (must have fieldCount == 0)
   !   file_geom       - ESMF_Geom matching the file's horizontal grid
   !   metadata_utils  - Utility object wrapping the file's FileMetadata
   !   only_vars       - Comma-separated list of variable names to include (optional)
   !   rc              - Return code (optional)
   !---------------------------------------------------------------------------
   subroutine FieldBundlePopulate(bundle, geom, vgrid, metadata_utils, only_vars, rc)
      type(ESMF_FieldBundle),  intent(inout) :: bundle
      type(ESMF_Geom),         intent(in)    :: geom
      class(VerticalGrid), pointer, intent(in)    :: vgrid
      type(FileMetadataUtils), intent(inout), target :: metadata_utils
      character(*), optional,  intent(in)    :: only_vars
      integer, optional,       intent(out)   :: rc

      integer :: status
      integer :: field_count
      type(ESMF_Field) :: field

      ! File metadata structures
      type(StringVariableMap), pointer     :: variables
      type(StringVariableMapIterator)      :: var_iter
      character(len=:), pointer            :: var_name_ptr
      class(Variable), pointer             :: this_variable
      type(StringVector), pointer          :: dim_names

      ! Grid geometry info
      type(MaplGeom), pointer :: mapl_geom
      type(StringVector)      :: gridded_dims

      ! Level info
      character(len=:), allocatable :: lev_name
      logical :: has_vertical_level
      integer :: lev_size
      logical :: var_has_levels

      ! Filtering
      character(len=:), allocatable :: exclude_list, only_list
      character(len=:), allocatable :: bracketed_name

      ! Variable attributes
      character(len=:), allocatable :: units, long_name
      type(VerticalStaggerLoc) :: vert_staggerloc

      ! Iterators
      type(StringVectorIterator) :: dim_iter
      character(len=:), pointer  :: dim_name_ptr

      if (present(only_vars)) then
         _ASSERT(len_trim(only_vars) > 0, 'FieldBundlePopulate: only_vars must not be empty if present')
      end if

      ! Verify the bundle is empty
      call ESMF_FieldBundleGet(bundle, fieldCount=field_count, _RC)
      _ASSERT(field_count == 0, 'FieldBundlePopulate: bundle must be empty')

      ! Identify coordinate/dimension variable names to exclude.
      ! Use mapl_geom%get_gridded_dims() which returns the horizontal
      ! dimension names (e.g., "lon,lat" for LatLon or "Xdim,Ydim,nf" for CS).
      mapl_geom => get_mapl_geom(geom, _RC)
      gridded_dims = mapl_geom%get_gridded_dims()

      ! Build comma-delimited exclude list: all horizontal coord names + lev/edge + time
      exclude_list = ",time,time_bnds,lev,edge,"
      dim_iter = gridded_dims%begin()
      do while (dim_iter /= gridded_dims%end())
         dim_name_ptr => dim_iter%of()
         exclude_list = exclude_list // trim(dim_name_ptr) // ","
         call dim_iter%next()
      end do

      ! Level dimension info
      lev_name = metadata_utils%get_level_name(_RC)
      has_vertical_level = (trim(lev_name) /= '')
      lev_size = 0
      if (has_vertical_level) then
         lev_size = metadata_utils%get_dimension(trim(lev_name), _RC)
      end if

      if (present(only_vars)) then
         only_list = "," // trim(only_vars) // ","
      end if

      ! Iterate variables in the file
      variables => metadata_utils%get_variables()
      var_iter = variables%ftn_begin()
      do while (var_iter /= variables%ftn_end())
         call var_iter%next()

         var_name_ptr => var_iter%first()

         ! Skip excluded coordinate/time variables
         bracketed_name = "," // trim(var_name_ptr) // ","
         if (index(exclude_list, bracketed_name) > 0) cycle

         ! Apply only_vars filter if provided
         if (present(only_vars)) then
            if (index(only_list, bracketed_name) == 0) cycle
         end if

         ! Determine if variable has a vertical level dimension
         var_has_levels = .false.
         if (has_vertical_level) then
            this_variable => var_iter%second()
            dim_names => this_variable%get_dimensions()
            dim_iter = dim_names%begin()
            do while (dim_iter /= dim_names%end())
               dim_name_ptr => dim_iter%of()
               if (trim(dim_name_ptr) == trim(lev_name)) var_has_levels = .true.
               call dim_iter%next()
            end do
         end if

         ! Get variable attributes from file
         units     = ''
         long_name = ''
         if (metadata_utils%var_has_attr(var_name_ptr, 'units')) then
            units = metadata_utils%get_var_attr_string(var_name_ptr, 'units', _RC)
         end if
         if (metadata_utils%var_has_attr(var_name_ptr, 'long_name')) then
            long_name = metadata_utils%get_var_attr_string(var_name_ptr, 'long_name', _RC)
         end if

         ! Create a MAPL3-compatible field on the file grid
         vert_staggerloc = VERTICAL_STAGGER_NONE
         if (var_has_levels) then
            ! Determine center vs. edge stagger from the level dimension name.
            ! Convention: if the name contains "edge" use EDGE stagger; otherwise CENTER.
            if (index(trim(lev_name), 'edge') > 0) then
               vert_staggerloc = VERTICAL_STAGGER_EDGE
            else
               vert_staggerloc = VERTICAL_STAGGER_CENTER
            end if

            field = MAPL_FieldCreate( &
                 geom, ESMF_TYPEKIND_R4, &
                 !num_levels=lev_size, &
                 vgrid=vgrid, &
                 vert_staggerloc=vert_staggerloc, &
                 name=trim(var_name_ptr), &
                 units=trim(units), &
                 long_name=trim(long_name), &
                 standard_name=trim(long_name), &
                 _RC)
         else
            field = MAPL_FieldCreate( &
                 geom=geom, typekind=ESMF_TYPEKIND_R4, &
                 vert_staggerloc=vert_staggerloc, &
                 name=trim(var_name_ptr), &
                 units=trim(units), &
                 long_name=trim(long_name), &
                 standard_name=trim(long_name), &
                 _RC)
         end if

         call MAPL_FieldBundleAdd(bundle, [field], _RC)
         block
            type(VerticalStaggerLoc) :: vloc_temp
            call MAPL_FieldGet(field, vert_staggerloc=vloc_temp, _RC)
         end block 

      end do

      _RETURN(_SUCCESS)
   end subroutine FieldBundlePopulate


   !---------------------------------------------------------------------------
   ! FieldBundleRead
   !
   ! Read data from a NetCDF file into an ESMF_FieldBundle using the GeomIO
   ! layer.  If the bundle is empty it is first populated from the file
   ! metadata.  If the file grid differs from the bundle grid, the data are
   ! regridded using the RegridderManager (horizontal only).
   !
   ! Template substitution (GrADS-style) is applied to file_tmpl with the
   ! supplied time to resolve the actual filename.  If file_override is
   ! provided it replaces the resolved filename (but template substitution is
   ! still performed for collection registration).
   !
   ! Arguments:
   !   bundle        - Bundle to read into (may be empty; populated if so)
   !   file_tmpl     - GrADS-style file template (or plain filename)
   !   time          - ESMF_Time for the time step to read
   !   only_vars     - Comma-separated subset of variables (optional)
   !   regrid_method - Regrid method integer constant (optional, default bilinear)
   !   noread        - If .true., only populate metadata; skip actual data read
   !   file_override - Override the resolved filename (template still used for
   !                   collection key); useful when averaging multiple files that
   !                   share the same schema (optional)
   !   rc            - Return code (optional)
   !---------------------------------------------------------------------------
   subroutine MAPL_read_bundle(bundle, file_tmpl, time, only_vars, regrid_method, &
        noread, file_override, rc)
      type(ESMF_FieldBundle), intent(inout) :: bundle
      character(*),           intent(in)    :: file_tmpl
      type(ESMF_Time),        intent(in)    :: time
      character(*), optional, intent(in)    :: only_vars
      integer, optional,      intent(in)    :: regrid_method
      logical, optional,      intent(in)    :: noread
      character(*), optional, intent(in)    :: file_override
      integer, optional,      intent(out)   :: rc

      integer :: status
      integer :: field_count, time_index, i, regrid_method_

      character(len=ESMF_MAXPATHLEN)  :: file_name
      type(NetCDF4_FileFormatter)     :: file_formatter
      type(FileMetaData)              :: metadata
      type(FileMetadataUtils)         :: metadata_utils
      type(ESMF_Time), allocatable    :: time_series(:)

      type(MaplGeom), pointer         :: file_mapl_geom
      type(ESMF_Geom)                 :: file_geom, bundle_geom
      logical                         :: same_grid

      type(ESMF_FieldBundle)          :: file_bundle
      type(ESMF_Field), allocatable   :: bundle_fields(:), file_fields(:)
      type(ESMF_Field)                :: file_field

      class(GeomPFIO), allocatable    :: reader
      type(RegridderManager), pointer :: regridder_mgr
      class(Regridder), pointer       :: mapl_regridder
      type(RegridderSpec)             :: spec

      type(ESMF_TypeKind_Flag)        :: typekind
      character(len=ESMF_MAXSTR)      :: timestring

      class(VerticalGrid), pointer :: file_vgrid
      class(VerticalGridManager), pointer :: vgrid_manager
      type(GeomManager), pointer :: geom_mgr

      !--- Resolve filename from template ---
      call ESMF_TimeGet(time, timeString=timestring, _RC)
      call fill_grads_template(file_name, file_tmpl, time=time, _RC)
      if (present(file_override)) file_name = trim(file_override)

      !--- Read file metadata ---
      call file_formatter%open(trim(file_name), PFIO_READ, _RC)
      metadata = file_formatter%read(_RC)
      call file_formatter%close(_RC)
      call metadata_utils%create(metadata, trim(file_name))

      !--- Confirm the requested time is on the file ---
      call metadata_utils%get_time_info(timeVector=time_series, _RC)
      time_index = -1
      do i = 1, size(time_series)
         if (time == time_series(i)) then
            time_index = i
            exit
         end if
      end do
      _ASSERT(time_index /= -1, 'FieldBundleRead: time '//trim(timestring)//' not found on file '//trim(file_name))
      deallocate(time_series)

      !--- Get the file's geometry from the geom manager ---
      geom_mgr => get_geom_manager()
      file_mapl_geom => geom_mgr%get_mapl_geom(metadata, _RC)
      file_geom = file_mapl_geom%get_geom()

      vgrid_manager => get_vertical_grid_manager(_RC)
      file_vgrid => vgrid_manager%create_grid_from_file_metadata(metadata, _RC)

      !--- Check whether file grid matches bundle grid ---
      call MAPL_FieldBundleGet(bundle, _RC)
      call ESMF_FieldBundleGet(bundle, geom=bundle_geom, _RC)
      same_grid = MAPL_SameGeom(file_geom, bundle_geom)

      !--- Populate empty bundle or verify existing fields ---
      call ESMF_FieldBundleGet(bundle, fieldCount=field_count, _RC)
      if (field_count == 0) then
         call FieldBundlePopulate(bundle, bundle_geom, file_vgrid, metadata_utils, &
              only_vars=only_vars, _RC)
      else
         ! Verify every field in the bundle exists in the file
         call MAPL_FieldBundleGet(bundle, fieldList=bundle_fields, _RC)
         do i = 1, size(bundle_fields)
            block
               character(len=ESMF_MAXSTR) :: fname_
               call ESMF_FieldGet(bundle_fields(i), name=fname_, _RC)
               _ASSERT(metadata_utils%has_variable(trim(fname_)), 'FieldBundleRead: field "'//trim(fname_)//'" not found in '//trim(file_name))
            end block
         end do
      end if

      !--- Early exit if noread requested ---
      if (present(noread)) then
         if (noread) _RETURN(_SUCCESS)
      end if

      !--- Build a file-grid bundle (alias if same grid, otherwise new) ---
      if (same_grid) then
         file_bundle = bundle
      else
         ! Create a minimal file_bundle with matching field names/types on file_geom.
         ! Vertical info is copied from the bundle field so the number of levels
         ! in the file field matches (horizontal regrid only; no vertical interp).
         call MAPL_FieldBundleGet(bundle, fieldList=bundle_fields, _RC)
         file_bundle = ESMF_FieldBundleCreate(_RC)
         call MAPL_FieldBundleSet(file_bundle, fieldBundleType=FIELDBUNDLETYPE_BASIC, _RC)
         do i = 1, size(bundle_fields)
            block
               character(len=ESMF_MAXSTR)  :: fname_
               type(ESMF_TypeKind_Flag)    :: tk
               type(VerticalStaggerLoc)    :: vstag
               integer                     :: nlevs
               call ESMF_FieldGet(bundle_fields(i), name=fname_, typekind=tk, _RC)
               call MAPL_FieldGet(bundle_fields(i), &
                    vert_staggerloc=vstag, _RC)
               if (vstag /= VERTICAL_STAGGER_NONE) then
                  file_field = MAPL_FieldCreate( &
                       geom=file_geom, typekind=tk, &
                       vert_staggerloc=vstag, vgrid=file_vgrid, &
                       name=trim(fname_), _RC)
               else
                  file_field = MAPL_FieldCreate( &
                       geom=file_geom, typekind=tk, &
                       name=trim(fname_), _RC)
               end if
               call ESMF_FieldBundleAdd(file_bundle, [file_field], _RC)
            end block
         end do
      end if

      !--- Read data into file_bundle via GeomIO ---
      allocate(reader, source=make_geom_pfio(metadata), _STAT)
      call reader%initialize(trim(file_name), file_geom, _RC)
      call reader%request_data_from_file(trim(file_name), file_bundle, _RC)
      call i_Clients%done_collective_prefetch()
      call i_Clients%wait()

      !--- Regrid if grids differ ---
      if (.not. same_grid) then
         regrid_method_ = REGRID_METHOD_BILINEAR
         if (present(regrid_method)) regrid_method_ = regrid_method

         call ESMF_FieldGet(bundle_fields(1), typekind=typekind, _RC)
         regridder_mgr => get_regridder_manager()
         spec = RegridderSpec( &
              generate_esmf_regrid_param(regrid_method_, typekind), &
              file_geom, bundle_geom)
         mapl_regridder => regridder_mgr%get_regridder(spec, _RC)
         call mapl_regridder%regrid(file_bundle, bundle, _RC)

         ! Destroy the temporary file-grid bundle and its fields
         call MAPL_FieldBundleGet(file_bundle, fieldList=file_fields, _RC)
         do i = 1, size(file_fields)
            call ESMF_FieldDestroy(file_fields(i), noGarbage=.true., _RC)
         end do
         call ESMF_FieldBundleDestroy(file_bundle, noGarbage=.true., _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine mapl_read_bundle

end module mapl3g_FieldBundleRead
