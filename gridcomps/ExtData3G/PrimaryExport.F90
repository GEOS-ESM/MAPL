#include "MAPL_ErrLog.h"
module mapl3g_PrimaryExport
   use ESMF
   use MAPL_ExceptionHandling 
   use mapl3g_AbstractDataSetFileSelector
   use mapl3g_NonClimDataSetFileSelector
   use mapl3g_ClimDataSetFileSelector
   use mapl3g_Geom_API 
   use MAPL_FileMetadataUtilsMod
   use generic3g
   use mapl3g_DataSetBracket
   use mapl3g_DataSetNode
   use mapl3g_ExtDataReader
   use gftl2_StringStringMap
   use gftl2_IntegerVector
   use mapl3g_ExtDataRule
   use mapl3g_ExtDataCollection
   use mapl3g_ExtDataSample
   use pfio, only: i_clients
   use VerticalCoordinateMod
   use mapl3g_FieldBundleSet
   implicit none

   public PrimaryExport

   type :: PrimaryExport
      character(len=:),  allocatable :: export_var
      character(len=:),  allocatable :: file_var
      integer :: client_collection_id
      class(AbstractDataSetFileSelector), allocatable :: file_selector
      type(DataSetBracket) :: bracket
      logical :: is_constant = .false.
      type(VerticalCoordinate) :: vcoord
      type(ESMF_Time), allocatable :: start_and_end(:)
      real :: linear_trans(2) ! offset, scaling

      contains
         procedure :: get_file_selector
         procedure :: complete_export_spec
         procedure :: update_export_spec
         procedure :: get_file_var_name
         procedure :: get_export_var_name
         procedure :: get_bracket
         procedure :: update_my_bracket
         procedure :: append_state_to_reader
   end type

   interface PrimaryExport
      module procedure new_PrimaryExport
   end interface PrimaryExport

   contains

   function new_PrimaryExport(export_var, rule, collection, sample, time_range, rc) result(primary_export) 
      type(PrimaryExport) :: primary_export
      character(len=*), intent(in) :: export_var
      type(ExtDataRule), pointer, intent(in) :: rule
      type(ExtDataCollection), pointer, intent(in) :: collection
      type(ExtDataSample), pointer, intent(in) :: sample
      type(ESMF_Time), intent(in) :: time_range(:)
      integer, optional, intent(out) :: rc
      
      type(NonClimDataSetFileSelector) :: non_clim_file_selector 
      type(ClimDataSetFileSelector) :: clim_file_selector 
      type(DataSetNode) :: left_node, right_node
      character(len=:), allocatable :: file_template
      integer :: status

      primary_export%export_var = export_var
      primary_export%is_constant = .not.associated(collection)
      if (associated(collection)) then
         if (sample%extrap_outside == 'clim') then
            clim_file_selector = ClimDataSetFileSelector(collection%file_template, collection%valid_range, collection%frequency, ref_time=collection%reff_time)
            allocate(primary_export%file_selector, source=clim_file_selector, _STAT)
         else
            non_clim_file_selector = NonClimDataSetFileSelector(collection%file_template, collection%frequency, ref_time=collection%reff_time, persist_closest = (sample%extrap_outside == "persist_closest") )
            allocate(primary_export%file_selector, source=non_clim_file_selector, _STAT)
         end if
         primary_export%file_var = rule%file_var
         primary_export%linear_trans = rule%linear_trans
         call left_node%set_node_side(NODE_LEFT)
         call right_node%set_node_side(NODE_RIGHT)
         call primary_export%bracket%set_node(NODE_LEFT, left_node)
         call primary_export%bracket%set_node(NODE_RIGHT, right_node)
         call primary_export%file_selector%get_file_template(file_template)
         primary_export%client_collection_id = i_clients%add_data_collection(file_template, _RC)
         call primary_export%bracket%set_parameters(time_interpolation=sample%time_interpolation)
         allocate(primary_export%start_and_end, source=time_range)
      end if
      _RETURN(_SUCCESS)

   end function new_PrimaryExport

   function get_file_selector(this) result(file_selector)
      class(AbstractDataSetFileSelector), allocatable :: file_selector
      class(PrimaryExport), intent(in) :: this
      file_selector = this%file_selector
   end function get_file_selector

   function get_bracket(this) result(bracket)
      type(DataSetBracket) :: bracket
      class(PrimaryExport), intent(in) :: this
      bracket = this%bracket
   end function get_bracket

   function get_file_var_name(this) result(varname)
      character(len=:), allocatable :: varname
      class(PrimaryExport), intent(in) :: this
      varname = this%file_var
   end function get_file_var_name

   function get_export_var_name(this) result(varname)
      character(len=:), allocatable :: varname
      class(PrimaryExport), intent(in) :: this
      varname = this%export_var
   end function get_export_var_name

   subroutine complete_export_spec(this, item_name, exportState, rc)
      class(PrimaryExport), intent(inout) :: this
      character(len=*), intent(in) :: item_name
      type(ESMF_State), intent(inout) :: exportState
      integer, optional, intent(out) :: rc

      integer :: status

      type(FileMetaDataUtils), pointer :: metadata
      type(MAPLGeom) :: geom
      type(ESMF_Geom) :: esmfgeom
      type(ESMF_FieldBundle) :: bundle
      type(GeomManager), pointer :: geom_mgr
      type(BasicVerticalGrid) :: vertical_grid

      if (this%is_constant) then
         _RETURN(_SUCCESS)
      end if

      metadata => this%file_selector%get_dataset_metadata(_RC)
      geom_mgr => get_geom_manager()
      geom = geom_mgr%get_mapl_geom_from_metadata(metadata%metadata, _RC)
      esmfgeom = geom%get_geom()

      this%vcoord = verticalCoordinate(metadata, this%file_var, _RC)

      call ESMF_StateGet(exportState, item_name, bundle, _RC)
      if (this%vcoord%vertical_type == NO_COORD) then
         call MAPL_FieldBundleModify(bundle, geom=esmfgeom, units='<unknown>', typekind=ESMF_TYPEKIND_R4, &
                 vertical_stagger=VERTICAL_STAGGER_NONE,  _RC)
      else if (this%vcoord%vertical_type == SIMPLE_COORD) then
         vertical_grid = BasicVerticalGrid(this%vcoord%num_levels)
         call MAPL_FieldBundleModify(bundle, geom=esmfgeom, units='<unknown>', &
                 typekind=ESMF_TYPEKIND_R4, vertical_grid=vertical_grid, &
                 vertical_stagger=VERTICAL_STAGGER_CENTER,  _RC)
      else
         _FAIL("unsupported vertical coordinate for item "//trim(this%export_var))
      end if

      _RETURN(_SUCCESS)
   end subroutine complete_export_spec
      
   subroutine update_export_spec(this, item_name, exportState, rc)
      class(PrimaryExport), intent(inout) :: this
      character(len=*), intent(in) :: item_name
      type(ESMF_State), intent(inout) :: exportState
      integer, optional, intent(out) :: rc

      integer :: status

      type(FileMetaDataUtils), pointer :: metadata
      type(MAPLGeom) :: geom
      type(ESMF_Geom) :: esmfgeom
      type(ESMF_FieldBundle) :: bundle
      type(GeomManager), pointer :: geom_mgr
      type(BasicVerticalGrid) :: vertical_grid

      if (this%is_constant) then
         _RETURN(_SUCCESS)
      end if

      metadata => this%file_selector%get_dataset_metadata(_RC)
      geom_mgr => get_geom_manager()
      geom = geom_mgr%get_mapl_geom_from_metadata(metadata%metadata, _RC)
      esmfgeom = geom%get_geom()

      this%vcoord = verticalCoordinate(metadata, this%file_var, _RC)

      call ESMF_StateGet(exportState, item_name, bundle, _RC)
      if (this%vcoord%vertical_type == NO_COORD) then
         call FieldBundleSet(bundle, geom=esmfgeom, units='<unknown>', typekind=ESMF_TYPEKIND_R4, &
                 vert_staggerloc=VERTICAL_STAGGER_NONE,  _RC)
      else if (this%vcoord%vertical_type == SIMPLE_COORD) then
         vertical_grid = BasicVerticalGrid(this%vcoord%num_levels)
         call FieldBundleSet(bundle, geom=esmfgeom, units='<unknown>', &
                 typekind=ESMF_TYPEKIND_R4, num_levels=this%vcoord%num_levels, &
                 vert_staggerloc=VERTICAL_STAGGER_CENTER,  _RC)
      else
         _FAIL("unsupported vertical coordinate for item "//trim(this%export_var))
      end if

      _RETURN(_SUCCESS)
   end subroutine update_export_spec
      
   subroutine update_my_bracket(this, bundle, current_time, weights, rc)
      class(PrimaryExport), intent(inout) :: this
      type(ESMF_FieldBundle), intent(inout) :: bundle
      type(ESMF_Time), intent(in) :: current_time
      real, intent(out) :: weights(3)
      integer, optional, intent(out) :: rc

      integer :: status
      real :: local_weights(2)

      call this%file_selector%update_file_bracket(bundle, current_time, this%bracket, _RC)
      local_weights = this%bracket%compute_bracket_weights(current_time, _RC)
      weights = [0.0, local_weights(1), local_weights(2)]

      ! apply optional linear transformation
      weights(1) = this%linear_trans(1)
      weights(2:3) = weights(2:3)*this%linear_trans(2)
      _RETURN(_SUCCESS)
   end subroutine update_my_bracket

   subroutine append_state_to_reader(this, export_state, reader,  rc)
      class(PrimaryExport), intent(inout) :: this
      type(ESMF_State), intent(inout) :: export_state
      type(ExtDataReader), intent(inout) :: reader
      integer, optional, intent(out) :: rc

      type(ESMF_FieldBundle) :: bundle
      integer :: status
      type(DataSetNode) :: node
      logical :: update_file
      type(ESMF_Field), allocatable :: field_list(:)
      character(len=:), allocatable :: filename
      integer :: time_index
     
      node = this%bracket%get_left_node()
      update_file = node%get_update()
      if (update_file) then
         call ESMF_StateGet(export_state, this%export_var, bundle, _RC)
         call MAPL_FieldBundleGet(bundle, fieldList=field_list, _RC)
         time_index = node%get_time_index()
         call node%get_file(filename)
         call reader%add_item(field_list(1), this%file_var, filename, time_index, this%client_collection_id, _RC)
      end if
      node = this%bracket%get_right_node()
      update_file = node%get_update()
      if (update_file) then
         call ESMF_StateGet(export_state, this%export_var, bundle, _RC)
         call MAPL_FieldBundleGet(bundle, fieldList=field_list, _RC)
         time_index = node%get_time_index()
         call node%get_file(filename)
         call reader%add_item(field_list(2), this%file_var, filename, time_index, this%client_collection_id, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine append_state_to_reader

end module mapl3g_PrimaryExport
