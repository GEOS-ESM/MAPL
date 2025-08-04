#include "MAPL_ErrLog.h"
module mapl3g_PrimaryExport
   use ESMF
   use MAPL_ExceptionHandling 
   use mapl3g_AbstractDataSetFileSelector
   use mapl3g_Geom_API 
   use MAPL_FileMetadataUtilsMod
   use generic3g
   use mapl3g_DataSetBracket
   use mapl3g_DataSetNode
   use gftl2_StringStringMap
   implicit none

   public PrimaryExport

   type :: PrimaryExport
      character(len=:),  allocatable :: export_var
      character(len=:),  allocatable :: file_var
      class(AbstractDataSetFileSelector), allocatable :: file_selector
      type(DataSetBracket) :: bracket
      contains
         procedure :: get_file_selector
         procedure :: complete_export_spec
         procedure :: get_file_var_name
         procedure :: get_export_var_name
         procedure :: get_bracket
         procedure :: update_my_bracket
         procedure :: append_read_state
   end type

   interface PrimaryExport
      module procedure new_PrimaryExport
   end interface PrimaryExport

   contains

   function new_PrimaryExport(export_var, file_var, file_selector) result(primary_export)
      type(PrimaryExport) :: primary_export
      character(len=*), intent(in) :: export_var
      character(len=*), intent(in) :: file_var
      class(AbstractDataSetFileSelector), intent(in) :: file_selector

      type(DataSetNode) :: left_node, right_node

      primary_export%export_var = export_var
      primary_export%file_var = file_var
      allocate(primary_export%file_selector, source=file_selector)
      call left_node%set_node_side(NODE_LEFT)
      call right_node%set_node_side(NODE_RIGHT)
      call primary_export%bracket%set_node(NODE_LEFT, left_node)
      call primary_export%bracket%set_node(NODE_RIGHT, right_node)

   end function 

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
      !type(BasicVerticalGriddd) :: vertical_grid

      !vertical_grid = BasicVerticalGrid(3)
      metadata => this%file_selector%get_dataset_metadata(_RC)
      geom_mgr => get_geom_manager()
      geom = geom_mgr%get_mapl_geom_from_metadata(metadata%metadata, _RC)
      esmfgeom = geom%get_geom()

      call ESMF_StateGet(exportState, item_name, bundle, _RC)
      call MAPL_FieldBundleModify(bundle, geom=esmfgeom, units='NA', typekind=ESMF_TYPEKIND_R4, &
              vertical_stagger=VERTICAL_STAGGER_NONE,  _RC)

      _RETURN(_SUCCESS)
   end subroutine complete_export_spec
      
   subroutine update_my_bracket(this, current_time, weights, rc)
      class(PrimaryExport), intent(inout) :: this
      type(ESMF_Time), intent(in) :: current_time
      real, intent(out) :: weights(3)
      integer, optional, intent(out) :: rc

      integer :: status
      real :: local_weights(2)

      call this%file_selector%update_file_bracket(current_time, this%bracket, _RC)
      local_weights = this%bracket%compute_bracket_weights(current_time, _RC)
      weights = [0.0, local_weights(1), local_weights(2)]
      _RETURN(_SUCCESS)
   end subroutine update_my_bracket

   subroutine append_read_state(this, export_state, read_state, alias_map, rc)
      class(PrimaryExport), intent(inout) :: this
      type(ESMF_State), intent(in) :: export_state
      type(ESMF_State), intent(out) :: read_state
      type(StringStringMap), intent(out) :: alias_map
      integer, optional, intent(out) :: rc

      type(ESMF_FieldBundle) :: bundle
      integer :: status
      type(DataSetNode) :: node
      logical :: update_file
      type(ESMF_Field), allocatable :: field_list(:)
      character(len=ESMF_MAXSTR) :: field_name
     
      node = this%bracket%get_left_node()
      update_file = node%get_update()
      if (update_file) then
         call ESMF_StateGet(export_state, this%export_var, bundle, _RC)
         call MAPL_FieldBundleGet(bundle, fieldList=field_list, _RC)
         call ESMF_FieldGet(field_list(1), name=field_name, _RC)
         call alias_map%insert(trim(field_name), this%file_var )
         call ESMF_StateAdd(read_state, [field_list(1)], _RC)
      end if
      node = this%bracket%get_right_node()
      update_file = node%get_update()
      if (update_file) then
         call ESMF_StateGet(export_state, this%export_var, bundle, _RC)
         call MAPL_FieldBundleGet(bundle, fieldList=field_list, _RC)
         call ESMF_FieldGet(field_list(2), name=field_name, _RC)
         call alias_map%insert(trim(field_name), this%file_var )
         call ESMF_StateAdd(read_state, [field_list(2)], _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine append_read_state
 
end module mapl3g_PrimaryExport
