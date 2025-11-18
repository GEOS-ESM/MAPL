#include "MAPL.h"
module mapl3g_ExtDataGridComp_private
   use mapl_ErrorHandlingMod
   use mapl_keywordenforcermod
   use esmf
   use mapl3
   use mapl3g_stateitem
   use mapl3g_PrimaryExportVector
   use mapl3g_PrimaryExport
   implicit none
   private

   public :: add_var_specs
   public :: set_weights
   public :: get_active_items
   public :: report_active_items

   character(len=*), parameter :: SUBCONFIG_KEY = 'subconfigs'
   character(len=*), parameter :: COLLECTIONS_KEY = 'Collections'
   character(len=*), parameter :: SAMPLINGS_KEY = 'Samplings'
   character(len=*), parameter :: EXPORTS_KEY = 'Exports'
   character(len=*), parameter :: DERIVED_KEY = 'Derived'

contains

   recursive subroutine add_var_specs(gridcomp, hconfig, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      logical :: is_seq, file_found, is_map
      integer :: status, i, bracket_size
      character(len=:), allocatable :: sub_configs(:)
      type(ESMF_HConfig) :: sub_config, export_config, temp_config
      type(ESMF_HConfigIter) :: hconfigIter,hconfigIterBegin,hconfigIterEnd
      character(len=:), allocatable :: short_name, collection_name, str_const
      type(VariableSpec) :: varspec
      type(ESMF_StateItem_Flag) :: item_type

      if (ESMF_HConfigIsDefined(hconfig, keyString='subconfigs')) then
         is_seq = ESMF_HConfigIsSequence(hconfig, keyString='subconfigs') 
         sub_configs = ESMF_HConfigAsStringSeq(hconfig, ESMF_MAXPATHLEN, keystring='subconfigs', _RC)
         do i=1,size(sub_configs)
            inquire(file=trim(sub_configs(i)), exist=file_found)
            _ASSERT(file_found,"could not find: "//trim(sub_configs(i)))
            sub_config = ESMF_HConfigCreate(filename=trim(sub_configs(i)), _RC)
            call add_var_specs(gridcomp, sub_config, _RC)
         enddo
      end if

      if (ESMF_HConfigIsDefined(hconfig, keyString='Exports')) then
         export_config = ESMF_HConfigCreateAt(hconfig, keyString='Exports', _RC)
         hconfigIterBegin = ESMF_HConfigIterBegin(export_config)
         hconfigIter = hconfigIterBegin
         hconfigIterEnd = ESMF_HConfigIterEnd(export_config)
         do while (ESMF_HConfigIterLoop(hconfigIter,hconfigIterBegin,hconfigIterEnd))
            short_name = ESMF_HConfigAsStringMapKey(hconfigIter, _RC)
            temp_config = ESMF_HConfigCreateAtMapVal(hconfigIter, _RC)
            is_map =ESMF_HConfigIsMap(temp_config, _RC)
            if (is_map) then
               collection_name = ESMF_HConfigAsString(temp_config, keyString='collection', _RC)
               if (collection_name == "/dev/null") then
                  str_const = get_constant(temp_config, _RC)
                  varspec = make_VariableSpec(ESMF_STATEINTENT_EXPORT, short_name, &
                  itemType=MAPL_STATEITEM_EXPRESSION, expression=str_const, units="<unknown>", &
                  _RC)
               else
                  item_type = get_maplitem_type(temp_config, _RC)
                  bracket_size = get_bracket_size(item_type)
                  varspec = make_VariableSpec(ESMF_STATEINTENT_EXPORT, short_name, &
                  itemType=item_type, bracket_size = bracket_size, &
                  _RC)
               end if
            else
               item_type = get_maplitem_type(temp_config, _RC)
               bracket_size = get_bracket_size(item_type)
               varspec = make_VariableSpec(ESMF_STATEINTENT_EXPORT, short_name, &
               itemType=item_type, bracket_size = bracket_size, &
               _RC)
            end if
            call MAPL_GridCompAddVarSpec(gridcomp, varspec, _RC)
         enddo
      end if
      _RETURN(_SUCCESS)
   end subroutine

   subroutine set_weights(state, export_name, weights, rc)
      type(ESMF_State), intent(inout) :: state
      character(len=*), intent(in) :: export_name
      real, intent(in) :: weights(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_FieldBundle) :: bundle

      call ESMF_StateGet(state, export_name, bundle, _RC)
      call MAPL_FieldBundleSet(bundle, interpolation_weights=weights, _RC)

      _RETURN(_SUCCESS)

   end subroutine set_weights

   function get_active_items(state, rc) result(active_list)
      type(StringVector) :: active_list
      type(ESMF_State), intent(inout) :: state
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR), allocatable :: itemNameList(:)
      type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
      integer itemCount,i
      type(ESMF_FieldBundle) :: bundle
      type(ESMF_Field) :: field
      type(StateItemAllocation) :: allocation_status

      call ESMF_StateGet(state, itemCount=itemCount, _RC)
      allocate(itemNameList(itemCount), _STAT)
      allocate(itemTypeList(itemCount), _STAT)
      call ESMF_StateGet(state, itemTypeList=itemTypeList, itemNameList=itemNameList, _RC)
      do i=1,itemCount
         if (itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then
            call ESMF_StateGet(state, trim(itemNameList(i)), bundle, _RC)
            call MAPL_FieldBundleGet(bundle, allocation_status=allocation_status, _RC)
         else if (itemTypeList(i) == ESMF_STATEITEM_FIELD) then
            call ESMF_StateGet(state, trim(itemNameList(i)), field, _RC)
            call MAPL_FieldGet(field, allocation_status=allocation_status, _RC)
         end if
         if (allocation_status >= STATEITEM_ALLOCATION_ACTIVE) call active_list%push_back(trim(itemNameList(i)))
      enddo 

      _RETURN(_SUCCESS)

   end function get_active_items

   subroutine report_active_items(exports, lgr)
      type(StringVector), intent(in) :: exports
      class(logger), pointer :: lgr

      type(StringVectorIterator) :: iter
      character(len=:), pointer :: export_name
      integer :: i

      call lgr%info('*******************************************************')
      call lgr%info('** Variables to be provided by the ExtData Component **')
      call lgr%info('*******************************************************')
      iter = exports%ftn_begin()
      i=0
      do while (iter /= exports%ftn_end())
         call iter%next()
         export_name => iter%of() 
         i=i+1
         call lgr%info('---- %i0.5~: %a', i, export_name)
      end do

   end subroutine

   function get_constant(hconfig, rc) result(constant_expression)
      character(len=:), allocatable :: constant_expression 
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      real, allocatable :: real_array(:)
      character(len=50) :: temp_str
      integer :: status

      constant_expression = "0."
      if (ESMF_HConfigIsDefined(hconfig, keyString="linear_transformation")) then 
         real_array = ESMF_HConfigAsR4Seq(hconfig, keyString="linear_transformation", _RC)
         write(temp_str, '(G0)') real_array(1) 
         constant_expression = trim(temp_str)
      end if
      _RETURN(_SUCCESS)
   end function get_constant

   function get_maplitem_type(hconfig, rc) result(item_type)
      type(ESMF_StateItem_Flag) :: item_type
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, intent(out) :: rc

      logical :: is_map, is_sequence, first_item
      integer :: status
      type(ESMF_HConfigIter) :: hconfigIter,hconfigIterBegin,hconfigIterEnd
      type(ESMF_HConfig) :: sub_hconfig
      type(ESMF_StateItem_Flag) :: last_type

      is_map = ESMF_HConfigIsMap(hconfig, _RC)
      is_sequence = ESMF_HConfigIsSequence(hconfig, _RC)
      if (is_map) then
         item_type =get_maplitem_type_single_map(hconfig, _RC)
      else if (is_sequence) then
         last_type = ESMF_STATEITEM_UNKNOWN
         first_item = .true.
         hconfigIterBegin = ESMF_HConfigIterBegin(hconfig)
         hconfigIter = hconfigIterBegin
         hconfigIterEnd = ESMF_HConfigIterEnd(hconfig)
         do while (ESMF_HConfigIterLoop(hconfigIter,hconfigIterBegin,hconfigIterEnd))
            sub_hconfig = ESMF_HConfigCreateAt(hconfigIter, _RC)
            item_type =get_maplitem_type_single_map(sub_hconfig, _RC)
            if (first_item .eqv. .false.) then
               _ASSERT(item_type == last_type, 'vector and scalar in multi rule item')
            end if
            first_item = .false.
         enddo
      end if
      _RETURN(_SUCCESS)
   end function get_maplitem_type

   function get_maplitem_type_single_map(hconfig, rc) result(item_type)
      type(ESMF_StateItem_Flag) :: item_type
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, intent(out) :: rc

      logical :: has_variable
      integer :: status
      character(len=:), allocatable :: variable_name

      item_type = MAPL_STATEITEM_BRACKET
      has_variable = ESMF_HConfigIsDefined(hconfig, keyString='variable', _RC)
      if (has_variable) then
         variable_name = ESMF_HConfigAsString(hconfig, keyString='variable', _RC)
         if (index(variable_name, ';') > 0) item_type = MAPL_STATEITEM_VECTOR_BRACKET
      end if
      _RETURN(_SUCCESS)
   end function get_maplitem_type_single_map


   function get_bracket_size(item_type) result(bracket_size)
      integer :: bracket_size
      type(ESMF_StateItem_Flag) :: item_type
      if (item_type == MAPL_STATEITEM_BRACKET) then
         bracket_size = 2
      else if (item_type == MAPL_STATEITEM_VECTOR_BRACKET) then
         bracket_size = 4
      end if
    end function get_bracket_size
end module mapl3g_ExtDataGridComp_private
