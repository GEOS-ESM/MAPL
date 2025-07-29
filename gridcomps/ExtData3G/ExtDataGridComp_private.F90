#include "MAPL.h"
module mapl3g_ExtDataGridComp_private
   use mapl_ErrorHandlingMod
   use mapl_keywordenforcermod
   use esmf
   use mapl3
   use mapl3g_stateitem
   use mapl3g_PrimaryExportVector
   use mapl3g_PrimaryExport
   use pFlogger, only: logger
   implicit none
   private

   public :: merge_config
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

   recursive subroutine merge_config(merged_hconfig, input_hconfig, rc)
      type(ESMF_HConfig), intent(inout) :: merged_hconfig
      type(ESMF_HConfig), intent(in) :: input_hconfig
      integer, intent(out), optional :: rc

      integer :: status

      character(len=:), allocatable :: sub_configs(:)
      type(ESMF_HConfig) :: sub_config
      integer :: i
      logical :: is_sequence
    
      if (ESMF_HConfigIsDefined(input_hconfig, keyString=SUBCONFIG_KEY)) then
         is_sequence = ESMF_HConfigIsSequence(input_hconfig, keyString=SUBCONFIG_KEY, _RC)
         _ASSERT(is_sequence, "subconfig list in extdata not a sequence")
         sub_configs = ESMF_HConfigAsStringSeq(input_hconfig, ESMF_MAXPATHLEN, keyString=SUBCONFIG_KEY, _RC) 
         do i=1,size(sub_configs)
            sub_config = ESMF_HConfigCreate(filename=trim(sub_configs(i)), _RC)
            call merge_config(merged_hconfig, sub_config, _RC)
            call ESMF_HConfigDestroy(sub_config, _RC)
         enddo
      end if
      call merge_map(merged_hconfig, input_hconfig, COLLECTIONS_KEY, _RC)
      call merge_map(merged_hconfig, input_hconfig, SAMPLINGS_KEY, _RC)
      call merge_map(merged_hconfig, input_hconfig, EXPORTS_KEY, _RC)
      call merge_map(merged_hconfig, input_hconfig, DERIVED_KEY, _RC)

      _RETURN(_SUCCESS)

      contains

      subroutine merge_map(hconfig_to, hconfig_from, key, rc)
         type(ESMF_HConfig), intent(inout) :: hconfig_to
         type(ESMF_HConfig), intent(in) :: hconfig_from
         character(len=*), intent(in) :: key
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_HConfig) :: hconfig_temp, hconfig_exist, hconfig_accum, iter_val
         type(ESMF_HConfigIter) :: iter, iter_begin,iter_end
         character(len=:), allocatable :: iter_key

         if (ESMF_HConfigIsDefined(hconfig_from, keyString=key)) then
            hconfig_temp = ESMF_HConfigCreateAt(hconfig_from, keyString=key, _RC)
         else
            _RETURN(_SUCCESS)
         end if

         if (ESMF_HConfigIsDefined(hconfig_to, keyString=key)) then
            hconfig_accum = ESMF_HConfigCreate(_RC)

            iter_begin = ESMF_HConfigIterBegin(hconfig_temp)
            iter_end = ESMF_HConfigIterEnd(hconfig_temp)
            iter = iter_begin
            do while (ESMF_HConfigIterLoop(iter, iter_begin, iter_end, rc=status))
               _VERIFY(status)
               iter_key = ESMF_HConfigAsStringMapKey(iter, _RC)
               iter_val = ESMF_HConfigCreateAtMapVal(iter, _RC)
               call ESMF_HConfigAdd(hconfig_accum, iter_val, addKeyString=iter_key, _RC)
            enddo

            hconfig_exist = ESMF_HConfigCreateAt(hconfig_to, keyString=key, _RC)
            iter_begin = ESMF_HConfigIterBegin(hconfig_exist)
            iter_end = ESMF_HConfigIterEnd(hconfig_exist)
            iter = iter_begin
            do while (ESMF_HConfigIterLoop(iter, iter_begin, iter_end, rc=status))
               _VERIFY(status)
               iter_key = ESMF_HConfigAsStringMapKey(iter, _RC)
               iter_val = ESMF_HConfigCreateAtMapVal(iter, _RC)
               call ESMF_HConfigAdd(hconfig_accum, iter_val, addKeyString=iter_key, _RC)
            enddo
            call ESMF_HConfigSet(hconfig_to, hconfig_accum, keyString=key, _RC)

         else
            call ESMF_HConfigAdd(hconfig_to, hconfig_temp, addKeyString=key, _RC)
         end if
         _RETURN(_SUCCESS)

      end subroutine
   end subroutine merge_config

   ! once we pass in the merged hconfig after bug is fixed
   ! in ESMF this will no longer need to be recursive
   recursive subroutine add_var_specs(gridcomp, hconfig, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      logical :: is_seq, file_found
      integer :: status, i
      character(len=:), allocatable :: sub_configs(:)
      type(ESMF_HConfig) :: sub_config, export_config
      type(ESMF_HConfigIter) :: hconfigIter,hconfigIterBegin,hconfigIterEnd
      character(len=:), allocatable :: short_name
      type(VariableSpec) :: varspec

      if (ESMF_HConfigIsDefined(hconfig, keyString='subconfigs')) then
         is_seq = ESMF_HConfigIsSequence(hconfig, keyString='subconfigs') 
         sub_configs = ESMF_HConfigAsStringSeq(hconfig, ESMF_MAXPATHLEN, keystring='subconfigs', _RC)
         do i=1,size(sub_configs)
            _ASSERT(file_found,"could not find: "//trim(sub_configs(i)))
            sub_config = ESMF_HConfigCreate(filename=sub_configs(i), _RC)
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
            varspec = make_VariableSpec(ESMF_STATEINTENT_EXPORT, short_name, &
            itemType=MAPL_STATEITEM_BRACKET, bracket_size = 2, &
            _RC)
            call MAPL_GridCompAddVarSpec(gridcomp, varspec, _RC)
         enddo
      end if
      _RETURN(_SUCCESS)
   end subroutine

   ! for now we hardcode some weights until we flesh this out 
   subroutine set_weights(state, export_name, weights, rc)
      type(ESMF_State), intent(inout) :: state
      character(len=*), intent(in) :: export_name
      real, intent(in) :: weights(3)
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
      logical :: is_active

      call ESMF_StateGet(state, itemCount=itemCount, _RC)
      allocate(itemNameList(itemCount), _STAT)
      allocate(itemTypeList(itemCount), _STAT)
      call ESMF_StateGet(state, itemTypeList=itemTypeList, itemNameList=itemNameList, _RC)
      do i=1,itemCount
         _ASSERT(itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE, 'all items in extdata exprot should be fieldbundles')
         call ESMF_StateGet(state, trim(itemNameList(i)), bundle, _RC)
         call MAPL_FieldBundleGet(bundle, is_active=is_active, _RC)
         if (is_active) call active_list%push_back(trim(itemNameList(i)))
      enddo 

      _RETURN(_SUCCESS)

   end function get_active_items

   subroutine report_active_items(exports, lgr)
      type(PrimaryExportVector), intent(in) :: exports
      class(logger), pointer :: lgr

      type(PrimaryExportVectorIterator) :: iter
      type(PrimaryExport), pointer :: export
      character(len=:), allocatable :: export_name
      integer :: i

      call lgr%info('*******************************************************')
      call lgr%info('** Variables to be provided by the ExtData Component **')
      call lgr%info('*******************************************************')
      iter = exports%begin()
      i=0
      do while (iter /= exports%end())
         export => iter%of() 
         export_name = export%get_export_var_name() 
         i=i+1
         call lgr%info('---- %i0.5~: %a', i, export_name)
         call iter%next()
      end do

   end subroutine

end module mapl3g_ExtDataGridComp_private
