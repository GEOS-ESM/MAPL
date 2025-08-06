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
      iter = exports%ftn_begin()
      i=0
      do while (iter /= exports%ftn_end())
         call iter%next()
         export => iter%of() 
         export_name = export%get_export_var_name() 
         i=i+1
         call lgr%info('---- %i0.5~: %a', i, export_name)
      end do

   end subroutine

end module mapl3g_ExtDataGridComp_private
