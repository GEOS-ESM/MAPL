#include "MAPL_Generic.h"
module mapl3g_ExtDataGridComp_private
   use mapl_ErrorHandlingMod
   use mapl_keywordenforcermod
   use esmf
   implicit none
   private

   public :: merge_config
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

end module mapl3g_ExtDataGridComp_private
