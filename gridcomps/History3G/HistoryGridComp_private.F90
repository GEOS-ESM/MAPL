#include "MAPL.h"

module mapl3g_HistoryGridComp_private
   use MAPL
   use mapl_ErrorHandlingMod
   use mapl_keywordenforcermod
   use mapl3g_HistoryConstants
   use mapl3g_HistoryUtilities
   use mapl3g_StatisticsGridComp, only: statistics_setServices => setServices
   use esmf

   implicit none
   private

   public :: make_child_name
   public :: make_child_hconfig
   public :: get_subconfig
   public :: add_stats_gc

contains

   ! Collection names are permitted to include period ('.') characters, but gridcomps
   ! are not. (Because we use "." as dive-down character in other syntax.)  So here
   ! we encode the collection name by replacing "." with "\.".
   function make_child_name(collection_name, rc) result(child_name)
      character(len=:), allocatable :: child_name
      character(len=*), intent(in) :: collection_name
      integer, optional, intent(out) :: rc

      integer :: i
      character(*), parameter :: ESCAPE = '\'

      child_name = ''
      do i = 1, len(collection_name)
         associate (c => collection_name(i:i))
           if (c == '.') then
              child_name = child_name // ESCAPE
           end if
           child_name = child_name // c
         end associate
     end do

      _RETURN(_SUCCESS)
   end function make_child_name

   function make_child_hconfig(hconfig, collection_name, rc) result(child_hconfig)
      type(ESMF_HConfig) :: child_hconfig
      type(ESMF_HConfig), intent(in) :: hconfig
      character(len=*), intent(in) :: collection_name
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_HConfig) :: collections_hconfig

      collections_hconfig = get_subconfig(hconfig, 'collections', _RC)
      child_hconfig = get_subconfig(collections_hconfig, collection_name, _RC)
      call ESMF_HConfigDestroy(collections_hconfig, _RC)

      call ESMF_HConfigAdd(child_hconfig, content=collection_name, addKeyString='collection_name', _RC)

      _RETURN(_SUCCESS)
   end function make_child_hconfig

   function get_subconfig(hconfig, keyString, rc) result(subconfig)
      type(ESMF_HConfig) :: subconfig
      type(ESMF_HConfig), intent(in) :: hconfig
      character(*), intent(in) :: keystring
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_key
      logical :: is_map

      has_key = ESMF_HConfigIsDefined(hconfig, keyString=keyString, _RC)
      _ASSERT(has_key, 'Hconfig is expected to have '//keyString//' but does not.')

      is_map = ESMF_HConfigIsMap(hconfig, keyString=keyString, _RC)
      _ASSERT(is_map, 'HConfig expected a YAML mapping for '//keyString//'but does not.')

      subconfig = ESMF_HConfigCreateAt(hconfig, keyString=keystring, _RC)

      _RETURN(_SUCCESS)
   end function get_subconfig

   subroutine add_stats_gc(gridcomp, child_name, child_hconfig, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      character(len=*), intent(in) :: child_name
      type(ESMF_HConfig), intent(in) :: child_hconfig
      integer, intent(out) :: rc

      integer :: status
      type(ESMF_HConfig) :: time_hconfig, stats_hconfig, var_list
      logical :: has_mode, has_frequency, has_ref_datetime
      character(len=:), allocatable :: mode, ref_datetime, frequency, short_name, name_in_comp
      type(ESMF_HConfigIter) :: iter, iter_begin, iter_end
      type(ESMF_HConfig) :: stat_item, stats_list, stats_mapl_section
      type(ChildSpec) :: child_spec

      time_hconfig = ESMF_HConfigCreateAt(child_hconfig, keyString='time_spec', _RC)
      has_mode = ESMF_HConfigIsDefined(time_hconfig, keyString=KEY_ACCUMULATION_TYPE, _RC)
      has_frequency = ESMF_HConfigIsDefined(time_hconfig, keyString='frequency', _RC)

      _RETURN_UNLESS(has_mode)

      mode = ESMF_HConfigAsString(time_hconfig, keyString='mode', _RC)
      _RETURN_IF(mode == 'instantaneous')
      _ASSERT(has_frequency, 'requested statitics performed on collection: '//child_name//' but did not provide frequency of the collection')

      stats_hconfig = ESMF_HConfigCreate(content='{}',_RC)
      ref_datetime = "'YYYY-MM-DDTHH:NN:SS'"
      has_ref_datetime = ESMF_HConfigIsDefined(child_hconfig, keyString='ref_datetime', _RC)
      if (has_ref_datetime) then
         ref_datetime = ESMF_HConfigAsString(child_hconfig, keyString='ref_datetime', _RC)
         ref_datetime = "'"//ref_datetime//"'"
      end if

      stats_list = ESMF_HConfigCreate(_RC)
      stats_mapl_section = create_stats_mapl_section(_RC)
      call ESMF_HConfigAdd(stats_hconfig, stats_mapl_section, addKeyString='mapl', _RC)
      frequency = ESMF_HConfigAsString(time_hconfig, keyString='frequency', _RC)
      var_list = ESMF_HConfigCreateAt(child_hconfig, keyString=VAR_LIST_KEY, _RC)
      iter_begin = ESMF_HConfigIterBegin(var_list,_RC)
      iter_end = ESMF_HConfigIterEnd(var_list,_RC)
      iter = iter_begin
      do while (ESMF_HConfigIterLoop(iter,iter_begin,iter_end,rc=status))
         _VERIFY(status)
         call parse_item(iter, short_name=short_name, name_in_comp=name_in_comp, _RC)
         stat_item = create_stats_entry(short_name, mode, frequency, ref_datetime, _RC)
         call ESMF_HConfigAdd(stats_list, stat_item, _RC)
         call MAPL_GridCompAddConnection(gridcomp, src_comp='stats_'//child_name, src_names=short_name, dst_comp=child_name, dst_names=name_in_comp, _RC)
      enddo
      call ESMF_HConfigAdd(stats_hconfig, stats_list, addKeyString='stats', _RC)
      child_spec = ChildSpec(user_setservices(statistics_setServices),hconfig=stats_hconfig)
      call MAPL_GridCompAddChild(gridcomp,'stats_'//child_name, child_spec, _RC)

      _RETURN(_SUCCESS)

   end subroutine add_stats_gc

   function create_stats_mapl_section(rc) result(stats_mapl_section)
      type(ESMF_HConfig) :: stats_mapl_section
      integer, optional, intent(out) :: rc

      integer :: status

      stats_mapl_section = ESMF_HConfigCreate(content='{misc: {checkpoint: {import: False, internal: False}, restart: {bootstrap: True, import: False, internal: True}}}', _RC)
      _RETURN(_SUCCESS)
   end function create_stats_mapl_section
      
   function create_stats_entry(name, action, period, ref_datetime, rc) result(stat_item)
       type(ESMF_HConfig) :: stat_item 
       ! Input arguments
       character(len=*), intent(in) :: name
       character(len=*), intent(in) :: action
       character(len=*), intent(in) :: period
       character(len=*), intent(in) :: ref_datetime
       
       integer, intent(out), optional :: rc
       integer :: status
       
       stat_item = ESMF_HConfigCreate(_RC)
           
           ! Add fields to this stat item
       call ESMF_HConfigAdd(stat_item, trim(name), AddKeyString="name", _RC)     
       call ESMF_HConfigAdd(stat_item, trim(action), AddkeyString="action", _RC)    
       call ESMF_HConfigAdd(stat_item, trim(period), AddKeyString="period", _RC)   
       call ESMF_HConfigAdd(stat_item, trim(ref_datetime), AddKeyString="ref_datetime", _RC)
           
       _RETURN(_SUCCESS)
       
   end function create_stats_entry

end module mapl3g_HistoryGridComp_private
