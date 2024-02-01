#include "MAPL_Generic.h"
module mapl3g_HistoryGridComp_private
   use generic3g
   use mapl_ErrorHandlingMod
   use mapl_keywordenforcermod
   use esmf
   use pflogger
!# use mapl3g_HistoryCollectionGridComp, only: collection_setServices => setServices
   implicit none
   private

   public :: setServices
   public :: init
   public :: run
   public :: make_child_name
   public :: make_child_hconfig
   public :: fill_entry_from_dict
   public :: get_subconfig

   contains

   subroutine setServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      type(ESMF_HConfig) :: hconfig, collections_config, child_hconfig
      character(len=:), allocatable :: child_name, collection_name
      type(ESMF_HConfigIter) :: iter, iter_begin, iter_end
      logical :: has_active_collections
      character(*), parameter :: PRIVATE_STATE = "HistoryGridComp"
      class(logger), pointer :: lgr
      integer :: num_collections, status

      ! Set entry points
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init, phase_name="GENERIC::INIT_USER")
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name='run', _RC)

      ! Attach private state
!#    _SET_NAMED_PRIVATE_STATE(gridcomp, HistoryGridComp, PRIVATE_STATE, history_gridcomp)

      ! Determine collections
      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)

      has_active_collections = ESMF_HConfigIsDefined(hconfig, keyString='active_collections', _RC)
      if (.not. has_active_collections) then
         call MAPL_GridCompGet(gridcomp,logger=lgr, _RC)
         call lgr%warning("no active collection specified in History")
         _RETURN(_SUCCESS)
      end if

      collections_config = ESMF_HConfigCreateAt(hconfig, keystring='active_collections', _RC)
      num_collections = ESMF_HConfigGetSize(collections_config, _RC)
      _RETURN_UNLESS(num_collections > 0)

      iter_begin = ESMF_HConfigIterBegin(collections_config,_RC)
      iter_end = ESMF_HConfigIterEnd(collections_config, _RC)
      iter = iter_begin

      do while (ESMF_HConfigIterLoop(iter, iter_begin, iter_end, rc=status))
         _VERIFY(status)

         collection_name = ESMF_HConfigAsStringMapKey(iter, _RC)
         child_hconfig = make_child_hconfig(hconfig, collection_name)
         child_name = make_child_name(collection_name, _RC)
!#       call MAPL_AddChild(gridcomp, child_name, collection_setServices, child_hconfig, _RC)
         call ESMF_HConfigDestroy(child_hconfig, _RC)
         
      end do
      
      _RETURN(_SUCCESS)
   end subroutine setServices

   subroutine init(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, intent(out)  :: rc

      integer :: status

      ! To Do:
      ! - determine run frequencey and offset (save as alarm)

      
      _RETURN(_SUCCESS)
   end subroutine init


   subroutine run(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, intent(out)  :: rc

      integer :: status

      call MAPL_RunChildren(gridcomp, phase_name='run', _RC)

      _RETURN(_SUCCESS)
   end subroutine run

   ! Collection names are permitted to include period ('.') characters, but gridcomps
   ! are not. (Because we use "." as dive-down character in other syntax.)  So here
   ! we encode the collection name by replacing "." with "\.".
   function make_child_name(collection_name, rc) result(child_name)
      character(len=:), allocatable :: child_name
      character(len=*), intent(in) :: collection_name
      integer, optional, intent(out) :: rc

      integer :: status
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
      type(ESMF_HConfig) :: collections_hconfig, collection_hconfig

      collections_hconfig = get_subconfig(hconfig, 'collections', _RC)
      collection_hconfig = get_subconfig(collections_hconfig, collection_name, _RC)
      call ESMF_HConfigDestroy(collections_hconfig, _RC)

      call fill_entry_from_dict(dest=collection_hconfig, dest_key='geom', src=hconfig, src_key='geoms', _RC)

      child_hconfig = collection_hconfig
      call ESMF_HConfigAdd(child_hconfig, content=collection_name, addKeyString='collection_name', _RC)

      _RETURN(_SUCCESS)
   end function make_child_hconfig

   subroutine fill_entry_from_dict(unusable, dest, dest_key, src, src_key, rc)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Hconfig), intent(inout) :: dest
      character(*), intent(in) :: dest_key
      type(ESMF_HConfig), intent(in) :: src
      character(*), intent(in) :: src_key
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: entry_name
      type(ESMF_Hconfig) :: entries_hconfig, entry_hconfig

      entries_hconfig = get_subconfig(src, keyString=src_key, _RC)
      entry_name = ESMF_HConfigAsString(dest, keystring=dest_key, _RC)
      entry_hconfig = get_subconfig(entries_hconfig, keyString=entry_name, _RC)

      call ESMF_HConfigRemove(dest, keyString=dest_key, _RC)
      call ESMF_HConfigAdd(dest, content=entry_hconfig, addKeyString=dest_key, _RC)

      call ESMF_HConfigDestroy(entry_hconfig, _RC)
      call ESMF_HConfigDestroy(entries_hconfig, _RC)

      _RETURN(_SUCCESS)
   end subroutine fill_entry_from_dict

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

end module mapl3g_HistoryGridComp_private
