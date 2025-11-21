#include "MAPL.h"
module mapl3g_HistoryGridComp_private
   use mapl_ErrorHandlingMod
   use mapl_keywordenforcermod
   use esmf
   implicit none
   private

   public :: make_child_name
   public :: make_child_hconfig
   public :: get_subconfig

contains

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

end module mapl3g_HistoryGridComp_private
