#include "MAPL_Generic.h"
module mapl3g_HistoryCollectionGridComp_private

   use generic3g
   use mapl3g_VariableSpec
   use esmf
   use Mapl_ErrorHandling
   use mapl3g_geom_mgr

   implicit none
   private

   public :: make_geom, register_imports

   character(len=*), parameter :: VARIABLE_DELIMITER = '.'
   character(len=*), parameter :: DELIMITER_REPLACEMENT = '/'

contains

   function make_geom(hconfig, rc) result(geom)
      type(ESMF_Geom) :: geom
      type(ESMF_HConfig), intent(inout) :: hconfig
      integer, optional, intent(out) :: rc
      integer :: status
      type(GeomManager), pointer :: geom_mgr
      type(ESMF_HConfig) :: geom_hconfig
      type(MaplGeom) :: mapl_geom

      geom_mgr => get_geom_manager()

      geom_hconfig = ESMF_HConfigCreateAt(hconfig, keystring='geom', _RC)
      mapl_geom = geom_mgr%get_mapl_geom(geom_hconfig, _RC)
      geom = mapl_geom%get_geom()

      call ESMF_HConfigDestroy(geom_hconfig, _RC)
      _RETURN(_SUCCESS)
   end function make_geom

   subroutine register_imports(gridcomp, hconfig, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: VAR_LIST_KEY = 'var_list'
      type(ESMF_HConfigIter) :: iter, iter_begin, iter_end
      type(ESMF_HConfig) :: var_list
      character(len=:), allocatable :: item_name
      character(len=:), allocatable :: short_name
      type(VariableSpec) :: varspec
      integer :: status

      var_list = ESMF_HConfigCreateAt(hconfig, keystring=VAR_LIST_KEY, _RC)
      iter_begin = ESMF_HConfigIterBegin(var_list,_RC)
      iter_end = ESMF_HConfigIterEnd(var_list,_RC)
      iter = iter_begin

      do while (ESMF_HConfigIterLoop(iter,iter_begin,iter_end,rc=status))
         call parse_item(iter, item_name, short_name, _RC)
         varspec = VariableSpec(ESMF_STATEINTENT_IMPORT, short_name)
         call MAPL_AddSpec(gridcomp, varspec, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine register_imports

   subroutine parse_item(item, item_name, short_name, rc)
      type(ESMF_HConfigIter), intent(in) :: item 
      character(len=:), allocatable, intent(out) :: item_name
      character(len=:), allocatable, intent(out) :: short_name
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: EXPRESSION_KEY = 'expr'
      integer :: status
      logical :: asOK, isScalar, isMap
      type(ESMF_HConfig) :: value
      type(ESMF_HConfigIter) :: iter, iterBegin, iterEnd
      character(len=:), allocatable :: part_key, part_value

      isScalar = ESMF_HConfigIsScalarMapKey(item, _RC)
      _ASSERT(isScalar, 'Variable list item does not have a scalar name.')

      isMap = ESMF_HConfigIsMapMapVal(item, _RC)
      _ASSERT(isMap, 'Variable list item does not have a map value.')

      item_name = ESMF_HConfigAsStringMapKey(item, asOkay=asOK, _RC)
      _ASSERT(asOK, 'Name could not be processed as a String.')

      value = ESMF_HConfigCreateAtMapVal(item, _RC)
      short_name = ESMF_HConfigAsString(value, keyString=EXPRESSION_KEY, _RC)
      short_name = replace_delimiter(short_name, VARIABLE_DELIMITER, DELIMITER_REPLACEMENT)

      _RETURN(_SUCCESS)
   end subroutine parse_item

   function replace_delimiter(string, delimiter, replacement) result(replaced)
      character(len=:), allocatable :: replaced
      character(len=*), intent(in) :: string
      character(len=*), intent(in) :: delimiter
      character(len=*), intent(in) :: replacement
      integer :: i

      replaced = trim(string)
      i = index(replaced, delimiter)
      if(i > 0) replaced = replaced(:(i-1))// replacement // replaced((i+len(delimiter)):)

   end function replace_delimiter

end module mapl3g_HistoryCollectionGridComp_private
