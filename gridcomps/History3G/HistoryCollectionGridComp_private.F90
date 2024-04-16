#include "MAPL_Generic.h"
module mapl3g_HistoryCollectionGridComp_private

   use generic3g
   use mapl3g_VariableSpec
   use esmf
   use Mapl_ErrorHandling
   use mapl3g_geom_mgr
   use gftl2_StringStringMap

   implicit none
   private

   public :: make_geom, make_import_state

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

   subroutine make_import_state(gridcomp, hconfig, rc) !wdb fixme change name to register_imports
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: SIMPLE_EXPRESSION = 'expr'
      type(ESMF_HConfigIter) :: iter, iter_begin, iter_end
      type(ESMF_HConfig) :: var_list
      character(len=:), allocatable :: item_name
      type(StringStringMap) :: item_map
      character(len=:), allocatable :: expression
      type(VariableSpec) :: varspec
      integer :: status, i
      character(len=:), allocatable :: short_names(:)

      var_list = ESMF_HConfigCreateAt(hconfig, keystring='var_list', _RC)
      iter_begin = ESMF_HConfigIterBegin(var_list,_RC)
      iter_begin = ESMF_HConfigIterEnd(var_list,_RC)
      iter = iter_begin

      do while (ESMF_HConfigIterLoop(iter,iter_begin,iter_end,rc=status))

         call parse_item(iter, item_name, item_map, _RC)
         _ASSERT(item_map%count(SIMPLE_EXPRESSION) == 1, 'Expression for item "' // item_name // '" not found.')
         expression = item_map%at(SIMPLE_EXPRESSION)
         call get_short_names(expression, short_names)
         do i = 1, size(short_names)
            varspec = VariableSpec(ESMF_STATEINTENT_IMPORT, short_names(i))
            call MAPL_AddSpec(gridcomp, varspec, _RC)
         end do
      end do

      _RETURN(_SUCCESS)
   end subroutine make_import_state

   subroutine parse_item(item, name, parts, rc)
      type(ESMF_HConfigIter), intent(in) :: item 
      character(len=:), allocatable, intent(out) :: name
      type(StringStringMap), intent(out) :: parts
!      character(len=:), allocatable, intent(out) :: expression_type
!      character(len=:), allocatable, intent(out) :: expression
      integer, optional, intent(out) :: rc
      integer :: status
      logical :: asOK, isScalar, isMap
      type(ESMF_HConfig) :: value
      type(ESMF_HConfigIter) :: iter, iterBegin, iterEnd
      character(len=:), allocatable :: part_key, part_value

      isScalar = ESMF_HConfigIsScalarMapKey(item, _RC)
      _ASSERT(isScalar, 'Variable list item does not have scalar name.')

      isMap = ESMF_HConfigIsMapMapVal(item, _RC)
      _ASSERT(isMap, 'Variable list item does not have a map value.')

      name = ESMF_HConfigAsStringMapKey(item, asOkay=asOK, _RC)
      _ASSERT(asOK, 'Name could not be processed as a String.')

      value = ESMF_HConfigCreateAtMapVal(item, _RC)
      do while (ESMF_HConfigIterLoop(iter, iterBegin, iterEnd, rc=rc))
         isScalar = ESMF_HConfigIsScalarMapKey(iter, _RC)
         _ASSERT(isScalar, 'Map key is not scalar.')

         isScalar = ESMF_HConfigIsScalarMapVal(iter, _RC)
         _ASSERT(isScalar, 'Map value is not scalar.')

         part_key = ESMF_HConfigAsStringMapKey(iter, _RC)
         part_value = ESMF_HConfigAsStringMapVal(iter, _RC)
         call parts%insert(part_key, part_value)

      end do
!      call process_value_string(value, expression_type, expression, _RC)
!      expression_type = ESMF_HConfigAsStringMapKey(value, asOkay=asOK, _RC)
!      _ASSERT(asOK, 'Expression type could not be processed as a String.')

!      expression = ESMF_HConfigAsStringMapVal(value, asOkay=asOK, _RC)
!      _ASSERT(asOK, 'Expression could not be processed as a String.')
      
      _RETURN(_SUCCESS)
   end subroutine parse_item

   subroutine process_value_string(string, label, expression, rc)
      character(len=*), intent(in) :: string
      character(len=:), allocatable, intent(out) :: label
      character(len=:), allocatable, intent(out) :: expression
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: OPENING = '{'
      character(len=*), parameter :: CLOSING = '}'
      character(len=*), parameter :: DELIMITER = ':'
      integer :: status, n, i

      expression = trim(adjustl(string))
      n = len(expression)
      _ASSERT(expression(1:1) == OPENING, 'String should begin with "' // OPENING // '".')
      _ASSERT(expression(n:n) == CLOSING, 'String should end with "' // CLOSING // '".')
      i = index(expression, DELIMITER)
      _ASSERT(i > 0, 'Delimiter "' // DELIMITER // '" was not found.')
      label = expression(:(i-1))
      expression = expression((i+len(DELIMITER)):)

      _RETURN(_SUCCESS)

   end subroutine process_value_string

   subroutine get_short_names(expression, names)
      character(len=*), intent(in) :: expression
      character(len=:), allocatable :: names(:)
      character(len=*), parameter :: DELIMITER = '.'
      character(len=*), parameter :: REPLACEMENT = '/'
      character(len=:), allocatable :: short_name
      integer :: i

      short_name = trim(expression)
      i = index(short_name, DELIMITER)
      if(i > 0) short_name = short_name(:(i-1))// REPLACEMENT // short_name((i+len(DELIMITER)):)
      names = [short_name]

   end subroutine get_short_names

end module mapl3g_HistoryCollectionGridComp_private
