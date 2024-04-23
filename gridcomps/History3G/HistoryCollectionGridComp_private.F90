#include "MAPL_Generic.h"
module mapl3g_HistoryCollectionGridComp_private

   use generic3g
   use mapl3g_VariableSpec
   use esmf
   use Mapl_ErrorHandling
   use gFTL2_StringVector, only: StringVector, StringVectorIterator
   use gFTL_StringVector, only: StringVectorV1 => StringVector, StringVectorIteratorV1 => StringVectorIterator
   use mapl3g_geom_mgr
   use MAPL_NewArthParserMod, only: parser_variables_in_expression

   implicit none
   private

   public :: make_geom, register_imports, create_output_bundle

   interface parse_item
      module procedure :: parse_item_expression
   end interface parse_item

   interface replace_delimiter
      module procedure :: replace_delimiter_expression
   end interface replace_delimiter

   interface convert_string_vector
      module procedure :: convert_string_vector_v2
   end interface convert_string_vector

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
      type(StringVector) :: variable_names
      integer :: status

      var_list = ESMF_HConfigCreateAt(hconfig, keystring=VAR_LIST_KEY, _RC)
      iter_begin = ESMF_HConfigIterBegin(var_list,_RC)
      iter_end = ESMF_HConfigIterEnd(var_list,_RC)
      iter = iter_begin

      do while (ESMF_HConfigIterLoop(iter,iter_begin,iter_end,rc=status))
         call parse_item(iter, item_name, variable_names, _RC)
         call add_spec(gridcomp, variable_names, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine register_imports

   subroutine add_spec(gridcomp, names, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(StringVector), intent(in) :: names
      integer, optional, intent(out) :: rc
      integer :: status
      type(StringVectorIterator) :: iter
      type(VariableSpec) :: varspec

      iter = names%begin()
      do while(iter /= names%end())
         varspec = VariableSpec(ESMF_STATEINTENT_IMPORT, iter%of())
         call MAPL_AddSpec(gridcomp, varspec, _RC)
         call iter%next()
      end do

      _RETURN(_SUCCESS)

   end subroutine add_spec

   subroutine parse_item_expression(item, item_name, short_names, rc)
      type(ESMF_HConfigIter), intent(in) :: item 
      character(len=:), allocatable, intent(out) :: item_name
      type(StringVector), intent(out) :: short_names
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: EXPRESSION_KEY = 'expr'
      integer :: status
      logical :: asOK, isScalar, isMap
      type(ESMF_HConfig) :: value
      type(ESMF_HConfigIter) :: iter, iterBegin, iterEnd
      character(len=:), allocatable :: expression
      type(StringVectorV1) :: v1svector

      isScalar = ESMF_HConfigIsScalarMapKey(item, _RC)
      _ASSERT(isScalar, 'Variable list item does not have a scalar name.')

      isMap = ESMF_HConfigIsMapMapVal(item, _RC)
      _ASSERT(isMap, 'Variable list item does not have a map value.')

      item_name = ESMF_HConfigAsStringMapKey(item, asOkay=asOK, _RC)
      _ASSERT(asOK, 'Name could not be processed as a String.')

      value = ESMF_HConfigCreateAtMapVal(item, _RC)
      expression = ESMF_HConfigAsString(value, keyString=EXPRESSION_KEY, _RC)
      expression = replace_delimiter(expression, VARIABLE_DELIMITER, DELIMITER_REPLACEMENT)
!      short_names = parser_variables_in_expression(expression, _RC) !wdb fixme Workaround until function returns gFTL2 StringVector
      v1svector = parser_variables_in_expression(expression, _RC)
      short_names = convert_string_vector(v1svector)

      _RETURN(_SUCCESS)
   end subroutine parse_item_expression

   function replace_delimiter_expression(string, delimiter, replacement) result(replaced)
      character(len=:), allocatable :: replaced
      character(len=*), intent(in) :: string
      character(len=*), intent(in) :: delimiter
      character(len=*), intent(in) :: replacement
      integer :: delwidth

      delwidth = len(delimiter)
      replaced = inner(string)

   contains

      recursive function inner(s_in) result(s_out)
         character(len=:), allocatable :: s_out
         character(len=*), intent(in) :: s_in
         integer :: i

         s_out = trim(s_in)
         i = index(s_out, delimiter)
         if(i == 0) return
         s_out = s_out(:(i-1)) // replacement // inner(s_in((i+delwidth):))

      end function inner

   end function replace_delimiter_expression

   subroutine parse_item_simple(item, item_name, short_name, rc)
      type(ESMF_HConfigIter), intent(in) :: item 
      character(len=:), allocatable, intent(out) :: item_name
      character(len=:), allocatable, intent(out) :: short_name
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: EXPRESSION_KEY = 'expr'
      integer :: status
      logical :: asOK, isScalar, isMap
      type(ESMF_HConfig) :: value
      type(ESMF_HConfigIter) :: iter, iterBegin, iterEnd

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
   end subroutine parse_item_simple

   function replace_delimiter_simple(string, delimiter, replacement) result(replaced)
      character(len=:), allocatable :: replaced
      character(len=*), intent(in) :: string
      character(len=*), intent(in) :: delimiter
      character(len=*), intent(in) :: replacement
      integer :: i

      replaced = trim(string)
      i = index(replaced, delimiter)
      if(i > 0) replaced = replaced(:(i-1))// replacement // replaced((i+len(delimiter)):)

   end function replace_delimiter_simple

   function convert_string_vector_v2(svector1) result(svector)
      type(StringVector) :: svector
      type(StringVectorV1) :: svector1
      type(StringVectorIteratorV1) :: iter

      iter = svector1%begin()
      do while(iter /= svector1%end())
        call svector%push_back(iter%of()) 
     end do

   end function convert_string_vector_v2

   function create_output_bundle(hconfig, import_state, rc) result(bundle)
      type(ESMF_FieldBundle) :: bundle
      type(ESMF_HConfig), intent(in) :: hconfig
      type(ESMF_State), intent(in) :: import_state
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: VAR_LIST_KEY = 'var_list'
      type(ESMF_HConfigIter) :: iter, iter_begin, iter_end
      type(ESMF_HConfig) :: var_list
      character(len=:), allocatable :: alias, short_name
      type(ESMF_Field) :: field, new_field
      type(ESMF_Info) :: info, new_info
      type(ESMF_StateItem_Flag) :: itemType

      var_list = ESMF_HConfigCreateAt(hconfig, keystring=VAR_LIST_KEY, _RC)
      iter_begin = ESMF_HConfigIterBegin(var_list,_RC)
      iter_end = ESMF_HConfigIterEnd(var_list,_RC)
      iter = iter_begin

      bundle = ESMF_FieldBundleCreate(_RC)
      do while (ESMF_HConfigIterLoop(iter,iter_begin,iter_end,rc=status))
         call parse_item(iter, alias, short_name, _RC)
         call ESMF_StateGet(import_state, short_name, field, _RC)
         new_field = ESMF_FieldCreate(field, dataCopyFlag=ESMF_DATACOPY_REFERENCE, name=alias,  _RC)
         call ESMF_InfoGetFromHost(field, info, _RC)
         call ESMF_InfoGetFromHost(new_field, new_info, _RC)
         call ESMF_InfoSet(new_info, key="", value=info, _RC)
         call ESMF_FieldBundleAdd(bundle, [new_field], _RC)
      end do

      _RETURN(_SUCCESS)
   end function create_output_bundle

end module mapl3g_HistoryCollectionGridComp_private
