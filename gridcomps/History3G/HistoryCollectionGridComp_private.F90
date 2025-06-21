#include "MAPL_Generic.h"
module mapl3g_HistoryCollectionGridComp_private
   use mapl3
   use esmf
   use gFTL2_StringVector
   use gFTL2_StringSet

   implicit none(type,external)
   private

   public :: make_geom
   public :: register_imports
   public :: create_output_bundle
   public :: set_start_stop_time
   public :: get_current_time_index
   ! These are public for testing.
   public :: parse_item_common
   public :: replace_delimiter
   public :: get_expression_variables

   type :: HistoryOptions
      character(len=:), allocatable :: units
      type(ESMF_TypeKind_Flag), allocatable :: typekind
      type(ESMF_TimeInterval), allocatable :: timeStep
      type(ESMF_TimeInterval), allocatable :: runTime_offset
      character(len=:), allocatable :: accumulation_type
   end type HistoryOptions

   interface parse_item
      module procedure :: parse_item_expression
      module procedure :: parse_item_simple
   end interface parse_item

   interface parse_options
      module procedure :: parse_options_hconfig
      module procedure :: parse_options_iter
   end interface parse_options

   character(len=*), parameter :: VAR_LIST_KEY = 'var_list'
   character(len=*), parameter :: KEY_TIMESTEP = 'frequency'
   character(len=*), parameter :: KEY_OFFSET = 'ref_time'
   character(len=*), parameter :: KEY_ACCUMULATION_TYPE = 'mode'
   character(len=*), parameter :: KEY_TIME_SPEC = 'time_spec'
   character(len=*), parameter :: KEY_TYPEKIND = 'typekind'
   character(len=*), parameter :: KEY_UNITS = 'units'

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

   function create_output_bundle(hconfig, import_state, rc) result(bundle)
      type(ESMF_FieldBundle) :: bundle
      type(ESMF_HConfig), intent(in) :: hconfig
      type(ESMF_State), intent(in) :: import_state
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_HConfigIter) :: iter, iter_begin, iter_end
      type(ESMF_HConfig) :: var_list
      character(len=:), allocatable :: alias, short_name
      type(ESMF_Field) :: field, new_field
      type(ESMF_Info) :: info, new_info

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

   function set_start_stop_time(clock, hconfig, rc) result(start_stop_time)
      type(ESMF_Time) :: start_stop_time(2)
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, intent(out), optional :: rc

      integer :: status
      logical :: has_start, has_stop
      character(len=:), allocatable :: time_string
      type(ESMF_HConfig) :: time_hconfig

      time_hconfig = ESMF_HConfigCreateAt(hconfig, keyString='time_spec', _RC)
      call ESMF_ClockGet(clock, startTime=start_stop_time(1), stopTime=start_stop_time(2), _RC)
      has_start = ESMF_HConfigIsDefined(time_hconfig, keyString='start', _RC)
      has_stop = ESMF_HConfigIsDefined(time_hconfig, keyString='stop', _RC)
      if (has_start) then
         time_string = ESMF_HConfigAsString(time_hconfig, keyString='start', _RC)
         call ESMF_TimeSet(start_stop_time(1), timeString=time_string, _RC)
      end if
      if (has_stop) then
         time_string = ESMF_HConfigAsString(time_hconfig, keyString='stop', _RC)
         call ESMF_TimeSet(start_stop_time(2), timeString=time_string, _RC)
      end if
      _RETURN(_SUCCESS)
   end function set_start_stop_time

   subroutine parse_item_expression(item, item_name, var_names, rc)
      type(ESMF_HConfigIter), intent(in) :: item
      character(len=:), allocatable, intent(out) :: item_name
      type(StringVector), intent(out) :: var_names
      integer, optional, intent(out) :: rc
      character(len=:), allocatable :: expression
      integer :: status

      call parse_item_common(item, item_name, expression, _RC)
      var_names = get_expression_variables(expression, _RC)

      _RETURN(_SUCCESS)
   end subroutine parse_item_expression

   subroutine parse_item_simple(item, item_name, var_name, rc)
      type(ESMF_HConfigIter), intent(in) :: item
      character(len=:), allocatable, intent(out) :: item_name
      character(len=:), allocatable, intent(out) :: var_name
      integer, optional, intent(out) :: rc
      character(len=:), allocatable :: expression
      integer :: status

      call parse_item_common(item, item_name, expression, _RC)
      var_name = replace_delimiter(expression)

      _RETURN(_SUCCESS)
   end subroutine parse_item_simple

   subroutine parse_item_common(item, item_name, expression, rc)
      type(ESMF_HConfigIter), intent(in) :: item
      character(len=:), allocatable, intent(out) :: item_name
      character(len=:), allocatable, intent(out) :: expression
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: EXPRESSION_KEY = 'expr'
      integer :: status
      logical :: asOK, isScalar, isMap
      type(ESMF_HConfig) :: value

      isScalar = ESMF_HConfigIsScalarMapKey(item, _RC)
      _ASSERT(isScalar, 'Variable list item does not have a scalar name.')
      isMap = ESMF_HConfigIsMapMapVal(item, _RC)
      _ASSERT(isMap, 'Variable list item does not have a map value.')

      item_name = ESMF_HConfigAsStringMapKey(item, asOkay=asOK, _RC)
      _ASSERT(asOK, 'Item name could not be processed as a String.')

      value = ESMF_HConfigCreateAtMapVal(item, _RC)
      expression = ESMF_HConfigAsString(value, keyString=EXPRESSION_KEY, _RC)

      _RETURN(_SUCCESS)
   end subroutine parse_item_common

   function replace_delimiter(string, delimiter, replacement) result(replaced)
      character(len=:), allocatable :: replaced
      character(len=*), intent(in) :: string
      character(len=*), optional, intent(in) :: delimiter
      character(len=*), optional, intent(in) :: replacement
      character(len=:), allocatable :: del, rep
      integer :: i

      replaced = string
      if(len(string) == 0) return

      del = '.'
      if(present(delimiter)) del = delimiter
      if(len(del) == 0) return

      rep = '/'
      if(present(replacement)) rep = replacement
      if(len(rep) == 0) return

      i = index(replaced, del)
      if(i > 0) replaced = replaced(:(i-1))// rep // replaced((i+len(del)):)

   end function replace_delimiter

   function get_expression_variables(expression, rc) result(variables)
      type(StringVector) :: variables
      character(len=*), intent(in) :: expression
      integer, optional, intent(out) :: rc
      integer :: status
      type(StringVector) :: raw_vars
      type(StringVectorIterator) :: iter

      raw_vars = MAPL_ParserVariablesInExpression(expression, _RC)
      iter = raw_vars%begin()
      do while(iter /= raw_vars%end())
        call variables%push_back(replace_delimiter(iter%of()))
        call iter%next()
      end do

      _RETURN(_SUCCESS)
   end function get_expression_variables

   function get_current_time_index(initial_time, current_time, frequency) result(time_index)
      integer :: time_index
      type(ESMF_Time), intent(in) :: initial_time
      type(ESMF_Time), intent(in) :: current_time
      type(ESMF_TimeInterval), intent(in) :: frequency

      type(ESMF_Time) :: temp_time
      time_index = 0
      temp_time = initial_time
      do while( temp_time <= current_time)
         temp_time = temp_time + frequency
         time_index = time_index + 1
      enddo
   end function get_current_time_index

   subroutine register_imports(gridcomp, hconfig, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc
      type(ESMF_HConfigIter) :: iter, iter_begin, iter_end
      type(ESMF_HConfig) :: var_list
      character(len=:), allocatable :: item_name
      type(StringVector) :: variable_names
      type(HistoryOptions) :: options
      integer :: status

      ! Get Options for collection
      call parse_options(hconfig, options, _RC)

      ! Get variable list
      var_list = ESMF_HConfigCreateAt(hconfig, keystring=VAR_LIST_KEY, rc=status)
      if(status==ESMF_RC_NOT_FOUND) then
         _FAIL(VAR_LIST_KEY // ' was not found.')
      end if
      _VERIFY(status)

      iter_begin = ESMF_HConfigIterBegin(var_list,_RC)
      iter_end = ESMF_HConfigIterEnd(var_list,_RC)
      iter = iter_begin

      ! Add VariableSpec objects
      do while (ESMF_HConfigIterLoop(iter,iter_begin,iter_end,rc=status))
         _VERIFY(status)
         call parse_item(iter, item_name, variable_names, _RC)
         call parse_options(iter, options, _RC)
         call add_var_specs(gridcomp, variable_names, options, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine register_imports

   subroutine add_var_specs(gridcomp, variable_names, opts, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(StringVector), intent(in) :: variable_names
      type(HistoryOptions), intent(in) :: opts
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=:), allocatable :: item_name
      type(StringVectorIterator) :: ftn_iter, ftn_end
      type(VariableSpec) :: varspec
      character(len=:), allocatable :: short_name

      ftn_end = variable_names%ftn_end()
      ftn_iter = variable_names%ftn_begin()
      do while (ftn_iter /= ftn_end)
         call ftn_iter%next()
         short_name = ftn_iter%of()
         varspec = make_VariableSpec(ESMF_STATEINTENT_IMPORT, short_name, &
              units=opts%units, typekind=opts%typekind, &
              accumulation_type=opts%accumulation_type, timestep = opts%timestep, offset=opts%runTime_offset, &
              vertical_stagger=VERTICAL_STAGGER_MIRROR, &
              _RC)
         call MAPL_GridCompAddVarSpec(gridcomp, varspec, _RC)
      end do
      _RETURN(_SUCCESS)

   end subroutine add_var_specs

   subroutine parse_options_hconfig(hconfig, options, rc)
      type(ESMF_HConfig), intent(in) :: hconfig
      class(HistoryOptions), intent(inout) :: options
      integer, optional, intent(out) :: rc
      integer :: status

      call parse_frequency_aspect_options(hconfig, options, _RC)
      call parse_units_aspect_options(hconfig, options, _RC)
      call parse_typekind_aspect_options(hconfig, options, _RC)
      _RETURN(_SUCCESS)

   end subroutine parse_options_hconfig

   subroutine parse_options_iter(iter, options, rc)
      type(ESMF_HConfigIter), intent(in) :: iter
      class(HistoryOptions), intent(inout) :: options
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_HConfig) :: hconfig

      hconfig = ESMF_HConfigCreateAtMapVal(iter, _RC)
      call parse_options(hconfig, options, _RC)
      call ESMF_HConfigDestroy(hconfig)

   end subroutine parse_options_iter

   subroutine parse_frequency_aspect_options(hconfig, options, rc)
      type(ESMF_HConfig), intent(in) :: hconfig
      class(HistoryOptions), intent(inout) :: options
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_HConfig) :: time_iter
      logical :: hasKey
      character(len=:), allocatable :: mapVal
      type(ESMF_TimeInterval) :: timeStep, offset

      hasKey = ESMF_HConfigIsDefined(hconfig, keyString=KEY_TIME_SPEC, _RC)
      _RETURN_UNLESS(hasKey)
      time_iter = ESMF_HConfigCreateAt(hconfig, keyString=KEY_TIME_SPEC, _RC)

      hasKey = ESMF_HConfigIsDefined(time_iter, keyString=KEY_ACCUMULATION_TYPE, _RC)
      if(hasKey) then
         options%accumulation_type = ESMF_HConfigAsString(time_iter, keyString=KEY_ACCUMULATION_TYPE, _RC)
      end if

      hasKey = ESMF_HConfigIsDefined(time_iter, keyString=KEY_TIMESTEP, _RC)
      if(hasKey) then
         mapVal = ESMF_HConfigAsString(time_iter, keyString=KEY_TIMESTEP, _RC)
         call ESMF_TimeIntervalSet(timeStep, timeIntervalString=mapVal, _RC)
         options%timeStep = timeStep
      end if

      hasKey = ESMF_HConfigIsDefined(time_iter, keyString=KEY_OFFSET, _RC)
      if(hasKey) then
         mapVal = ESMF_HConfigAsString(time_iter, keyString=KEY_OFFSET, _RC)
         call ESMF_TimeIntervalSet(offset, timeIntervalString=mapVal, _RC)
         options%runTime_offset = offset
      end if

      call ESMF_HConfigDestroy(time_iter, _RC)
      _RETURN(_SUCCESS)

   end subroutine parse_frequency_aspect_options

   subroutine parse_units_aspect_options(hconfig, options, rc)
      type(ESMF_HConfig), intent(in) :: hconfig
      class(HistoryOptions), intent(inout) :: options
      integer, optional, intent(out) :: rc
      integer :: status
      logical :: hasKey
      character(len=:), allocatable :: mapVal

      hasKey = ESMF_HConfigIsDefined(hconfig, keyString=KEY_UNITS, _RC)
      _RETURN_UNLESS(hasKey)
      mapVal = ESMF_HConfigAsString(hconfig, keyString=KEY_UNITS, _RC)
      options%units = mapVal
      _RETURN(_SUCCESS)

   end subroutine parse_units_aspect_options

   subroutine parse_typekind_aspect_options(hconfig, options, rc)
      type(ESMF_HConfig), intent(in) :: hconfig
      class(HistoryOptions), intent(inout) :: options
      integer, optional, intent(out) :: rc
      integer :: status
      logical :: hasKey
      character(len=:), allocatable :: mapVal
      logical :: found
      type(ESMF_TypeKind_Flag) :: tk

      hasKey = ESMF_HConfigIsDefined(hconfig, keyString=KEY_TYPEKIND, _RC)
      _RETURN_UNLESS(hasKey)
      mapVal = ESMF_HConfigAsString(hconfig, keyString=KEY_TYPEKIND, _RC)
      tk = get_typekind(mapVal, found, _RC)
      _ASSERT(found, 'Unknown typekind')
      options%typekind = tk
      _RETURN(_SUCCESS)

   end subroutine parse_typekind_aspect_options

   function get_typekind(tk_string, found, rc) result(typekind)
      type(ESMF_TypeKind_Flag) :: typekind
      character(len=*), intent(in) :: tk_string
      logical, optional, intent(out) :: found
      integer, optional, intent(out) :: rc
      integer :: status
      integer, parameter :: L = 10
      integer, parameter :: ML = 2
      character(len=L), parameter :: CODES(*) = [character(len=L) :: &
         & 'I4', 'I8', 'R4', 'R8', 'LOGICAL', 'CHARACTER']
      type(ESMF_TypeKind_Flag), parameter :: TK(size(CODES)) = [ &
         & ESMF_TYPEKIND_I4, ESMF_TYPEKIND_I8, ESMF_TYPEKIND_R4, &
         & ESMF_TYPEKIND_R8, ESMF_TYPEKIND_LOGICAL, ESMF_TYPEKIND_CHARACTER]
      integer :: i
      logical :: tk_found

      _ASSERT(len(tk_string) >= ML, 'tk_string is too short.')
      do i=1, size(CODES)
         tk_found = index(tk_string, trim(CODES(i))) > 0
         if(tk_found) typekind = TK(i)
         exit
      end do

      if(present(found)) then
         found = tk_found
         _RETURN(_SUCCESS)
      end if

      _ASSERT(tk_found, 'Typekind was not found.')

   end function get_typekind

end module mapl3g_HistoryCollectionGridComp_private
