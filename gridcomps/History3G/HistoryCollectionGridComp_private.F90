#include "MAPL_Generic.h"
module mapl3g_HistoryCollectionGridComp_private

   use generic3g
   use mapl3g_VariableSpec
   use esmf
   use Mapl_ErrorHandling
   use gFTL2_StringVector
   use mapl3g_geom_mgr
   use MAPL_NewArthParserMod, only: parser_variables_in_expression
   use MAPL_TimeStringConversion
   use MAPL_BaseMod, only: MAPL_UnpackTime
   use mapl3g_UngriddedDims
   use gFTL2_StringSet

   implicit none
   private

   public :: make_geom
   public :: register_imports
   public :: create_output_bundle
   public :: create_output_alarm
   public :: set_start_stop_time
   public :: get_current_time_index
   ! These are public for testing.
   public :: parse_item_common
   public :: replace_delimiter
   public :: get_expression_variables

   interface parse_item
      module procedure :: parse_item_expression
      module procedure :: parse_item_simple
   end interface parse_item

   character(len=*), parameter :: VAR_LIST_KEY = 'var_list'

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
      type(ESMF_HConfigIter) :: iter, iter_begin, iter_end
      type(ESMF_HConfig) :: var_list
      character(len=:), allocatable :: item_name
      type(StringVector) :: variable_names
      integer :: status

      var_list = ESMF_HConfigCreateAt(hconfig, keystring=VAR_LIST_KEY, rc=status)
      if(status==ESMF_RC_NOT_FOUND) then
         _FAIL(VAR_LIST_KEY // ' was not found.')
      end if
      _VERIFY(status)

      iter_begin = ESMF_HConfigIterBegin(var_list,_RC)
      iter_end = ESMF_HConfigIterEnd(var_list,_RC)
      iter = iter_begin
      do while (ESMF_HConfigIterLoop(iter,iter_begin,iter_end,rc=status))
         _VERIFY(status)
         call parse_item(iter, item_name, variable_names, _RC)
         call add_specs(gridcomp, variable_names, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine register_imports

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

   subroutine create_output_alarm(clock, hconfig, comp_name, rc)
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_HConfig), intent(in) :: hconfig
      character(len=*), intent(in) :: comp_name
      integer, intent(out), optional :: rc

      type(ESMF_Alarm) :: alarm
      integer :: status
      type(ESMF_HConfig) :: time_hconfig
      type(ESMF_TimeInterval) :: time_interval
      character(len=:), allocatable :: iso_time
      type(ESMF_Time) :: first_ring_time, currTime, startTime
      integer :: int_time, yy, mm, dd, m, h, s
      logical :: has_ref_time, has_frequency

      call ESMF_ClockGet(clock, currTime=currTime, timeStep=time_interval, startTime = startTime, _RC)

      time_hconfig = ESMF_HConfigCreateAt(hconfig, keyString='time_spec', _RC)

      has_frequency = ESMF_HConfigIsDefined(time_hconfig, keyString='frequency', _RC)
      if (has_frequency) then
         time_interval = hconfig_to_esmf_timeinterval(time_hconfig, 'frequency', _RC)
      end if

      int_time = 0
      has_ref_time = ESMF_HConfigIsDefined(time_hconfig, keyString='ref_time', _RC)
      if (has_ref_time) then
         iso_time = ESMF_HConfigAsString(time_hconfig, keyString='ref_time', _RC)
         int_time = string_to_integer_time(iso_time, _RC)
      end if

      call MAPL_UnpackTime(int_time, h, m, s)
      call ESMF_TimeGet(currTime, yy=yy, mm=mm, dd=dd, _RC)
      call ESMF_TimeSet(first_ring_time, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, _RC)

      ! These 2 lines are borrowed from old History. Unforunately until ESMF alarms
      ! get fixed kluges like this are neccessary so alarms will acutally ring
      if (first_ring_time == startTime) first_ring_time = first_ring_time + time_interval
      if (first_ring_time < currTime) &
           first_ring_time = first_ring_time +(INT((currTime - first_ring_time)/time_interval)+1)*time_interval

      alarm = ESMF_AlarmCreate(clock=clock, RingInterval=time_interval, RingTime=first_ring_time, sticky=.false., name=comp_name//"_write_alarm",  _RC)

      _RETURN(_SUCCESS)
   end subroutine create_output_alarm

   function set_start_stop_time(clock, hconfig, rc) result(start_stop_time)
      type(ESMF_Time) :: start_stop_time(2)
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, intent(out), optional :: rc

      integer :: status
      logical :: has_start, has_stop, has_timespec
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

   subroutine add_specs(gridcomp, names, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(StringVector), intent(in) :: names
      integer, optional, intent(out) :: rc
      integer :: status
      type(StringVectorIterator) :: ftn_iter, ftn_end
      type(VariableSpec) :: varspec
      character(len=:), allocatable :: short_name

      ftn_end = names%ftn_end()
      ftn_iter = names%ftn_begin()
      do while (ftn_iter /= ftn_end)
         call ftn_iter%next()
         short_name = ftn_iter%of()
         varspec = make_VariableSpec(ESMF_STATEINTENT_IMPORT, short_name, vertical_dim_spec=VERTICAL_DIM_MIRROR, _RC)
         call MAPL_AddSpec(gridcomp, varspec, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine add_specs

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

      raw_vars = parser_variables_in_expression(expression, _RC)
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

end module mapl3g_HistoryCollectionGridComp_private
