#include "MAPL.h"

!> ProfileReporter - Generate formatted profiling reports
!!
!! The ProfileReporter can be configured in two ways:
!!
!! 1. Programmatic configuration (default):
!!    ```fortran
!!    reporter = ProfileReporter(empty)
!!    call reporter%add_column(NameColumn(20))
!!    call reporter%add_column(FormattedTextColumn('cycles', '(i8.0)', 8, NumCyclesColumn()))
!!    ```
!!
!! 2. YAML configuration (recommended for flexibility):
!!    ```fortran
!!    use esmf, only: ESMF_HConfig, ESMF_HConfigCreate
!!    config = ESMF_HConfigCreate(content='{ &
!!       &columns: [ &
!!       &  {type: name, width: 20}, &
!!       &  {type: num_cycles}, &
!!       &  {type: multi, name: "Inclusive", separator: "=", &
!!       &   columns: [{type: inclusive}, {type: percentage_inclusive}]} &
!!       &]}')
!!    reporter = ProfileReporter(empty, config)
!!    ```
!!
!! Available column types:
!!   - name: Timer names with indentation
!!   - num_cycles: Number of times each timer was called
!!   - inclusive: Total time including children
!!   - exclusive: Time excluding children  
!!   - percentage_inclusive: Percentage of total inclusive time
!!   - percentage_exclusive: Percentage of total exclusive time
!!   - std_dev: Standard deviation of cycle times
!!   - min_cycle, max_cycle, mean_cycle: Cycle statistics
!!   - separator: Column separator (e.g., '|')
!!   - multi: Group columns together with shared header (supports nesting)
!!
!! See profiler/profiler_report_config.yaml for a complete configuration example.
module MAPL_ProfileReporter
   use MAPL_AbstractMeterNode
   use MAPL_AbstractColumn
   use MAPL_TextColumn
   use MAPL_SeparatorColumn
   use MAPL_TextColumnVector
   use MAPL_MultiColumn
   use MAPL_BaseProfiler
   use gFTL2_StringVector
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf, only: ESMF_HConfig
   use esmf, only: ESMF_HConfigAsString, ESMF_HConfigAsI4
   use esmf, only: ESMF_HConfigIsDefined, ESMF_HConfigIsSequence, ESMF_HConfigGetSize
   use esmf, only: ESMF_HConfigCreateAt
   implicit none
   private

   public :: ProfileReporter

   type, extends(MultiColumn) :: ProfileReporter
      private
   contains
      procedure :: generate_report_profiler
      generic :: generate_report => generate_report_profiler
   end type ProfileReporter


   interface ProfileReporter
      module procedure :: new_ProfileReporter
      module procedure :: new_ProfileReporter_config
   end interface ProfileReporter

   
contains

   function new_ProfileReporter(header) result(reporter)
      type(ProfileReporter) :: reporter
      character(*), intent(in) :: header(:)
      reporter%MultiColumn = MultiColumn(header)
   end function new_ProfileReporter

   function new_ProfileReporter_config(header, config, unusable, rc) result(reporter)
      type(ProfileReporter) :: reporter
      character(*), intent(in) :: header(:)
      type(ESMF_HConfig), intent(in) :: config
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      reporter%MultiColumn = MultiColumn(header)
      call populate_columns(reporter, config, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end function new_ProfileReporter_config

   ! Helper to populate a MultiColumn from config
   subroutine populate_columns(multi_col, config, unusable, rc)
      use MAPL_TextColumn
      class(MultiColumn), intent(inout) :: multi_col
      type(ESMF_HConfig), intent(in) :: config
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: i, num_columns
      integer :: status
      type(ESMF_HConfig) :: columns_config
      type(ESMF_HConfig) :: column_config
      class(TextColumn), allocatable :: col
      logical :: is_defined, is_sequence

      ! Check if columns are configured
      is_defined = ESMF_HConfigIsDefined(config, keyString='columns', _RC)
      if (.not. is_defined) return
      is_sequence = ESMF_HConfigIsSequence(config, keyString='columns', _RC)
      _ASSERT(is_sequence, 'columns must be a sequence')

      num_columns = ESMF_HConfigGetSize(config, keyString='columns', _RC)

      ! Get the columns array as an HConfig
      columns_config = ESMF_HConfigCreateAt(config, keyString='columns', _RC)

      do i = 1, num_columns
         ! Access elements of the sequence with 1-based index
         column_config = ESMF_HConfigCreateAt(columns_config, index=i, _RC)
         col = column_from_config(column_config, _RC)
         call multi_col%add_column(col)
      end do

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine populate_columns

   ! Helper to get string from config with default fallback
   function get_config_string(config, key, default, unusable, rc) result(value)
      type(ESMF_HConfig), intent(in) :: config
      character(*), intent(in) :: key
      character(*), intent(in) :: default
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      character(:), allocatable :: value

      integer :: status
      logical :: is_defined

      value = default
      is_defined = ESMF_HConfigIsDefined(config, keyString=key, _RC)
      if (is_defined) then
         value = ESMF_HConfigAsString(config, keyString=key, _RC)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function get_config_string

   ! Helper to get integer from config with default fallback
   function get_config_integer(config, key, default, unusable, rc) result(value)
      type(ESMF_HConfig), intent(in) :: config
      character(*), intent(in) :: key
      integer, intent(in) :: default
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: value

      integer :: status
      logical :: is_defined

      value = default
      is_defined = ESMF_HConfigIsDefined(config, keyString=key, _RC)
      if (is_defined) then
         value = ESMF_HConfigAsI4(config, keyString=key, _RC)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function get_config_integer

   ! Factory: create a single column from config
   function column_from_config(column_config, unusable, rc) result(col)
      use MAPL_TextColumn
      use MAPL_NameColumn
      use MAPL_NumCyclesColumn
      use MAPL_InclusiveColumn
      use MAPL_ExclusiveColumn
      use MAPL_StdDevColumn
      use MAPL_MinCycleColumn
      use MAPL_MaxCycleColumn
      use MAPL_MeanCycleColumn
      use MAPL_PercentageColumn
      use MAPL_FormattedTextColumn
      use MAPL_SeparatorColumn
      class(TextColumn), allocatable :: col
      type(ESMF_HConfig), intent(in) :: column_config
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: column_type
      character(len=:), allocatable :: column_name
      character(len=:), allocatable :: format_str
      character(len=:), allocatable :: separator_str
      character(:), allocatable :: sep
      integer :: width
      integer :: status
      type(MultiColumn) :: nested_multi_col
      logical :: has_separator
      character(1) :: empty(0)

      column_type = ESMF_HConfigAsString(column_config, keyString='type', _RC)
      
      ! Get separator if defined
      has_separator = ESMF_HConfigIsDefined(column_config, keyString='separator', _RC)
      if (has_separator) then
         separator_str = get_config_string(column_config, 'separator', '=', _RC)
         sep = separator_str(1:1)
      end if
      
      select case (trim(column_type))
      case ('separator')
         column_name = get_config_string(column_config, 'char', '|', _RC)
         col = SeparatorColumn(column_name)
         
      case ('multi', 'group')
         column_name = get_config_string(column_config, 'name', '', _RC)
         if (len_trim(column_name) > 0) then
            nested_multi_col = MultiColumn([column_name], separator=sep)
         else
            nested_multi_col = MultiColumn(empty, separator=sep)
         end if
         call populate_columns(nested_multi_col, column_config, _RC)
         col = nested_multi_col
         
      case ('name')
         width = get_config_integer(column_config, 'width', 20, _RC)
         col = NameColumn(width, separator=sep)
         
      case ('num_cycles')
         column_name = get_config_string(column_config, 'name', '# cycles', _RC)
         format_str = get_config_string(column_config, 'format', '(i8.0)', _RC)
         width = get_config_integer(column_config, 'width', 8, _RC)
         col = FormattedTextColumn(column_name, format_str, width, NumCyclesColumn(), separator=sep)
         
      case ('inclusive')
         column_name = get_config_string(column_config, 'name', 'T(inc)', _RC)
         format_str = get_config_string(column_config, 'format', '(f12.6)', _RC)
         width = get_config_integer(column_config, 'width', 12, _RC)
         col = FormattedTextColumn(column_name, format_str, width, InclusiveColumn(), separator=sep)
         
      case ('exclusive')
         column_name = get_config_string(column_config, 'name', 'T(exc)', _RC)
         format_str = get_config_string(column_config, 'format', '(f12.6)', _RC)
         width = get_config_integer(column_config, 'width', 12, _RC)
         col = FormattedTextColumn(column_name, format_str, width, ExclusiveColumn(), separator=sep)
         
      case ('std_dev')
         column_name = get_config_string(column_config, 'name', 'std dev', _RC)
         format_str = get_config_string(column_config, 'format', '(f12.4)', _RC)
         width = get_config_integer(column_config, 'width', 12, _RC)
         col = FormattedTextColumn(column_name, format_str, width, StdDevColumn(), separator=sep)
         
      case ('min_cycle')
         column_name = get_config_string(column_config, 'name', 'min cyc', _RC)
         format_str = get_config_string(column_config, 'format', '(f12.8)', _RC)
         width = get_config_integer(column_config, 'width', 12, _RC)
         col = FormattedTextColumn(column_name, format_str, width, MinCycleColumn(), separator=sep)
         
      case ('max_cycle')
         column_name = get_config_string(column_config, 'name', 'max cyc', _RC)
         format_str = get_config_string(column_config, 'format', '(f12.8)', _RC)
         width = get_config_integer(column_config, 'width', 12, _RC)
         col = FormattedTextColumn(column_name, format_str, width, MaxCycleColumn(), separator=sep)
         
      case ('mean_cycle')
         column_name = get_config_string(column_config, 'name', 'mean cyc', _RC)
         format_str = get_config_string(column_config, 'format', '(f12.8)', _RC)
         width = get_config_integer(column_config, 'width', 12, _RC)
         col = FormattedTextColumn(column_name, format_str, width, MeanCycleColumn(), separator=sep)
         
      case ('percentage_inclusive')
         column_name = get_config_string(column_config, 'name', '%(inc)', _RC)
         format_str = get_config_string(column_config, 'format', '(f6.2)', _RC)
         width = get_config_integer(column_config, 'width', 6, _RC)
         col = FormattedTextColumn(column_name, format_str, width, PercentageColumn(InclusiveColumn()), separator=sep)
         
      case ('percentage_exclusive')
         column_name = get_config_string(column_config, 'name', '%(exc)', _RC)
         format_str = get_config_string(column_config, 'format', '(f6.2)', _RC)
         width = get_config_integer(column_config, 'width', 6, _RC)
         col = FormattedTextColumn(column_name, format_str, width, PercentageColumn(ExclusiveColumn()), separator=sep)
         
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end function column_from_config

   function generate_report_profiler(this, p) result(report_lines)
      type(StringVector) :: report_lines
      class (ProfileReporter), target, intent(in) :: this
      class (BaseProfiler), target, intent(in) :: p

      integer :: i
      character(:), allocatable :: rows(:)
      character(:), allocatable :: header(:)
      class (AbstractMeterNode), pointer :: node

      ! If ProfileReporter has no columns (incomplete construction), return empty report
      if (this%get_num_columns() == 0) then
         return
      end if

      call this%get_header(header)
      node => p%get_root_node()
      call this%get_rows(node, rows)
      
      do i = 1, size(header)
         call report_lines%push_back(header(i))
      end do
      do i = 1, size(rows)
         call report_lines%push_back(rows(i))
      end do

   end function generate_report_profiler
   

   
end module MAPL_ProfileReporter
