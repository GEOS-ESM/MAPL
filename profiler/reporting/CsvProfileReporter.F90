#include "MAPL.h"

!> CsvProfileReporter - Generate CSV-formatted profiling reports
!!
!! This reporter generates comma-separated value (CSV) output that can be
!! easily imported into spreadsheets, visualization tools, or other analysis software.
!!
!! Usage example:
!!
!! ```fortran
!! use esmf, only: ESMF_HConfig, ESMF_HConfigCreate
!! config = ESMF_HConfigCreate(content='{ &
!!    &columns: [ &
!!    &  {type: name, name: "timer_name"}, &
!!    &  {type: num_cycles, name: "count", format: "(i8)"}, &
!!    &  {type: inclusive, name: "time_sec", format: "(f12.6)"} &
!!    &]}')
!! reporter = CsvProfileReporter(config)
!! csv_lines = reporter%generate_report(profiler)
!! ```
!!
!! Note: The 'width' parameter is ignored for CSV output. The 'format' parameter
!! controls numeric precision and output format.
module MAPL_CsvProfileReporter
   use MAPL_AbstractMeterNode
   use MAPL_AbstractColumn
   use MAPL_TextColumn
   use MAPL_TextColumnVector
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

   public :: CsvProfileReporter

   type :: CsvProfileReporter
      private
      type(TextColumnVector) :: columns
      character(:), allocatable :: column_names(:)
      ! For multi-row headers: store the full hierarchy path for each column
      type(StringVector), allocatable :: column_paths(:)
      integer :: max_header_depth = 0
   contains
      procedure :: add_column
      procedure :: generate_report
   end type CsvProfileReporter

   interface CsvProfileReporter
      module procedure :: new_CsvProfileReporter
   end interface CsvProfileReporter

contains

   function new_CsvProfileReporter(config, unusable, rc) result(reporter)
      type(CsvProfileReporter) :: reporter
      type(ESMF_HConfig), intent(in) :: config
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status, i, j, depth_diff
      type(StringVector) :: padded_path, original_path
      type(StringVectorIterator) :: iter

      call populate_columns(reporter, config, _RC)

      ! Normalize all paths to have the same depth by left-padding with empty strings
      if (allocated(reporter%column_paths)) then
         do i = 1, size(reporter%column_paths)
            depth_diff = reporter%max_header_depth - reporter%column_paths(i)%size()
            if (depth_diff > 0) then
               ! Save original path first
               original_path = reporter%column_paths(i)
               
               ! Create new vector with padding at front
               padded_path = StringVector()
               do j = 1, depth_diff
                  call padded_path%push_back('')
               end do
               
               ! Append original path elements
               iter = original_path%begin()
               do while (iter /= original_path%end())
                  call padded_path%push_back(iter%of())
                  call iter%next()
               end do
               
               ! Replace with padded version
               reporter%column_paths(i) = padded_path
            end if
         end do
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end function new_CsvProfileReporter

   subroutine add_column(this, column, column_name, path)
      class(CsvProfileReporter), intent(inout) :: this
      class(TextColumn), intent(in) :: column
      character(*), intent(in) :: column_name
      type(StringVector), intent(in) :: path

      character(:), allocatable :: temp_names(:)
      type(StringVector), allocatable :: temp_paths(:)
      integer :: n, i

      call this%columns%push_back(column)
      
      ! Grow column_names array
      n = this%columns%size()
      if (allocated(this%column_names)) then
         allocate(character(len=max(len(this%column_names), len(column_name))) :: temp_names(n-1))
         temp_names(:) = this%column_names(:)
         deallocate(this%column_names)
         allocate(character(len=max(len(temp_names), len(column_name))) :: this%column_names(n))
         this%column_names(1:n-1) = temp_names(:)
         this%column_names(n) = column_name
      else
         allocate(character(len=len(column_name)) :: this%column_names(1))
         this%column_names(1) = column_name
      end if

      ! Grow column_paths array
      if (allocated(this%column_paths)) then
         allocate(temp_paths(n-1))
         temp_paths(:) = this%column_paths(:)
         deallocate(this%column_paths)
         allocate(this%column_paths(n))
         this%column_paths(1:n-1) = temp_paths(:)
         this%column_paths(n) = path
      else
         allocate(this%column_paths(1))
         this%column_paths(1) = path
      end if

      ! Track maximum depth
      this%max_header_depth = max(this%max_header_depth, path%size())

   end subroutine add_column

   ! Helper to populate columns from config
   subroutine populate_columns(reporter, config, unusable, rc)
      use MAPL_TextColumn
      type(CsvProfileReporter), intent(inout) :: reporter
      type(ESMF_HConfig), intent(in) :: config
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: i, num_columns
      integer :: status
      type(ESMF_HConfig) :: columns_config
      type(ESMF_HConfig) :: column_config
      class(TextColumn), allocatable :: col
      character(:), allocatable :: column_name
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
         call process_column_config(reporter, column_config, StringVector(), _RC)
      end do

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine populate_columns

   ! Helper to process a column config (handles flattening of multi/group)
   recursive subroutine process_column_config(reporter, column_config, path, unusable, rc)
      type(CsvProfileReporter), intent(inout) :: reporter
      type(ESMF_HConfig), intent(in) :: column_config
      type(StringVector), intent(in) :: path
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      character(:), allocatable :: column_type
      character(:), allocatable :: multi_name
      type(StringVector) :: new_path
      integer :: i, num_nested
      type(ESMF_HConfig) :: nested_columns_config, nested_config
      class(TextColumn), allocatable :: col
      character(:), allocatable :: column_name
      logical :: is_defined
      integer :: status

      column_type = ESMF_HConfigAsString(column_config, keyString='type', _RC)

      ! For multi/group, recursively process nested columns with updated path
      if (trim(column_type) == 'multi' .or. trim(column_type) == 'group') then
         ! Get the name of this multi-column to add to path
         multi_name = get_config_string(column_config, 'name', '', _RC)
         new_path = path
         if (len_trim(multi_name) > 0) then
            call new_path%push_back(trim(multi_name))
         end if
         
         is_defined = ESMF_HConfigIsDefined(column_config, keyString='columns', _RC)
         if (is_defined) then
            num_nested = ESMF_HConfigGetSize(column_config, keyString='columns', _RC)
            nested_columns_config = ESMF_HConfigCreateAt(column_config, keyString='columns', _RC)
            do i = 1, num_nested
               nested_config = ESMF_HConfigCreateAt(nested_columns_config, index=i, _RC)
               call process_column_config(reporter, nested_config, new_path, _RC)
            end do
         end if
      else if (trim(column_type) == 'separator') then
         ! Ignore separator columns for CSV (no-op)
      else
         ! Regular column - create and add it with full path
         call column_from_config(column_config, col, column_name, _RC)
         new_path = path
         call new_path%push_back(trim(column_name))
         call reporter%add_column(col, column_name, new_path)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine process_column_config

   ! Helper to create a column from config and extract its name
   subroutine column_from_config(column_config, col, column_name, unusable, rc)
      use MAPL_PlainNameColumn
      use MAPL_DepthColumn
      use MAPL_NumCyclesColumn
      use MAPL_InclusiveColumn
      use MAPL_ExclusiveColumn
      use MAPL_StdDevColumn
      use MAPL_MinCycleColumn
      use MAPL_MaxCycleColumn
      use MAPL_MeanCycleColumn
      use MAPL_PercentageColumn
      use MAPL_FormattedTextColumn
      class(TextColumn), allocatable, intent(out) :: col
      character(:), allocatable, intent(out) :: column_name
      type(ESMF_HConfig), intent(in) :: column_config
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: column_type
      character(len=:), allocatable :: format_str
      integer :: status

      column_type = ESMF_HConfigAsString(column_config, keyString='type', _RC)
      
      select case (trim(column_type))
      case ('name')
         column_name = get_config_string(column_config, 'name', 'Name', _RC)
         col = PlainNameColumn(20)  ! Plain name without indent markers
         
      case ('depth')
         column_name = get_config_string(column_config, 'name', 'Depth', _RC)
         format_str = get_config_string(column_config, 'format', '(i3)', _RC)
         col = DepthColumn(format_str)
         
      case ('num_cycles')
         column_name = get_config_string(column_config, 'name', 'cycles', _RC)
         format_str = get_config_string(column_config, 'format', '(i8)', _RC)
         col = FormattedTextColumn('', format_str, 10, NumCyclesColumn())
         
      case ('inclusive')
         column_name = get_config_string(column_config, 'name', 'inclusive', _RC)
         format_str = get_config_string(column_config, 'format', '(f12.6)', _RC)
         col = FormattedTextColumn('', format_str, 12, InclusiveColumn())
         
      case ('exclusive')
         column_name = get_config_string(column_config, 'name', 'exclusive', _RC)
         format_str = get_config_string(column_config, 'format', '(f12.6)', _RC)
         col = FormattedTextColumn('', format_str, 12, ExclusiveColumn())
         
      case ('std_dev')
         column_name = get_config_string(column_config, 'name', 'std_dev', _RC)
         format_str = get_config_string(column_config, 'format', '(f12.8)', _RC)
         col = FormattedTextColumn('', format_str, 12, StdDevColumn())
         
      case ('min_cycle')
         column_name = get_config_string(column_config, 'name', 'min_cycle', _RC)
         format_str = get_config_string(column_config, 'format', '(f12.8)', _RC)
         col = FormattedTextColumn('', format_str, 12, MinCycleColumn())
         
      case ('max_cycle')
         column_name = get_config_string(column_config, 'name', 'max_cycle', _RC)
         format_str = get_config_string(column_config, 'format', '(f12.8)', _RC)
         col = FormattedTextColumn('', format_str, 12, MaxCycleColumn())
         
      case ('mean_cycle')
         column_name = get_config_string(column_config, 'name', 'mean_cycle', _RC)
         format_str = get_config_string(column_config, 'format', '(f12.8)', _RC)
         col = FormattedTextColumn('', format_str, 12, MeanCycleColumn())
         
      case ('percentage_inclusive')
         column_name = get_config_string(column_config, 'name', 'pct_inclusive', _RC)
         format_str = get_config_string(column_config, 'format', '(f6.2)', _RC)
         col = FormattedTextColumn('', format_str, 6, PercentageColumn(InclusiveColumn()))
         
      case ('percentage_exclusive')
         column_name = get_config_string(column_config, 'name', 'pct_exclusive', _RC)
         format_str = get_config_string(column_config, 'format', '(f6.2)', _RC)
         col = FormattedTextColumn('', format_str, 6, PercentageColumn(ExclusiveColumn()))
         
      case default
         _FAIL('Unknown column type for CSV: ' // trim(column_type))
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine column_from_config

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

   function generate_report(this, p) result(csv_lines)
      type(StringVector) :: csv_lines
      class(CsvProfileReporter), target, intent(in) :: this
      class(BaseProfiler), target, intent(in) :: p

      integer :: i, j, depth, num_rows
      character(:), allocatable :: row_data(:)
      character(:), allocatable :: csv_row
      character(:), allocatable :: cell_value
      class(AbstractMeterNode), pointer :: node
      class(TextColumn), pointer :: col
      type(StringVectorIterator) :: iter

      ! Return empty if no columns configured
      if (this%columns%size() == 0) return
      
      ! Return empty if no header depth or paths not allocated
      if (this%max_header_depth == 0 .or. .not. allocated(this%column_paths)) return

      ! Generate multi-row header
      do depth = 1, this%max_header_depth
         csv_row = ''
         do i = 1, this%columns%size()
            if (i > 1) csv_row = csv_row // ','
            ! All paths now have the same depth (normalized in constructor)
            iter = this%column_paths(i)%begin()
            do j = 1, depth - 1
               call iter%next()
            end do
            ! Only access if iterator is valid
            if (iter /= this%column_paths(i)%end()) then
               csv_row = csv_row // trim(iter%of())
            end if
         end do
         call csv_lines%push_back(csv_row)
      end do

      ! Get the root node
      node => p%get_root_node()
      
      ! Get rows from first column to determine number of rows
      col => this%columns%at(1)
      call col%get_rows(node, row_data)
      num_rows = size(row_data)
      
      ! Data rows
      do i = 1, num_rows
         csv_row = ''
         do j = 1, this%columns%size()
            col => this%columns%at(j)
            call col%get_rows(node, row_data)
            cell_value = trim(adjustl(row_data(i)))
            
            ! CSV escaping: quote if contains comma or quote
            if (index(cell_value, ',') > 0 .or. index(cell_value, '"') > 0) then
               ! Escape quotes by doubling them
               cell_value = quote_and_escape(cell_value)
            end if
            
            if (j > 1) csv_row = csv_row // ','
            csv_row = csv_row // cell_value
         end do
         call csv_lines%push_back(csv_row)
      end do

   end function generate_report

   ! Helper to quote and escape CSV values
   function quote_and_escape(str) result(escaped)
      character(*), intent(in) :: str
      character(:), allocatable :: escaped
      integer :: i, n
      
      n = len_trim(str)
      escaped = '"'
      do i = 1, n
         if (str(i:i) == '"') then
            escaped = escaped // '""'  ! Double the quote
         else
            escaped = escaped // str(i:i)
         end if
      end do
      escaped = escaped // '"'
   end function quote_and_escape

end module MAPL_CsvProfileReporter
