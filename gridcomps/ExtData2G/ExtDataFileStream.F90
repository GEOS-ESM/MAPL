#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_ExtDataFileStream
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_TimeStringConversion
   use MAPL_DataCollectionMod
   use MAPL_CollectionVectorMod
   use MAPL_DataCollectionManagerMod
   use MAPL_FileMetadataUtilsMod
   use MAPL_StringTemplate
   implicit none
   private

   type, public :: ExtDataFileStream
      character(len=:), allocatable :: file_template
      type(ESMF_TimeInterval) :: frequency
      type(ESMF_Time) :: reff_time
      integer :: collection_id
      type(ESMF_Time), allocatable :: valid_range(:)
      type(ESMF_Time), allocatable :: on_disk_range(:)  ! set by check_data_availability; actual first/last file timestamps
      type(FileMetaData) :: metadata
      contains
         procedure :: detect_metadata
         procedure :: check_data_availability
         procedure :: get_required_files_hconfig
         procedure, private :: refine_valid_range
   end type

    interface ExtDataFileStream
       module procedure new_ExtDataFileStream
    end interface ExtDataFileStream
contains

   function new_ExtDataFileStream(config,current_time,unusable,rc) result(data_set)
      type(ESMF_HConfig), intent(in) :: config
      type(ESMF_Time), intent(in) :: current_time
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ExtDataFileStream) :: data_set
      integer :: status
      integer :: last_token
      integer :: iyy,imm,idd,ihh,imn,isc,idx
      character(len=2) :: token
      character(len=:), allocatable :: file_frequency, file_reff_time,range_str
      logical :: is_present

      _UNUSED_DUMMY(unusable)


      is_present = ESMF_HConfigIsDefined(config,keyString="template",_RC)
      _ASSERT(is_present,"no file template in the collection")

      if (is_present) then
         data_set%file_template = ESMF_HConfigAsString(config,keyString="template",_RC)
         file_frequency = get_string_with_default(config,"freq")
         file_reff_time = get_string_with_default(config,"ref_time")
         range_str = get_string_with_default(config,"valid_range")
      end if

      if (file_frequency /= '') then
         data_set%frequency = string_to_esmf_timeinterval(file_frequency)
      else
         last_token = index(data_set%file_template,'%',back=.true.)
         if (last_token.gt.0) then
            token = data_set%file_template(last_token+1:last_token+2)
            select case(token)
            case("y4")
               call ESMF_TimeIntervalSet(data_set%frequency,yy=1,_RC)
            case("m2")
               call ESMF_TimeIntervalSet(data_set%frequency,mm=1,_RC)
            case("d2")
               call ESMF_TimeIntervalSet(data_set%frequency,d=1,_RC)
            case("h2")
               call ESMF_TimeIntervalSet(data_set%frequency,h=1,_RC)
            case("n2")
               call ESMF_TimeIntervalSet(data_set%frequency,m=1,_RC)
            end select
         else
            ! couldn't find any tokens so all the data must be on one file
            call ESMF_TimeIntervalSet(data_set%frequency,_RC)
         end if
      end if

      if (file_reff_time /= '') then
         data_set%reff_time = string_to_esmf_time(file_reff_time)
      else
         last_token = index(data_set%file_template,'%',back=.true.)
         if (last_token.gt.0) then
            call ESMF_TimeGet(current_time, yy=iyy, mm=imm, dd=idd,h=ihh, m=imn, s=isc  ,_RC)
            token = data_set%file_template(last_token+1:last_token+2)
            select case(token)
            case("y4")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=1,dd=1,h=0,m=0,s=0,_RC)
            case("m2")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=imm,dd=1,h=0,m=0,s=0,_RC)
            case("d2")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=imm,dd=idd,h=0,m=0,s=0,_RC)
            case("h2")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=0,s=0,_RC)
            case("n2")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=imn,s=0,_RC)
            end select
         else
            data_set%reff_time = current_time
         end if
      end if

      if (range_str /= '') then
         idx = index(range_str,'/')
         _ASSERT(idx/=0,'invalid specification of time range')
         if (allocated(data_set%valid_range)) deallocate(data_set%valid_range)
         allocate(data_set%valid_range(2))
         data_set%valid_range(1)=string_to_esmf_time(range_str(:idx-1))
         data_set%valid_range(2)=string_to_esmf_time(range_str(idx+1:))

         last_token = index(data_set%file_template,'%',back=.true.)
         if (last_token.gt.0) then
            call ESMF_TimeGet(data_set%valid_range(1), yy=iyy, mm=imm, dd=idd,h=ihh, m=imn, s=isc  ,_RC)
            token = data_set%file_template(last_token+1:last_token+2)
            select case(token)
            case("y4")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=1,dd=1,h=0,m=0,s=0,_RC)
            case("m2")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=imm,dd=1,h=0,m=0,s=0,_RC)
            case("d2")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=imm,dd=idd,h=0,m=0,s=0,_RC)
            case("h2")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=0,s=0,_RC)
            case("n2")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=imn,s=0,_RC)
            end select
         end if

      end if
      data_set%collection_id = MAPL_DataAddCollection(data_set%file_template)

      _RETURN(_SUCCESS)

      contains

         function get_string_with_default(config,selector) result(string)
            type(ESMF_HConfig), intent(in) :: config
            character(len=*), intent(In) :: selector
            character(len=:), allocatable :: string

            if (ESMF_HConfigIsDefined(config,keyString=selector)) then
               string = ESMF_HConfigAsString(config,keyString=selector,_RC)
            else
               string=''
            end if
         end function

   end function new_ExtDataFileStream

   subroutine detect_metadata(this,metadata_out,time,multi_rule,get_range,rc)
      class(ExtDataFileStream), intent(inout) :: this
      type(FileMetadataUtils), intent(inout) :: metadata_out
      type(ESMF_Time),          intent(in)  :: time
      logical, intent(in)  :: multi_rule
      logical, optional, intent(in)  :: get_range
      integer, optional, intent(out) :: rc

      logical :: get_range_
      type(MAPLDataCollection), pointer :: collection
      type(FileMetadataUtils), pointer :: metadata
      type(ESMF_Time), allocatable :: time_series(:)
      integer :: status

      if (multi_rule) then
         _ASSERT(allocated(this%valid_range),"must use a collection with valid range")
      end if

      if (present(get_range)) then
         get_range_ = get_range
      else
         get_range_ = .false.
      end if

      collection => DataCollections%at(this%collection_id)
      if (get_range_ .and. (.not.allocated(this%valid_range))) then
         if (index(this%file_template, '%') == 0) then
            metadata => collection%find(this%file_template)
            call metadata%get_time_info(timeVector=time_series,_RC)
            allocate(this%valid_range(2))
            this%valid_range(1)=time_series(1)
            this%valid_range(2)=time_series(size(time_series))
         end if
      end if

      _RETURN(_SUCCESS)

      _UNUSED_DUMMY(metadata_out)
      _UNUSED_DUMMY(time)

   end subroutine detect_metadata

   ! Given a scan window (scan_range), find the actual first and last file
   ! times on the filesystem within that window.  The file series is defined
   ! by reff_time + n * frequency for any integer n, and files are assumed to
   ! form a single contiguous block within the range.
   !
   ! Returns on_disk_first and on_disk_last (timestamps from inside the first
   ! and last files found), and found_any=.false. if no files exist.
   ! valid_range on this is never modified.
   !
   ! Algorithm: probe midpoint then right-quarter point as anchor (O(1));
   ! if both miss, fall back to a linear scan from n_lo.  Once an anchor
   ! (any confirmed file position) is established, binary search on each
   ! monotone half gives O(log N) total filesystem probes for the common
   ! case, with linear fallback only for unusual narrow-block situations.
   subroutine refine_valid_range(this, scan_range, on_disk_first, on_disk_last, found_any, unusable, rc)
      class(ExtDataFileStream), intent(in)    :: this
      type(ESMF_Time),          intent(in)    :: scan_range(2)
      type(ESMF_Time),          intent(out)   :: on_disk_first
      type(ESMF_Time),          intent(out)   :: on_disk_last
      logical,                  intent(out)   :: found_any
      class(KeywordEnforcer),   optional, intent(in)  :: unusable
      integer,                  optional, intent(out) :: rc

      integer(ESMF_KIND_I8) :: interval_seconds
      integer :: n_lo, n_hi, n_first, n_last, n_mid, n_anchor, lo, hi, n
      integer :: status
      logical :: file_found
      type(ESMF_Time) :: t_mid
      type(ESMF_Time), allocatable :: time_series(:)
      character(len=ESMF_MAXPATHLEN) :: filename
      type(MAPLDataCollection),  pointer :: collection
      type(FileMetadataUtils),   pointer :: file_metadata

      _UNUSED_DUMMY(unusable)

      found_any = .false.

      ! Determine if interval is absolute (representable in seconds) or
      ! relative (months/years).  Follows the same idiom as get_file in
      ! ExtDataSimpleFileHandler: a zero s_i8 result means relative.
      call ESMF_TimeIntervalGet(this%frequency, s_i8=interval_seconds)

      if (interval_seconds /= 0) then

         ! --- Absolute interval: compute index bounds via ESMF division ---
         ! n_lo: last integer n with reff_time + n*freq <= scan_range(1)
         ! (includes the file whose period covers scan_range(1) even if its
         ! start timestamp is slightly before it).
         ! ESMF division truncates toward zero, which is floor for positive
         ! differences but ceiling for negative; the guard below corrects the
         ! latter so both signs give the floor (i.e. the desired last n).
         n_lo = (scan_range(1) - this%reff_time) / this%frequency
         if (this%reff_time + n_lo * this%frequency > scan_range(1)) n_lo = n_lo - 1

         ! n_hi: last integer n with reff_time + n*freq <= scan_range(2)
         n_hi = (scan_range(2) - this%reff_time) / this%frequency
         if (this%reff_time + n_hi * this%frequency > scan_range(2)) n_hi = n_hi - 1
      else

         ! --- Relative interval (months/years): walk to find index bounds ---
         ! n_lo = last n such that reff_time + n*freq <= scan_range(1).
         ! This includes the file whose start is at or just before scan_range(1)
         ! so that files whose period covers scan_range(1) are not skipped.
         n = 0
         if (this%reff_time < scan_range(1)) then
            do while (this%reff_time + (n + 1) * this%frequency <= scan_range(1))
               n = n + 1
            end do
         else
            do while (this%reff_time + n * this%frequency > scan_range(1))
               n = n - 1
            end do
         end if
         n_lo = n

         ! Walk forward from n_lo to the last index within scan_range(2).
         n = n_lo
         do while (this%reff_time + (n + 1) * this%frequency <= scan_range(2))
            n = n + 1
         end do
         n_hi = n

      end if

      _ASSERT(n_lo <= n_hi, &
         "no candidate file times found within scan range for: "//trim(this%file_template))

      ! === Phase 1: Find anchor — up to 2 probes, then linear scan fallback ===
      ! A contiguous block needs a confirmed anchor so that each directed binary
      ! search operates on a monotone sub-range: [n_lo, n_anchor] is 0...01...1
      ! and [n_anchor, n_hi] is 1...10...0.

      ! Probe 1: midpoint
      n_anchor = (n_lo + n_hi) / 2
      t_mid = this%reff_time + n_anchor * this%frequency
      call fill_grads_template(filename, this%file_template, time=t_mid, _RC)
      inquire(file=trim(filename), exist=file_found)

      if (.not. file_found) then
         ! Probe 2: right-quarter point — covers the common case where data
         ! starts after the midpoint of the guess range.
         n_anchor = (n_anchor + n_hi) / 2
         t_mid = this%reff_time + n_anchor * this%frequency
         call fill_grads_template(filename, this%file_template, time=t_mid, _RC)
         inquire(file=trim(filename), exist=file_found)
      end if

      if (.not. file_found) then
         ! Both probes missed; scan forward from n_lo to locate n_first.
         do n = n_lo, n_hi
            t_mid = this%reff_time + n * this%frequency
            call fill_grads_template(filename, this%file_template, time=t_mid, _RC)
            inquire(file=trim(filename), exist=file_found)
            if (file_found) then
               n_first = n
               exit
            end if
         end do
         if (.not. file_found) then
            ! No files found anywhere in valid_range — return found_any=.false.
            _RETURN(_SUCCESS)
         end if
         n_anchor = n_first
      end if

      ! === Phase 2: Binary search for n_first in [n_lo, n_anchor] ===
      ! n_anchor is confirmed to have a file; range is monotone 0...01...1.
      lo = n_lo
      hi = n_anchor
      do while (lo < hi)
         n_mid = (lo + hi) / 2
         t_mid = this%reff_time + n_mid * this%frequency
         call fill_grads_template(filename, this%file_template, time=t_mid, _RC)
         inquire(file=trim(filename), exist=file_found)
         if (file_found) then
            hi = n_mid       ! could be leftmost; search left half
         else
            lo = n_mid + 1   ! no file here; discard left half
         end if
      end do
      n_first = lo

      ! === Phase 3: Binary search for n_last in [n_anchor, n_hi] ===
      ! n_anchor is confirmed to have a file; range is monotone 1...10...0.
      lo = n_anchor
      hi = n_hi
      do while (lo < hi)
         n_mid = (lo + hi + 1) / 2   ! ceiling to avoid infinite loop when hi = lo+1
         t_mid = this%reff_time + n_mid * this%frequency
         call fill_grads_template(filename, this%file_template, time=t_mid, _RC)
         inquire(file=trim(filename), exist=file_found)
         if (file_found) then
            lo = n_mid       ! could be rightmost; search right half
         else
            hi = n_mid - 1   ! no file here; discard right half
         end if
      end do
      n_last = lo

      ! Open the first file and read its earliest internal timestamp.
      collection => DataCollections%at(this%collection_id)

      t_mid = this%reff_time + n_first * this%frequency
      call fill_grads_template(filename, this%file_template, time=t_mid, _RC)
      file_metadata => collection%find(trim(filename), _RC)
      call file_metadata%get_time_info(timeVector=time_series, _RC)
      on_disk_first = time_series(1)
      deallocate(time_series)

      ! Open the last file and read its latest internal timestamp.
      t_mid = this%reff_time + n_last * this%frequency
      call fill_grads_template(filename, this%file_template, time=t_mid, _RC)
      file_metadata => collection%find(trim(filename), _RC)
      call file_metadata%get_time_info(timeVector=time_series, _RC)
      on_disk_last = time_series(size(time_series))

      found_any = .true.
      _RETURN(_SUCCESS)

   end subroutine refine_valid_range

   ! Validate that sufficient files exist on disk to satisfy the combination
   ! of valid_range, run_range, and extrap_outside.
   !
   ! Three scenarios:
   !
   !   1. extrap_outside = "none"
   !      Scan window = run_range (valid_range not required).
   !      If valid_range is set and run_range extends outside it → config error.
   !      On-disk data must cover the full run_range.
   !
   !   2. extrap_outside = "persist_closest"
   !      Scan window = valid_range (required).
   !      If run overlaps valid_range: on-disk data must cover the overlap.
   !      If run is outside valid_range: found_any is sufficient (persist endpoint).
   !
   !   3. extrap_outside = "clim"
   !      Scan window = valid_range (required).
   !      If run overlaps valid_range: on-disk data must cover the overlap.
   !      If run is outside valid_range: full-cycle + direction + gap-scan checks.
   !
   ! valid_range on this is never modified.
   subroutine check_data_availability(this, run_range, extrap_outside, unusable, rc)
      class(ExtDataFileStream), intent(inout)   :: this
      type(ESMF_Time),          intent(in)   :: run_range(2)
      character(len=*),         intent(in)   :: extrap_outside
      class(KeywordEnforcer),   optional, intent(in)  :: unusable
      integer,                  optional, intent(out) :: rc

       type(ESMF_Time) :: on_disk_first, on_disk_last
       type(ESMF_Time) :: overlap_start, overlap_end
       type(ESMF_Time) :: scan_range(2)
       logical         :: found_any, has_overlap, full_cycle
       integer         :: yr_first, yr_last, mm_first, mm_last
       integer         :: vr_yr1, vr_yr2, scan_year
       integer         :: n_scan_lo, n_scan_hi, n_scan, n_missing
       integer(ESMF_KIND_I8) :: scan_interval_seconds
       type(ESMF_Time) :: t_scan_lo, t_scan_hi, t_probe
       logical         :: do_gap_scan, probe_found
       character(len=ESMF_MAXPATHLEN) :: probe_filename
       character(len=ESMF_MAXSTR)     :: t_missing
       character(len=:), allocatable  :: missing_list
       integer         :: status
       character(len=ESMF_MAXSTR) :: t_str1, t_str2

      _UNUSED_DUMMY(unusable)

      select case (trim(extrap_outside))

      ! -----------------------------------------------------------------------
      case ("none")
      ! -----------------------------------------------------------------------
      ! Scenario 1: no extrapolation.  Scan run_range directly.
      ! If valid_range is set, run_range must lie within it (else config error).

         scan_range = run_range
         call this%refine_valid_range(scan_range, on_disk_first, on_disk_last, found_any, _RC)

         ! Config error: valid_range specified but run is outside it
         if (allocated(this%valid_range)) then
            if (run_range(1) < this%valid_range(1) .or. run_range(2) > this%valid_range(2)) then
               call ESMF_TimeGet(run_range(1),          timeString=t_str1, _RC)
               call ESMF_TimeGet(run_range(2),          timeString=t_str2, _RC)
               call ESMF_TimeGet(this%valid_range(1),   timeString=t_missing, _RC)
               _FAIL("extrap_outside=none but run_range ["//trim(t_str1)//"/"//trim(t_str2)// &
                     "] extends outside valid_range ["//trim(t_missing)//"...] for template: "// &
                     trim(this%file_template))
            end if
         end if

         if (.not. found_any) then
            call ESMF_TimeGet(run_range(1), timeString=t_str1, _RC)
            call ESMF_TimeGet(run_range(2), timeString=t_str2, _RC)
            _FAIL("No files found within run_range ["//trim(t_str1)//"/"//trim(t_str2)// &
                  "] for template: "//trim(this%file_template))
         end if

         ! Store the on-disk range so handlers can use it for runtime clamping.
         if (allocated(this%on_disk_range)) deallocate(this%on_disk_range)
         allocate(this%on_disk_range(2))
         this%on_disk_range(1) = on_disk_first
         this%on_disk_range(2) = on_disk_last

         ! Data must cover the full run_range (same one-period tolerance as overlap check).
         if (on_disk_first > run_range(1) + this%frequency) then
            call ESMF_TimeGet(on_disk_first, timeString=t_str1, _RC)
            call ESMF_TimeGet(run_range(1),  timeString=t_str2, _RC)
            _FAIL("extrap_outside=none: on-disk data starts at "//trim(t_str1)// &
                  " but data is required from "//trim(t_str2)// &
                  " for template: "//trim(this%file_template))
         end if
         if (on_disk_last < run_range(2) - this%frequency) then
            call ESMF_TimeGet(on_disk_last, timeString=t_str1, _RC)
            call ESMF_TimeGet(run_range(2), timeString=t_str2, _RC)
            _FAIL("extrap_outside=none: on-disk data ends at "//trim(t_str1)// &
                  " but data is required through "//trim(t_str2)// &
                  " for template: "//trim(this%file_template))
         end if

         ! Gap scan: probe every file slot needed to bracket the full run_range,
         ! including one frequency on each side for interpolation brackets.
         ! Clamp to valid_range if set.
         t_scan_lo = run_range(1) - this%frequency
         if (allocated(this%valid_range)) then
            if (t_scan_lo < this%valid_range(1)) t_scan_lo = this%valid_range(1)
         end if
         t_scan_hi = run_range(2) + this%frequency
         if (allocated(this%valid_range)) then
            if (t_scan_hi > this%valid_range(2)) t_scan_hi = this%valid_range(2)
         end if
         call ESMF_TimeIntervalGet(this%frequency, s_i8=scan_interval_seconds, _RC)
         if (scan_interval_seconds /= 0) then
            n_scan_lo = (t_scan_lo - this%reff_time) / this%frequency
            if (this%reff_time + n_scan_lo * this%frequency > t_scan_lo) n_scan_lo = n_scan_lo - 1
            n_scan_hi = (t_scan_hi - this%reff_time) / this%frequency
            if (this%reff_time + n_scan_hi * this%frequency > t_scan_hi) n_scan_hi = n_scan_hi - 1
         else
            n_scan = 0
            if (this%reff_time < t_scan_lo) then
               do while (this%reff_time + (n_scan + 1) * this%frequency <= t_scan_lo)
                  n_scan = n_scan + 1
               end do
            else
               do while (this%reff_time + n_scan * this%frequency > t_scan_lo)
                  n_scan = n_scan - 1
               end do
            end if
            n_scan_lo = n_scan
            n_scan = n_scan_lo
            do while (this%reff_time + (n_scan + 1) * this%frequency <= t_scan_hi)
               n_scan = n_scan + 1
            end do
            n_scan_hi = n_scan
         end if
         n_missing    = 0
         missing_list = ""
         do n_scan = n_scan_lo, n_scan_hi
            t_probe = this%reff_time + n_scan * this%frequency
            call fill_grads_template(probe_filename, this%file_template, time=t_probe, _RC)
            inquire(file=trim(probe_filename), exist=probe_found)
            if (.not. probe_found) then
               call ESMF_TimeGet(t_probe, timeString=t_missing, _RC)
               missing_list = missing_list // new_line('a') // "  " // trim(t_missing)
               n_missing = n_missing + 1
            end if
         end do
         if (n_missing > 0) then
            _FAIL("The following files are required to bracket run_range" // &
                  " for template: "//trim(this%file_template)//":"//trim(missing_list))
         end if

      ! -----------------------------------------------------------------------
      case ("persist_closest")
      ! -----------------------------------------------------------------------
      ! Scenario 2: persistence extrapolation.  Scan valid_range.
      ! Overlap: must cover the full overlap.  Outside: found_any is sufficient.

         scan_range = this%valid_range
         call this%refine_valid_range(scan_range, on_disk_first, on_disk_last, found_any, _RC)

         if (.not. found_any) then
            call ESMF_TimeGet(this%valid_range(1), timeString=t_str1, _RC)
            call ESMF_TimeGet(this%valid_range(2), timeString=t_str2, _RC)
            _FAIL("No files found within valid_range ["//trim(t_str1)//"/"//trim(t_str2)// &
                  "] for template: "//trim(this%file_template))
         end if

         ! Store the on-disk range.
         if (allocated(this%on_disk_range)) deallocate(this%on_disk_range)
         allocate(this%on_disk_range(2))
         this%on_disk_range(1) = on_disk_first
         this%on_disk_range(2) = on_disk_last

         ! Overlap check.
         if (run_range(1) > this%valid_range(1)) then
            overlap_start = run_range(1)
         else
            overlap_start = this%valid_range(1)
         end if
         if (run_range(2) < this%valid_range(2)) then
            overlap_end = run_range(2)
         else
            overlap_end = this%valid_range(2)
         end if
         has_overlap = (overlap_start <= overlap_end)

         if (has_overlap) then
            if (on_disk_first > overlap_start + this%frequency) then
               call ESMF_TimeGet(on_disk_first, timeString=t_str1, _RC)
               call ESMF_TimeGet(overlap_start, timeString=t_str2, _RC)
               _FAIL("Run period overlaps valid_range but on-disk data starts at "// &
                     trim(t_str1)//". Data is required from "//trim(t_str2)// &
                     " for template: "//trim(this%file_template))
            end if
            if (on_disk_last < overlap_end - this%frequency) then
               call ESMF_TimeGet(on_disk_last, timeString=t_str1, _RC)
               call ESMF_TimeGet(overlap_end,  timeString=t_str2, _RC)
               _FAIL("Run period overlaps valid_range but on-disk data ends at "// &
                     trim(t_str1)//". Data is required through "//trim(t_str2)// &
                     " for template: "//trim(this%file_template))
            end if
            ! Gap scan: probe every file slot needed to bracket the overlap window,
            ! including one frequency on each side for interpolation brackets,
            ! clamped to valid_range boundaries.
            if (overlap_start - this%frequency > this%valid_range(1)) then
               t_scan_lo = overlap_start - this%frequency
            else
               t_scan_lo = this%valid_range(1)
            end if
            if (overlap_end + this%frequency < this%valid_range(2)) then
               t_scan_hi = overlap_end + this%frequency
            else
               t_scan_hi = this%valid_range(2)
            end if
            call ESMF_TimeIntervalGet(this%frequency, s_i8=scan_interval_seconds, _RC)
            if (scan_interval_seconds /= 0) then
               n_scan_lo = (t_scan_lo - this%reff_time) / this%frequency
               if (this%reff_time + n_scan_lo * this%frequency > t_scan_lo) n_scan_lo = n_scan_lo - 1
               n_scan_hi = (t_scan_hi - this%reff_time) / this%frequency
               if (this%reff_time + n_scan_hi * this%frequency > t_scan_hi) n_scan_hi = n_scan_hi - 1
            else
               n_scan = 0
               if (this%reff_time < t_scan_lo) then
                  do while (this%reff_time + (n_scan + 1) * this%frequency <= t_scan_lo)
                     n_scan = n_scan + 1
                  end do
               else
                  do while (this%reff_time + n_scan * this%frequency > t_scan_lo)
                     n_scan = n_scan - 1
                  end do
               end if
               n_scan_lo = n_scan
               n_scan = n_scan_lo
               do while (this%reff_time + (n_scan + 1) * this%frequency <= t_scan_hi)
                  n_scan = n_scan + 1
               end do
               n_scan_hi = n_scan
            end if
            n_missing    = 0
            missing_list = ""
            do n_scan = n_scan_lo, n_scan_hi
               t_probe = this%reff_time + n_scan * this%frequency
               call fill_grads_template(probe_filename, this%file_template, time=t_probe, _RC)
               inquire(file=trim(probe_filename), exist=probe_found)
               if (.not. probe_found) then
                  call ESMF_TimeGet(t_probe, timeString=t_missing, _RC)
                  missing_list = missing_list // new_line('a') // "  " // trim(t_missing)
                  n_missing = n_missing + 1
               end if
            end do
            if (n_missing > 0) then
               _FAIL("The following files are required to bracket the overlap of run_range" // &
                     " and valid_range for template: "//trim(this%file_template)//":"//trim(missing_list))
            end if
         end if
         ! Run outside valid_range: found_any already verified above — sufficient
         ! for persist_closest (will clamp to on_disk_range endpoint at runtime).

      ! -----------------------------------------------------------------------
      case ("clim")
      ! -----------------------------------------------------------------------
      ! Scenario 3: climatology extrapolation.  Scan valid_range.

         scan_range = this%valid_range
         call this%refine_valid_range(scan_range, on_disk_first, on_disk_last, found_any, _RC)

         if (.not. found_any) then
            call ESMF_TimeGet(this%valid_range(1), timeString=t_str1, _RC)
            call ESMF_TimeGet(this%valid_range(2), timeString=t_str2, _RC)
            _FAIL("No files found within valid_range ["//trim(t_str1)//"/"//trim(t_str2)// &
                  "] for template: "//trim(this%file_template))
         end if

         ! Store the on-disk range.
         if (allocated(this%on_disk_range)) deallocate(this%on_disk_range)
         allocate(this%on_disk_range(2))
         this%on_disk_range(1) = on_disk_first
         this%on_disk_range(2) = on_disk_last

         ! Overlap check.
         if (run_range(1) > this%valid_range(1)) then
            overlap_start = run_range(1)
         else
            overlap_start = this%valid_range(1)
         end if
         if (run_range(2) < this%valid_range(2)) then
            overlap_end = run_range(2)
         else
            overlap_end = this%valid_range(2)
         end if
         has_overlap = (overlap_start <= overlap_end)

         if (has_overlap) then
            if (on_disk_first > overlap_start + this%frequency) then
               call ESMF_TimeGet(on_disk_first, timeString=t_str1, _RC)
               call ESMF_TimeGet(overlap_start, timeString=t_str2, _RC)
               _FAIL("Run period overlaps valid_range but on-disk data starts at "// &
                     trim(t_str1)//". Data is required from "//trim(t_str2)// &
                     " for template: "//trim(this%file_template))
            end if
            if (on_disk_last < overlap_end - this%frequency) then
               call ESMF_TimeGet(on_disk_last, timeString=t_str1, _RC)
               call ESMF_TimeGet(overlap_end,  timeString=t_str2, _RC)
               _FAIL("Run period overlaps valid_range but on-disk data ends at "// &
                     trim(t_str1)//". Data is required through "//trim(t_str2)// &
                     " for template: "//trim(this%file_template))
            end if
            ! Gap scan: probe every file slot needed to bracket the overlap window,
            ! including one frequency on each side for interpolation brackets,
            ! clamped to valid_range boundaries.
            if (overlap_start - this%frequency > this%valid_range(1)) then
               t_scan_lo = overlap_start - this%frequency
            else
               t_scan_lo = this%valid_range(1)
            end if
            if (overlap_end + this%frequency < this%valid_range(2)) then
               t_scan_hi = overlap_end + this%frequency
            else
               t_scan_hi = this%valid_range(2)
            end if
            call ESMF_TimeIntervalGet(this%frequency, s_i8=scan_interval_seconds, _RC)
            if (scan_interval_seconds /= 0) then
               n_scan_lo = (t_scan_lo - this%reff_time) / this%frequency
               if (this%reff_time + n_scan_lo * this%frequency > t_scan_lo) n_scan_lo = n_scan_lo - 1
               n_scan_hi = (t_scan_hi - this%reff_time) / this%frequency
               if (this%reff_time + n_scan_hi * this%frequency > t_scan_hi) n_scan_hi = n_scan_hi - 1
            else
               n_scan = 0
               if (this%reff_time < t_scan_lo) then
                  do while (this%reff_time + (n_scan + 1) * this%frequency <= t_scan_lo)
                     n_scan = n_scan + 1
                  end do
               else
                  do while (this%reff_time + n_scan * this%frequency > t_scan_lo)
                     n_scan = n_scan - 1
                  end do
               end if
               n_scan_lo = n_scan
               n_scan = n_scan_lo
               do while (this%reff_time + (n_scan + 1) * this%frequency <= t_scan_hi)
                  n_scan = n_scan + 1
               end do
               n_scan_hi = n_scan
            end if
            n_missing    = 0
            missing_list = ""
            do n_scan = n_scan_lo, n_scan_hi
               t_probe = this%reff_time + n_scan * this%frequency
               call fill_grads_template(probe_filename, this%file_template, time=t_probe, _RC)
               inquire(file=trim(probe_filename), exist=probe_found)
               if (.not. probe_found) then
                  call ESMF_TimeGet(t_probe, timeString=t_missing, _RC)
                  missing_list = missing_list // new_line('a') // "  " // trim(t_missing)
                  n_missing = n_missing + 1
               end if
            end do
            if (n_missing > 0) then
               _FAIL("The following files are required to bracket the overlap of run_range" // &
                     " and valid_range for template: "//trim(this%file_template)//":"//trim(missing_list))
            end if
         end if

         ! --- Extrapolation sufficiency (run outside valid_range) ---
         ! Extract on-disk year/month info for the full_cycle check.
         call ESMF_TimeGet(on_disk_first, yy=yr_first, mm=mm_first, _RC)
         call ESMF_TimeGet(on_disk_last,  yy=yr_last,  mm=mm_last,  _RC)

         ! Extract valid_range years and determine run direction up front so
         ! the scan scope is known if full_cycle fails.
         call ESMF_TimeGet(this%valid_range(1), yy=vr_yr1, _RC)
         call ESMF_TimeGet(this%valid_range(2), yy=vr_yr2, _RC)

         do_gap_scan = .false.
         if (run_range(2) < this%valid_range(1)) then
            scan_year   = vr_yr1
            do_gap_scan = .true.
         else if (run_range(1) > this%valid_range(2)) then
            scan_year   = vr_yr2
            do_gap_scan = .true.
         end if

         ! --- Full-cycle check ---
         ! Require files spanning a full annual cycle: either Jan-Dec within
         ! one year, or across a year boundary (yr_last > yr_first).
         ! On failure, scan the target year (or full valid_range if overlapping)
         ! and report every missing file explicitly.
         full_cycle = (yr_last > yr_first) .or. &
                      (yr_last == yr_first .and. mm_first == 1 .and. mm_last == 12)
         if (.not. full_cycle) then
            ! Set scan bounds: target year if direction known, else full valid_range.
            if (do_gap_scan) then
               call ESMF_TimeSet(t_scan_lo, yy=scan_year, mm=1,  dd=1,  h=0,  m=0,  s=0,  _RC)
               call ESMF_TimeSet(t_scan_hi, yy=scan_year, mm=12, dd=31, h=23, m=59, s=59, _RC)
            else
               t_scan_lo = this%valid_range(1)
               t_scan_hi = this%valid_range(2)
            end if

            call ESMF_TimeIntervalGet(this%frequency, s_i8=scan_interval_seconds, _RC)
            if (scan_interval_seconds /= 0) then
               n_scan_lo = (t_scan_lo - this%reff_time) / this%frequency
               if (this%reff_time + n_scan_lo * this%frequency > t_scan_lo) n_scan_lo = n_scan_lo - 1
               n_scan_hi = (t_scan_hi - this%reff_time) / this%frequency
               if (this%reff_time + n_scan_hi * this%frequency > t_scan_hi) n_scan_hi = n_scan_hi - 1
            else
               n_scan = 0
               if (this%reff_time < t_scan_lo) then
                  do while (this%reff_time + (n_scan + 1) * this%frequency <= t_scan_lo)
                     n_scan = n_scan + 1
                  end do
               else
                  do while (this%reff_time + n_scan * this%frequency > t_scan_lo)
                     n_scan = n_scan - 1
                  end do
               end if
               n_scan_lo = n_scan
               n_scan = n_scan_lo
               do while (this%reff_time + (n_scan + 1) * this%frequency <= t_scan_hi)
                  n_scan = n_scan + 1
               end do
               n_scan_hi = n_scan
            end if

            n_missing    = 0
            missing_list = ""
            do n_scan = n_scan_lo, n_scan_hi
               t_probe = this%reff_time + n_scan * this%frequency
               call fill_grads_template(probe_filename, this%file_template, time=t_probe, _RC)
               inquire(file=trim(probe_filename), exist=probe_found)
               if (.not. probe_found) then
                  call ESMF_TimeGet(t_probe, timeString=t_missing, _RC)
                  missing_list = missing_list // new_line('a') // "  " // trim(t_missing)
                  n_missing = n_missing + 1
               end if
            end do

            if (n_missing > 0) then
               _FAIL("clim extrapolation requires files spanning a full annual cycle "// &
                     "but the following files are missing for template: "// &
                     trim(this%file_template)//trim(missing_list))
            else
               ! full_cycle was false based on on_disk_first/last alone, but all
               ! files are actually present — this should not happen with a
               ! contiguous block assumption, but fall back to the original message.
               call ESMF_TimeGet(on_disk_first, timeString=t_str1, _RC)
               call ESMF_TimeGet(on_disk_last,  timeString=t_str2, _RC)
               _FAIL("clim extrapolation requires files spanning a full annual cycle "// &
                     "but on-disk data only covers "//trim(t_str1)//" to "//trim(t_str2)// &
                     " for template: "//trim(this%file_template))
            end if
         end if

         ! --- Direction-aware endpoint check ---
         ! The clim handler uses valid_years(1) when the run is before valid_range,
         ! and valid_years(2) when it is after.  The on-disk contiguous block must
         ! cover valid_range(1) (resp. valid_range(2)) so that the full first (resp.
         ! last) year is available for year-wrapping.  We apply the same one-period
         ! tolerance used in the overlap check.
         if (run_range(2) < this%valid_range(1)) then
            if (on_disk_first > this%valid_range(1) + this%frequency) then
               call ESMF_TimeGet(on_disk_first,       timeString=t_str1, _RC)
               call ESMF_TimeGet(this%valid_range(1), timeString=t_str2, _RC)
               _FAIL("clim extrapolation: run is before valid_range (starts "// &
                     trim(t_str2)//") so year "//trim(t_str2(1:4))//" is needed, "// &
                     "but on-disk data only starts at "//trim(t_str1)// &
                     " for template: "//trim(this%file_template))
            end if
         else if (run_range(1) > this%valid_range(2)) then
            if (on_disk_last < this%valid_range(2) - this%frequency) then
               call ESMF_TimeGet(on_disk_last,        timeString=t_str1, _RC)
               call ESMF_TimeGet(this%valid_range(2), timeString=t_str2, _RC)
               _FAIL("clim extrapolation: run is after valid_range (ends "// &
                     trim(t_str2)//") so year "//trim(t_str2(1:4))//" is needed, "// &
                     "but on-disk data only ends at "//trim(t_str1)// &
                     " for template: "//trim(this%file_template))
            end if
         end if

         ! --- Gap scan: probe every expected file in the target year ---
         ! Checks that the year the clim handler will actually use has no
         ! missing files.  The scan bounds are [Jan 1, Dec 31] of scan_year,
         ! converted to file-series indices using the same abs/rel arithmetic
         ! as refine_valid_range.
         if (do_gap_scan) then
            call ESMF_TimeSet(t_scan_lo, yy=scan_year, mm=1,  dd=1,  h=0,  m=0,  s=0,  _RC)
            call ESMF_TimeSet(t_scan_hi, yy=scan_year, mm=12, dd=31, h=23, m=59, s=59, _RC)

            call ESMF_TimeIntervalGet(this%frequency, s_i8=scan_interval_seconds, _RC)
            if (scan_interval_seconds /= 0) then
               n_scan_lo = (t_scan_lo - this%reff_time) / this%frequency
               if (this%reff_time + n_scan_lo * this%frequency > t_scan_lo) n_scan_lo = n_scan_lo - 1
               n_scan_hi = (t_scan_hi - this%reff_time) / this%frequency
               if (this%reff_time + n_scan_hi * this%frequency > t_scan_hi) n_scan_hi = n_scan_hi - 1
            else
               n_scan = 0
               if (this%reff_time < t_scan_lo) then
                  do while (this%reff_time + (n_scan + 1) * this%frequency <= t_scan_lo)
                     n_scan = n_scan + 1
                  end do
               else
                  do while (this%reff_time + n_scan * this%frequency > t_scan_lo)
                     n_scan = n_scan - 1
                  end do
               end if
               n_scan_lo = n_scan
               n_scan = n_scan_lo
               do while (this%reff_time + (n_scan + 1) * this%frequency <= t_scan_hi)
                  n_scan = n_scan + 1
               end do
               n_scan_hi = n_scan
            end if

            do n_scan = n_scan_lo, n_scan_hi
               t_probe = this%reff_time + n_scan * this%frequency
               call fill_grads_template(probe_filename, this%file_template, time=t_probe, _RC)
               inquire(file=trim(probe_filename), exist=probe_found)
               if (.not. probe_found) then
                  call ESMF_TimeGet(t_probe, timeString=t_str1, _RC)
                  _FAIL("clim extrapolation: file missing within year "// &
                        achar(48 + mod(scan_year/1000,10))// &
                        achar(48 + mod(scan_year/100, 10))// &
                        achar(48 + mod(scan_year/10,  10))// &
                        achar(48 + mod(scan_year,     10))// &
                        " needed for year-wrapping; expected file at "// &
                        trim(t_str1)//" for template: "//trim(this%file_template))
               end if
            end do
         end if

      end select

      _RETURN(_SUCCESS)

   end subroutine check_data_availability

   ! Compute the first (n_lo) and last (n_hi) file-series indices within
   ! the window [t_lo, t_hi] inclusive.
   ! Uses the same abs/rel arithmetic as refine_valid_range.
   subroutine compute_index_bounds(reff_time, frequency, t_lo, t_hi, n_lo, n_hi, rc)
      type(ESMF_Time),         intent(in)  :: reff_time
      type(ESMF_TimeInterval), intent(in)  :: frequency
      type(ESMF_Time),         intent(in)  :: t_lo
      type(ESMF_Time),         intent(in)  :: t_hi
      integer,                 intent(out) :: n_lo
      integer,                 intent(out) :: n_hi
      integer, optional,       intent(out) :: rc

      integer(ESMF_KIND_I8) :: interval_seconds
      integer :: n, status

      call ESMF_TimeIntervalGet(frequency, s_i8=interval_seconds, rc=status)
      if (status /= ESMF_SUCCESS) then
         if (present(rc)) rc = status
         return
      end if

      if (interval_seconds /= 0) then
         n_lo = (t_lo - reff_time) / frequency
         if (reff_time + n_lo * frequency > t_lo) n_lo = n_lo - 1
         n_hi = (t_hi - reff_time) / frequency
         if (reff_time + n_hi * frequency > t_hi) n_hi = n_hi - 1
      else
         n = 0
         if (reff_time < t_lo) then
            do while (reff_time + (n + 1) * frequency <= t_lo)
               n = n + 1
            end do
         else
            do while (reff_time + n * frequency > t_lo)
               n = n - 1
            end do
         end if
         n_lo = n
         n = n_lo
         do while (reff_time + (n + 1) * frequency <= t_hi)
            n = n + 1
         end do
         n_hi = n
      end if

      if (present(rc)) rc = ESMF_SUCCESS

   end subroutine compute_index_bounds

   ! Build an ESMF_HConfig map entry describing all files this dataset requires
   ! for the given run_range and extrap_outside mode.  No filesystem probing is
   ! performed — the list is derived purely from the template, frequency, and ranges.
   !
   ! Output structure (one entry in the manifest sequence):
   !   template:       <file_template>
   !   extrap_outside: <extrap_outside>
   !   run_range:      [<run_range(1)>, <run_range(2)>]
   !   files:
   !     - <expanded filename>
   !     - ...
   !
   ! Enumeration scope per scenario:
   !   "none"            — indices within run_range
   !   "persist_closest" — indices in overlap(run_range, valid_range);
   !                       if run is entirely outside valid_range, just the
   !                       single endpoint file (first or last of valid_range)
   !   "clim" inside     — indices in overlap(run_range, valid_range)
   !   "clim" outside    — indices in the target year (vr_yr1 or vr_yr2)
   subroutine get_required_files_hconfig(this, run_range, extrap_outside, entry_hconfig, unusable, rc)
      class(ExtDataFileStream), intent(in)    :: this
      type(ESMF_Time),          intent(in)    :: run_range(2)
      character(len=*),         intent(in)    :: extrap_outside
      type(ESMF_HConfig),       intent(out)   :: entry_hconfig
      class(KeywordEnforcer),   optional, intent(in)  :: unusable
      integer,                  optional, intent(out) :: rc

      integer :: n, n_lo, n_hi, status
      integer :: vr_yr1, vr_yr2, scan_year
      type(ESMF_Time) :: t_enum_lo, t_enum_hi, t_probe
      type(ESMF_Time) :: overlap_start, overlap_end
      type(ESMF_HConfig) :: files_seq
      character(len=ESMF_MAXPATHLEN) :: probe_filename

      _UNUSED_DUMMY(unusable)

      ! Build the entry map: template, extrap_outside, files.
      entry_hconfig = ESMF_HConfigCreate(content='{}', rc=status)
      _VERIFY(status)

      call ESMF_HConfigAdd(entry_hconfig, this%file_template, addKeyString='template', rc=status)
      _VERIFY(status)
      call ESMF_HConfigAdd(entry_hconfig, trim(extrap_outside), addKeyString='extrap_outside', rc=status)
      _VERIFY(status)

      ! Determine enumeration window based on scenario.
      select case (trim(extrap_outside))

      case ("none")
         ! Enumerate all file indices within run_range.
         t_enum_lo = run_range(1)
         t_enum_hi = run_range(2)

      case ("persist_closest")
         ! Compute overlap of run_range and valid_range.
         if (run_range(1) > this%valid_range(1)) then
            overlap_start = run_range(1)
         else
            overlap_start = this%valid_range(1)
         end if
         if (run_range(2) < this%valid_range(2)) then
            overlap_end = run_range(2)
         else
            overlap_end = this%valid_range(2)
         end if

         if (overlap_start <= overlap_end) then
            ! Run overlaps valid_range — enumerate the overlap.
            t_enum_lo = overlap_start
            t_enum_hi = overlap_end
         else if (run_range(2) < this%valid_range(1)) then
            ! Run is entirely before valid_range — need the first file.
            t_enum_lo = this%valid_range(1)
            t_enum_hi = this%valid_range(1)
         else
            ! Run is entirely after valid_range — need the last file.
            t_enum_lo = this%valid_range(2)
            t_enum_hi = this%valid_range(2)
         end if

      case ("clim")
         ! Compute overlap of run_range and valid_range.
         if (run_range(1) > this%valid_range(1)) then
            overlap_start = run_range(1)
         else
            overlap_start = this%valid_range(1)
         end if
         if (run_range(2) < this%valid_range(2)) then
            overlap_end = run_range(2)
         else
            overlap_end = this%valid_range(2)
         end if

         if (overlap_start <= overlap_end) then
            ! Run overlaps valid_range — enumerate the overlap only.
            t_enum_lo = overlap_start
            t_enum_hi = overlap_end
         else
            ! Run is outside valid_range — enumerate the target year.
            call ESMF_TimeGet(this%valid_range(1), yy=vr_yr1, rc=status)
            _VERIFY(status)
            call ESMF_TimeGet(this%valid_range(2), yy=vr_yr2, rc=status)
            _VERIFY(status)
            if (run_range(2) < this%valid_range(1)) then
               scan_year = vr_yr1
            else
               scan_year = vr_yr2
            end if
            call ESMF_TimeSet(t_enum_lo, yy=scan_year, mm=1,  dd=1,  h=0,  m=0,  s=0,  rc=status)
            _VERIFY(status)
            call ESMF_TimeSet(t_enum_hi, yy=scan_year, mm=12, dd=31, h=23, m=59, s=59, rc=status)
            _VERIFY(status)
         end if

      case default
         ! Unknown extrap_outside — enumerate run_range as a safe default.
         t_enum_lo = run_range(1)
         t_enum_hi = run_range(2)

      end select

      ! Compute index bounds and emit file list.
      call compute_index_bounds(this%reff_time, this%frequency, t_enum_lo, t_enum_hi, n_lo, n_hi, rc=status)
      _VERIFY(status)

      files_seq = ESMF_HConfigCreate(content='[]', rc=status)
      _VERIFY(status)

      do n = n_lo, n_hi
         t_probe = this%reff_time + n * this%frequency
         call fill_grads_template(probe_filename, this%file_template, time=t_probe, rc=status)
         _VERIFY(status)
         call ESMF_HConfigAdd(files_seq, trim(probe_filename), rc=status)
         _VERIFY(status)
      end do

      call ESMF_HConfigAdd(entry_hconfig, content=files_seq, addKeyString='files', rc=status)
      _VERIFY(status)
      call ESMF_HConfigDestroy(files_seq, rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)

   end subroutine get_required_files_hconfig

end module MAPL_ExtDataFileStream

module MAPL_ExtDataFileStreamMap
   use MAPL_ExtDataFileStream

#include "types/key_deferredLengthString.inc"
#define _value type(ExtDataFileStream)
#define _alt

#define _pair ExtDataFileStreamPair
#define _map ExtDataFileStreamMap
#define _iterator ExtDataFileStreamMapIterator

#include "templates/map.inc"

#undef _iterator
#undef _map
#undef _pair

#undef _alt
#undef _value

end module MAPL_ExtDataFileStreamMap
