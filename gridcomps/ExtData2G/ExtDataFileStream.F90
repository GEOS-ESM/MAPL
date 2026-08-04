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

   ! Given a guess for the valid range of a file series, find the actual
   ! first and last file times on the filesystem.  The file series is
   ! defined by reff_time + n * frequency for any integer n, and files are
   ! assumed to form a single contiguous block within the range.
   !
   ! Returns on_disk_first and on_disk_last (timestamps from inside the
   ! first and last files found), and found_any=.false. if no files exist.
   ! valid_range on this is never modified.
   !
   ! Algorithm: probe midpoint then right-quarter point as anchor (O(1));
   ! if both miss, fall back to a linear scan from n_lo.  Once an anchor
   ! (any confirmed file position) is established, binary search on each
   ! monotone half gives O(log N) total filesystem probes for the common
   ! case, with linear fallback only for unusual narrow-block situations.
   subroutine refine_valid_range(this, on_disk_first, on_disk_last, found_any, unusable, rc)
      class(ExtDataFileStream), intent(in)    :: this
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
         ! n_lo: last integer n with reff_time + n*freq <= valid_range(1)
         ! (includes the file whose period covers valid_range(1) even if its
         ! start timestamp is slightly before it).
         ! ESMF division truncates toward zero, which is floor for positive
         ! differences but ceiling for negative; the guard below corrects the
         ! latter so both signs give the floor (i.e. the desired last n).
         n_lo = (this%valid_range(1) - this%reff_time) / this%frequency
         if (this%reff_time + n_lo * this%frequency > this%valid_range(1)) n_lo = n_lo - 1

         ! n_hi: last integer n with reff_time + n*freq <= valid_range(2)
         n_hi = (this%valid_range(2) - this%reff_time) / this%frequency
         if (this%reff_time + n_hi * this%frequency > this%valid_range(2)) n_hi = n_hi - 1
      else

         ! --- Relative interval (months/years): walk to find index bounds ---
         ! n_lo = last n such that reff_time + n*freq <= valid_range(1).
         ! This includes the file whose start is at or just before valid_range(1)
         ! so that files whose period covers valid_range(1) are not skipped.
         ! (Using "first n >= valid_range(1)" would skip that file on any call
         ! where valid_range(1) has been refined to a sub-period timestamp.)
         n = 0
         if (this%reff_time < this%valid_range(1)) then
            do while (this%reff_time + (n + 1) * this%frequency <= this%valid_range(1))
               n = n + 1
            end do
         else
            do while (this%reff_time + n * this%frequency > this%valid_range(1))
               n = n - 1
            end do
         end if
         n_lo = n

         ! Walk forward from n_lo to the last index within valid_range(2).
         n = n_lo
         do while (this%reff_time + (n + 1) * this%frequency <= this%valid_range(2))
            n = n + 1
         end do
         n_hi = n

      end if

      _ASSERT(n_lo <= n_hi, &
         "no candidate file times found within guess valid range for: "//trim(this%file_template))

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
   ! of valid_range, run_range, and extrap_outside.  Calls refine_valid_range
   ! internally to discover what is actually on disk, then applies two checks:
   !
   !   1. Overlap check: if the run period overlaps valid_range, the on-disk
   !      data must cover the full overlap at both endpoints.
   !
   !   2. Extrapolation sufficiency check: if the run period extends outside
   !      valid_range, the extrapolation mode must have enough data to work:
   !        persist_closest — found_any is sufficient (at least one file exists
   !                          to persist from either endpoint).
   !        clim            — on-disk data must span a full annual cycle
   !                          (Jan through Dec within one year, or across a
   !                          year boundary).
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
      logical         :: found_any, has_overlap, full_cycle
      integer         :: yr_first, yr_last, mm_first, mm_last
      integer         :: vr_yr1, vr_yr2
      integer         :: status
      character(len=ESMF_MAXSTR) :: t_str1, t_str2

      _UNUSED_DUMMY(unusable)

      call this%refine_valid_range(on_disk_first, on_disk_last, found_any, _RC)

      if (.not. found_any) then
         call ESMF_TimeGet(this%valid_range(1), timeString=t_str1, _RC)
         call ESMF_TimeGet(this%valid_range(2), timeString=t_str2, _RC)
         _FAIL("No files found within valid_range ["//trim(t_str1)//"/"//trim(t_str2)// &
               "] for template: "//trim(this%file_template))
      end if

      ! Store the on-disk range so handlers can use it for runtime clamping.
      if (allocated(this%on_disk_range)) deallocate(this%on_disk_range)
      allocate(this%on_disk_range(2))
      this%on_disk_range(1) = on_disk_first
      this%on_disk_range(2) = on_disk_last

      ! --- Overlap check ---
      ! Compute intersection of run_range and valid_range.
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
         ! File timestamps are mid-period (e.g. Jan 15 for a monthly file whose
         ! period starts Jan 1), so we allow on_disk_first to be up to one file
         ! period after overlap_start, and on_disk_last to be up to one file
         ! period before overlap_end.  This is the standard bracketing tolerance.
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
      end if

      ! --- Extrapolation sufficiency check ---
      select case (trim(extrap_outside))

      case ("persist_closest")
         ! found_any is sufficient: at least one file exists to persist from.
         ! (Already verified above.)

      case ("clim")
         ! Require files spanning a full annual cycle: either Jan-Dec within
         ! one year, or across a year boundary (yr_last > yr_first).
         call ESMF_TimeGet(on_disk_first, yy=yr_first, mm=mm_first, _RC)
         call ESMF_TimeGet(on_disk_last,  yy=yr_last,  mm=mm_last,  _RC)
         full_cycle = (yr_last > yr_first) .or. &
                      (yr_last == yr_first .and. mm_first == 1 .and. mm_last == 12)
         if (.not. full_cycle) then
            call ESMF_TimeGet(on_disk_first, timeString=t_str1, _RC)
            call ESMF_TimeGet(on_disk_last,  timeString=t_str2, _RC)
            _FAIL("clim extrapolation requires files spanning a full annual cycle "// &
                  "but on-disk data only covers "//trim(t_str1)//" to "//trim(t_str2)// &
                  " for template: "//trim(this%file_template))
         end if

         ! Direction-aware check: the clim handler uses valid_years(1) when the
         ! run is before valid_range, and valid_years(2) when it is after.
         ! The on-disk contiguous block must include the year that will actually
         ! be used; checking that on_disk_first/last fall in the right year is
         ! sufficient given the contiguous-block assumption in refine_valid_range.
         call ESMF_TimeGet(this%valid_range(1), yy=vr_yr1, _RC)
         call ESMF_TimeGet(this%valid_range(2), yy=vr_yr2, _RC)

         if (run_range(2) < this%valid_range(1)) then
            ! Run is entirely before valid_range: clim handler will use valid_years(1).
            ! on_disk_first must be in vr_yr1 so the full contiguous block covers it.
            if (yr_first /= vr_yr1) then
               call ESMF_TimeGet(on_disk_first,       timeString=t_str1, _RC)
               call ESMF_TimeGet(this%valid_range(1), timeString=t_str2, _RC)
               _FAIL("clim extrapolation: run is before valid_range (starts "// &
                     trim(t_str2)//") so year "//trim(t_str2(1:4))//" is needed, "// &
                     "but on-disk data only starts at "//trim(t_str1)// &
                     " for template: "//trim(this%file_template))
            end if
         else if (run_range(1) > this%valid_range(2)) then
            ! Run is entirely after valid_range: clim handler will use valid_years(2).
            ! on_disk_last must be in vr_yr2 so the full contiguous block covers it.
            if (yr_last /= vr_yr2) then
               call ESMF_TimeGet(on_disk_last,        timeString=t_str1, _RC)
               call ESMF_TimeGet(this%valid_range(2), timeString=t_str2, _RC)
               _FAIL("clim extrapolation: run is after valid_range (ends "// &
                     trim(t_str2)//") so year "//trim(t_str2(1:4))//" is needed, "// &
                     "but on-disk data only ends at "//trim(t_str1)// &
                     " for template: "//trim(this%file_template))
            end if
         end if

      end select

      _RETURN(_SUCCESS)

   end subroutine check_data_availability

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
