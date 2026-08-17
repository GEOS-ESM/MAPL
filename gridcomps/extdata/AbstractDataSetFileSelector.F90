#include "MAPL.h"

module mapl_AbstractDataSetFileSelector_mod

   use ESMF
   use MAPL
   use mapl_DataSetBracket_mod
   use mapl_ExtDataConstants_mod

   implicit none(type,external)
   private

   public AbstractDataSetFileSelector
   public NUM_SEARCH_TRIES

   integer, parameter :: MAX_TRIALS = 10
   integer, parameter :: NUM_SEARCH_TRIES = 1

   type, abstract :: AbstractDataSetFileSelector
      character(:), allocatable :: file_template
      type(ESMF_TimeInterval)  :: file_frequency
      type(ESMF_Time) :: ref_time
      type(ESMF_Time), allocatable :: valid_range(:)
      type(ESMF_Time), allocatable :: on_disk_range(:)  ! actual first/last file timestamps, set by check_data_availability
      type(ESMF_Time), allocatable :: last_updated
      type(ESMF_TimeInterval), allocatable :: timeStep
      integer :: collection_id
      logical :: single_file = .false.
   contains
      procedure :: find_any_file
      procedure :: compute_trial_time
      procedure :: compute_time_at_index
      procedure :: check_data_availability
      procedure :: get_required_files_hconfig
      procedure :: set_last_update
      procedure :: detect_time_flow
      procedure :: get_dataset_metadata
      procedure :: get_file_template
      procedure :: get_valid_range_single_file
      procedure, private :: refine_valid_range
      procedure, private :: compute_index_bounds
      procedure(I_update_file_bracket), deferred :: update_file_bracket
   end type AbstractDataSetFileSelector

   abstract interface
      subroutine I_update_file_bracket(this, bundle, current_time, bracket, rc)
         use ESMF, only: ESMF_Time, ESMF_FieldBundle
         use mapl_DataSetBracket_mod
         import AbstractDataSetFileSelector
         class(AbstractDataSetFileSelector), intent(inout) :: this
         type(ESMF_FieldBundle), intent(inout) :: bundle
         type(ESMF_Time), intent(in) :: current_time
         type(DataSetBracket), intent(inout) :: bracket
         integer, optional, intent(out) :: rc
      end subroutine I_update_file_bracket
   end interface

contains

   function find_any_file(this, current_time, rc) result(filename)
      character(len=:), allocatable :: filename
      class(AbstractDataSetFileSelector), intent(inout) :: this
      type(ESMF_Time), intent(in) :: current_time
      integer, optional, intent(out) :: rc

      integer :: status, i
      type(ESMF_Time) :: useable_time
      character(len=ESMF_MAXPATHLEN) :: trial_file
      logical :: file_found

      filename = file_not_found

      ! First try the current time directly.
      useable_time = current_time
      call mapl_fill_grads_template(trial_file, this%file_template, time=useable_time, _RC)
      inquire(file=trim(trial_file),exist=file_found)
      if (file_found) then
         filename = trial_file
         _RETURN(_SUCCESS)
      end if

      ! If on_disk_range is set, probe both endpoints immediately.
      if (allocated(this%on_disk_range)) then
         ! Try on_disk_range(2) first — covers sparse datasets where only the
         ! endpoint file exists (e.g. persist_closest run outside valid_range).
         call mapl_fill_grads_template(trial_file, this%file_template, time=this%on_disk_range(2), _RC)
         inquire(file=trim(trial_file), exist=file_found)
         if (file_found) then
            filename = trial_file
            _RETURN(_SUCCESS)
         end if
         useable_time = this%on_disk_range(1)
      else
         useable_time = this%ref_time
      end if

      ! Forward scan.
      do i=0, MAX_TRIALS
         call mapl_fill_grads_template(trial_file, this%file_template, time=useable_time, _RC)
         useable_time = useable_time + this%file_frequency
         inquire(file=trim(trial_file),exist=file_found)
         if (file_found) then
            filename = trial_file
            _RETURN(_SUCCESS)
         end if
      enddo

      ! Forward scan missed; if valid_range is set, try scanning backward from
      ! valid_range(2) — handles sparse datasets where data is only near the end.
      if (.not. file_found .and. allocated(this%valid_range)) then
         useable_time = this%valid_range(2)
         do i=1, MAX_TRIALS
            call mapl_fill_grads_template(trial_file, this%file_template, time=useable_time, _RC)
            inquire(file=trim(trial_file),exist=file_found)
            if (file_found) then
               filename = trial_file
               _RETURN(_SUCCESS)
            end if
            useable_time = useable_time - this%file_frequency
         enddo
      end if

      _FAIL("could not find a file")
   end function find_any_file

   function get_dataset_metadata(this, current_time, rc) result(metadata)
      type(FileMetadataUtils), pointer :: metadata
      class(AbstractDataSetFileSelector), intent(inout) :: this
      type(ESMF_Time), intent(in) :: current_time
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: filename
      integer :: status
      type(mapl_DataCollection), pointer :: collection

      filename = this%find_any_file(current_time, _RC)
      collection => mapl_DataCollections%at(this%collection_id)
      metadata => collection%find(filename, _RC)

      _RETURN(_SUCCESS)
   end function get_dataset_metadata

   function compute_trial_time(this, target_time, shift, rc) result(trial_time)
      type(ESMF_Time) :: trial_time
      class(AbstractDataSetFileSelector), intent(inout) :: this
      type(ESMF_Time), intent(in) :: target_time
      integer, intent(in) :: shift
      integer, optional, intent(out) :: rc

      integer :: status, n
      integer(ESMF_KIND_I8) :: int_sec

      if (this%single_file) then
         trial_time = target_time
         _RETURN(_SUCCESS)
      end if

      call ESMF_TimeIntervalGet(this%file_frequency, s_i8=int_sec, _RC)
      if (int_sec == 0) then
         trial_time = this%ref_time
         do while(trial_time <= target_time)
            trial_time = trial_time + this%file_frequency
         enddo
         trial_time = trial_time - this%file_frequency + shift*this%file_frequency
      else
         n = (target_time-this%ref_time)/this%file_frequency
         trial_time = this%ref_time+(n+shift)*this%file_frequency
      end if

      _RETURN(_SUCCESS)
   end function compute_trial_time

   subroutine set_last_update(this, update_time, rc)
      class(AbstractDataSetFileSelector), intent(inout) :: this
      type(ESMF_Time), intent(in) :: update_time
      integer, optional, intent(out) :: rc

      this%last_updated = update_time

      _RETURN(_SUCCESS)
   end subroutine set_last_update

   function detect_time_flow(this, current_time, rc) result(time_jumped)
      logical :: time_jumped
      class(AbstractDataSetFileSelector), intent(inout) :: this
      type(ESMF_Time), intent(in) :: current_time
      integer, optional, intent(inout) :: rc

      integer :: status
      type(ESMF_TimeInterval) :: time_interval
      integer(ESMF_KIND_I8) :: f1, f2

      time_jumped = .false.
      _RETURN_UNLESS(allocated(this%last_updated) .and. allocated(this%timeStep))
      time_interval = current_time - this%last_updated
      call ESMF_TimeIntervalGet(time_interval, s_i8=f1, _RC)
      call ESMF_TimeIntervalGet(this%timeStep, s_i8=f2, _RC)
      time_jumped = abs(f1) > f2

      _RETURN(_SUCCESS)
   end function detect_time_flow

   subroutine get_file_template(this, file_template)
      class(AbstractDataSetFileSelector), intent(in) :: this
      character(len=:), allocatable :: file_template

      if (allocated(this%file_template)) file_template = this%file_template
   end subroutine get_file_template

   subroutine get_valid_range_single_file(this, rc)
      class(AbstractDataSetFileSelector), intent(inout) :: this
      integer, intent(out), optional :: rc

      type(mapl_DataCollection), pointer :: collection
      type(FileMetadataUtils), pointer :: metadata
      type(ESMF_Time), allocatable :: time_series(:)
      integer :: status

      allocate(this%valid_range(2), _STAT)
      collection => mapl_DataCollections%at(this%collection_id)
      metadata => collection%find(this%file_template)
      call metadata%get_time_info(timeVector=time_series, _RC)
      this%valid_range(1)=time_series(1)
      this%valid_range(2)=time_series(size(time_series))

      _RETURN(_SUCCESS)
   end subroutine get_valid_range_single_file

   ! Returns the time corresponding to index n in the file series:
   !   ref_time + n * file_frequency
   ! For absolute intervals (representable in seconds) uses integer arithmetic.
   ! For relative intervals (months/years) walks from ref_time.
   function compute_time_at_index(this, n, rc) result(t)
      type(ESMF_Time) :: t
      class(AbstractDataSetFileSelector), intent(in) :: this
      integer, intent(in) :: n
      integer, optional, intent(out) :: rc

      integer :: status, i
      integer(ESMF_KIND_I8) :: int_sec

      call ESMF_TimeIntervalGet(this%file_frequency, s_i8=int_sec, _RC)
      if (int_sec /= 0) then
         t = this%ref_time + n * this%file_frequency
      else
         t = this%ref_time
         if (n >= 0) then
            do i = 1, n
               t = t + this%file_frequency
            end do
         else
            do i = -1, n, -1
               t = t - this%file_frequency
            end do
         end if
      end if

      _RETURN(_SUCCESS)
   end function compute_time_at_index

   ! Compute the first (n_lo) and last (n_hi) file-series indices within
   ! the window [t_lo, t_hi] inclusive.
   ! Uses the same abs/rel arithmetic as refine_valid_range.
   subroutine compute_index_bounds(this, t_lo, t_hi, n_lo, n_hi, rc)
      class(AbstractDataSetFileSelector), intent(in) :: this
      type(ESMF_Time), intent(in)  :: t_lo
      type(ESMF_Time), intent(in)  :: t_hi
      integer,         intent(out) :: n_lo
      integer,         intent(out) :: n_hi
      integer, optional, intent(out) :: rc

      integer(ESMF_KIND_I8) :: interval_seconds
      integer :: n, status
      type(ESMF_Time) :: t_probe

      call ESMF_TimeIntervalGet(this%file_frequency, s_i8=interval_seconds, rc=status)
      _VERIFY(status)

      if (interval_seconds /= 0) then
         n_lo = (t_lo - this%ref_time) / this%file_frequency
         t_probe = this%compute_time_at_index(n_lo, _RC)
         if (t_probe > t_lo) n_lo = n_lo - 1
         n_hi = (t_hi - this%ref_time) / this%file_frequency
         t_probe = this%compute_time_at_index(n_hi, _RC)
         if (t_probe > t_hi) n_hi = n_hi - 1
      else
         n = 0
         if (this%ref_time < t_lo) then
            t_probe = this%compute_time_at_index(n + 1, _RC)
            do while (t_probe <= t_lo)
               n = n + 1
               t_probe = this%compute_time_at_index(n + 1, _RC)
            end do
         else
            t_probe = this%compute_time_at_index(n, _RC)
            do while (t_probe > t_lo)
               n = n - 1
               t_probe = this%compute_time_at_index(n, _RC)
            end do
         end if
         n_lo = n
         n = n_lo
         t_probe = this%compute_time_at_index(n + 1, _RC)
         do while (t_probe <= t_hi)
            n = n + 1
            t_probe = this%compute_time_at_index(n + 1, _RC)
         end do
         n_hi = n
      end if

      if (present(rc)) rc = ESMF_SUCCESS

   end subroutine compute_index_bounds

   ! Probes the filesystem to find the first and last files that actually exist
   ! within scan_range.  Returns on_disk_first and on_disk_last (internal
   ! timestamps from file metadata), and found_any=.false. if no files exist.
   !
   ! valid_range on this is never modified.
   !
   ! Uses a 2-probe anchor strategy then binary search on each monotone half,
   ! requiring O(log N) filesystem probes for the common contiguous-block case.
   subroutine refine_valid_range(this, scan_range, on_disk_first, on_disk_last, found_any, rc)
      class(AbstractDataSetFileSelector), intent(in) :: this
      type(ESMF_Time), intent(in)  :: scan_range(2)
      type(ESMF_Time), intent(out) :: on_disk_first
      type(ESMF_Time), intent(out) :: on_disk_last
      logical, intent(out) :: found_any
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: n_lo, n_hi, n_first, n_last, n_mid, n_anchor, lo, hi, n
      logical :: file_found
      type(ESMF_Time) :: t_probe
      type(ESMF_Time), allocatable :: time_series(:)
      character(len=ESMF_MAXPATHLEN) :: filename
      type(mapl_DataCollection), pointer :: collection
      type(FileMetadataUtils), pointer :: file_metadata

      found_any = .false.

      call this%compute_index_bounds(scan_range(1), scan_range(2), n_lo, n_hi, _RC)

      _ASSERT(n_lo <= n_hi, &
         "no candidate file times found within scan range for: "//trim(this%file_template))

      ! === Phase 1: Find anchor — up to 2 probes, then linear scan fallback ===
      n_anchor = (n_lo + n_hi) / 2
      t_probe = this%compute_time_at_index(n_anchor, _RC)
      call mapl_fill_grads_template(filename, this%file_template, time=t_probe, _RC)
      inquire(file=trim(filename), exist=file_found)

      if (.not. file_found) then
         n_anchor = (n_anchor + n_hi) / 2
         t_probe = this%compute_time_at_index(n_anchor, _RC)
         call mapl_fill_grads_template(filename, this%file_template, time=t_probe, _RC)
         inquire(file=trim(filename), exist=file_found)
      end if

      if (.not. file_found) then
         do n = n_lo, n_hi
            t_probe = this%compute_time_at_index(n, _RC)
            call mapl_fill_grads_template(filename, this%file_template, time=t_probe, _RC)
            inquire(file=trim(filename), exist=file_found)
            if (file_found) then
               n_first = n
               exit
            end if
         end do
         if (.not. file_found) then
            ! No files anywhere in scan range.
            _RETURN(_SUCCESS)
         end if
         n_anchor = n_first
      end if

      ! === Phase 2: Binary search for n_first in [n_lo, n_anchor] ===
      lo = n_lo
      hi = n_anchor
      do while (lo < hi)
         n_mid = (lo + hi) / 2
         t_probe = this%compute_time_at_index(n_mid, _RC)
         call mapl_fill_grads_template(filename, this%file_template, time=t_probe, _RC)
         inquire(file=trim(filename), exist=file_found)
         if (file_found) then
            hi = n_mid
         else
            lo = n_mid + 1
         end if
      end do
      n_first = lo

      ! === Phase 3: Binary search for n_last in [n_anchor, n_hi] ===
      lo = n_anchor
      hi = n_hi
      do while (lo < hi)
         n_mid = (lo + hi + 1) / 2
         t_probe = this%compute_time_at_index(n_mid, _RC)
         call mapl_fill_grads_template(filename, this%file_template, time=t_probe, _RC)
         inquire(file=trim(filename), exist=file_found)
         if (file_found) then
            lo = n_mid
         else
            hi = n_mid - 1
         end if
      end do
      n_last = lo

      ! Read first file's earliest internal timestamp.
      collection => mapl_DataCollections%at(this%collection_id)

      t_probe = this%compute_time_at_index(n_first, _RC)
      call mapl_fill_grads_template(filename, this%file_template, time=t_probe, _RC)
      file_metadata => collection%find(trim(filename), _RC)
      call file_metadata%get_time_info(timeVector=time_series, _RC)
      on_disk_first = time_series(1)
      deallocate(time_series)

      ! Read last file's latest internal timestamp.
      t_probe = this%compute_time_at_index(n_last, _RC)
      call mapl_fill_grads_template(filename, this%file_template, time=t_probe, _RC)
      file_metadata => collection%find(trim(filename), _RC)
      call file_metadata%get_time_info(timeVector=time_series, _RC)
      on_disk_last = time_series(size(time_series))

      found_any = .true.
      _RETURN(_SUCCESS)
   end subroutine refine_valid_range

   ! Validates that sufficient files exist on disk for the combination of
   ! valid_range, run_range, and extrap_outside.
   !
   ! Three scenarios:
   !
   !   1. extrap_outside = "none"
   !      Scan window = run_range (valid_range not required).
   !      If valid_range is set and run_range extends outside it -> config error.
   !      On-disk data must cover the full run_range.
   !
   !   2. extrap_outside = "persist_closest"
   !      Scan window = valid_range (required).
   !      Overlap: must cover the full overlap.  Outside: found_any is sufficient.
   !
   !   3. extrap_outside = "clim"
   !      Scan window = valid_range (required).
   !      Overlap: must cover the full overlap.
   !      Outside: full-cycle + direction-aware endpoint + gap-scan checks.
   !
   ! valid_range on this is never modified.
   subroutine check_data_availability(this, run_range, extrap_outside, unusable, rc)
      class(AbstractDataSetFileSelector), intent(inout) :: this
      type(ESMF_Time), intent(in) :: run_range(2)
      character(len=*), intent(in) :: extrap_outside
      class(mapl_KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ESMF_Time) :: on_disk_first, on_disk_last
      type(ESMF_Time) :: overlap_start, overlap_end
      type(ESMF_Time) :: scan_range(2)
      logical :: found_any, has_overlap, full_cycle
      integer :: yr_first, yr_last, mm_first, mm_last
      integer :: vr_yr1, vr_yr2, scan_year
      integer :: n_scan_lo, n_scan_hi, n_scan, n_missing
      integer(ESMF_KIND_I8) :: scan_interval_seconds
      type(ESMF_Time) :: t_scan_lo, t_scan_hi, t_probe
      logical :: do_gap_scan, probe_found
      character(len=ESMF_MAXPATHLEN) :: probe_filename
      character(len=ESMF_MAXSTR)     :: t_missing
      character(len=:), allocatable  :: missing_list
      integer :: status
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

         if (allocated(this%valid_range)) then
            if (run_range(1) < this%valid_range(1) .or. run_range(2) > this%valid_range(2)) then
               call ESMF_TimeGet(run_range(1),        timeString=t_str1, _RC)
               call ESMF_TimeGet(run_range(2),        timeString=t_str2, _RC)
               call ESMF_TimeGet(this%valid_range(1), timeString=t_missing, _RC)
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

         if (allocated(this%on_disk_range)) deallocate(this%on_disk_range)
         allocate(this%on_disk_range(2))
         this%on_disk_range(1) = on_disk_first
         this%on_disk_range(2) = on_disk_last

         if (on_disk_first > run_range(1) + this%file_frequency) then
            call ESMF_TimeGet(on_disk_first, timeString=t_str1, _RC)
            call ESMF_TimeGet(run_range(1),  timeString=t_str2, _RC)
            _FAIL("extrap_outside=none: on-disk data starts at "//trim(t_str1)// &
                  " but data is required from "//trim(t_str2)// &
                  " for template: "//trim(this%file_template))
         end if
         if (on_disk_last < run_range(2) - this%file_frequency) then
            call ESMF_TimeGet(on_disk_last, timeString=t_str1, _RC)
            call ESMF_TimeGet(run_range(2), timeString=t_str2, _RC)
            _FAIL("extrap_outside=none: on-disk data ends at "//trim(t_str1)// &
                  " but data is required through "//trim(t_str2)// &
                  " for template: "//trim(this%file_template))
         end if

      ! -----------------------------------------------------------------------
      case ("persist_closest")
      ! -----------------------------------------------------------------------
      ! Scenario 2: persistence extrapolation.  Scan valid_range.

         scan_range = this%valid_range
         call this%refine_valid_range(scan_range, on_disk_first, on_disk_last, found_any, _RC)

         if (.not. found_any) then
            call ESMF_TimeGet(this%valid_range(1), timeString=t_str1, _RC)
            call ESMF_TimeGet(this%valid_range(2), timeString=t_str2, _RC)
            _FAIL("No files found within valid_range ["//trim(t_str1)//"/"//trim(t_str2)// &
                  "] for template: "//trim(this%file_template))
         end if

         if (allocated(this%on_disk_range)) deallocate(this%on_disk_range)
         allocate(this%on_disk_range(2))
         this%on_disk_range(1) = on_disk_first
         this%on_disk_range(2) = on_disk_last

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
            if (on_disk_first > overlap_start + this%file_frequency) then
               call ESMF_TimeGet(on_disk_first, timeString=t_str1, _RC)
               call ESMF_TimeGet(overlap_start, timeString=t_str2, _RC)
               _FAIL("Run period overlaps valid_range but on-disk data starts at "// &
                     trim(t_str1)//". Data is required from "//trim(t_str2)// &
                     " for template: "//trim(this%file_template))
            end if
             if (on_disk_last < overlap_end - this%file_frequency) then
                call ESMF_TimeGet(on_disk_last, timeString=t_str1, _RC)
                call ESMF_TimeGet(overlap_end,  timeString=t_str2, _RC)
                _FAIL("Run period overlaps valid_range but on-disk data ends at "// &
                      trim(t_str1)//". Data is required through "//trim(t_str2)// &
                      " for template: "//trim(this%file_template))
             end if
             ! Gap scan: probe every expected file slot within the overlap window
             call this%compute_index_bounds(overlap_start, overlap_end, n_scan_lo, n_scan_hi, _RC)
             n_missing    = 0
             missing_list = ""
             do n_scan = n_scan_lo, n_scan_hi
                t_probe = this%compute_time_at_index(n_scan, _RC)
                call mapl_fill_grads_template(probe_filename, this%file_template, time=t_probe, _RC)
                inquire(file=trim(probe_filename), exist=probe_found)
                if (.not. probe_found) then
                   call ESMF_TimeGet(t_probe, timeString=t_missing, _RC)
                   missing_list = missing_list // new_line('a') // "  " // trim(t_missing)
                   n_missing = n_missing + 1
                end if
             end do
             if (n_missing > 0) then
                _FAIL("Run period overlaps valid_range but the following files are missing" // &
                      " for template: "//trim(this%file_template)//":"//missing_list)
             end if
          end if
          ! Run outside valid_range: found_any already verified — sufficient
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

         if (allocated(this%on_disk_range)) deallocate(this%on_disk_range)
         allocate(this%on_disk_range(2))
         this%on_disk_range(1) = on_disk_first
         this%on_disk_range(2) = on_disk_last

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
            if (on_disk_first > overlap_start + this%file_frequency) then
               call ESMF_TimeGet(on_disk_first, timeString=t_str1, _RC)
               call ESMF_TimeGet(overlap_start, timeString=t_str2, _RC)
               _FAIL("Run period overlaps valid_range but on-disk data starts at "// &
                     trim(t_str1)//". Data is required from "//trim(t_str2)// &
                     " for template: "//trim(this%file_template))
            end if
            if (on_disk_last < overlap_end - this%file_frequency) then
               call ESMF_TimeGet(on_disk_last, timeString=t_str1, _RC)
               call ESMF_TimeGet(overlap_end,  timeString=t_str2, _RC)
               _FAIL("Run period overlaps valid_range but on-disk data ends at "// &
                     trim(t_str1)//". Data is required through "//trim(t_str2)// &
                      " for template: "//trim(this%file_template))
             end if
             ! Gap scan: probe every expected file slot within the overlap window
             call this%compute_index_bounds(overlap_start, overlap_end, n_scan_lo, n_scan_hi, _RC)
             n_missing    = 0
             missing_list = ""
             do n_scan = n_scan_lo, n_scan_hi
                t_probe = this%compute_time_at_index(n_scan, _RC)
                call mapl_fill_grads_template(probe_filename, this%file_template, time=t_probe, _RC)
                inquire(file=trim(probe_filename), exist=probe_found)
                if (.not. probe_found) then
                   call ESMF_TimeGet(t_probe, timeString=t_missing, _RC)
                   missing_list = missing_list // new_line('a') // "  " // trim(t_missing)
                   n_missing = n_missing + 1
                end if
             end do
             if (n_missing > 0) then
                _FAIL("Run period overlaps valid_range but the following files are missing" // &
                      " for template: "//trim(this%file_template)//":"//missing_list)
             end if
          end if

          call ESMF_TimeGet(on_disk_first, yy=yr_first, mm=mm_first, _RC)
         call ESMF_TimeGet(on_disk_last,  yy=yr_last,  mm=mm_last,  _RC)
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
         full_cycle = (yr_last > yr_first) .or. &
                      (yr_last == yr_first .and. mm_first == 1 .and. mm_last == 12)
         if (.not. full_cycle) then
            if (do_gap_scan) then
               call ESMF_TimeSet(t_scan_lo, yy=scan_year, mm=1,  dd=1,  h=0,  m=0,  s=0,  _RC)
               call ESMF_TimeSet(t_scan_hi, yy=scan_year, mm=12, dd=31, h=23, m=59, s=59, _RC)
            else
               t_scan_lo = this%valid_range(1)
               t_scan_hi = this%valid_range(2)
            end if

            call this%compute_index_bounds(t_scan_lo, t_scan_hi, n_scan_lo, n_scan_hi, _RC)

            n_missing    = 0
            missing_list = ""
            do n_scan = n_scan_lo, n_scan_hi
               t_probe = this%compute_time_at_index(n_scan, _RC)
               call mapl_fill_grads_template(probe_filename, this%file_template, time=t_probe, _RC)
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
               call ESMF_TimeGet(on_disk_first, timeString=t_str1, _RC)
               call ESMF_TimeGet(on_disk_last,  timeString=t_str2, _RC)
               _FAIL("clim extrapolation requires files spanning a full annual cycle "// &
                     "but on-disk data only covers "//trim(t_str1)//" to "//trim(t_str2)// &
                     " for template: "//trim(this%file_template))
            end if
         end if

         ! --- Direction-aware endpoint check ---
         if (run_range(2) < this%valid_range(1)) then
            if (on_disk_first > this%valid_range(1) + this%file_frequency) then
               call ESMF_TimeGet(on_disk_first,       timeString=t_str1, _RC)
               call ESMF_TimeGet(this%valid_range(1), timeString=t_str2, _RC)
               _FAIL("clim extrapolation: run is before valid_range (starts "// &
                     trim(t_str2)//") so year "//trim(t_str2(1:4))//" is needed, "// &
                     "but on-disk data only starts at "//trim(t_str1)// &
                     " for template: "//trim(this%file_template))
            end if
         else if (run_range(1) > this%valid_range(2)) then
            if (on_disk_last < this%valid_range(2) - this%file_frequency) then
               call ESMF_TimeGet(on_disk_last,        timeString=t_str1, _RC)
               call ESMF_TimeGet(this%valid_range(2), timeString=t_str2, _RC)
               _FAIL("clim extrapolation: run is after valid_range (ends "// &
                     trim(t_str2)//") so year "//trim(t_str2(1:4))//" is needed, "// &
                     "but on-disk data only ends at "//trim(t_str1)// &
                     " for template: "//trim(this%file_template))
            end if
         end if

         ! --- Gap scan ---
         if (do_gap_scan) then
            call ESMF_TimeSet(t_scan_lo, yy=scan_year, mm=1,  dd=1,  h=0,  m=0,  s=0,  _RC)
            call ESMF_TimeSet(t_scan_hi, yy=scan_year, mm=12, dd=31, h=23, m=59, s=59, _RC)

            call this%compute_index_bounds(t_scan_lo, t_scan_hi, n_scan_lo, n_scan_hi, _RC)

            do n_scan = n_scan_lo, n_scan_hi
               t_probe = this%compute_time_at_index(n_scan, _RC)
               call mapl_fill_grads_template(probe_filename, this%file_template, time=t_probe, _RC)
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

   ! Build an ESMF_HConfig map entry describing all files this dataset requires
   ! for the given run_range and extrap_outside mode.  No filesystem probing is
   ! performed — the list is derived purely from template, frequency, and ranges.
   !
   ! Output structure:
   !   template:       <file_template>
   !   extrap_outside: <extrap_outside>
   !   files:
   !     - <expanded filename>
   !     - ...
   !
   ! Enumeration scope per scenario:
   !   "none"            — indices within run_range
   !   "persist_closest" — indices in overlap(run_range, valid_range);
   !                       if run entirely outside valid_range, just endpoint file
   !   "clim" inside     — indices in overlap(run_range, valid_range)
   !   "clim" outside    — indices in target year (vr_yr1 or vr_yr2)
   subroutine get_required_files_hconfig(this, run_range, extrap_outside, entry_hconfig, unusable, rc)
      class(AbstractDataSetFileSelector), intent(in)  :: this
      type(ESMF_Time),                    intent(in)  :: run_range(2)
      character(len=*),                   intent(in)  :: extrap_outside
      type(ESMF_HConfig),                 intent(out) :: entry_hconfig
      class(mapl_KeywordEnforcer), optional, intent(in)  :: unusable
      integer,                     optional, intent(out) :: rc

      integer :: n, n_lo, n_hi, status
      integer :: vr_yr1, vr_yr2, scan_year
      type(ESMF_Time) :: t_enum_lo, t_enum_hi, t_probe
      type(ESMF_Time) :: overlap_start, overlap_end
      type(ESMF_HConfig) :: files_seq
      character(len=ESMF_MAXPATHLEN) :: probe_filename

      _UNUSED_DUMMY(unusable)

      entry_hconfig = ESMF_HConfigCreate(content='{}', rc=status)
      _VERIFY(status)

      call ESMF_HConfigAdd(entry_hconfig, this%file_template, addKeyString='template', rc=status)
      _VERIFY(status)
      call ESMF_HConfigAdd(entry_hconfig, trim(extrap_outside), addKeyString='extrap_outside', rc=status)
      _VERIFY(status)

      select case (trim(extrap_outside))

      case ("none")
         t_enum_lo = run_range(1)
         t_enum_hi = run_range(2)

      case ("persist_closest")
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
            t_enum_lo = overlap_start
            t_enum_hi = overlap_end
         else if (run_range(2) < this%valid_range(1)) then
            t_enum_lo = this%valid_range(1)
            t_enum_hi = this%valid_range(1)
         else
            t_enum_lo = this%valid_range(2)
            t_enum_hi = this%valid_range(2)
         end if

      case ("clim")
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
            t_enum_lo = overlap_start
            t_enum_hi = overlap_end
         else
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
         t_enum_lo = run_range(1)
         t_enum_hi = run_range(2)

      end select

      call this%compute_index_bounds(t_enum_lo, t_enum_hi, n_lo, n_hi, rc=status)
      _VERIFY(status)

      files_seq = ESMF_HConfigCreate(content='[]', rc=status)
      _VERIFY(status)

      do n = n_lo, n_hi
         t_probe = this%compute_time_at_index(n, rc=status)
         _VERIFY(status)
         call mapl_fill_grads_template(probe_filename, this%file_template, time=t_probe, rc=status)
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

end module mapl_AbstractDataSetFileSelector_mod
