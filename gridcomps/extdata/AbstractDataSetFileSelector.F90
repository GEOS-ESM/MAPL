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
      procedure :: set_last_update
      procedure :: detect_time_flow
      procedure :: get_dataset_metadata
      procedure :: get_file_template
      procedure :: get_valid_range_single_file
      procedure, private :: refine_valid_range
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
      useable_time = current_time
      call mapl_fill_grads_template(trial_file, this%file_template, time=useable_time, _RC)
      inquire(file=trim(trial_file),exist=file_found)
      if (file_found) then
         filename = trial_file
         _RETURN(_SUCCESS)
      end if
      useable_time = this%ref_time
      do i=0, MAX_TRIALS
         call mapl_fill_grads_template(trial_file, this%file_template, time=useable_time, _RC)
         useable_time = useable_time + this%file_frequency
         inquire(file=trim(trial_file),exist=file_found)
         if (file_found) then
            filename = trial_file
            _RETURN(_SUCCESS)
         end if
      enddo

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
   ! This is a direct index probe, unlike compute_trial_time which derives n
   ! from a target_time first.
   function compute_time_at_index(this, n, rc) result(t)
      type(ESMF_Time) :: t
      class(AbstractDataSetFileSelector), intent(in) :: this
      integer, intent(in) :: n
      integer, optional, intent(out) :: rc

      integer :: status, i
      integer(ESMF_KIND_I8) :: int_sec

      call ESMF_TimeIntervalGet(this%file_frequency, s_i8=int_sec, _RC)
      if (int_sec /= 0) then
         ! Absolute interval: simple arithmetic.
         t = this%ref_time + n * this%file_frequency
      else
         ! Relative interval (months/years): walk from ref_time.
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

   ! Probes the filesystem to find the first and last files that actually exist
   ! within valid_range.  Returns on_disk_first and on_disk_last (internal
   ! timestamps from file metadata), and found_any=.false. if no files exist.
   !
   ! Uses a 2-probe anchor strategy then binary search on each monotone half,
   ! requiring O(log N) filesystem probes for the common contiguous-block case.
   subroutine refine_valid_range(this, on_disk_first, on_disk_last, found_any, rc)
      class(AbstractDataSetFileSelector), intent(in) :: this
      type(ESMF_Time), intent(out) :: on_disk_first
      type(ESMF_Time), intent(out) :: on_disk_last
      logical, intent(out) :: found_any
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: n_lo, n_hi, n_first, n_last, n_mid, n_anchor, lo, hi, n
      integer(ESMF_KIND_I8) :: interval_seconds
      logical :: file_found
      type(ESMF_Time) :: t_probe
      type(ESMF_Time), allocatable :: time_series(:)
      character(len=ESMF_MAXPATHLEN) :: filename
      type(mapl_DataCollection), pointer :: collection
      type(FileMetadataUtils), pointer :: file_metadata

      found_any = .false.

      call ESMF_TimeIntervalGet(this%file_frequency, s_i8=interval_seconds, _RC)

      if (interval_seconds /= 0) then
         ! Absolute interval: use integer division to find index bounds.
         ! n_lo: last integer n with ref_time + n*freq <= valid_range(1)
         n_lo = (this%valid_range(1) - this%ref_time) / this%file_frequency
         t_probe = this%compute_time_at_index(n_lo, _RC)
         if (t_probe > this%valid_range(1)) n_lo = n_lo - 1

         ! n_hi: last integer n with ref_time + n*freq <= valid_range(2)
         n_hi = (this%valid_range(2) - this%ref_time) / this%file_frequency
         t_probe = this%compute_time_at_index(n_hi, _RC)
         if (t_probe > this%valid_range(2)) n_hi = n_hi - 1
      else
         ! Relative interval: walk to find index bounds.
         n = 0
         if (this%ref_time < this%valid_range(1)) then
            t_probe = this%compute_time_at_index(n + 1, _RC)
            do while (t_probe <= this%valid_range(1))
               n = n + 1
               t_probe = this%compute_time_at_index(n + 1, _RC)
            end do
         else
            t_probe = this%compute_time_at_index(n, _RC)
            do while (t_probe > this%valid_range(1))
               n = n - 1
               t_probe = this%compute_time_at_index(n, _RC)
            end do
         end if
         n_lo = n

         n = n_lo
         t_probe = this%compute_time_at_index(n + 1, _RC)
         do while (t_probe <= this%valid_range(2))
            n = n + 1
            t_probe = this%compute_time_at_index(n + 1, _RC)
         end do
         n_hi = n
      end if

      _ASSERT(n_lo <= n_hi, &
         "no candidate file times found within valid_range for: "//trim(this%file_template))

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
            ! No files anywhere in valid_range.
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
   ! valid_range, run_range, and extrap_outside.  Calls refine_valid_range to
   ! discover what is actually on disk, then applies:
   !
   !   1. found_any check: fails if no files exist in valid_range at all.
   !   2. Stores on_disk_range(1:2) for runtime use by handlers.
   !   3. Overlap check: if run_range intersects valid_range, on-disk data
   !      must cover the overlap (with ±frequency tolerance for mid-period
   !      file timestamps).
   !   4. Extrapolation sufficiency check per extrap_outside mode:
   !        persist_closest — found_any is sufficient.
   !        clim            — on-disk data must span a full annual cycle AND
   !                          the correct endpoint year (direction-aware) must
   !                          be present on disk.
   subroutine check_data_availability(this, run_range, extrap_outside, unusable, rc)
      class(AbstractDataSetFileSelector), intent(inout) :: this
      type(ESMF_Time), intent(in) :: run_range(2)
      character(len=*), intent(in) :: extrap_outside
      class(mapl_KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ESMF_Time) :: on_disk_first, on_disk_last
      type(ESMF_Time) :: overlap_start, overlap_end
      logical :: found_any, has_overlap, full_cycle
      integer :: yr_first, yr_last, mm_first, mm_last
      integer :: vr_yr1, vr_yr2
      integer :: status
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

end module mapl_AbstractDataSetFileSelector_mod
