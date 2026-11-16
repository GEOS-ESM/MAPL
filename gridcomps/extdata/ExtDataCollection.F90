#include "MAPL.h"
module mapl_ExtDataCollection_mod
   use ESMF
   use MAPL
   use mapl_AbstractDataSetFileSelector_mod
   use mapl_NonClimDataSetFileSelector_mod
   implicit none
   private

   ! Default per-collection coordinate-comparison tolerance, applied
   ! when a collection's config does not set "coordinate_tolerance"
   ! explicitly. MAPL2 treated file-based grids that differed only by
   ! numerical noise as the same grid by default (see GEOS-ESM/MAPL#5385
   ! - "MAPL2 allows 2 file-based grids that differ slightly in their
   ! coordinates to be treated as the same grid... An override is given
   ! if someone wants to insist that the file grid be respected"). This
   ! default (10% of a grid's own coordinate spacing/DX) preserves that
   ! historical, existing-user-expected behavior; it is not necessarily
   ! the ideal default for a brand-new feature, but changing existing
   ! users' grid-reuse behavior by default would be a worse regression
   ! than an imperfect default value. A collection may set an explicit
   ! `coordinate_tolerance: 0` to opt into strict/bitwise comparison -
   ! the override MAPL2 itself provided.
   real(kind=ESMF_KIND_R8), parameter :: DEFAULT_COORDINATE_TOLERANCE = 0.1_ESMF_KIND_R8

   type, public :: ExtDataCollection
      character(len=:), allocatable :: file_template
      type(ESMF_TimeInterval) :: frequency
      type(ESMF_Time), allocatable :: reff_time
      integer :: collection_id
      type(ESMF_Time), allocatable :: valid_range(:)
      ! Per-collection coordinate-comparison tolerance (see
      ! GEOS-ESM/MAPL#5385), expressed as a FRACTION of a grid's own
      ! local spacing (DX) - e.g. 0.01 means "within 1% of the minimum
      ! spacing between adjacent grid points" - not an absolute
      ! coordinate difference. Always has an effective value: either the
      ! config's explicit `coordinate_tolerance`, or
      ! DEFAULT_COORDINATE_TOLERANCE when the key is absent. ExtData
      ! stamps this value as a "coordinate_tolerance" attribute onto
      ! each file's FileMetadata before requesting a geom for it, so
      ! files whose grids differ only by numerical noise can reuse
      ! geoms/RouteHandles. An explicit `coordinate_tolerance: 0`
      ! disables this (strict/bitwise comparison). Only the
      ! not-yet-registered grid's own tolerance is ever consulted (see
      ! CoordinateAxis::equal_to) - a collection's tolerance never
      ! affects how *other* collections' already-cached grids compare.
      real(kind=ESMF_KIND_R8) :: coordinate_tolerance = DEFAULT_COORDINATE_TOLERANCE
      contains
         procedure :: get_file_template
         procedure :: get_frequency
         procedure :: get_reff_time
         procedure :: get_collection_id
         procedure :: get_valid_range
         procedure :: get_coordinate_tolerance
         procedure :: is_reff_time_allocated
         procedure :: is_valid_range_allocated
   end type

    interface ExtDataCollection
       module procedure new_ExtDataCollection
    end interface ExtDataCollection
contains

   function new_ExtDataCollection(config,current_time, unusable,rc) result(data_set)
      type(ESMF_HConfig), intent(in) :: config
      type(ESMF_Time), intent(in) :: current_time
      class(mapl_KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ExtDataCollection) :: data_set
      integer :: status
      integer :: last_token
      integer :: iyy,imm,idd,ihh,imn,isc
      character(len=2) :: token
      character(len=:), allocatable :: file_frequency, file_reff_time,range_str
      logical :: is_present

      is_present = ESMF_HConfigIsDefined(config,keyString="template",_RC)
      _ASSERT(is_present,"no file template in the collection")

      data_set%file_template = ESMF_HConfigAsString(config,keyString="template",_RC)
      file_frequency = get_string_with_default(config,"freq")
      file_reff_time = get_string_with_default(config,"ref_time")
      range_str = get_string_with_default(config,"valid_range")

      if (ESMF_HConfigIsDefined(config,keyString="coordinate_tolerance")) then
         data_set%coordinate_tolerance = ESMF_HConfigAsR8(config,keyString="coordinate_tolerance",_RC)
      else
         data_set%coordinate_tolerance = DEFAULT_COORDINATE_TOLERANCE
      end if
      _ASSERT(data_set%coordinate_tolerance >= 0.0_ESMF_KIND_R8, "coordinate_tolerance must be non-negative")

      if (file_frequency /= '') then
         data_set%frequency = mapl_HConfigAsTimeInterval(config, keyString="freq", _RC)
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
            case default
               _FAIL("Unsupported token")
            end select
         else
            ! couldn't find any tokens so all the data must be on one file
            call ESMF_TimeIntervalSet(data_set%frequency,_RC)
         end if
      end if

      if (file_reff_time /= '') then
         allocate(data_set%reff_time)
         call ESMF_TimeSet(data_set%reff_time, timeString=file_reff_time, _RC)
      else
         last_token = index(data_set%file_template,'%',back=.true.)
         allocate(data_set%reff_time)
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
            case default
               _FAIL("Unsupported token")
            end select
         else
            data_set%reff_time = current_time
         end if
      end if

      if (range_str /= '') then
         if (allocated(data_set%valid_range)) deallocate(data_set%valid_range)
         data_set%valid_range = mapl_HConfigAsTimeRange(config, keyString="valid_range", _RC)

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
            case default
               _FAIL("Unsupported token")
            end select
         end if

      end if

      _UNUSED_DUMMY(unusable)
      _RETURN(_SUCCESS)

      contains

         function get_string_with_default(config,selector) result(string)
            type(ESMF_HConfig), intent(in) :: config
            character(len=*), intent(In) :: selector
            character(len=:), allocatable :: string

           string=''
           if (ESMF_HConfigIsDefined(config,keyString=selector)) then
               string = ESMF_HConfigAsString(config,keyString=selector,_RC)
           end if
         end function

   end function new_ExtDataCollection

   ! file_template accessors
   function get_file_template(this) result(template)
      class(ExtDataCollection), intent(in) :: this
      character(len=:), allocatable :: template

      template = ''
      if (allocated(this%file_template)) then
         template = this%file_template
      end if
   end function get_file_template

   ! frequency accessors
   function get_frequency(this) result(freq)
      class(ExtDataCollection), intent(in) :: this
      type(ESMF_TimeInterval) :: freq

      freq = this%frequency
   end function get_frequency

   ! reff_time accessors
   subroutine get_reff_time(this, time)
      class(ExtDataCollection), intent(in) :: this
      type(ESMF_Time), intent(out), allocatable :: time

      if (allocated(this%reff_time)) then
         time = this%reff_time
      end if
   end subroutine get_reff_time

   ! collection_id accessors
   function get_collection_id(this) result(id)
      class(ExtDataCollection), intent(in) :: this
      integer :: id

      id = this%collection_id
   end function get_collection_id

   ! valid_range accessors
   subroutine get_valid_range(this, valid_range)
      class(ExtDataCollection), intent(in) :: this
      type(ESMF_Time), intent(out), allocatable :: valid_range(:)

      if (allocated(this%valid_range)) then
         valid_range = this%valid_range
      end if
   end subroutine get_valid_range

   ! Check if reff_time is allocated
   function is_reff_time_allocated(this) result(is_allocated)
      class(ExtDataCollection), intent(in) :: this
      logical :: is_allocated

      is_allocated = allocated(this%reff_time)
   end function is_reff_time_allocated

   ! Check if valid_range is allocated
   function is_valid_range_allocated(this) result(is_allocated)
      class(ExtDataCollection), intent(in) :: this
      logical :: is_allocated

      is_allocated = allocated(this%valid_range)
   end function is_valid_range_allocated

   ! coordinate_tolerance accessor. Always returns an effective value:
   ! either the config's explicit setting, or DEFAULT_COORDINATE_TOLERANCE.
   function get_coordinate_tolerance(this) result(tolerance)
      class(ExtDataCollection), intent(in) :: this
      real(kind=ESMF_KIND_R8) :: tolerance

      tolerance = this%coordinate_tolerance
   end function get_coordinate_tolerance

end module mapl_ExtDataCollection_mod
