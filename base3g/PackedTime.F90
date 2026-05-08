#include "MAPL.h"

!> Provides named constructors for legacy integer-packed date/time representations,
!> with overloads accepting either individual integer components or an ESMF_Time.
!>
!> ### Packed-integer conventions
!>   - **Packed date**:     YYYYMMDD integer  (e.g. 20050215 → 2005-02-15)
!>   - **Packed time**:     HHMMSS   integer  (e.g.  130000  → 13:00:00)
!>   - **Packed datetime**: integer(2), element 1 = YYYYMMDD, element 2 = HHMMSS
!>
!> These routines exist as a migration bridge so that callers interfacing
!> with routines that still require packed integers do not need to inline
!> arithmetic.  The canonical representation of time in MAPL3 is ESMF_Time;
!> the packed forms will be retired once all consumers have migrated.

module MAPL_PackedTimeMod
   use ESMF
   use mapl_ErrorHandling, only: MAPL_Verify, MAPL_Return
   implicit none(type, external)
   private

   public :: PackedDateCreate
   public :: PackedTimeCreate
   public :: PackedDateTimeCreate
   public :: ESMFTimeFromPacked
   public :: UnpackDate
   public :: UnpackTime
   public :: UnpackDateTime

   !> Create a packed date integer (YYYYMMDD) from either integer components
   !> (yy, mm, dd) or an ESMF_Time.
   interface PackedDateCreate
      module procedure packed_date_from_integers
      module procedure packed_date_from_esmf
   end interface PackedDateCreate

   !> Create a packed time-of-day integer (HHMMSS) from either integer components
   !> (h, m, s) or an ESMF_Time.
   interface PackedTimeCreate
      module procedure packed_time_from_integers
      module procedure packed_time_from_esmf
   end interface PackedTimeCreate

   !> Create a packed datetime array (integer(2): [YYYYMMDD, HHMMSS]) from either
   !> integer components (yy, mm, dd, h, m, s), a pair of already-packed integers
   !> (nymd, nhms), or an ESMF_Time.
   interface PackedDateTimeCreate
      module procedure packed_datetime_from_integers
      module procedure packed_datetime_from_packed
      module procedure packed_datetime_from_esmf
   end interface PackedDateTimeCreate

   !> Convert a pair of packed integers (YYYYMMDD, HHMMSS) to an ESMF_Time.
   interface ESMFTimeFromPacked
      module procedure esmf_time_from_packed
   end interface ESMFTimeFromPacked

contains

   ! ---------------------------------------------------------------------------
   ! Private helpers — shared packing/unpacking logic
   ! ---------------------------------------------------------------------------

   pure integer function pack_date_(yy, mm, dd)
      integer, intent(in) :: yy, mm, dd
      pack_date_ = yy*10000 + mm*100 + dd
   end function pack_date_

   pure integer function pack_time_(h, m, s)
      integer, intent(in) :: h, m, s
      pack_time_ = h*10000 + m*100 + s
   end function pack_time_

   ! ---------------------------------------------------------------------------
   ! PackedDateCreate overloads
   ! ---------------------------------------------------------------------------

   function packed_date_from_integers(yy, mm, dd, rc) result(nymd)
      integer,           intent(in)  :: yy, mm, dd
      integer, optional, intent(out) :: rc
      integer :: nymd
      nymd = pack_date_(yy, mm, dd)
      if (present(rc)) rc = ESMF_SUCCESS
   end function packed_date_from_integers

   function packed_date_from_esmf(time, rc) result(nymd)
      type(ESMF_Time),   intent(in)  :: time
      integer, optional, intent(out) :: rc
      integer :: nymd

      integer :: status
      integer :: yy, mm, dd

      call ESMF_TimeGet(time, yy=yy, mm=mm, dd=dd, _RC)
      nymd = pack_date_(yy, mm, dd)
      _RETURN(ESMF_SUCCESS)
   end function packed_date_from_esmf

   ! ---------------------------------------------------------------------------
   ! PackedTimeCreate overloads
   ! ---------------------------------------------------------------------------

   function packed_time_from_integers(h, m, s, rc) result(nhms)
      integer,           intent(in)  :: h, m, s
      integer, optional, intent(out) :: rc
      integer :: nhms
      nhms = pack_time_(h, m, s)
      if (present(rc)) rc = ESMF_SUCCESS
   end function packed_time_from_integers

   function packed_time_from_esmf(time, rc) result(nhms)
      type(ESMF_Time),   intent(in)  :: time
      integer, optional, intent(out) :: rc
      integer :: nhms

      integer :: status
      integer :: h, m, s

      call ESMF_TimeGet(time, h=h, m=m, s=s, _RC)
      nhms = pack_time_(h, m, s)
      _RETURN(ESMF_SUCCESS)
   end function packed_time_from_esmf

   ! ---------------------------------------------------------------------------
   ! PackedDateTimeCreate overloads
   ! ---------------------------------------------------------------------------

   function packed_datetime_from_integers(yy, mm, dd, h, m, s, rc) result(date_time)
      integer,           intent(in)  :: yy, mm, dd, h, m, s
      integer, optional, intent(out) :: rc
      integer :: date_time(2)
      date_time(1) = pack_date_(yy, mm, dd)
      date_time(2) = pack_time_(h, m, s)
      if (present(rc)) rc = ESMF_SUCCESS
   end function packed_datetime_from_integers

   function packed_datetime_from_packed(nymd, nhms, rc) result(date_time)
      integer,           intent(in)  :: nymd, nhms
      integer, optional, intent(out) :: rc
      integer :: date_time(2)
      date_time(1) = nymd
      date_time(2) = nhms
      if (present(rc)) rc = ESMF_SUCCESS
   end function packed_datetime_from_packed

   function packed_datetime_from_esmf(time, rc) result(date_time)
      type(ESMF_Time),   intent(in)  :: time
      integer, optional, intent(out) :: rc
      integer :: date_time(2)

      integer :: status
      integer :: yy, mm, dd, h, m, s

      call ESMF_TimeGet(time, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, _RC)
      date_time(1) = pack_date_(yy, mm, dd)
      date_time(2) = pack_time_(h, m, s)
      _RETURN(ESMF_SUCCESS)
    end function packed_datetime_from_esmf

   ! ---------------------------------------------------------------------------
   ! UnpackDate / UnpackTime / UnpackDateTime
   ! Pure-integer inverses of the pack operations; no ESMF dependency.
   ! ---------------------------------------------------------------------------

   !> Unpack a YYYYMMDD integer into year, month, day components.
   pure subroutine UnpackDate(nymd, yy, mm, dd)
      integer, intent(in)  :: nymd
      integer, intent(out) :: yy, mm, dd
      yy = nymd / 10000
      mm = mod(nymd, 10000) / 100
      dd = mod(nymd, 100)
   end subroutine UnpackDate

   !> Unpack a HHMMSS integer into hour, minute, second components.
   pure subroutine UnpackTime(nhms, h, m, s)
      integer, intent(in)  :: nhms
      integer, intent(out) :: h, m, s
      h = nhms / 10000
      m = mod(nhms, 10000) / 100
      s = mod(nhms, 100)
   end subroutine UnpackTime

   !> Unpack an integer(2) packed datetime ([YYYYMMDD, HHMMSS]) into six components.
   pure subroutine UnpackDateTime(date_time, yy, mm, dd, h, m, s)
      integer, intent(in)  :: date_time(2)
      integer, intent(out) :: yy, mm, dd, h, m, s
      call UnpackDate(date_time(1), yy, mm, dd)
      call UnpackTime(date_time(2), h,  m,  s)
   end subroutine UnpackDateTime

   ! ---------------------------------------------------------------------------
   ! ESMFTimeFromPacked
   ! ---------------------------------------------------------------------------

   function esmf_time_from_packed(nymd, nhms, rc) result(time)
      integer,           intent(in)  :: nymd   !< Date as YYYYMMDD
      integer,           intent(in)  :: nhms   !< Time of day as HHMMSS
      integer, optional, intent(out) :: rc
      type(ESMF_Time) :: time

      integer :: status
      integer :: yy, mm, dd, h, m, s

      yy = nymd / 10000
      mm = mod(nymd, 10000) / 100
      dd = mod(nymd, 100)
      h  = nhms / 10000
      m  = mod(nhms, 10000) / 100
      s  = mod(nhms, 100)

      call ESMF_TimeSet(time, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, _RC)
       _RETURN(ESMF_SUCCESS)
   end function esmf_time_from_packed

end module MAPL_PackedTimeMod
