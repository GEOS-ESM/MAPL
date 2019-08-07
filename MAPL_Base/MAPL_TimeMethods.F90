#include "MAPL_Generic.h"

module MAPL_TimeDataMod
  use ESMF
  use MAPL_GenericMod
  use MAPL_BaseMod
  use MAPL_IntegerVectorMod
  use pFIO
  use MAPL_ErrorHandlingMod
  use MAPL_ESMFTimeVectorMod
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none

  private

  type, public :: timeData
     type(ESMF_Clock) :: clock
     integer :: ntime
     integer :: tcount
     type(ESMFTimeVector) :: tvec
     integer :: frequency
     type(ESMF_TimeInterval) :: offset
     character(len=:), allocatable :: funits
   contains
     procedure :: add_time_to_metadata
     procedure :: define_time_variable
     procedure :: compute_time_vector
     procedure :: get_start_time
  end type timeData

  interface timeData
     module procedure new_time_data
  end interface timeData
contains

  function new_time_data(clock,ntime,frequency,offset,rc) result(tData)
    type(timeData) :: tData
    type(ESMF_Clock),intent(inout) :: clock
    integer, intent(in) :: ntime
    integer, intent(in) :: frequency
    type(ESMF_TimeInterval) :: offset
    integer, optional, intent(Out) :: rc

    integer :: status

    tdata%clock=clock
    tdata%ntime=ntime
    tdata%frequency=frequency
    tdata%offset=offset
    tdata%funits="minutes"

  end function new_time_data

  function define_time_variable(this,rc) result(v)
    class(TimeData), intent(inout) :: this
    integer, optional, intent(out) :: rc

    type(Variable) :: v
    integer :: status
    character(len=ESMF_MAXSTR) :: startTime,timeUnits
    type(ESMF_Time) :: currTime

    call ESMF_CLockGet(this%clock,currTime=currTime,rc=status)
    _VERIFY(status)
    currTime = currTime - this%offset
    call ESMF_TimeGet(currTime,timeString=StartTime,rc=status)
    _VERIFY(status)
    timeUnits = trim(this%funits)//" since "//startTime( 1: 10)//" "//startTime(12:19)
    v = Variable(PFIO_REAL32,dimensions='time')
    call v%add_attribute('long_name','time')
    call v%add_attribute('units',trim(timeUnits))
    call this%tvec%clear()
    this%tcount=0
    
    _RETURN(ESMF_SUCCESS)

  end function define_time_variable

  function compute_time_vector(this,metadata,rc) result(times)
    class(timeData), intent(inout) :: this
    type(FileMetadata), intent(inout) :: metadata
    integer, optional, intent(out) :: rc

    real, allocatable :: times(:)
    integer :: status
    
    real :: scaleFactor
    type(ESMF_Time) :: currTime,startTime
    type(ESMF_TimeInterval) :: tint
    integer :: tindex,i
    real(ESMF_KIND_R8) :: tint_s
    type(ESMFTimeVectorIterator) :: iter
    type(ESMF_Time), pointer :: tptr
    ! for now return minutes, this should be optional argument in future
  
    this%tcount=this%tcount+1 
    call ESMF_CLockGet(this%clock,currTime=currTime,rc=status)
    _VERIFY(status)
    startTime = this%get_start_time(metadata,rc=status)
    _VERIFY(status)
    currTime=currTime-this%offSet
    call this%tvec%push_back(currTime)
    iter = this%tvec%begin()
    allocate(times(this%tvec%size()))
    i=0
    do while (iter /= this%tvec%end())
       i=i+1
       tptr => iter%get()
       tint = tptr - startTime
       select case(trim(this%funits))
       case('seconds')
          call ESMF_TimeIntervalGet(tint,s_r8=tint_s,rc=status)
          _VERIFY(status)
          times(i)=tint_s
       case('minutes')
          call ESMF_TimeIntervalGet(tint,m_r8=tint_s,rc=status)
          _VERIFY(status)
          times(i)=tint_s
       case('hours')
          call ESMF_TimeIntervalGet(tint,h_r8=tint_s,rc=status)
          _VERIFY(status)
          times(i)=tint_s
       case('days')
          call ESMF_TimeIntervalGet(tint,d_r8=tint_s,rc=status)
          _VERIFY(status)
          times(i)=tint_s
       case default
       end select
       call iter%next()
    enddo


    !tint = currTime-startTime
    !call ESMF_TimeIntervalGet(tint,s=tint_s,rc=status)
    !_VERIFY(status)
    !if (mod(tint_s,this%frequency)/=0) then
       !_ASSERT(.false.,"history writing time not even multiple of frequency")
    !end if
    !select case(trim(this%funits))
    !case('seconds') 
       !scaleFactor = 1.0
    !case('minutes')
       !scaleFactor = 60.0
    !case('hours')
       !scaleFactor = 3600.0
    !case('days')
       !scaleFactor = 86400.0
    !case default
       !_ASSERT(.false.,"invalid time units")
    !end select
    !tindex = 1+(tint_s/this%frequency)
    !allocate(times(tindex))
    !times(1)=0
    !do i=2,size(times)
       !times(i)=times(i-1)+(real(this%frequency)/scaleFactor)
    !enddo
    _RETURN(ESMF_SUCCESS) 

  end function compute_time_vector 

  subroutine add_time_to_metadata(this,metadata,rc)
    class(timeData), intent(inout) :: this
    type(FileMetadata), intent(inout) :: metadata
    integer, intent(out), optional :: rc

    type(Variable) :: v
    integer :: tdim
    integer :: status

    if (this%ntime==-1) then
       tdim = pFIO_UNLIMITED
    else
       tdim = this%ntime
    end if
    call metadata%add_dimension('time',tdim)
    v = this%define_time_variable(rc=status)
    _VERIFY(status)
    call metadata%add_variable('time',v,rc=status)
    _VERIFY(status)

  end subroutine add_time_to_metadata

  function get_start_time(this,metadata,rc) result(startTime)
    class(timeData), intent(inout) :: this
    type(FileMetaData), intent(inout) :: metadata
    integer, optional, intent(out) :: rc

    type(ESMF_Time) :: startTime
    integer :: status
    class(Variable), pointer :: v
    type(Attribute), pointer :: attr
    class(*), pointer :: units
    v => metadata%get_variable('time',rc=status)
    _VERIFY(status)
    attr => v%get_attribute('units')
    units => attr%get_value()
    select type(units)
    type is (character(*))
       startTime = parse_time_string(units,rc=status)
       _VERIFY(status)
    class default
       _ASSERT(.false.,'unsupported subclass for units')
    end select

    
  end function get_start_time

  function parse_time_string(timeUnits,rc) result(time)
    character(len=*), intent(inout) :: timeUnits
    integer, optional, intent(out) :: rc

    type(ESMF_Time) :: time
    integer :: status

    integer        year               ! 4-digit year
    integer        month              ! month
    integer        day                ! day
    integer        hour               ! hour
    integer        min                ! minute
    integer        sec                ! second

    integer ypos(2), mpos(2), dpos(2), hpos(2), spos(2)
    integer strlen
    integer firstdash, lastdash
    integer firstcolon, lastcolon
    integer lastspace
    strlen = LEN_TRIM (TimeUnits)

    firstdash = index(TimeUnits, '-')
    lastdash  = index(TimeUnits, '-', BACK=.TRUE.)

    if (firstdash .LE. 0 .OR. lastdash .LE. 0) then
       _ASSERT(.false.,'time string is not a valid format')
    endif
    ypos(2) = firstdash - 1
    mpos(1) = firstdash + 1
    ypos(1) = ypos(2) - 3

    mpos(2) = lastdash - 1
    dpos(1) = lastdash + 1
    dpos(2) = dpos(1) + 1

    read ( TimeUnits(ypos(1):ypos(2)), * ) year
    read ( TimeUnits(mpos(1):mpos(2)), * ) month
    read ( TimeUnits(dpos(1):dpos(2)), * ) day

    firstcolon = index(TimeUnits, ':')
    if (firstcolon .LE. 0) then

       ! If no colons, check for hour.

       ! Logic below assumes a null character or something else is after the hour
       ! if we do not find a null character add one so that it correctly parses time
       if (TimeUnits(strlen:strlen) /= char(0)) then
          TimeUnits = trim(TimeUnits)//char(0)
          strlen=len_trim(TimeUnits)
       endif
       lastspace = index(TRIM(TimeUnits), ' ', BACK=.TRUE.)
       if ((strlen-lastspace).eq.2 .or. (strlen-lastspace).eq.3) then
          hpos(1) = lastspace+1
          hpos(2) = strlen-1
          read (TimeUnits(hpos(1):hpos(2)), * ) hour
          min  = 0
          sec  = 0
       else
          hour = 0
          min  = 0
          sec  = 0
       endif
    else
       hpos(1) = firstcolon - 2
       hpos(2) = firstcolon - 1
       lastcolon =  index(TimeUnits, ':', BACK=.TRUE.)
       if ( lastcolon .EQ. firstcolon ) then
          mpos(1) = firstcolon + 1
          mpos(2) = firstcolon + 2
          read (TimeUnits(hpos(1):hpos(2)), * ) hour
          read (TimeUnits(mpos(1):mpos(2)), * ) min
          sec = 0
       else
          mpos(1) = firstcolon + 1
          mpos(2) = lastcolon - 1
          spos(1) = lastcolon + 1
          spos(2) = lastcolon + 2
          read (TimeUnits(hpos(1):hpos(2)), * ) hour
          read (TimeUnits(mpos(1):mpos(2)), * ) min
          read (TimeUnits(spos(1):spos(2)), * ) sec
       endif
    endif

    call ESMF_TimeSet(time,yy=year,mm=month,dd=day,h=hour,m=min,s=sec,rc=status)
    _VERIFY(status)

  end function parse_time_string


end module MAPL_TimeDataMod
