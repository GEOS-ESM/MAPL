!Allow 60 for seconds
!No fractions other than milliseconds (explicit)
!No weeks
!Z required for time
!No parts of day
!Allow ordinal dates as input/output

module mod_ISO8601_time

    type :: date_fields
        integer :: year
        integer :: month
        integer :: day
    end type date_parts

    type :: time_fields
        integer :: hour
        integer :: minute
        integer :: second
        integer :: millisecond
    end type time_fields

    type :: MAPL_ISO8601_date
        type(date_fields) :: fields
    end type MAPL_ISO8601_date

    type :: MAPL_ISO8601_time
        type(time_fields) :: fields
        integer :: timezone
    end type MAPL_ISO8601_time

    type :: MAPL_ISO8601_datetime
        type(MAPL_ISO8601_date) :: date
        type(MAPL_ISO8601_time) :: time
    end type MAPL_ISO8601_datetime

    type :: MAPL_ISO8601_duration
        type(date_fields) :: date
        type(time_fields) :: time
    end type :: MAPL_ISO8601_duration

    type :: MAPL_ISO8601_timeinterval
        type(MAPL_ISO8601_datetime) :: start_datetime
        type(MAPL_ISO8601_datetime) :: end_datetime
        integer :: repetitions
    end type MAPL_ISO8601_timeinterval

contains

end module mod_ISO8601_time
