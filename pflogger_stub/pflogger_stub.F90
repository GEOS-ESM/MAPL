#include "MAPL_ErrLog.h"
#define __SUCCESS 0
#ifdef __RETURN
#undef __RETURN
#endif
#define __RETURN(status) if(present(rc))rc=status; return

module PFL_SeverityLevels
   implicit none
   public :: NOTSET
   public :: DEBUG
   public :: INFO
   public :: WARNING
   public :: ERROR
   public :: CRITICAL

   enum, bind(c)
      enumerator :: &
           & NOTSET   =  0, &
           & DEBUG    = 10, &
           & INFO     = 20, &
           & WARNING  = 30, &
           & ERROR    = 40, &
           & CRITICAL = 50
   end enum

end module PFL_SeverityLevels

module PFL_Logger
   use PFL_SeverityLevels, only: NOTSET
   use PFL_SeverityLevels, only: DEBUG_LEVEL => DEBUG
   use PFL_SeverityLevels, only: INFO_LEVEL => INFO
   use PFL_SeverityLevels, only: WARNING_LEVEL => WARNING
   use PFL_SeverityLevels, only: ERROR_LEVEL => ERROR
   use PFL_SeverityLevels, only: CRITICAL_LEVEL => critical
   use gFTL_StringUnlimitedMap
   use PFL_KeywordEnforcerMod
   implicit none
   private

   public :: Logger

   type :: Logger
   contains
      procedure :: debug
      procedure :: info
      procedure :: warning
      procedure :: error
      procedure :: critical
      procedure :: free
      procedure :: isEnabledFor
   end type Logger

#define ARG_LIST arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9

contains

   subroutine free(this)
      class (Logger), intent(inout) :: this
      __UNUSED_DUMMY(this)
   end subroutine free

   subroutine debug(this, message, ARG_LIST, unusable, extra, line, file, rc)
      class (Logger), target, intent(inout) :: this
      character(len=*), intent(in) :: message
      include 'recordOptArgs.inc'
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, target, intent(in) :: extra
      integer, optional, intent(in) :: line
      character(*), optional, intent(in) :: file
      integer, optional, intent(out) :: rc

      __UNUSED_DUMMY(this)
      __UNUSED_DUMMY(message)
      __UNUSED_DUMMY(arg1)
      __UNUSED_DUMMY(arg2)
      __UNUSED_DUMMY(arg3)
      __UNUSED_DUMMY(arg4)
      __UNUSED_DUMMY(arg5)
      __UNUSED_DUMMY(arg6)
      __UNUSED_DUMMY(arg7)
      __UNUSED_DUMMY(arg8)
      __UNUSED_DUMMY(arg9)
      __UNUSED_DUMMY(unusable)
      __UNUSED_DUMMY(extra)
      __UNUSED_DUMMY(line)
      __UNUSED_DUMMY(file)

      __RETURN(__SUCCESS)
   end subroutine debug

   subroutine info(this, message, ARG_LIST, unusable, extra, line, file, rc)
      class (Logger), target, intent(inout) :: this
      character(len=*), intent(in) :: message
      include 'recordOptArgs.inc'
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, target, intent(in) :: extra
      integer, optional, intent(in) :: line
      character(*), optional, intent(in) :: file
      integer, optional, intent(out) :: rc

      __UNUSED_DUMMY(this)
      __UNUSED_DUMMY(message)
      __UNUSED_DUMMY(arg1)
      __UNUSED_DUMMY(arg2)
      __UNUSED_DUMMY(arg3)
      __UNUSED_DUMMY(arg4)
      __UNUSED_DUMMY(arg5)
      __UNUSED_DUMMY(arg6)
      __UNUSED_DUMMY(arg7)
      __UNUSED_DUMMY(arg8)
      __UNUSED_DUMMY(arg9)
      __UNUSED_DUMMY(unusable)
      __UNUSED_DUMMY(extra)
      __UNUSED_DUMMY(line)
      __UNUSED_DUMMY(file)

      __RETURN(__SUCCESS)
   end subroutine info

   subroutine warning(this, message, ARG_LIST, unusable, extra, line, file, rc)
      class (Logger), target, intent(inout) :: this
      character(len=*), intent(in) :: message
      include 'recordOptArgs.inc'
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, target, intent(in) :: extra
      integer, optional, intent(in) :: line
      character(*), optional, intent(in) :: file
      integer, optional, intent(out) :: rc

      __UNUSED_DUMMY(this)
      __UNUSED_DUMMY(message)
      __UNUSED_DUMMY(arg1)
      __UNUSED_DUMMY(arg2)
      __UNUSED_DUMMY(arg3)
      __UNUSED_DUMMY(arg4)
      __UNUSED_DUMMY(arg5)
      __UNUSED_DUMMY(arg6)
      __UNUSED_DUMMY(arg7)
      __UNUSED_DUMMY(arg8)
      __UNUSED_DUMMY(arg9)
      __UNUSED_DUMMY(unusable)
      __UNUSED_DUMMY(extra)
      __UNUSED_DUMMY(line)
      __UNUSED_DUMMY(file)

      __RETURN(__SUCCESS)
   end subroutine warning

   subroutine error(this, message, ARG_LIST, unusable, extra, line, file, rc)
      ! Log message with the integer severity 'DEBUG'.
      class (Logger), target, intent(inout) :: this
      character(len=*), intent(in) :: message
      include 'recordOptArgs.inc'
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, target, intent(in) :: extra
      integer, optional, intent(in) :: line
      character(*), optional, intent(in) :: file
      integer, optional, intent(out) :: rc

      __UNUSED_DUMMY(this)
      __UNUSED_DUMMY(message)
      __UNUSED_DUMMY(arg1)
      __UNUSED_DUMMY(arg2)
      __UNUSED_DUMMY(arg3)
      __UNUSED_DUMMY(arg4)
      __UNUSED_DUMMY(arg5)
      __UNUSED_DUMMY(arg6)
      __UNUSED_DUMMY(arg7)
      __UNUSED_DUMMY(arg8)
      __UNUSED_DUMMY(arg9)
      __UNUSED_DUMMY(unusable)
      __UNUSED_DUMMY(extra)
      __UNUSED_DUMMY(line)
      __UNUSED_DUMMY(file)

      __RETURN(__SUCCESS)
   end subroutine error

   subroutine critical(this, message, ARG_LIST, unusable, extra, line, file, rc)
      class (Logger), target, intent(inout) :: this
      character(len=*), intent(in) :: message
      include 'recordOptArgs.inc'
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, target, intent(in) :: extra
      integer, optional, intent(in) :: line
      character(*), optional, intent(in) :: file
      integer, optional, intent(out) :: rc

      __UNUSED_DUMMY(this)
      __UNUSED_DUMMY(message)
      __UNUSED_DUMMY(arg1)
      __UNUSED_DUMMY(arg2)
      __UNUSED_DUMMY(arg3)
      __UNUSED_DUMMY(arg4)
      __UNUSED_DUMMY(arg5)
      __UNUSED_DUMMY(arg6)
      __UNUSED_DUMMY(arg7)
      __UNUSED_DUMMY(arg8)
      __UNUSED_DUMMY(arg9)
      __UNUSED_DUMMY(unusable)
      __UNUSED_DUMMY(extra)
      __UNUSED_DUMMY(line)
      __UNUSED_DUMMY(file)

      __RETURN(__SUCCESS)
   end subroutine critical

   logical function isEnabledFor(this, level)
      class (Logger), intent(in) :: this
      integer, intent(in) :: level
      isEnabledFor = .false.
   end function isEnabledFor

end module PFL_Logger

module PFL_LoggerManager
   use PFL_Logger, only: Logger
   implicit none
   private

   public :: logging ! singleton instance

   type :: LoggerManager
      private
      type(Logger) :: log_
   contains
      procedure :: get_logger_name
      procedure :: get_logger_root
      generic :: get_logger => get_logger_name
      generic :: get_logger => get_logger_root
      procedure :: free
   end type LoggerManager

   type (LoggerManager), target, save :: logging

contains

   function get_logger_root(this, rc) result(lgr)
      class (Logger), pointer :: lgr
      class (LoggerManager), target, intent(inout) :: this
      integer, optional, intent(out) :: rc
      lgr => this%log_
      __RETURN(__SUCCESS)
   end function get_logger_root

   function get_logger_name(this, name, rc) result(lgr)
      class (Logger), pointer :: lgr
      class (LoggerManager), target, intent(inout) :: this
      character(len=*), intent(in) :: name
      integer, optional, intent(out) :: rc
      __UNUSED_DUMMY(name)
      lgr => this%log_
      __RETURN(__SUCCESS)
   end function get_logger_name

   subroutine free(this)
      class(LoggerManager), intent(inout) :: this
      __UNUSED_DUMMY(this)
   end subroutine free

end module PFL_LoggerManager

module pflogger
   use PFL_SeverityLevels
   use PFL_Logger
   use PFL_LoggerManager
   use PFL_WrapArray
   use PFL_KeywordEnforcerMod
   implicit none
   private

   public :: initialize
   public :: finalize

   public :: logging
   public :: Logger
   public :: WrapArray

   public :: NOTSET
   public :: DEBUG
   public :: INFO
   public :: WARNING
   public :: ERROR
   public :: CRITICAL

contains
   subroutine initialize(unusable, comm, logging_config, logger_name, rc)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: comm
      character(len=*), optional,intent(in) :: logging_config
      character(len=*), optional,intent(in) :: logger_name
      integer, optional, intent(out) :: rc
      __UNUSED_DUMMY(unusable)
      __UNUSED_DUMMY(comm)
      __UNUSED_DUMMY(logging_config)
      __UNUSED_DUMMY(logger_name)
      __RETURN(__SUCCESS)
   end subroutine initialize

   subroutine finalize()
   end subroutine finalize

end module pflogger
