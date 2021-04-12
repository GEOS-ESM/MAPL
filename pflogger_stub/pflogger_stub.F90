#include "error_handling_macros.fh"
module PFL_Logger
   use gFTL_StringUnlimitedMap
   use PFL_KeywordEnforcer
   implicit none
   private

   public :: Logger

   public :: NOTSET
   public :: DEBUG_LEVEL
   public :: INFO_LEVEL
   public :: WARNING_LEVEL
   public :: ERROR_LEVEL
   public :: CRITICAL_LEVEL

   enum, bind(c)
      enumerator :: &
           & NOTSET   =  0, &
           & DEBUG_LEVEL    = 10, &
           & INFO_LEVEL     = 20, &
           & WARNING_LEVEL  = 30, &
           & ERROR_LEVEL    = 40, &
           & CRITICAL_LEVEL = 50
   end enum

   type :: Logger
   contains
      procedure :: debug
      procedure :: info
      procedure :: warning
      procedure :: error
      procedure :: critical
      procedure :: free
   end type Logger

#define ARG_LIST arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9

contains

   subroutine free(this)
      class (Logger), intent(inout) :: this
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
      _RETURN(_SUCCESS,rc)
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
      _RETURN(_SUCCESS,rc)
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
      
      _RETURN(_SUCCESS,rc)

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

      _RETURN(_SUCCESS,rc)

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
      _RETURN(_SUCCESS,rc)
   end subroutine critical
end module PFL_Logger

module PFL_LoggerManager
   use gFTL_StringUnlimitedMap
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
      _RETURN(_SUCCESS,rc)
   end function get_logger_root

   function get_logger_name(this, name, rc) result(lgr)
      class (Logger), pointer :: lgr
      class (LoggerManager), target, intent(inout) :: this
      character(len=*), intent(in) :: name
      integer, optional, intent(out) :: rc
      lgr => this%log_
      _RETURN(_SUCCESS,rc)
   end function get_logger_name

   subroutine free(this)
      class(LoggerManager), intent(inout) :: this
   end subroutine free

end module PFL_LoggerManager

module pflogger
   implicit none
   use PFL_Logger
   use PFL_LoggerManager
   use PFL_WrapArray
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
      _UNUSED_DUMMY(unusable)
      _RETURN(_SUCCESS)
   end subroutine initialize

   subroutine finalize()
   end subroutine finalize

end module pflogger
