#include "unused_dummy.H"

module MAPL_RssMemoryGauge
   use, intrinsic :: iso_fortran_env, only: REAL64, INT64
   use MAPL_AbstractGauge
   implicit none
   private

   public :: RssMemoryGauge


   type, extends(AbstractGauge) :: RssMemoryGauge
      private
      integer(kind=INT64) :: baseline = 0
      character(:), allocatable :: command
   contains
      procedure :: get_measurement
   end type RssMemoryGauge

   interface RssMemoryGauge
      module procedure :: new_RssMemoryGauge
   end interface RssMemoryGauge

#define PID_T kind(1)

   interface
      function getpid() bind(c)
         integer(kind=PID_T) :: getpid
      end function getpid
   end interface

contains


   function new_RssMemoryGauge() result(gauge)
      type (RssMemoryGauge) :: gauge

      integer :: length

      call get_command_argument(0, length=length)
      allocate(character(len=length) :: gauge%command)
      call get_command_argument(0, value=gauge%command)

   end function new_RssMemoryGauge


   function get_measurement(this) result(mem_use)
      class (RssMemoryGauge), intent(inout) :: this
      real(kind=REAL64) :: mem_use

      integer :: unit
      integer(kind=INT64) :: MEM_UNITS = 1024 ! KB
      character(:), allocatable :: tmp_file

      integer(kind=PID_T) :: pid
      
      character(16) :: buffer
      character(:), allocatable :: pid_str

      _UNUSED_DUMMY(this)
      pid = getpid()
      write(buffer,'(i0)')pid
      pid_str = trim(buffer)
      tmp_file = 'tmp.pid'//pid_str
      call execute_command_line("ps -p " // pid_str // " -ocommand='',rss='' | awk '{ print $NF }'> " // tmp_file)
      
      open(newunit=unit, file=tmp_file, form='formatted', access='sequential', status='old')
      read(unit,*) mem_use
      mem_use = mem_use * MEM_UNITS
      close(unit, status='delete')


   end function get_measurement


end module MAPL_RssMemoryGauge

   
   
   

