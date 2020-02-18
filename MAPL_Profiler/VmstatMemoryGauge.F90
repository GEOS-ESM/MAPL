module MAPL_VmstatMemoryGauge
   use, intrinsic :: iso_fortran_env, only: REAL64, INT64
   use MAPL_AbstractGauge
   implicit none
   private

   public :: VmstatMemoryGauge


   type, extends(AbstractGauge) :: VmstatMemoryGauge
      private
      integer(kind=INT64) :: baseline = 0
   contains
      procedure :: get_measurement
   end type VmstatMemoryGauge

   interface VmstatMemoryGauge
      module procedure :: new_VmstatMemoryGauge
   end interface VmstatMemoryGauge


contains


   function new_VmstatMemoryGauge() result(gauge)
      type (VmstatMemoryGauge) :: gauge

   end function new_VmstatMemoryGauge


   function get_measurement(this) result(mem_use)
      class (VmstatMemoryGauge), intent(inout) :: this
      real(kind=REAL64) :: mem_use

      integer :: unit
      integer(kind=INT64) :: MEM_UNITS = 4096 ! page size is 4096 bytes
      character(:), allocatable :: tmp_file
      block
        use MPI
        integer :: rank, ierror
        call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)
        allocate(character(4) :: tmp_file)
        write(tmp_file,'(i4.4)')rank
        tmp_file = 'tmp_' // tmp_file // '.dat'
        if (rank == 0) then
           call execute_command_line("vm_stat | grep free | awk '{ print $3 }'> " // tmp_file)
      
           open(newunit=unit, file=tmp_file, form='formatted', access='sequential', status='old')
           read(unit,*) mem_use
           mem_use = - mem_use * MEM_UNITS ! mem free is negative memory used
           close(unit, status='delete')
        else
           mem_use = 0
        end if
      end block



   end function get_measurement


end module MAPL_VmstatMemoryGauge

   
   
   

