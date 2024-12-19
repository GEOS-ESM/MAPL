module MAPL_AbstractColumn
   use GFTL_UnlimitedVector
   use MAPL_AbstractMeterNode
   use MAPL_DistributedMeter
   implicit none
   private

   public :: AbstractColumn

   type, abstract :: AbstractColumn
      private
   contains
      procedure(i_get_rows), deferred :: get_rows
      procedure, nopass :: fill_row_real64_stats
      procedure, nopass :: fill_row_integer_stats
      generic :: fill_row => fill_row_real64_stats, fill_row_integer_stats
   end type AbstractColumn


   abstract interface

      function i_get_rows(this, node) result(rows)
         import AbstractColumn
         import AbstractMeterNode
         import UnlimitedVector
         ! Some columns return reals, others return integers
         type(UnlimitedVector) :: rows
         class(AbstractColumn), intent(in) :: this
         class(AbstractMeterNode), target, intent(in) :: node

      end function i_get_rows

   end interface


contains


   ! These probably belong somewhere else.
   subroutine fill_row_real64_stats(stats, option, row)
      type(DistributedReal64), intent(in) :: stats
      character(*), intent(in) :: option
      class(*), allocatable, intent(out) :: row

      select case (option)
      case ('MAX')
         allocate(row, source=stats%max)
      case ('MAX_PE')
         allocate(row, source=stats%max_pe)
      case ('MIN')
         allocate(row, source=stats%min)
      case ('MIN_PE')
         allocate(row, source=stats%min_pe)
      case ('MEAN')
         allocate(row, source=stats%total / stats%num_pes)
      case ('TOTAL')
         allocate(row, source=stats%total)
      case default
         print*,__FILE__,__LINE__,'ERROR: unsupported option '//option
      end select

   end subroutine fill_row_real64_stats

   subroutine fill_row_integer_stats(stats, option, row)
      type(DistributedInteger), intent(in) :: stats
      character(*), intent(in) :: option
      class(*), allocatable, intent(out) :: row

      select case (option)
      case ('MAX')
         allocate(row, source=stats%max)
      case ('MAX_PE')
         allocate(row, source=stats%max_pe)
      case ('MIN')
         allocate(row, source=stats%min)
      case ('MIN_PE')
         allocate(row, source=stats%min_pe)
      case ('MEAN')
         allocate(row, source=stats%total / stats%num_pes)
      case ('TOTAL')
         allocate(row, source=stats%total)
      case default
         print*,__FILE__,__LINE__,'ERROR: unsupported option '//option
      end select

   end subroutine fill_row_integer_stats


end module MAPL_AbstractColumn
