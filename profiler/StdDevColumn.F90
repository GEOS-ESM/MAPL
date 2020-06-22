module MAPL_StdDevColumn
   use, intrinsic :: iso_fortran_env, only: REAL64
   use MAPL_AbstractColumn
   use MAPL_SimpleColumn
   use MAPL_AbstractMeterNode
   use MAPL_AbstractMeter
   use MAPL_AdvancedMeter
   use Mapl_DistributedMeter
   implicit none
   private

   public :: StdDevColumn

   type, extends(SimpleColumn) :: StdDevColumn
      private
      logical :: relative = .false.
      character(:), allocatable :: option
   contains
      procedure :: get_row
      procedure :: get_row_dist
   end type StdDevColumn

   interface StdDevColumn
      module procedure :: new_StdDevColumn
   end interface StdDevColumn


contains


   function new_StdDevColumn(relative, option) result(column)
      type(StdDevColumn) :: column
      logical, optional, intent(in) :: relative
      character(*), optional, intent(in) :: option

      if (present(relative)) column%relative = relative
      if (present(option)) column%option = option

   end function new_StdDevColumn


   function get_row(this, node) result(row)
      class(*), allocatable :: row
      class (StdDevColumn), intent(in) :: this
      class (AbstractMeterNode), target, intent(in) :: node

      class (AbstractMeter), pointer :: tmr


      if (.not. allocated(this%option)) then
         tmr => node%get_meter()
         select type (tmr)
         class is (AdvancedMeter)
            if (this%relative) then
               allocate(row, source=tmr%get_relative_deviation())
            else
               allocate(row, source=tmr%get_standard_deviation())
            end if
         class default
            print*,'error handling here'
         end select
      else
         call this%get_row_dist(node, row)
      end if

   end function get_row


   subroutine get_row_dist(this, node, row)
      class (StdDevColumn), target, intent(in) :: this
      class (AbstractMeterNode), target, intent(in) :: node
      class(*), allocatable, intent(out) :: row

      class(AbstractMeter), pointer :: m
      type(DistributedStatistics) :: stats
      type(DistributedReal64) :: std_deviation

      m => node%get_meter()

      select type (m)
      class is (DistributedMeter)
         stats = m%get_statistics()
         std_deviation = stats%sum_square_deviation
         print*,__FILE__,__LINE__,'std deviation not fully implemented'
         call this%fill_row(std_deviation, this%option, row)
      end select
   end subroutine get_row_dist

end module MAPL_StdDevColumn


