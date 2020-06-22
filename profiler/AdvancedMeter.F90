#include "unused_dummy.H"

module MAPL_AdvancedMeter
   use, intrinsic :: iso_fortran_env, only: REAL64
   use MAPL_AbstractMeter
   use MAPL_AbstractGauge
   implicit none
   private

   public :: AdvancedMeter

   public :: MAPL_METER_IS_VALID
   public :: MAPL_METER_START_ACTIVE
   public :: MAPL_METER_STOP_INACTIVE

   enum, bind(c)
      enumerator :: MAPL_METER_IS_VALID = 0
      enumerator :: MAPL_METER_START_ACTIVE
      enumerator :: MAPL_METER_STOP_INACTIVE
   end enum

   type, extends(AbstractMeter) :: AdvancedMeter
      private

      class(AbstractGauge), allocatable :: gauge
      real(kind=REAL64) :: start_value

      real(kind=REAL64) :: total = 0
      logical :: active = .false.
      integer :: status = MAPL_METER_IS_VALID

      real(kind=REAL64) :: min_cycle = huge(1._REAL64)
      real(kind=REAL64) :: max_cycle = 0.
      real(kind=REAL64) :: sum_square_deviation = 0.

      integer :: num_cycles = 0

   contains

      procedure :: start
      procedure :: stop
      
      procedure :: reset
      procedure :: is_active
      procedure :: get_status
      procedure :: get_overhead

      procedure :: get_total

      procedure :: get_min_cycle
      procedure :: get_max_cycle
      procedure :: get_mean_cycle
      procedure :: get_sum_square_deviation
      procedure :: get_standard_deviation
      procedure :: get_relative_deviation
      procedure :: get_num_cycles

      procedure :: add_cycle
      procedure :: accumulate


   end type AdvancedMeter



   interface AdvancedMeter
      module procedure :: new_AdvancedMeter
   end interface AdvancedMeter


contains


   function new_AdvancedMeter(gauge) result(meter)
      type(AdvancedMeter) :: meter
      class(AbstractGauge), intent(in) :: gauge

      meter%gauge = gauge
      
   end function new_AdvancedMeter


   subroutine start(this)
      class(AdvancedMeter), intent(inout) :: this

      if (this%active) then
         this%status = MAPL_METER_START_ACTIVE
         return
      end if

      this%active = .true.

      this%start_value = this%gauge%get_measurement()

   end subroutine start


   subroutine stop(this)
      class(AdvancedMeter), intent(inout) :: this

      real(kind=REAL64) :: increment
      
      if (.not. this%active) then
         this%status = MAPL_METER_STOP_INACTIVE
         return
      end if

      this%active = .false.
      increment = this%gauge%get_measurement() - this%start_value
      call this%add_cycle(increment)

   end subroutine stop


   function get_total(this) result(val)
      real(kind=REAL64) :: val
      class(AdvancedMeter), intent(in) :: this

      val = this%total

   end function get_total
   

   logical function is_active(this)
      class(AdvancedMeter), intent(in) :: this
      is_active = this%active
   end function is_active


   integer function get_status(this) result(status)
      class(AdvancedMeter), intent(in) :: this
      status = this%status
   end function get_status


   subroutine add_cycle(this, increment)
      class(AdvancedMeter), intent(inout) :: this
      real(kind=REAL64), intent(in) :: increment

      real(kind=REAL64) :: old_mean, new_mean

      associate ( n => this%num_cycles, t => increment )
        this%min_cycle = min(this%min_cycle, t)
        this%max_cycle = max(this%max_cycle, t)

        old_mean = this%get_mean_cycle()
        n = n + 1
        new_mean = old_mean + (t - old_mean) / n ! denominator provably always > 0 (modulo integer overflow)

        this%sum_square_deviation = this%sum_square_deviation + (t - old_mean)*(t - new_mean)

        this%total = this%total + t

      end associate


   end subroutine add_cycle


   subroutine reset(this)
      class(AdvancedMeter), intent(inout) :: this

      this%total = 0
      this%active = .false.

      this%num_cycles = 0
      this%min_cycle = huge(1._REAL64)
      this%max_cycle = 0._REAL64
      this%sum_square_deviation = 0._REAL64

   end subroutine reset



   function get_min_cycle(this) result(min_cycle)
      real(kind=REAL64) :: min_cycle
      class(AdvancedMeter), intent(in) :: this

      min_cycle = this%min_cycle

   end function get_min_cycle
   

   function get_max_cycle(this) result(max_cycle)
      real(kind=REAL64) :: max_cycle
      class(AdvancedMeter), intent(in) :: this

      max_cycle = this%max_cycle

   end function get_max_cycle
   

   function get_mean_cycle(this) result(mean_cycle)
      real(kind=REAL64) :: mean_cycle
      class(AdvancedMeter), intent(in) :: this

      integer :: n

      n = this%get_num_cycles()
      if (n > 0) then
         mean_cycle = this%total / n
      else
         mean_cycle = 0  ! undefined actually
      end if

   end function get_mean_cycle
   

   function get_sum_square_deviation(this) result(sum_square_deviation)
      real(kind=REAL64) :: sum_square_deviation
      class(AdvancedMeter), intent(in) :: this

      sum_square_deviation =  this%sum_square_deviation

   end function get_sum_square_deviation
   

   function get_standard_deviation(this) result(standard_deviation)
      real(kind=REAL64) :: standard_deviation
      class(AdvancedMeter), intent(in) :: this

      standard_deviation =  sqrt(this%sum_square_deviation / this%num_cycles)

   end function get_standard_deviation
   

   ! Relative standard deviation (expressed as percentage)
   !      R = 100 * standard_deviation / mean 
   ! https://en.wikipedia.org/wiki/Coefficient_of_variation
   function get_relative_deviation(this) result(relative_deviation)
      use, intrinsic :: ieee_arithmetic, only: IEEE_POSITIVE_INF, ieee_value
      real(kind=REAL64) :: relative_deviation
      class(AdvancedMeter), intent(in) :: this

      real(kind=REAL64) :: abs_mean
      
      abs_mean = abs(this%get_mean_cycle())
      if (abs_mean > 0) then
         relative_deviation = 100*(this%get_standard_deviation()/abs_mean)
      else
         ! Gfortran stops with overflow exception even to do the assignment below.
         ! So we default to 0 when there the mean is 0.
         relative_deviation = ieee_value(1.0_REAL64, IEEE_POSITIVE_INF)
      end if

   end function get_relative_deviation
   

   integer function get_num_cycles(this) result(num_cycles)
      class(AdvancedMeter), intent(in) :: this

      num_cycles = this%num_cycles

   end function get_num_cycles
      

   function get_overhead(this) result(overhead)
      real(kind=REAL64) :: overhead
      class(AdvancedMeter), intent(in) :: this
      
      class(AdvancedMeter), allocatable :: t_outer
      class(AdvancedMeter), allocatable :: t_inner
      
      call t_outer%start()
      call t_inner%start()
      call t_inner%stop()
      call t_outer%stop()

      overhead = t_outer%get_total()

   end function get_overhead


   subroutine accumulate(this, lap)
      class(AdvancedMeter), intent(inout) :: this
      class(AbstractMeter), intent(in) :: lap

      select type(lap)
      class is (AdvancedMeter)
         this%min_cycle = min(this%min_cycle, lap%min_cycle)
         this%max_cycle = max(this%max_cycle, lap%max_cycle)

         this%total = this%total + lap%total
         this%num_cycles = this%num_cycles + lap%num_cycles
         this%sum_square_deviation = this%sum_square_deviation + lap%sum_square_deviation
      class default
         print*,'add error handling here'
      end select

   end subroutine accumulate

end module MAPL_AdvancedMeter
