module mapl3g_Accumulator

   use mapl3g_AccumulationCounter
   implicit none
   private
   public :: Accumulator

   type :: Accumulator
      logical :: is_active = .FALSE.
      type(ESMF_Field), allocatable :: accumulation_data
      type(AccumulationCounter) :: accumulation_counter => null()
      type(ESMF_Alarm), pointer :: time_to_clear => null()
      type(ESMF_Alarm), pointer :: time_to_couple => null()
      integer :: scalar_count = -1
   contains
      procedure :: has_counter
   end type Accumulator

   interface Accumulator
      module procedure :: construct_accumulator
   end interface Accumulator

contains

   function construct_accumulator(accumulator_type) result(acc)
      type(Accumulator) :: acc
      integer, intent(in) :: accumulator_type
      real(kind=ESMF_KIND_R4), intent(in) :: clear_value
      integer(kind=ESMF_KIND_I4), intent(in) :: counter_reset

   end function construct_accumulator

end module mapl3g_Accumulator
