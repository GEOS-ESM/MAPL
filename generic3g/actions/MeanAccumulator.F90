#include "MAPL_Generic.h"
module mapl3g_MeanAccumulator
   use mapl3g_AccumulatorAction
   use MAPL_ExceptionHandling
   use ESMF
   implicit none
   private
   public :: MeanAccumulator

   type, extends(AccumulatorAction) :: MeanAccumulator
      private
      integer(ESMF_KIND_R8) :: counter_scalar = 0_ESMF_KIND_I8
      logical, allocatable :: valid_points(:)
   contains
      procedure :: invalidate => invalidate_mean_accumulator
      procedure, private :: clear_accumulator => clear_accumulator_mean
      procedure, private :: increment_counter
      procedure, private :: clear_counter
      procedure, private :: calculate_result => calculate_mean
   end type MeanAccumulator

contains

   subroutine clear_accumulator_mean(this, rc)
      class(MeanAccumulator), intent(inout) :: this
      integer, optional, intent(out) :: rc
      
      integer :: status

      call this%AccumulatorAction%clear_accumulator(_RC)
      call this%clear_counter()
      call this%clear_valid_points(_RC)
      _RETURN(_SUCCESS)

   end subroutine clear_accumulator_mean

   subroutine clear_counter(this)
      class(MeanAccumulator), intent(inout) :: this

      this%counter_scalar = 0_ESMF_KIND_R8

   end subroutine clear_counter

   subroutine clear_valid_points(this, rc)
      class(MeanAccumulator), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: local_size

      associate(valid => this%valid_points)
         if(allocated(valid)) deallocate(valid)
         local_size = FieldGetLocalSize(this%accumulation_field, _RC)
         allocate(valid(local_size), source = .FALSE.)
      end associate
      _SUCCESS(_RETURN)

   end subroutine clear_valid_points

   subroutine calculate_mean(this, rc)
      class(MeanAccumulator), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TypeKind_Flag) :: tk

      _ASSERT(this%counter_scalar> 0, 'Cannot calculate mean for zero steps')
      call ESMF_FieldGet(this%accumulation_field, typekind=tk, _RC)
      if(tk == ESMF_TypeKind_R4) then
         call this%calculate_mean_R4(_RC)
      else
         _FAIL('Unsupported typekind')
      end if
      call this%result_field = this%accumulation_field
      _RESULT(_SUCCESS)

   end subroutine calculate_mean

   subroutine invalidate_mean_accumulator(this, importState, exportState, clock, rc)
      class(MeanAccumulator), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      
      call this%AccumulatorAction%invalidate(importState, exportState, clock, rc)
      call this%increment_counter(_RC)
      _RETURN(_SUCCESS)

   end subroutine invalidate_mean_accumulator

   subroutine increment_counter(this, _RC)
      class(MeanAccumulator), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      this%counter_scalar = this%counter_scalar + 1
      _RETURN(_SUCCESS)

   end subroutine increment_counter

   subroutine calculate_mean_R4(this, rc)
      class(MeanAccumulator), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: current_ptr(:) => null()
      real(kind=ESMF_KIND_R4), pointer :: calculated_ptr(:) => null()

      associate(undef => this%get_undef_R4(), counter => this%counter_scalar, valid => this%valid_points)
         assign_fptr(this%accumulation_field, current_ptr, _RC)
         assign_fptr(this%result_field, calculated_ptr, _RC)
         where(current_ptr /= undef .and. valid)
            calculated_ptr => current_ptr / counter
         elsewhere
            calculated_ptr => undef
         end where
      end associate

      _RETURN(_SUCCESS)

   end subroutine calculate_mean_R4

end module mapl3g_MeanAccumulator
