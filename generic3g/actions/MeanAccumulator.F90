#include "MAPL_Generic.h"
module mapl3g_MeanAccumulator
   use mapl3g_AccumulatorAction
   use MAPL_InternalConstantsMod, only: MAPL_UNDEFINED_REAL, MAPL_UNDEFINED_REAL64
   use MAPL_ExceptionHandling
   use MAPL_FieldPointerUtilities
   use ESMF
   implicit none
   private
   public :: MeanAccumulator

   type, extends(AccumulatorAction) :: MeanAccumulator
      !private
      integer(ESMF_KIND_R8) :: counter_scalar = 0_ESMF_KIND_I8
      logical, allocatable :: valid_points(:)
   contains
      procedure :: invalidate => invalidate_mean_accumulator
      procedure :: clear_accumulator => clear_mean_accumulator
      procedure :: update => update_mean_accumulator
      procedure :: calculate_mean
      procedure :: calculate_mean_R4
      procedure :: clear_valid_points
   end type MeanAccumulator

contains

   subroutine clear_mean_accumulator(this, rc)
      class(MeanAccumulator), intent(inout) :: this
      integer, optional, intent(out) :: rc
      
      integer :: status

      this%counter_scalar = 0_ESMF_KIND_R8
      call this%clear_valid_points(_RC)
      call this%AccumulatorAction%clear_accumulator(_RC)
      _RETURN(_SUCCESS)

   end subroutine clear_mean_accumulator

   subroutine clear_valid_points(this, rc)
      class(MeanAccumulator), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: local_size

      if(allocated(this%valid_points)) deallocate(this%valid_points)
      local_size = FieldGetLocalSize(this%accumulation_field, _RC)
      allocate(this%valid_points(local_size), source = .FALSE.)
      _RETURN(_SUCCESS)

   end subroutine clear_valid_points

   subroutine calculate_mean(this, rc)
      class(MeanAccumulator), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TypeKind_Flag) :: tk

      _ASSERT(this%counter_scalar > 0, 'Cannot calculate mean for zero steps')
      call ESMF_FieldGet(this%accumulation_field, typekind=tk, _RC)
      if(tk == ESMF_TypeKind_R4) then
         call this%calculate_mean_R4(_RC)
      else
         _FAIL('Unsupported typekind')
      end if
      !_RESULT(_SUCCESS)

   end subroutine calculate_mean

   subroutine update_mean_accumulator(this, importState, exportState, clock, rc)
      class(MeanAccumulator), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      
      _ASSERT(this%initialized(), 'Accumulator has not been initialized.')
      if(.not. this%update_calculated) then
         call this%calculate_mean(_RC)
      end if
      call this%AccumulatorAction%update(importState, exportState, clock, _RC)
      _RETURN(_SUCCESS)

   end subroutine update_mean_accumulator

   subroutine invalidate_mean_accumulator(this, importState, exportState, clock, rc)
      class(MeanAccumulator), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      
      call this%AccumulatorAction%invalidate(importState, exportState, clock, _RC)
      this%counter_scalar = this%counter_scalar + 1
      _RETURN(_SUCCESS)

   end subroutine invalidate_mean_accumulator

   subroutine calculate_mean_R4(this, rc)
      class(MeanAccumulator), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: current_ptr(:) => null()
      real(kind=ESMF_KIND_R4), pointer :: calculated_ptr(:) => null()
      real(kind=ESMF_KIND_R4), parameter :: UNDEF = MAPL_UNDEFINED_REAL

      call assign_fptr(this%accumulation_field, current_ptr, _RC)
      call assign_fptr(this%result_field, calculated_ptr, _RC)
      associate(counter => this%counter_scalar, valid => this%valid_points)
         where(current_ptr /= UNDEF .and. valid)
            calculated_ptr = current_ptr / counter
         elsewhere
            calculated_ptr = UNDEF
         end where
      end associate

      _RETURN(_SUCCESS)

   end subroutine calculate_mean_R4

end module mapl3g_MeanAccumulator
