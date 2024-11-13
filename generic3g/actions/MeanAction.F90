#include "MAPL_Generic.h"
module mapl3g_MeanAction
   use mapl3g_AccumulatorAction
   use MAPL_InternalConstantsMod, only: MAPL_UNDEFINED_REAL, MAPL_UNDEFINED_REAL64
   use MAPL_ExceptionHandling
   use MAPL_FieldPointerUtilities
   use ESMF
   implicit none
   private
   public :: MeanAction

   type, extends(AccumulatorAction) :: MeanAction
      !private
      integer(ESMF_KIND_R8) :: counter_scalar = 0_ESMF_KIND_I8
      logical, allocatable :: valid_mean(:)
   contains
      procedure :: invalidate => invalidate_mean_accumulator
      procedure :: clear_accumulator => clear_mean_accumulator
      procedure :: update => update_mean_accumulator
      procedure :: calculate_mean
      procedure :: calculate_mean_R4
      procedure :: clear_valid_mean
      procedure :: accumulate_R4 => accumulate_mean_R4
   end type MeanAction

contains

   subroutine clear_mean_accumulator(this, rc)
      class(MeanAction), intent(inout) :: this
      integer, optional, intent(out) :: rc
      
      integer :: status

      this%counter_scalar = 0_ESMF_KIND_R8
      call this%clear_valid_mean(_RC)
      call this%AccumulatorAction%clear_accumulator(_RC)
      _RETURN(_SUCCESS)

   end subroutine clear_mean_accumulator

   subroutine clear_valid_mean(this, rc)
      class(MeanAction), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: local_size

      if(allocated(this%valid_mean)) deallocate(this%valid_mean)
      local_size = FieldGetLocalSize(this%accumulation_field, _RC)
      allocate(this%valid_mean(local_size), source = .FALSE.)
      _RETURN(_SUCCESS)

   end subroutine clear_valid_mean

   subroutine calculate_mean(this, rc)
      class(MeanAction), intent(inout) :: this
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
      _RETURN(_SUCCESS)

   end subroutine calculate_mean

   subroutine update_mean_accumulator(this, importState, exportState, clock, rc)
      class(MeanAction), intent(inout) :: this
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
      class(MeanAction), intent(inout) :: this
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
      class(MeanAction), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: current_ptr(:) => null()
      real(kind=ESMF_KIND_R4), parameter :: UNDEF = MAPL_UNDEFINED_REAL

      call assign_fptr(this%accumulation_field, current_ptr, _RC)
      where(current_ptr /= UNDEF .and. this%valid_mean)
         current_ptr = current_ptr / this%counter_scalar
      elsewhere
         current_ptr = UNDEF
      end where
      _RETURN(_SUCCESS)

   end subroutine calculate_mean_R4

   subroutine accumulate_mean_R4(this, update_field, rc)
      class(MeanAction), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: update_field
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: current(:)
      real(kind=ESMF_KIND_R4), pointer :: latest(:)
      real(kind=ESMF_KIND_R4) :: undef

      undef = MAPL_UNDEFINED_REAL
      call assign_fptr(this%accumulation_field, current, _RC)
      call assign_fptr(update_field, latest, _RC)
      where(current /= undef .and. latest /= undef)
        current = current + latest
        this%valid_mean = .TRUE.
      elsewhere(latest == undef)
        current = undef
      end where
      _RETURN(_SUCCESS)

   end subroutine accumulate_mean_R4

end module mapl3g_MeanAction
