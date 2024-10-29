#include "MAPL_Generic.h"
module mapl3g_AccumulatorAction
   use mapl3g_ExtensionAction
   use MAPL_ExceptionHandling
   use ESMF
   implicit none
   private
   public :: AccumulatorAction

   type, extends(ExtensionAction), abstract :: AccumulatorAction
      private
      type(ESMF_Field), allocatable :: accumulation_field
      type(ESMF_Field), allocatable :: result_field
   contains
      ! Implementations of deferred procedures
      procedure :: invalidate => invalidate_default_action
      procedure :: invalidate_default_action
      procedure :: initialize
      procedure :: update
      ! Helpers
      procedure, private :: accumulate
      procedure, private :: initialized => action_initialized
      procedure, private :: clear_accumulator => clear_accumulator_default
      procedure, private :: clear_accumulator_default
      procedure, private :: get_undef_R4
      procedure, private :: get_clear_value_R4
      procedure, private :: calculate_result
      ! Deferred
      procedure(AccumulateR4),  deferred :: accumulate_R4
   end type AccumulatorAction

   abstract interface
      subroutine AccumulateR4(this, update_field, rc)
         import :: AccumulatorAction
         class(AccumulatorAction), intent(in) :: this
         type(ESMF_Field), intent(inout) :: update_field
         integer, optional, intent(out) :: rc
      end subroutine AccumulateR4
   end interface

   type, extends(AccumulatorAction) :: MeanAccumulator
      private
      integer(ESMF_KIND_R8) :: counter_scalar = 0_ESMF_KIND_I8
      real(kind=ESMF_KIND_R4) :: clear_value_R4 = 0.0_ESMF_KIND_I4
   contains
      procedure :: invalidate => invalidate_mean_accumulator
      procedure, private :: clear_accumulator => clear_accumulator_mean
      procedure, private :: increment_counter
      procedure, private :: clear_counter
      procedure, private :: calculate_result => calculate_mean
      procedure, private :: accumulate_R4 => add_accumulate_R4
   end type MeanAccumulator

   type, extends(AccumulatorAction) :: MaxAccumulator
      private
   contains
      procedure, private :: get_clear_value_R4_undef
      procedure, private :: accumulate_R4 => max_accumulate_R4
   end type MaxAccumulator

contains

   logical function action_initialized(this) result(lval)
      class(AccumulatorAction), intent(in) :: this

      lval = allocated(this%accumulation_field)

   end function action_initialized
   
   function get_undef_R4(this) result(undef)
      real(kind=ESMF_KIND_R4) :: undef
      class(AccumulatorAction), intent(in) :: this

      undef = MAPL_UNDEFINED_REAL

   end function get_undef_R4

   function get_clear_value_R4(this) result(clear_value)
      real(kind=ESMF_KIND_R4) :: clear_value
      class(AccumulatorAction), intent(in) :: this

      clear_value = this%clear_value_R4

   end function get_clear_value_R4

   function get_clear_value_R4_undef(this) result(clear_value)
      real(kind=ESMF_KIND_R4) :: clear_value
      class(AccumulatorAction), intent(in) :: this

      clear_value = this%get_undef_R4()

   end function get_clear_value_R4_undef

   subroutine clear_accumulator_default(this, rc)
      class(AccumulatorAction), intent(inout) :: this
      integer, optional, intent(out) :: rc
      
      integer :: status
      type(ESMF_TypeKind_Flag) :: tk
      real(kind=ESMF_KIND_R4) :: clear_value_R4

      call ESMF_FieldGet(this%accumulation_field, typekind=tk, _RC)
      if(tk == ESMF_TYPEKIND_R4) then
         call FieldSet(this%accumulation_field, this%get_clear_value_R4(), _RC)
      else
         _FAIL('Unsupported typekind')
      end if
      _RETURN(_SUCCESS)

   end subroutine clear_accumulator_default

   subroutine clear_accumulator_mean(this, rc)
      class(MeanAccumulator), intent(inout) :: this
      integer, optional, intent(out) :: rc
      
      integer :: status

      call this%clear_accumulator_default(_RC)
      call this%clear_counter()
      _RETURN(_SUCCESS)

   end subroutine clear_accumulator_mean

   subroutine clear_counter(this, rc)
      class(MeanAccumulator), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      this%counter_scalar = 0_ESMF_KIND_R8
      _RETURN(_SUCCESS)

   end subroutine clear_counter

   subroutine initialize(this, importState, exportState, clock, rc)
      class(AccumulatorAction), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: import_field

      call get_field(importState, import_field, _RC)
      call FieldCopy(import_field, this%accumulation_field, _RC)
      call FieldSet(this%accumulation_field, this%get_clear_value(), _RC)
      call this%clear_accumulator(_RC)
      this%result_field = this%accumulation_field
      _RETURN(_SUCCESS)

   end subroutine initialize

   subroutine update(this, importState, exportState, clock, rc)
      class(AccumulatorAction), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: export_field
      
      if(.not. this%update_calculated) then
         call this%calculate_result(this, _RC)
         this%update_calculated = .TRUE.
      end if
      call get_field(exportState, export_field, _RC)
      export_field = this%result_field
      call this%clear_accumulator(_RC)
      _RETURN(_SUCCESS)

   end subroutine update

   subroutine calculate_result(this, rc)
      class(AccumulatorAction), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      this%result_field = this%accumulation_field
      _RETURN(_SUCCESS)

   end subroutine calculate_result

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
      this%result_field = this%accumulation_field
      _RESULT(_SUCCESS)

   end subroutine calculate_mean

   subroutine invalidate_default_action(this, importState, exportState, clock, rc)
      class(AccumulatorAction), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: import_field
      
      this%update_calculated = .FALSE.
      call get_field(importState, import_field, _RC)
      call this%accumulate(import_field, _RC)
      _RETURN(_SUCCESS)

   end subroutine invalidate_default_action

   subroutine invalidate_mean_accumulator(this, 
      class(MeanAccumulator), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: import_field
      
      call this%invalidate_default_action(importState, exportState, clock, rc)
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

   subroutine get_field(state, field, rc)
      type(ESMF_State), intent(inout) :: state
      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: itemCount
      character(len=ESMF_MAXSTR) :: itemNameList(1)

      call ESMF_StateGet(state, itemCount=itemCount, _RC)
      _ASSERT(itemCount == size(itemNameList, 'itemCount does not equal the expected value.'))
      call ESMF_StateGet(state, itemNameList=itemNameList, _RC)
      call ESMF_StateGet(state, itemName=itemNameList(1), _RC)
      _RETURN(_SUCCESS

   end subroutine get_field

   subroutine calculate_mean_R4(this, rc)
      class(MeanAccumulator), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4) pointer :: current_ptr(:)
      real(kind=ESMF_KIND_R4) pointer :: calculated_ptr(:)

      associate(current => this%accumulation_field, calculated => this%result_field,&
         & undef => this%get_undef_R4(), count => this%counter_scalar)
         assign_fptr(current, current_ptr, _RC)
         assign_fptr(calculated, calculated_ptr, _RC)
         where(current_ptr /= undef)
            calculated_ptr => current_ptr / count
         elsewhere
            calculated_ptr => undef
         end where
      end associate

      _RETURN(_SUCCESS)

   end subroutine calculate_mean_R4

   subroutine accumulate(this, update_field, rc)
      class(AccumulatorAction), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: update_field
      integer, optional, intent(out) :: rc
      
      integer :: status
      type(ESMF_TypeKind_Flag) :: tk, tk_field

      call ESMF_FieldGet(this%accumulation_field, typekind=tk, _RC)
      call ESMF_FieldGet(update_field, typekind=tk_field, _RC)
      _ASSERT(tk == tk_field, 'Update field must be the same typekind as the accumulation field.')
      if(tk == ESMF_TYPEKIND_R4) then
         call this%accumulate_R4(this, update_field, _RC)
      else
         _FAIL('Unsupported typekind value')
      end if

      _RETURN(_SUCCESS)

   end subroutine accumulate

   subroutine add_accumulate_R4(this, update_field, rc)
      class(AccumulatorAction), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: update_field
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer, intent(inout) :: current(:)
      real(kind=ESMF_KIND_R4), pointer, intent(in) :: latest(:)
      real(kind=ESMF_KIND_R4) :: undef

      undef = this%get_undef_R4()
      call assign_fptr(this%accumulation_field, current, _RC)
      call assign_fptr(update_field, latest, _RC)
      where(current /= undef .and. latest /= undef)
        current = current + latest
      elsewhere(latest == undef)
        current = undef
      end where
      _RETURN(_SUCCESS)

   end subroutine add_accumulate_R4

    subroutine max_accumulate_R4(this, update_field, rc)
      class(AccumulatorAction), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: update_field
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer, intent(inout) :: current(:)
      real(kind=ESMF_KIND_R4), pointer, intent(in) :: latest(:)
      real(kind=ESMF_KIND_R4) :: undef
      
      undef = this%get_undef_R4()
      call assign_fptr(this%accumulation_field, current, _RC)
      call assign_fptr(update_field, latest, _RC)
      where(current /= undef .and. latest /= undef)
         current = max(current, latest)
      end where
      _RETURN(_SUCCESS)

   end function max_accumulate_R4

end module mapl3g_AccumulatorAction
