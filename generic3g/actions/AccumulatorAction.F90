#include "MAPL_Generic.h"
module mapl3g_AccumulatorAction
   use mapl3g_ExtensionAction
   use MAPL_InternalConstantsMod, only: MAPL_UNDEFINED_REAL, MAPL_UNDEFINED_REAL64
   use MAPL_FieldUtilities
   use MAPL_FieldPointerUtilities
   use MAPL_ExceptionHandling
   use ESMF
   implicit none
   private
   public :: AccumulatorAction

   type, extends(ExtensionAction) :: AccumulatorAction
      private
      logical :: update_calculated = .FALSE.
      type(ESMF_Field), allocatable :: accumulation_field
      type(ESMF_Field), allocatable :: result_field
      real(kind=ESMF_KIND_R4) :: undef_value_R4 = MAPL_UNDEFINED_REAL
      real(kind=ESMF_KIND_R4) :: clear_value_R4 = 0.0_ESMF_KIND_R4
   contains
      ! Implementations of deferred procedures
      procedure :: invalidate
      procedure :: initialize
      procedure :: update
      ! Helpers
      procedure, private :: accumulate
      procedure, private :: initialized
      procedure, private :: clear_accumulator
      procedure, private :: clear_fields
      procedure, private :: accumulate_R4
      procedure, private :: calculate_result
   end type AccumulatorAction

contains

   logical function initialized(this) result(lval)
      class(AccumulatorAction), intent(in) :: this

      lval = allocated(this%accumulation_field)

   end function initialized

   subroutine clear_accumulator(this, rc)
      class(AccumulatorAction), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status
   
      call this%clear_fields(_RC)
      _RETURN(_SUCCESS)

   end subroutine clear_accumulator

   subroutine clear_fields(this, rc)
      class(AccumulatorAction), intent(inout) :: this
      integer, optional, intent(out) :: rc
      
      integer :: status
      type(ESMF_TypeKind_Flag) :: tk
      real(kind=ESMF_KIND_R4) :: clear_value_R4 !wdb fixme deleteme

      call ESMF_FieldGet(this%accumulation_field, typekind=tk, _RC)
      if(tk == ESMF_TYPEKIND_R4) then
         call FieldSet(this%accumulation_field, this%clear_value_R4, _RC)
      else
         _FAIL('Unsupported typekind')
      end if
      _RETURN(_SUCCESS)

   end subroutine clear_fields

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
      call this%clear_accumulator(_RC)
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
         call this%calculate_result(_RC)
         call FieldCopy(this%accumulation_field, this%result_field, _RC)
      end if
      call get_field(exportState, export_field, _RC)
      export_field = this%result_field !wdb fixme deleteme Does this need to be a copy?

      call this%clear_accumulator(_RC)
      _RETURN(_SUCCESS)

   end subroutine update

   subroutine calculate_result(this, rc)
      class(AccumulatorAction), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      this%update_calculated = .TRUE.

      _RETURN(_SUCCESS)

   end subroutine calculate_result

   subroutine invalidate(this, importState, exportState, clock, rc)
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

   end subroutine invalidate

   subroutine get_field(state, field, rc)
      type(ESMF_State), intent(inout) :: state
      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: itemCount
      integer, parameter :: N = 1
      character(len=ESMF_MAXSTR) :: itemNameList(N)
      type(ESMF_StateItem_Flag) :: itemTypeList(N)

      call ESMF_StateGet(state, itemCount=itemCount, _RC)
      _ASSERT(itemCount == N, 'itemCount does not equal the expected value.')
      call ESMF_StateGet(state, itemNameList=itemNameList, itemTypeList=itemTypeList, _RC)
      _ASSERT(itemTypeList(N) == ESMF_STATEITEM_FIELD, 'State item is the wrong type.')
      call ESMF_StateGet(state, itemName=itemNameList(N), field=field, _RC)
      _RETURN(_SUCCESS)

   end subroutine get_field

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
         call this%accumulate_R4(update_field, _RC)
      else
         _FAIL('Unsupported typekind value')
      end if

      _RETURN(_SUCCESS)

   end subroutine accumulate

   subroutine accumulate_R4(this, update_field, rc)
      class(AccumulatorAction), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: update_field
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: current(:)
      real(kind=ESMF_KIND_R4), pointer :: latest(:)
      real(kind=ESMF_KIND_R4) :: undef

      undef = this%undef_value_R4
      call assign_fptr(this%accumulation_field, current, _RC)
      call assign_fptr(update_field, latest, _RC)
      where(current /= undef .and. latest /= undef)
        current = current + latest
      elsewhere(latest == undef)
        current = undef
      end where
      _RETURN(_SUCCESS)

   end subroutine accumulate_R4

end module mapl3g_AccumulatorAction
