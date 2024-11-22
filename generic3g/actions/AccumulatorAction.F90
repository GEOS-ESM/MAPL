#include "MAPL_Generic.h"
module mapl3g_AccumulatorAction
   use mapl3g_ExtensionAction
   use MAPL_InternalConstantsMod, only: MAPL_UNDEFINED_REAL, MAPL_UNDEFINED_REAL64
   use MAPL_FieldUtilities, only: FieldSet 
   use MAPL_FieldPointerUtilities
   use MAPL_ExceptionHandling
   use ESMF
   implicit none
   private
   public :: AccumulatorAction

   type, extends(ExtensionAction) :: AccumulatorAction
      type(ESMF_Field) :: accumulation_field
      type(ESMF_Field) :: result_field
      real(kind=ESMF_KIND_R4) :: CLEAR_VALUE_R4 = 0.0_ESMF_KIND_R4
      logical :: update_calculated = .FALSE.
      type(ESMF_TypeKind_Flag) :: typekind = ESMF_TYPEKIND_R4
   contains
      ! Implementations of deferred procedures
      procedure :: invalidate
      procedure :: initialize
      procedure :: update
      ! Helpers
      procedure :: initialized
      procedure :: accumulate
      procedure :: accumulate_R4
      procedure :: clear
      procedure :: create_fields
      procedure :: update_result
   end type AccumulatorAction

contains

   logical function initialized(this) result(lval)
      class(AccumulatorAction), intent(in) :: this

      lval = ESMF_FieldIsCreated(this%accumulation_field) 

   end function initialized

   subroutine clear(this, rc)
      class(AccumulatorAction), intent(inout) :: this
      integer, optional, intent(out) :: rc
      
      integer :: status

      if(this%typekind == ESMF_TYPEKIND_R4) then
         call FieldSet(this%accumulation_field, this%CLEAR_VALUE_R4, _RC)
      else
         _FAIL('Unsupported typekind')
      end if
      _RETURN(_SUCCESS)

   end subroutine clear

   subroutine initialize(this, importState, exportState, clock, rc)
      class(AccumulatorAction), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: import_field, export_field
      type(ESMF_TypeKind_Flag) :: typekind
      logical :: conformable
      logical :: same_typekind

      conformable = .FALSE.
      same_typekind = .FALSE.
      call get_field(importState, import_field, _RC)
      call ESMF_FieldGet(import_field, typekind=typekind, _RC)
      _ASSERT(typekind==ESMF_TYPEKIND_R4, 'Only ESMF_TYPEKIND_R4 is supported.')
      call get_field(exportState, export_field, _RC)
      conformable = FieldsAreConformable(import_field, export_field, _RC)
      _ASSERT(conformable, 'Import and export fields are not conformable.')
      same_typekind = FieldsAreSameTypeKind(import_field, export_field, _RC)
      _ASSERT(same_typekind, 'Import and export fields are different typekinds.')
      this%typekind = typekind
      call this%create_fields(import_field, export_field, _RC)
      call this%clear(_RC)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(clock)

   end subroutine initialize

   subroutine create_fields(this, import_field, export_field, rc)
      class(AccumulatorAction), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: import_field
      type(ESMF_Field), intent(inout) :: export_field
      integer, optional, intent(out) :: rc

      integer :: status

      if(this%initialized()) then
         call ESMF_FieldDestroy(this%accumulation_field, _RC)
         call ESMF_FieldDestroy(this%result_field, _RC)
      end if
      this%accumulation_field = ESMF_FieldCreate(import_field, _RC)
      this%result_field = ESMF_FieldCreate(export_field, _RC)
      _RETURN(_SUCCESS)

   end subroutine create_fields

   subroutine update(this, importState, exportState, clock, rc)
      class(AccumulatorAction), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: export_field
      
      _ASSERT(this%initialized(), 'Accumulator has not been initialized.')
      if(.not. this%update_calculated) then
         call this%update_result(_RC)
      end if
      call get_field(exportState, export_field, _RC)
      call FieldCopy(this%result_field, export_field, _RC)

      call this%clear(_RC)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(clock)
      _UNUSED_DUMMY(importState)

   end subroutine update

   subroutine update_result(this, rc)
      class(AccumulatorAction), intent(inout) :: this
      integer, optional, intent(out) :: rc
      
      integer :: status

      call FieldCopy(this%accumulation_field, this%result_field, _RC)
      this%update_calculated = .true.
      _RETURN(_SUCCESS)
      
   end subroutine update_result

   subroutine invalidate(this, importState, exportState, clock, rc)
      class(AccumulatorAction), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: import_field
      
      _ASSERT(this%initialized(), 'Accumulator has not been initialized.')
      this%update_calculated = .FALSE.
      call get_field(importState, import_field, _RC)
      call this%accumulate(import_field, _RC)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(clock)
      _UNUSED_DUMMY(exportState)

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
      type(ESMF_TypeKind_Flag) :: tk_field

      call ESMF_FieldGet(update_field, typekind=tk_field, _RC)
      _ASSERT(this%typekind == tk_field, 'Update field must be the same typekind as the accumulation field.')
      if(this%typekind == ESMF_TYPEKIND_R4) then
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
      real(kind=ESMF_KIND_R4), parameter :: UNDEF = MAPL_UNDEFINED_REAL

      current => null()
      latest => null()
      call assign_fptr(this%accumulation_field, current, _RC)
      call assign_fptr(update_field, latest, _RC)
      where(current /= UNDEF .and. latest /= UNDEF)
        current = current + latest
      elsewhere(latest == UNDEF)
        current = UNDEF
      end where
      _RETURN(_SUCCESS)

   end subroutine accumulate_R4

end module mapl3g_AccumulatorAction
