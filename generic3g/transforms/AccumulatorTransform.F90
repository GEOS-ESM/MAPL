#include "MAPL.h"
module mapl3g_AccumulatorTransform
   use mapl3g_TransformId
   use mapl3g_ExtensionTransform
   use MAPL_InternalConstantsMod, only: MAPL_UNDEFINED_REAL, MAPL_UNDEFINED_REAL64
   use MAPL_FieldUtilities, only: FieldSet 
   use MAPL_FieldPointerUtilities
   use MAPL_ExceptionHandling
   use ESMF
   implicit none
   private
   public :: AccumulatorTransform
   public :: construct_AccumulatorTransform

   type, extends(ExtensionTransform) :: AccumulatorTransform
      type(ESMF_TypeKind_Flag) :: typekind = ESMF_TYPEKIND_R4
      type(ESMF_Field), allocatable :: accumulation_field
      type(ESMF_Field), allocatable :: result_field
      real(kind=ESMF_KIND_R4) :: CLEAR_VALUE_R4 = 0.0_ESMF_KIND_R4
      real(kind=ESMF_KIND_R8) :: CLEAR_VALUE_R8 = 0.0_ESMF_KIND_R8
      logical :: update_calculated = .FALSE.
      logical :: initialized = .FALSE.
   contains
      ! Implementations of deferred procedures
      procedure :: initialize
      procedure :: update
      procedure :: get_transformId
      ! Override procedures
      procedure :: invalidate
      procedure :: runs_invalidate
      ! Helpers
      procedure :: accumulate
      procedure :: accumulate_R4
      procedure :: accumulate_R8
      procedure :: clear
      procedure :: create_fields
      procedure :: update_result
   end type AccumulatorTransform

contains

   function construct_AccumulatorTransform(typekind) result(acc)
      type(AccumulatorTransform) :: acc
      type(ESMF_TypeKind_Flag), intent(in) :: typekind

      acc%typekind = typekind

   end function construct_AccumulatorTransform

   subroutine clear(this, rc)
      class(AccumulatorTransform), intent(inout) :: this
      integer, optional, intent(out) :: rc
      
      integer :: status

      if(this%typekind == ESMF_TYPEKIND_R4) then
         call FieldSet(this%accumulation_field, this%CLEAR_VALUE_R4, _RC)
      else
         call FieldSet(this%accumulation_field, this%CLEAR_VALUE_R8, _RC)
      end if
      _RETURN(_SUCCESS)

   end subroutine clear

   subroutine initialize(this, importState, exportState, clock, rc)
      class(AccumulatorTransform), intent(inout) :: this
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

      ! Get fields from state and confirm typekind match and conformable.
      call get_field(importState, import_field, _RC)
      call ESMF_FieldGet(import_field, typekind=typekind, _RC)
      _ASSERT(typekind == ESMF_TYPEKIND_R4 .or. typekind == ESMF_TYPEKIND_R8, 'Invalid typekind')
      this%typekind = typekind

      call get_field(exportState, export_field, _RC)
      same_typekind = FieldsAreSameTypeKind(import_field, export_field, _RC)
      _ASSERT(same_typekind, 'Import and export fields are different typekinds.')

      conformable = FieldsAreConformable(import_field, export_field, _RC)
      _ASSERT(conformable, 'Import and export fields are not conformable.')

      ! Create and initialize field values. 
      call this%create_fields(import_field, export_field, _RC)
      call this%clear(_RC)
      this%initialized = .TRUE.
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(clock)

   end subroutine initialize

   subroutine create_fields(this, import_field, export_field, rc)
      class(AccumulatorTransform), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: import_field
      type(ESMF_Field), intent(inout) :: export_field
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN_IF(this%initialized)
      this%accumulation_field = ESMF_FieldCreate(import_field, _RC)
      this%result_field = ESMF_FieldCreate(export_field, _RC)
      _RETURN(_SUCCESS)

   end subroutine create_fields

   subroutine update(this, importState, exportState, clock, rc)
      class(AccumulatorTransform), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: export_field
      
      _ASSERT(this%initialized, 'Accumulator has not been initialized.')
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
      class(AccumulatorTransform), intent(inout) :: this
      integer, optional, intent(out) :: rc
      
      integer :: status

      call FieldCopy(this%accumulation_field, this%result_field, _RC)
      this%update_calculated = .true.
      _RETURN(_SUCCESS)
      
   end subroutine update_result

   subroutine invalidate(this, importState, exportState, clock, rc)
      class(AccumulatorTransform), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: import_field
      
      _ASSERT(this%initialized, 'Accumulator has not been initialized.')
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
      class(AccumulatorTransform), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: update_field
      integer, optional, intent(out) :: rc
      
      integer :: status
      type(ESMF_TypeKind_Flag) :: tk_field

      call ESMF_FieldGet(update_field, typekind=tk_field, _RC)
      _ASSERT(this%typekind == tk_field, 'Update field must be the same typekind as the accumulation field.')
      if(this%typekind == ESMF_TYPEKIND_R4) then
         call this%accumulate_R4(update_field, _RC)
      else
         call this%accumulate_R8(update_field, _RC)
      end if

      _RETURN(_SUCCESS)

   end subroutine accumulate

#include "macros_undef.h"
#include "macros.h"
   subroutine accumulate_R4(this, update_field, rc)
      class(AccumulatorTransform), intent(inout) :: this
#include "accumulate_template.h"
   end subroutine accumulate_R4

#include "macros_undef.h"
#define DP_
#include "macros.h"
   subroutine accumulate_R8(this, update_field, rc)
      class(AccumulatorTransform), intent(inout) :: this
#include "accumulate_template.h"
   end subroutine accumulate_R8
#undef DP_

   logical function runs_invalidate(this)
      class(AccumulatorTransform), intent(in) :: this
      runs_invalidate = .TRUE.
   end function runs_invalidate
      
   function get_transformId(this) result(id)
      type(TransformId) :: id
      class(AccumulatorTransform), intent(in) :: this

      id = FREQUENCY_TRANSFORM_ID
   end function get_transformId

end module mapl3g_AccumulatorTransform
