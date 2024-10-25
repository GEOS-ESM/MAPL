#include "MAPL_Generic.h"
module mapl3g_AccumulatorAction
   use mapl3g_AccumulatorConstants
   use mapl3g_ExtensionAction
   use MAPL_ExceptionHandling
   use ESMF
   implicit none
   private
   public :: AccumulatorAction

   type, extends(ExtensionAction), abstract :: AccumulatorAction
      type(ESMF_Field), allocatable :: accumulation_field
      type(ESMF_Field), allocatable :: counter_field
      real(kind=ESMF_KIND_R4) :: undefR4
      logical, private :: update_on_undef
      logical, private :: couple_elements
   contains
      ! Implementations of deferred procedures
      procedure :: initialize => initialize_action
      procedure :: update => update_action
      procedure :: invalidate => invalidate_action
      ! Field procedures
      procedure, private :: clear ! need this
      procedure, private :: couple ! need this, specific to mean (and simple accumulate)
      procedure, private :: accumulate ! need this
      !  procedures
      generic, private :: accumulate_pointer => accumulate_pointer_R4
      generic, private :: couple_pointer => couple_pointer_R4
      generic, private :: clear_pointer => clear_pointer_R4
      procedure(BinaryFunctionR4), deferred :: accumulate_pointer_R4
      procedure, private :: couple_pointer_R4
      procedure(UnaryFunctionR4), deferred :: clear_pointer_R4
      ! Helpers
      generic, private :: set_undef => set_undef_R4
      procedure, private :: set_undef_R4
      procedure, private :: initialized => action_initialized
      generic, private :: clear => clear_R4
      procedure, private :: clear_R4
   end type AccumulatorAction

   abstract interface
       function BinaryFunctionR4(this, left, right)
         class(AccumulatorAction), intent(in) :: this
         real(kind=ESMF_KIND_R4) :: BinaryFunctionR4
         real(kind=ESMF_KIND_R4), intent(in) :: left, right
       end function BinaryFunctionR4
       function UnaryFunctionR4(this, val)
         class(AccumulatorAction), intent(in) :: this
         real(kind=ESMF_KIND_R4) :: UnaryFunctionR4
         real(kind=ESMF_KIND_R4), intent(in) :: val
      end function UnaryFunctionR4
   end interface

   type, extends(AccumulatorAction) :: MeanAccumulator
   contains
      procedure, private, pass :: accumulate_pointer_R4 => add_accumulate_R4
      procedure, private, pass :: clear_pointer_R4 => set_zero_R4
   end type MeanAccumulator

   interface MeanAccumulator
      module procedure :: construct_MeanAccumulator
   end interface MeanAccumulator

   type, extends(AccumulatorAction) :: MaxAccumulator
   contains
      procedure, private, pass :: accumulate_pointer_R4 => max_accumulate_R4
      procedure, private, pass :: clear_pointer_R4 => clear_undef_R4
   end type MaxAccumulator

   interface MaxAccumulator
      module procedure :: construct_MaxAccumulator
   end interface MaxAccumulator

contains

   logical function action_initialized(this) result(lval)
      class(AccumulatorAction), intent(in) :: this

      lval = allocated(this%accumulation_field)

   end function action_initialized
   
   subroutine set_undef_R4(this, undef_val)
      class(AccumulatorAction), intent(in) :: this
      real(kind=ESMF_KIND_R4), intent(inout) :: val

      val = this%undefR4

   end subroutine set_undef_R4

   subroutine initialize_action(this, importState, exportState, clock, rc)
      class(AccumulatorAction), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc
      type(ESMF_Field) :: import_field, export_field, field

      call get_field(importState, import_field, _RC)
      call get_field(exportState, export_field, _RC)
      call FieldCopy(import_field, field, _RC)
      call this%clear(field, _RC)
      acc%accumulation_field = field
      if(acc%has_coupler()) then
         call FieldCopy(import_field, field, _RC)
         call FieldReallocate(field, typekind=COUNTER_TYPEKIND, _RC)
         call this%clear(field, _RC)
         acc%counter_field = field
      end if
      acc%destination_field = export_field
      _RETURN(_SUCCESS)

   end subroutine initialize_action

   subroutine update_action(this, importState, exportState, clock, rc)
      class(AccumulatorAction), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc
      type(ESMF_Field) :: import_field
      
      call get_field(importState, import_field, _RC)
      call get_field(exportState, export_field, _RC)
      if(this%couple_elements) then
         call this%couple(this%accumulation_field, this%counter_field, _RC)
      end if
      export_field = this%accumulation_field
      call this%clear(_RC)
      _RETURN(_SUCCESS)

   end subroutine update_action

   subroutine invalidate_action(this, importState, exportState, clock, rc)
      class(AccumulatorAction), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc
      type(ESMF_Field) :: import_field
      type(ESMF_Field) :: export_field
      
      call get_field(importState, import_field, _RC)
      call this%accumulate(import_field, _RC)
      _RETURN(_SUCCESS)

   end subroutine invalidate_action

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

    function couple_element_R4(this, current_value, count) result(coupled)
      class(AccumulatorAction), intent(inout) :: this
      real(kind=ESMF_KIND_R4) :: coupled
      real(kind=ESMF_KIND_R4), intent(in) :: current_value
      real(kind=ESMF_KIND_R8), intent(in) :: count

      coupled = current_value / count

   end function couple_element_R4

   function construct_MeanAccumulator() result(acc)
      type(MeanAccumulator) :: acc

      acc%update_on_undef = .TRUE.
      acc%couple_elements = .TRUE.

   end function construct_MeanAccumulator

    subroutine add_accumulate_R4(this, val, value_update)
      class(AccumulatorAction), intent(inout) :: this
      real(kind=ESMF_KIND_R4), pointer, intent(inout) :: val(:)
      real(kind=ESMF_KIND_R4), pointer, intent(in) :: value_update(:)
      real(kind=ESMF_KIND_R4) :: undef
      
      call this%set_undef(undef)
      where(val == undef .or. value_update == undef)
         val = undef
      elsewhere
         val = val + value_update
      end where

   end subroutine add_accumulate_R4

   subroutine clear_pointer_R4(this, val, clear_value)
      class(AccumulatorAction), intent(inout) :: this
      real(kind=ESMF_KIND_R4), pointer, intent(inout) :: val(:)

   function construct_MaxAccumulator() result(acc)
      type(MaxAccumulator) :: acc

      acc%update_on_undef = .FALSE.
      acc%couple_elements = .FALSE.

   end function construct_MaxAccumulator

    function max_accumulate_R4(current_value, value_update) :: result(updated_value)
      real(kind=ESMF_KIND_R4) :: updated_value
      real(kind=ESMF_KIND_R4), intent(in) :: current_value, value_update
      
      updated_value = max(current_value, value_update)

   end function max_accumulate_R4

! These are procedures used to construct the AccumulatorAction.

   subroutine increment_counter(counter, field, rc)
      type(ESMF_Field), intent(inout) :: counter
      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_TypeKind_Flag) :: tk_counter
      type(ESMF_TypeKind_Flag) :: tk_field
      logical :: conformable
      real(kind=COUNTER_KIND), pointer :: ptrCounter(:) => null()

      conformable = FieldsAreConformable(counter, field,_RC)
      _ASSERT(conformable,"counter is not conformable with field.")

      call ESMF_FieldGet(field,typekind=tk_field,_RC)
      call ESMF_FieldGet(counter,typekind=tk_counter,_RC)
      _ASSERT(tk_counter == COUNTER_TYPEKIND, "counter must be " // COUNTER_TYPEKIND_NAME)

      call assign_fptr(counter, ptrCounter, _RC)
      if(tk_field == ESMF_TypeKindFlag_R8) then
         call incrementR8(counter, field, _RC)
      else
         call incrementR4(counter, field, _RC)
      end if
      _RETURN(_SUCCESS)

   contains

      subroutine incrementR8(counter, field, rc)
         type(ESMF_Field), intent(inout) :: counter
         type(ESMF_Field), intent(inout) :: field
         integer, optional, intent(out) :: rc
         integer :: status
         real(kind=ESMF_KIND_R8), pointer :: ptrFieldR8(:) => null()
         real(kind=ESMF_KIND_R8) :: undefR8(1)

         call assign_fptr(field, ptrFieldR8, _RC)
         call GetFieldsUndef([field], undefR8, _RC)
         where(ptrFieldR8 /= undefR8(1)) 
            ptrCounter = ptrCount + 1
         end where
         _RETURN(_SUCCESS)

      end subroutine incrementR8

      subroutine incrementR4(counter, field, rc)
         type(ESMF_Field), intent(inout) :: counter
         type(ESMF_Field), intent(inout) :: field
         integer, optional, intent(out) :: rc
         integer :: status
         real(kind=ESMF_KIND_R4), pointer :: ptrFieldR4(:) => null()
         real(kind=ESMF_KIND_R4) :: undefR4(1)

         call assign_fptr(field, ptrFieldR4, _RC)
         call GetFieldsUndef([field], undefR4, _RC)
         where(ptrFieldR4 /= undefR4(1)) 
            ptrCounter = ptrCount + 1
         end where
         _RETURN(_SUCCESS)

      end subroutine incrementR4

   end subroutine increment_counter

end module mapl3g_AccumulatorAction
