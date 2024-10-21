module mapl3g_AccumulatorAction
   use mapl3g_ExtensionAction
   use ESMF
   implicit none
   private
   public :: AccumulatorAction

   type, extends(ExtensionAction) :: AccumulatorAction
      procedure(UpdateField), pointer :: accumulate => null()
      procedure(UpdateField), pointer :: couple => null()
      procedure(ModifyField), pointer :: clear => null()
      logical :: is_active = .FALSE.
      type(ESMF_Field), allocatable :: accumulation_field
      type(ESMF_Field), allocatable :: counter_field
      logical, private :: run_couple_ = .FALSE.
   contains
      procedure :: initialize => initialize_action
      procedure :: update => update_action
      procedure :: invalidate => invalidate_action
   end type AccumulatorAction

   abstract interface
      subroutine UpdateField(field, update, rc)
         type(ESMF_Field), intent(inout) :: field
         type(ESMF_Field), intent(inout) :: update
         integer, optional, intent(out) :: rc
      end subroutine UpdateAccumulatorAction
      subroutine ModifyField(field, rc)
         type(ESMF_Field), intent(inout) :: field
         integer, optional, intent(out) :: rc
      end subroutine ModifyField
   end interface interface

   interface AccumulatorAction
      module procedure :: construct_accumulator_action
   end interface AccumulatorAction

contains

   function construct_accumulator_action(accumulate, clear, increment, couple) result(acc)
      type(AccumulatorAction) :: acc
      procedure(UpdateField), pointer, intent(in) :: accumulate
      procedure(ModifyField), pointer, intent(in) :: clear
      procedure(UpdateField), optional, pointer, intent(in) :: couple

      acc%accumulate => accumulate
      acc%clear => clear
      acc%run_couple_ = present(couple)
      if(acc%run_couple()) acc%couple => couple

   end function construct_accumulator_action

   subroutine initialize_action(this, importState, exportState, clock, rc)
      class(AccumulatorAction), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc
   end subroutine initialize_action

   subroutine update_action(this, importState, exportState, clock, rc)
      class(AccumulatorAction), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc
      type(ESMF_Field) :: import_field
      type(ESMF_Field) :: export_field
      
      call get_field(importState, import_field, _RC)
      call get_field(exportState, export_field, _RC)
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
      call get_field(exportState, export_field, _RC)
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
   subroutine accumulate(acc, field_update, rc)
      class(AccumulatorAction), intent(inout) :: acc
      type(ESMF_Field), intent(inout) :: field_update
      integer, optional, intent(out) :: rc
      integer :: status
   
      call acc%accumulate(acc%accumulation_field, field_update, _RC)
      _RETURN(_SUCCESS)

   end subroutine accumulate

   subroutine clear(acc, rc)
      class(AccumulatorAction), intent(inout) :: acc
      integer, optional, intent(out) :: rc
      integer :: status

      call acc%clear(acc%accumulation_field, _RC)
      if(acc%run_couple()) call reset_counter(acc%counter_field)
      _RETURN(_SUCCESS)

   end subroutine clear

   subroutine couple(acc, rc)
      class(AccumulatorAction), intent(inout) :: acc
      integer, optional, intent(out) :: rc
      integer :: status

      if(acc%run_couple()) then
         call acc%couple(acc%accumulation_field, acc%counter_field, _RC)
      end if
      _RETURN(_SUCCESS)

   end subroutine couple

   subroutine increment(counter, field, rc)
      type(ESMF_Field), intent(inout) :: counter
      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(out) :: rc
      integer :: status

      ! increment counter based on accumulation field
      _RETURN(_SUCCESS)

   end subroutine increment

   subroutine accumulate_add(accumulated, update, rc)
      type(ESMF_Field), intent(inout) :: accumulated
      type(ESMF_Field), intent(inout) :: update
      integer, optional, intent(out) :: rc
   end subroutine accumulate_add

   subroutine accumulate_apply_function(acc, update, rc)
      type(ESMF_Field), intent(inout) :: accumulated
      type(ESMF_Field), intent(inout) :: update
      integer, optional, intent(out) :: rc
   end subroutine accumulate_apply_function

end module mapl3g_AccumulatorAction
