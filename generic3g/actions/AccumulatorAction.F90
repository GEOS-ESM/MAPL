module mapl3g_AccumulatorAction
   use mapl3g_AccumulatorConstants
   use mapl3g_ExtensionAction
   use ESMF
   implicit none
   private
   public :: AccumulatorAction

   type :: AccumulatorFunctions
      procedure(FunctionR4R4), pointer :: accumulateR4point => null()
      procedure(FunctionR8R8), pointer :: accumulateR8point => null()
      procedure(FunctionR4), pointer :: clearR4point => null()
      procedure(FunctionR8), pointer :: clearR8point => null()
      logical :: keep_left
      logical :: check_divide
   contains
      procedure :: accumulate
      procedure :: increment
      procedure :: couple
      procedure :: clear
   end type AccumulatorFunctions

   type, extends(ExtensionAction) :: AccumulatorAction
      integer(kind=c_int) :: accumulator_type
      type(AccumulatorFunctions), pointer :: functions => null()
      type(ESMF_Field), allocatable :: accumulation_field
      type(ESMF_Field), allocatable :: counter_field
   contains
      procedure :: initialize => initialize_action
      procedure :: update => update_action
      procedure :: invalidate => invalidate_action
      procedure, private :: has_coupler
      procedure, private :: has_counter
      procedure, private :: is_active
   end type AccumulatorAction

   abstract interface
      function FunctionR4R4(left, right) result(val)
         real(kind=ESMF_KIND_R4) :: val
         real(kind=ESMF_KIND_R4), intent(in) :: left, right
      end function FunctionR4R4
      function FunctionR4R8(left, right) result(val)
         real(kind=ESMF_KIND_R4) :: val
         real(kind=ESMF_KIND_R4), intent(in) :: left
         real(kind=ESMF_KIND_R8), intent(in) :: right
      end function FunctionR4R8
      function FunctionR8R8(left, right) result(val)
         real(kind=ESMF_KIND_R8) :: val
         real(kind=ESMF_KIND_R8), intent(in) :: left, right
      end function FunctionR8R8
      function FunctionR4(point) result(val)
         real(kind=ESMF_KIND_R4) :: val
         real(kind=ESMF_KIND_R4), intent(in) :: point
      end function FunctionR4
      function FunctionR8(point) result(val)
         real(kind=ESMF_KIND_R8) :: val
         real(kind=ESMF_KIND_R8), intent(in) :: point
      end function FunctionR8
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

   enum, bind(c)
      enumerator :: SIMPLE_ACCUMULATOR
      enumerator :: MEAN_ACCUMULATOR
      enumerator :: MAX_ACCUMULATOR
      enumerator :: MIN_ACCUMULATOR
   end enum

contains

   function construct_accumulator_action(accumulator_type)
      type(AccumulatorAction) :: acc
      integer(kind=SIMPLE_ACCUMULATOR), intent(in) :: accumulator_type

      acc%accumulator_type = accumulator_type

   end function construct_accumulator_action

   subroutine set_accumulator_action(acc) result(acc)
      class(AccumulatorAction), intent(inout) :: acc
      procedure(UpdateField), pointer :: accumulate => null()
      procedure(UpdateField), pointer :: increment => null()
      procedure(ModifyField), pointer :: clear => null()
      procedure(UpdateField), pointer :: couple => null()

      clear => clear_to_zero
      select case(acc%accumulator_type)
         case MEAN_ACCUMULATOR
            accumulate => accumulate_add
            couple => couple_mean
            increment => increment_counter
         case MIN_ACCUMULATOR
            accumulate => accumulate_min
            clear => clear_to_undef
         case MAX_ACCUMULATOR
            accumulate => accumulate_max
            clear => clear_to_undef
         case default
            _FAIL("Unsupported AccumulatorAction")
      end select
      acc = AccumulatorAction(accumulate, increment, couple, clear)

   end subroutine set_accumulator_action

   subroutine initialize_action(this, importState, exportState, clock, rc)
      class(AccumulatorAction), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc
      type(ESMF_Field) :: import_field, export_field, field

      call this%set_accumulator_action(_RC)
      _ASSERT(assigned(this%couple) .eqv. assigned(this%increment), 'couple requires increment')
      call get_field(importState, import_field, _RC)
      call get_field(exportState, export_field, _RC)
      call FieldCopy(import_field, field, _RC)
      call acc%clear(field, _RC)
      acc%accumulation_field = field
      if(acc%has_coupler()) then
         call FieldCopy(import_field, field, _RC)
         call FieldReallocate(field, typekind=COUNTER_TYPEKIND, _RC)
         call this%clear(field, _RC)
         acc%counter_field = field
      end if
       
      _ASSERT(this%has_coupler() .eqv.this%has_counter())
      
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
      if(this%has_coupler()) then
         call this%couple(this%accumulation_field, this%counter_field, _RC)
      end if
      export_field = this%accumulation_field
      call this%clear(this%accumulation_field, _RC)
      if(this%has_counter()) then
         call this%clear(this%accumulation_field, _RC)
      end if
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
      call get_field(exportState, export_field, _RC)
      call this%accumulate(this%accumulation_field, field_update, _RC)
      if(this%has_counter()) then
         call this%increment(this%counter_field, this%accumulation_field, _RC)
      end if
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

   subroutine accumulate_add(accumulated, update, rc)
      type(ESMF_Field), intent(inout) :: accumulated
      type(ESMF_Field), intent(inout) :: update
      integer, optional, intent(out) :: rc

      call fieldAdd(accumulated, update, _RC)

   end subroutine accumulate_add

   subroutine accumulate_max(acc, update, rc)
      type(ESMF_Field), intent(inout) :: accumulated
      type(ESMF_Field), intent(inout) :: update
      integer, optional, intent(out) :: rc

      call fieldMax(accumulated, update, _RC)

   end subroutine accumulate_max

   subroutine accumulate_min(acc, update, rc)
      type(ESMF_Field), intent(inout) :: acc
      type(ESMF_Field), intent(inout) :: update
      integer, optional, intent(out) :: rc

      call fieldmin(accumulated, update, _RC)

   end subroutine accumulate_min

   subroutine couple_mean(acc, count, rc)
      type(ESMF_Field), intent(inout) :: acc
      type(ESMF_Field), intent(inout) :: count
      
      call fieldDivide(acc, count, _RC)

   end couple_mean

   subroutine clear_to_undef(field, rc)
      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(out) :: rc
      integer :: status

      call ESMF_Field_Get(field, typekind=tk, _RC)
      if(tk == ESMF_TypeKind_R8) then
         call FieldSet(field, MAPL_UNDEFINED_REAL64, _RC)
      else
         call FieldSet(field, MAPL_UNDEFINED_REAL, _RC)
      end if
      _RETURN(_SUCCESS)

   end subroutine clear_to_undef

   subroutine clear_to_zero(field, rc)
      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(out) :: rc
      integer :: status

      call Field_Set(field, 0.0_ESMF_KIND_R4, _RC)
      _RETURN(_SUCCESS)

   end subroutine clear_to_zero

   logical function has_coupler(this) result(lval)
      class(AccumulatorAction), intent(in) :: this

      lval = associated(this%couple)

   end function has_coupler

   logical function has_counter(this) result(lval)
      class(AccumulatorAction), intent(in) :: this

      lval = allocated(this%counter_field)

   end function has_counter

   logical function is_active(this) result(lval)
      class(AccumulatorAction), intent(in) :: this

      lval = allocated(this%accumulation_field)

   end function is_active

end module mapl3g_AccumulatorAction
