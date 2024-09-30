#if defined(IS_UNDEF_)
#  undef(IS_UNDEF_)
#endif
#define IS_UNDEF_(T) ieee_class(T) == ieee_quiet_nan

#if defined(SET_UNDEF_)
#  undef(SET_UNDEF_)
#endif
#define SET_UNDEF_(T) ieee_value(T, ieee_quiet_nan)

#if defined(TERTIARY_)
#  undef(TERTIARY_)
#endif
#define TERTIARY_(I, B, E) E; if(B) I

#if defined(CLEAR_)
#  undef(CLEAR_)
#endif
#define CLEAR_(T) TERTIARY_(T, this%accumulate, set_undef(cleared))

module mapl3g_TimeAccumulatorCouplerComponent
   use mapl3g_GenericCouplerComponent
   use, intrinsic :: iso_c_binding, only: c_int
   use, intrinsic :: ieee_arithmetic
   implicit none
   private
!_   public ::

   ! Do current compilers support: intel, gcc, nag
   ! MAPL_UNDEF ?
   ! Where for ESMF_Field?
   ! Do we need the specs?
   type :: Accumulator(k)
      integer, kind :: k
      ! Set at construction
!      ! VarSpec or FieldSpec
!      type(VarSpec), pointer :: source => null()
!      type(VarSpec), pointer :: destination => null()
      logical :: is_active = .FALSE.
      procedure(BinaryR8Function), pointer :: couple_function_R8 => null()
      procedure(BinaryR8Function), pointer :: accumulate_function_R8 => null()
      procedure(ClearR8Sub), pointer :: clear_sub_R8 => null()
      procedure(BinaryR4Function), pointer :: couple_function_R4 => null()
      procedure(BinaryR4Function), pointer :: accumulate_function_R4 => null()
      procedure(ClearR4Sub), pointer :: clear_sub_R4 => null()
      ! defaults
      integer(ESMF_I4) :: scalar_count = -1
      integer(ESMF_I4) :: clear_interval = -1
      integer(ESMF_I4) :: couple_interval = -1
      ! Set at initialization
      type(ESMF_Alarm), pointer   :: time_to_clear => null()
      type(ESMF_Alarm), pointer   :: time_to_couple => null()
      type(ESMF_Field), pointer :: field_data => null()
      integer(ESMF_KIND_I4), pointer :: array_count(:) => null()
      integer, allocatable :: shape(:)
   end type Accumulator

   abstract interface
      elemental function BinaryR8Function(t1, t2) result(t3)
         real(kind=ESMF_KIND_R8) :: t3
         real(kind=ESMF_KIND_R8), intent(in) :: t1, t2
      end function BinaryR8Function
      elemental function BinaryR4Function(t1, t2) result(t3)
         real(kind=ESMF_KIND_R4) :: t3
         real(kind=ESMF_KIND_R4), intent(in) :: t1, t2
      end function BinaryR4Function
      elemental subroutine ClearR8Sub(t)
         real(kind=ESMF_KIND_R8), intent(inout) :: t
      end subroutine ClearR8Sub
      elemental subroutine ClearR4Sub(t)
         real(kind=ESMF_KIND_R4), intent(inout) :: t
      end subroutine ClearR4Sub

   end abstract interface

   interface accumulate
      module procedure :: accumulateR4
      module procedure :: accumulateR8
   end interface accumulate

   interface couple
      module procedure :: coupleR4
      module procedure :: coupleR8
   end interface couple

   interface clear
      module procedure :: clearR4
      module procedure :: clearR8
   end interface clear

contains

   elemental subroutine clearR4(t, z)
      real(kind=R4), intent(inout) :: t
      real(kind=R4), intent(in) :: z
   end subroutine clearR4

   elemental subroutine clearR8(t, z)
      real(kind=R8), intent(inout) :: t
      real(kind=R8), intent(in) :: z
   end subroutine clearR8

   function construct_accumulator_R8() result(acc)
      type(Accumulator(R8)) :: acc
      procedure(BinaryR8Function), pointer, intent(in) :: ptrCouple
      procedure(BinaryR8Function), pointer, intent(in) :: ptrAccumulate
      real(kind=ESMF_KIND_R8), intent(in) :: clear_value
      integer(kind=ESMF_KIND_I4), intent(in) :: counter_reset

      acc%couple_function_R8 = ptrCouple
      acc%accumulate_function_R8 = ptrAccumulate
      acc%clear_value = clear_value
      acc%counter_reset = counter_reset

   end function construct_accumulator_R8

   function construct_accumulator_R4() result(acc)
      type(Accumulator(R4)) :: acc
      procedure(BinaryR4Function), pointer, intent(in) :: ptrCouple
      procedure(BinaryR4Function), pointer, intent(in) :: ptrAccumulate
      real(kind=ESMF_KIND_R4), intent(in) :: clear_value
      integer(kind=ESMF_KIND_I4), intent(in) :: counter_reset

      acc%couple_function_R4 = ptrCouple
      acc%accumulate_function_R4 = ptrAccumulate
      acc%clear_value = clear_value
      acc%counter_reset = counter_reset

   end function construct_accumulator_R4

   subroutine initialize_Accumulator_internal(state, source_specs, destination_specs)
      type(AccumulatorInternal), pointer, intent(in) :: state
      type(VarSpec), intent(in) :: source_specs(:)
      type(VarSpec), intent(in) :: destination_specs(:)
      integer :: i, sz
      character(ESMF_MAXSTR) :: source_name, destination_name

      sz = size(source_specs)
      _ASSERT(sz == size(destination_specs), 'Spec lists must have same length.')

      do i = 1, sz
         call MAPL_VarSpecGet(source_specs(i), short_name=source_name, _RC)
         call MAPL_VarSpecGet(destination_specs(i), short_name=destination_name, _RC)
         _ASSERT(source_name == destination_name, 'short_name must match.')
      end do

      state%
   end subroutine initialize_Accumulator_internal

   subroutine run_Accumulator_internal(state)
      type(AccumulatorInternal), pointer, intent(in) :: state
   end subroutine run_Accumulator_internal

   subroutine finalize_Accumulator_internal(state)
      type(AccumulatorInternal), pointer, intent(in) :: state
   end subroutine finalize_Accumulator_internal

!   function assign_fptr_i4_1d(farray) result(fptr) !wdb fixme deleteme
!      integer(ESMF_KIND_I4), pointer :: fptr
!      integer(ESMF_KIND_I4), pointer, intent(in) :: farray(:)
!
!      call c_f_pointer(c_loc(farray), fptr, cptr, product(shape(farray)))
!
!   end function assign_fptr_i4_1d

   function construct_Accumulator_field(source, destination, use_mean, minimize) result(f)
      type(Accumulator) :: f
      type(VarSpec), pointer, intent(in) :: source
      type(VarSpec), pointer, intent(in) :: destination
      logical, optional, intent(in) :: use_mean
      logical, optional, intent(in) :: minimize

      f%source => source
      f%destination => destination
      if(present(use_mean)) f%mean = use_mean
      if(present(minimize)) f%minimize = minimize

   end function construct_Accumulator_field

   elemental function accumulateR4(current, update, minimize) result(updated)
      real(ESMF_KIND_R4) :: updated
      real(ESMF_KIND_R4), intent(inout) :: current
      real(ESMF_KIND_R4), intent(in) :: update
      logical, optional, intent(in) :: minimize

      updated = update
      if(IS_UNDEF(current)) return
      updated = current + updated
      if(.not. present(minimize)) return
      updated = max(current, update)
      if(this%min) updated = min(current, update)

   end function accumulateR4

   elemental function accumulateR8(current, update, minimize) result(updated)
      real(ESMF_KIND_R8) :: updated
      real(ESMF_KIND_R8), intent(inout) :: current
      real(ESMF_KIND_R8), intent(in) :: update
      logical, optional, intent(in) :: minimize

      updated = update
      if(IS_UNDEF(current)) return
      updated = current + updated
      if(.not. present(minimize)) return
      updated = max(current, update)
      if(this%min) updated = min(current, update)

   end function accumulateR8

end module mapl3g_TimeAccumulatorCouplerComponent


! QUESTIONS
! Use ESMF_Field for counter:
!   - requires additional code for field_utils,
!   - is the field overkill
! What to make the counter if not a field?
! Generalize FieldBinaryOperatorTemplate.H to:
! - include BiFuncs?
! - Could the template be templated to handle different kinds and ranks?
! - Could be used to create functions as needed, possibly pointers?
! Why does MAPL2 time averaging index over arrays instead of utilizing elemental?
