module mapl3g_TimeAccumulatorCouplerComponent
   use mapl3g_GenericCouplerComponent
   use mapl3g_Accumulator
   use mapl3g_AccumulatorProcedures
   implicit none
   private

   type(AccumulatorProcedures) :: MeanProcedures
   type(AccumulatorProcedures) :: MinProcedures
   type(AccumulatorProcedures) :: MaxProcedures
   
   integer, parameter :: SIMPLE_ACCUMULATOR = 0
   integer, parameter :: MEAN_ACCUMULATOR = 1
   integer, parameter :: MIN_ACCUMULATOR = 2
   integer, parameter :: MAX_ACCUMULATOR = 3

contains

   subroutine accumulate(acc, field_update, rc)
      class(Accumulator), intent(inout) :: acc
      type(ESMF_Field), intent(in) :: field_update
      integer, optional, intent(out) :: rc
      integer :: status
   end subroutine accumulate

   subroutine couple(acc, rc)
      class(Accumulator), intent(inout) :: acc
      integer, optional, intent(out) :: rc
      integer :: status
   end subroutine couple

   subroutine clear(acc, rc)
      class(Accumulator), intent(inout) :: acc
      integer, optional, intent(out) :: rc
      integer :: status
   end subroutine clear

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

   function construct_Accumulator_field(source, destination, use_mean, minimize) result(f)
      type(AbstractAccumulator) :: f
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
