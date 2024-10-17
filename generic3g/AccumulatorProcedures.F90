
   subroutine add_acc_procedures(existing, addition, rc)
      class(AccumulatorProcedures), allocatable, intent(inout) :: existing(:)
      class(AccumulatorProcedures), pointer, intent(in) :: addition
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i      

      if(.not. allocated(existing))
         existing = [addition]
         _RETURN(_SUCCESS)
      end if
      do i=1, size(existing)
         _ASSERT(existing(i)%name == addition%name, "AccumulatorProcedures name is already in use.") 
      end do

      existing = [type(AccumulatorProcedures) :: existing, addition]

   end subroutine add_acc_procedures
   type :: AccumulatorProcedures
      character(len=:), allocatable :: name
      procedure(UpdateAccumulator), pointer :: accumulate => null()
      procedure(ModifyAccumulator), pointer :: couple => null()
      procedure(ModifyAccumulator), pointer :: clear => null()
   end type AccumulatorProcedures

