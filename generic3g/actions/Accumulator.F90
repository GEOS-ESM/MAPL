module mapl3g_Accumulator
   implicit none
   private
   public :: Accumulator

   type :: Accumulator
      procedure(UpdateField), pointer :: accumulate => null()
      procedure(UpdateField), pointer :: couple => null()
      procedure(ModifyField), pointer :: clear => null()
      logical :: is_active = .FALSE.
      type(ESMF_Alarm), pointer :: time_to_clear => null()
      type(ESMF_Alarm), pointer :: time_to_couple => null()
      type(ESMF_Field), allocatable :: accumulation_field
      type(ESMF_Field), allocatable :: counter_field
   end type Accumulator

   abstract interface
      subroutine UpdateField(accumulated, update, rc)
         type(ESMF_Field), intent(inout) :: accumulated
         type(ESMF_Field), intent(inout) :: update
         integer, optional, intent(out) :: rc
      end subroutine UpdateAccumulator
      subroutine ModifyField(field, rc)
         type(ESMF_Field), intent(inout) :: field
         integer, optional, intent(out) :: rc
      end subroutine ModifyField
   end interface interface

   type :: GenericElementalFunction
      procedure(R4ElementalFunction), pointer :: ptrR4 => null()
      procedure(R8ElementalFunction), pointer :: ptrR8 => null()
   contains
      generic :: apply => applyR4, applyR8
      procedure :: applyR4
      procedure :: applyR8
   end type GenericElementalFunction

   abstract interface
      elemental function R4ElementalFunction(a, b) result(c)
         real(ESMF_KIND_R4) :: c
         real(ESMF_KIND_R4), intent(in) :: a, b
      end function R4ElementalFunction
      elemental function R8ElementalFunction(a, b) result(c)
         real(ESMF_KIND_R8) :: c
         real(ESMF_KIND_R8), intent(in) :: a, b
      end function R8ElementalFunction
   end interface
         
contains

   elemental function applyR4(this, a, b) result(c)
      class(GenericElementalFunction), intent(in) :: this
      real(ESMF_KIND_R4) :: c
      real(ESMF_KIND_R4), intent(in) :: a, b

      c = this%ptrR4(a, b)

   end function applyR4
      
   elemental function applyR8(this, a, b) result(c)
      class(GenericElementalFunction), intent(in) :: this
      real(ESMF_KIND_R8) :: c
      real(ESMF_KIND_R8), intent(in) :: a, b

      c = this%ptrR8(a, b)

   end function applyR8

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

end module mapl3g_Accumulator
