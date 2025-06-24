module FieldBLASIntrinicFunctions

   implicit none

   public :: IntrinsicReal64Function
   public :: IntrinsicReal64BiFunction
   public :: Sin

   private 

   abstract interface
      function IntrinsicReal64Function(x) result(fx)
         real(real64), intent(in) :: x
         real(real64) :: fx
      end function IntrinsicReal64Function

      function IntrinsicReal64BiFunction(x, y) result(fx)
         real(real64), intent(in) :: x, y
         real(real64) :: fx
      end function IntrinsicReal64BiFunction
   end abstract interface

contains

   subroutine intrinsic_real64_func(func, f, func_f, rc)
      procedure(IntrinsicReal64Function), pointer, intent(in) :: func
      type(ESMF_Field), intent(inout) :: f
      type(ESMF_Field), intent(out) :: func_f
      integer, optional, intent(out) :: rc
      
      ! Apply func to f.
      ! Set rc based on errors from func. 
      ! Probably a lot of generic framework to apply func to field and set rc.
   end subroutine intrinsic_real64_func

   subroutine intrinsic_real64_bifunc(bifunc, f, bifunc_f, rc)
      procedure(IntrinsicReal64BiFunction), pointer, intent(in) :: bifunc
      type(ESMF_Field), intent(inout) :: f1, f2
      type(ESMF_Field), intent(out)  :: bifunc_f
      integer, optional, intent(out) :: rc
      integer :: status
      
      ! Apply bifunc to f.
      ! Set rc based on errors from bifunc. 
      ! Probably a lot of generic framework to apply bifunc to field and set rc.
   end subroutine intrinsic_real64_bifunc 

   function Sin(f, rc) result(sinf)
      type(ESMF_Field), intent(inout) :: f
      integer, optional, intent(in) :: rc
      type(ESMF_Field), intent(out) :: sinf
      procedure(IntrinsicReal64Function), pointer :: func 
      integer :: status

      func => dsin ! Is this right?
      call intrinsic_real64_func(func, f, func_f=sinf, __RC)
      __RETURN(__SUCCESS)
   end function sin_field

end module FieldBLASIntrinicFunctions
