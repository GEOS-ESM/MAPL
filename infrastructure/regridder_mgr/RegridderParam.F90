module mapl_RegridderParam_mod
   implicit none
   private

   public :: RegridderParam

   type, abstract :: RegridderParam
   contains
      procedure(I_equal_to), deferred :: equal_to
      procedure(I_to_string), deferred :: to_string
      generic :: operator(==) => equal_to
   end type RegridderParam

   abstract interface
      logical function I_equal_to(this, other)
         import RegridderParam
         class(RegridderParam), intent(in) :: this
         class(RegridderParam), intent(in) :: other
      end function I_equal_to

      ! Deterministic one-line text of the param, equal for equal params.
      ! rc is nonzero if the param cannot be rendered.
      function I_to_string(this, rc) result(str)
         import RegridderParam
         class(RegridderParam), intent(in) :: this
         integer, optional, intent(out) :: rc
         character(:), allocatable :: str
      end function I_to_string
   end interface

end module mapl_RegridderParam_mod
