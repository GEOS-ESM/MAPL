module mapl_RegridderSpec
   implicit none
   private

   public :: RegridderSpec

   type, abstract :: RegridderSpec
      private
   contains
      procedure(I_make_regridder), deferred :: make_regridder

      procedure(I_equal_to), deferred :: equal_to
      generic :: operator(==) => equal_to
   end type RegridderSpec

end module mapl_RegridderSpec
