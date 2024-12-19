module MAPL_GridTypeMod
   implicit none
   private

   public :: GridType

   type :: GridType
      private
      character(len=:), allocatable :: name
   contains
      procedure :: equals
      procedure :: not_equals
      procedure :: less_than
      procedure :: greater_than_or_equal
      generic :: operator(<) => less_than
      generic :: operator(>=) => greater_than_or_equal
      generic :: operator(==) => equals
      generic :: operator(/=) => not_equals
   end type GridType

   interface GridType
      module procedure newGridType_name
      module procedure newGridType_mapl
   end interface GridType


contains


   function newGridType_name(name) result (grid_type)
      type (GridType) :: grid_type
      character(len=*), intent(in) :: name

      grid_type%name = name

   end function newGridType_name

   function newGridType_mapl(grid) result (grid_type)
      use ESMF
      type (GridType) :: grid_type
      type (ESMF_Grid), intent(in) :: grid

      character(len=60) :: name
      logical :: isPresent

      call ESMF_AttributeGet(grid, name='GridType', isPresent=isPresent)
      if (isPresent) then
         call ESMF_AttributeGet(grid, name='GridType', value=name)

         grid_type%name = name
      end if

   end function newGridType_mapl


   logical function less_than(a, b)
      class (GridType), intent(in) :: a
      class (GridType), intent(in) :: b

      
      if (allocated(a%name)) then
         if (allocated(b%name)) then
            less_than = (a%name < b%name)
         else
            less_than = .true.
         end if
      else
         less_than = .false.
      end if

   end function less_than


   logical function greater_than_or_equal(a, b)
      class (GridType), intent(in) :: a
      class (GridType), intent(in) :: b

      greater_than_or_equal = .not. a%less_than(b)

   end function greater_than_or_equal


   logical function equals(a, b)
      class (GridType), intent(in) :: a
      class (GridType), intent(in) :: b

      
      if (allocated(a%name)) then
         if (allocated(b%name)) then
            equals = (a%name == b%name)
         else
            equals = .false.
         end if
      else
         equals = .false.
      end if

   end function equals


   logical function not_equals(a, b)
      class (GridType), intent(in) :: a
      class (GridType), intent(in) :: b

      not_equals = .not. (a == b)

   end function not_equals

end module MAPL_GridTypeMod
