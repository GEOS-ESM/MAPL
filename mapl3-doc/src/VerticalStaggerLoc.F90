module mapl3g_VerticalStaggerLoc
   implicit none
   private

   public :: VerticalStaggerLoc
   public :: VERTICAL_STAGGER_NONE
   public :: VERTICAL_STAGGER_EDGE
   public :: VERTICAL_STAGGER_CENTER
   public :: VERTICAL_STAGGER_INVALID

   public :: operator(==)
   public :: operator(/=)

   ! The type below has an "extraneous" component ID.  The purpose of
   ! this is to allow the default structure constructor to be usable
   ! in constant expressions (parameter statements), while still allowing
   ! private components which require a non-default constructor for external
   ! modules. Subtle.
   type :: VerticalStaggerLoc
      private
      integer :: id = -1
      character(24) :: name = "VERTICAL_STAGGER_INVALID"
   contains
      procedure :: to_string
   end type VerticalStaggerLoc

   interface VerticalStaggerLoc
      procedure :: new_VerticalStaggerLoc
   end interface VerticalStaggerLoc

   interface operator(==)
      procedure are_equal
   end interface operator(==)

   interface operator(/=)
      procedure are_not_equal
   end interface operator(/=)

   type(VerticalStaggerLoc), parameter :: VERTICAL_STAGGER_NONE = VerticalStaggerLoc(0, "VERTICAL_STAGGER_NONE")
   type(VerticalStaggerLoc), parameter :: VERTICAL_STAGGER_EDGE = VerticalStaggerLoc(1, "VERTICAL_STAGGER_EDGE")
   type(VerticalStaggerLoc), parameter :: VERTICAL_STAGGER_CENTER = VerticalStaggerLoc(2, "VERTICAL_STAGGER_CENTER")
   type(VerticalStaggerLoc), parameter :: VERTICAL_STAGGER_INVALID = VerticalStaggerLoc(3, "VERTICAL_STAGGER_INVALID")

contains

   ! Restrict values to just the 4 defined options.
   function new_VerticalStaggerLoc(name) result(staggerloc)
      type(VerticalStaggerLoc) :: staggerloc
      character(*), intent(in) :: name

      select case (name)
      case (VERTICAL_STAGGER_NONE%name)
         staggerloc = VERTICAL_STAGGER_NONE
      case (VERTICAL_STAGGER_EDGE%name)
         staggerloc = VERTICAL_STAGGER_EDGE
      case (VERTICAL_STAGGER_CENTER%name)
         staggerloc = VERTICAL_STAGGER_CENTER
      case default
         staggerloc = VERTICAL_STAGGER_INVALID
      end select
   end function new_VerticalStaggerLoc

   function to_string(this) result(s)
      character(:), allocatable :: s
      class(VerticalStaggerLoc), intent(in) :: this

      s = trim(this%name)

   end function to_string

   elemental logical function are_equal(this, that)
      type(VerticalStaggerLoc), intent(in) :: this
      type(VerticalStaggerLoc), intent(in) :: that
      are_equal = this%name == that%name
   end function are_equal

   elemental logical function are_not_equal(this, that)
      type(VerticalStaggerLoc), intent(in) :: this
      type(VerticalStaggerLoc), intent(in) :: that
      are_not_equal = .not. (this == that)
   end function are_not_equal

end module mapl3g_VerticalStaggerLoc
