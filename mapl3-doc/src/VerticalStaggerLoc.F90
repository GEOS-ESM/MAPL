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

   public :: make_VerticalStaggerLoc

   type :: VerticalStaggerLoc
      private
      integer :: id
   contains
      ! TODO: Convert to DTIO once compilers support allocatable internal files
      procedure :: to_string
   end type VerticalStaggerLoc

   interface operator(==)
      procedure are_equal
   end interface operator(==)

   interface operator(/=)
      procedure are_not_equal
   end interface operator(/=)

   type(VerticalStaggerLoc), parameter :: VERTICAL_STAGGER_NONE = VerticalStaggerLoc(1)
   type(VerticalStaggerLoc), parameter :: VERTICAL_STAGGER_EDGE = VerticalStaggerLoc(2)
   type(VerticalStaggerLoc), parameter :: VERTICAL_STAGGER_CENTER = VerticalStaggerLoc(3)
   type(VerticalStaggerLoc), parameter :: VERTICAL_STAGGER_INVALID = VerticalStaggerLoc(4)

   character(*), parameter :: VERTICAL_STAGGER_NONE_NAME = "VERTICAL_STAGGER_NONE"
   character(*), parameter :: VERTICAL_STAGGER_EDGE_NAME = "VERTICAL_STAGGER_EDGE"
   character(*), parameter :: VERTICAL_STAGGER_CENTER_NAME = "VERTICAL_STAGGER_CENTER"

contains

   function make_VerticalStaggerLoc(string) result(vert_staggerLoc)
      type(VerticalStaggerLoc) :: vert_staggerLoc
      character(*), intent(in) :: string

      select case (string)
      case (VERTICAL_STAGGER_NONE_NAME)
         vert_staggerLoc = VERTICAL_STAGGER_NONE
      case (VERTICAL_STAGGER_EDGE_NAME)
         vert_staggerLoc = VERTICAL_STAGGER_EDGE
      case (VERTICAL_STAGGER_CENTER_NAME)
         vert_staggerLoc = VERTICAL_STAGGER_CENTER
      case default
         vert_staggerLoc = VERTICAL_STAGGER_INVALID
      end select
      
   end function make_VerticalStaggerLoc
   
  
   function to_string(this) result(s)
      character(:), allocatable :: s
      class(VerticalStaggerLoc), intent(in) :: this

      if (this == VERTICAL_STAGGER_NONE) then
         s = VERTICAL_STAGGER_NONE_NAME
         return
      end if

      if (this == VERTICAL_STAGGER_EDGE) then
         s = VERTICAL_STAGGER_EDGE_NAME
         return
      end if

      if (this == VERTICAL_STAGGER_CENTER) then
         s = VERTICAL_STAGGER_CENTER_NAME
         return
      end if

      s = "VERTICAL_STAGGER_INVALID"
   end function to_string

   elemental logical function are_equal(this, that)
      type(VerticalStaggerLoc), intent(in) :: this
      type(VerticalStaggerLoc), intent(in) :: that
      are_equal = this%id == that%id
   end function are_equal

   elemental logical function are_not_equal(this, that)
      type(VerticalStaggerLoc), intent(in) :: this
      type(VerticalStaggerLoc), intent(in) :: that
      are_not_equal = .not. (this == that)
   end function are_not_equal

end module mapl3g_VerticalStaggerLoc
