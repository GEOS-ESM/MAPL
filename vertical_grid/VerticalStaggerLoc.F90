#include "MAPL.h"
module mapl3g_VerticalStaggerLoc
   implicit none
   private

   public :: VerticalStaggerLoc
   public :: VERTICAL_STAGGER_NONE
   public :: VERTICAL_STAGGER_EDGE
   public :: VERTICAL_STAGGER_CENTER
   public :: VERTICAL_STAGGER_MIRROR
   public :: VERTICAL_STAGGER_INVALID

   public :: operator(==)
   public :: operator(/=)

   enum, bind(c)
      enumerator :: NONE=0
      enumerator :: EDGE=1
      enumerator :: CENTER=2
      enumerator :: MIRROR=3
      enumerator :: INVALID=-1
   end enum

   ! The type below has an "extraneous" component ID.  The purpose of
   ! this is to allow the default structure constructor to be usable
   ! in constant expressions (parameter statements), while still allowing
   ! private components which require a non-default constructor for external
   ! modules. Subtle.
   type :: VerticalStaggerLoc
      private
      integer :: id = INVALID
      character(24) :: name = "VERTICAL_STAGGER_INVALID"
   contains
      procedure :: to_string
      procedure :: get_dimension_name
      procedure :: get_num_levels
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

   character(*), parameter :: DIM_NAME_NONE = ""
   character(*), parameter :: DIM_NAME_EDGE = "edge"
   character(*), parameter :: DIM_NAME_CENTER = "lev"
   character(*), parameter :: DIM_NAME_MIRROR = "mirror"
   character(*), parameter :: DIM_NAME_INVALID = "invalid"

   type(VerticalStaggerLoc), parameter :: VERTICAL_STAGGER_NONE = VerticalStaggerLoc(NONE, "VERTICAL_STAGGER_NONE")
   type(VerticalStaggerLoc), parameter :: VERTICAL_STAGGER_EDGE = VerticalStaggerLoc(EDGE, "VERTICAL_STAGGER_EDGE")
   type(VerticalStaggerLoc), parameter :: VERTICAL_STAGGER_CENTER = VerticalStaggerLoc(CENTER, "VERTICAL_STAGGER_CENTER")
   type(VerticalStaggerLoc), parameter :: VERTICAL_STAGGER_MIRROR = VerticalStaggerLoc(MIRROR, "VERTICAL_STAGGER_MIRROR")
   type(VerticalStaggerLoc), parameter :: VERTICAL_STAGGER_INVALID = VerticalStaggerLoc(INVALID, "VERTICAL_STAGGER_INVALID")

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
      case (VERTICAL_STAGGER_MIRROR%name)
         staggerloc = VERTICAL_STAGGER_MIRROR
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

      integer :: n_mirror

      are_equal = (this%name == that%name)
      if (are_equal) return

      ! Note: we may only want to allow imports (dst) to be mirror in
      ! the future.
      n_mirror = count([this%id,that%id] == MIRROR)
      are_equal = (n_mirror == 1)

   end function are_equal

   elemental logical function are_not_equal(this, that)
      type(VerticalStaggerLoc), intent(in) :: this
      type(VerticalStaggerLoc), intent(in) :: that
      are_not_equal = .not. (this == that)
   end function are_not_equal

   function get_dimension_name(this) result(dim_name)
      character(:), allocatable :: dim_name
      class(VerticalStaggerLoc), intent(in) :: this

      select case (this%id)
      case (NONE)
         dim_name = DIM_NAME_NONE
      case (EDGE)
         dim_name = DIM_NAME_EDGE
      case (CENTER)
         dim_name = DIM_NAME_CENTER
      case (MIRROR)
         dim_name = DIM_NAME_MIRROR
      case default
         dim_name = DIM_NAME_INVALID
      end select
   end function get_dimension_name

   integer function get_num_levels(this, num_vgrid_levels) result(num_levels)
      class(VerticalStaggerLoc), intent(in) :: this
      integer, intent(in) :: num_vgrid_levels

      select case (this%id)
      case (NONE)
         num_levels = 0
      case (EDGE)
         num_levels = num_vgrid_levels
      case (CENTER)
         num_levels = num_vgrid_levels - 1
      case (MIRROR)
         num_levels = num_vgrid_levels
      case default
         num_levels = -1
      end select
   end function get_num_levels

end module mapl3g_VerticalStaggerLoc
