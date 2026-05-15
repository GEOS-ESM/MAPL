module mapl3g_RestartModes

   implicit none(type, external)
   private

   public :: RestartMode
   public :: operator(==)
   public :: operator(/=)

   public :: MAPL_RESTART_REQUIRED
   public :: MAPL_RESTART_SKIP

   type :: RestartMode
      private
      integer :: mode
   contains
      procedure :: set_mode
      procedure :: get_mode
   end type RestartMode

   type(RestartMode), parameter :: MAPL_RESTART_INVALID = RestartMode(mode=-1)
   type(RestartMode), parameter :: MAPL_RESTART_REQUIRED = RestartMode(mode=0)
   type(RestartMode), parameter :: MAPL_RESTART_SKIP = RestartMode(mode=1)

   interface operator(==)
      procedure equal
   end interface operator(==)

   interface operator(/=)
      procedure not_equal
   end interface operator(/=)

contains

   subroutine set_mode(this, mode)
      class(RestartMode), intent(inout) :: this
      integer, intent(in) :: mode

      this%mode = mode
   end subroutine set_mode

   function get_mode(this) result(mode)
      class(RestartMode), intent(in) :: this
      integer :: mode ! result

      mode = this%mode
   end function get_mode

   logical function equal(a, b)
      class(RestartMode), intent(in) :: a, b
      equal = (a%mode == b%mode)
   end function equal

   logical function not_equal(a, b)
      class(RestartMode), intent(in) :: a, b
      not_equal = .not. (a == b)
   end function not_equal

end module mapl3g_RestartModes
