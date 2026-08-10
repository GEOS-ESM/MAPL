module mapl_GraphAssemblyStatus_mod
   implicit none(type, external)
   private

   public :: GraphAssemblyStatus

   integer, parameter :: UNINITIALIZED = 0
   integer, parameter :: DECLARING = 1
   integer, parameter :: ADVERTISED = 2
   integer, parameter :: MODIFIED = 3
   integer, parameter :: REALIZED = 4

   type :: GraphAssemblyStatus
      private
      integer :: phase_ = UNINITIALIZED
   contains
      procedure :: mark_initialized
      procedure :: mark_advertised
      procedure :: mark_modified
      procedure :: mark_realized
      procedure :: is_initialized
      procedure :: is_declaring
      procedure :: is_advertised
      procedure :: is_modified
      procedure :: is_realized
   end type GraphAssemblyStatus

contains

   subroutine mark_initialized(this)
      class(GraphAssemblyStatus), intent(inout) :: this
      this%phase_ = DECLARING
   end subroutine mark_initialized

   subroutine mark_advertised(this)
      class(GraphAssemblyStatus), intent(inout) :: this
      this%phase_ = ADVERTISED
   end subroutine mark_advertised

   subroutine mark_modified(this)
      class(GraphAssemblyStatus), intent(inout) :: this
      this%phase_ = MODIFIED
   end subroutine mark_modified

   subroutine mark_realized(this)
      class(GraphAssemblyStatus), intent(inout) :: this
      this%phase_ = REALIZED
   end subroutine mark_realized

   pure logical function is_initialized(this)
      class(GraphAssemblyStatus), intent(in) :: this
      is_initialized = this%phase_ /= UNINITIALIZED
   end function is_initialized

   pure logical function is_declaring(this)
      class(GraphAssemblyStatus), intent(in) :: this
      is_declaring = this%phase_ == DECLARING
   end function is_declaring

   pure logical function is_advertised(this)
      class(GraphAssemblyStatus), intent(in) :: this
      is_advertised = this%phase_ == ADVERTISED
   end function is_advertised

   pure logical function is_modified(this)
      class(GraphAssemblyStatus), intent(in) :: this
      is_modified = this%phase_ == MODIFIED
   end function is_modified

   pure logical function is_realized(this)
      class(GraphAssemblyStatus), intent(in) :: this
      is_realized = this%phase_ == REALIZED
   end function is_realized

end module mapl_GraphAssemblyStatus_mod
