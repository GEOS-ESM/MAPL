module mapl3g_AbstractStateItemSpec
   implicit none
   private

   public :: AbstractStateItemSpec

   type, abstract :: AbstractStateItemSpec
      private

      logical :: active = .false.
      logical :: created = .false.
      logical :: allocated = .false.

   contains

      procedure(I_make), deferred :: create
      procedure(I_make), deferred :: destroy
      procedure(I_make), deferred :: allocate

      procedure(I_connect), deferred :: connect_to
      procedure(I_can_connect), deferred :: can_connect_to
      procedure(I_can_connect), deferred :: requires_extension

      procedure(I_add_to_state), deferred :: add_to_state

      procedure, non_overridable :: set_created
      procedure, non_overridable :: is_created
      procedure, non_overridable :: set_allocated
      procedure, non_overridable :: is_allocated
      procedure, non_overridable :: is_active
      procedure, non_overridable :: set_active

   end type AbstractStateItemSpec

   abstract interface

      subroutine I_connect(this, src_spec, rc)
         use mapl3g_ConnectionSpec
         import AbstractStateItemSpec
         class(AbstractStateItemSpec), intent(inout) :: this
         class(AbstractStateItemSpec), intent(in) :: src_spec
         integer, optional, intent(out) :: rc
      end subroutine I_connect

      logical function I_can_connect(this, src_spec)
         use mapl3g_ConnectionSpec
         import AbstractStateItemSpec
         class(AbstractStateItemSpec), intent(in) :: this
         class(AbstractStateItemSpec), intent(in) :: src_spec
      end function I_can_connect

      ! Will use ESMF so cannot be PURE
      subroutine I_make(this, rc)
         import AbstractStateItemSpec
         class(AbstractStateItemSpec), intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine I_make

      subroutine I_add_to_state(this, state, short_name, rc)
         use ESMF, only: ESMF_State
         import AbstractStateItemSpec
         class(AbstractStateItemSpec), intent(in) :: this
         type(ESMF_State), intent(inout) :: state
         character(*), intent(in) :: short_name
         integer, optional, intent(out) :: rc
      end subroutine I_add_to_state

   end interface

contains
   
!!$   ! Non overridable methods
!!$   ! ------------------------
!!$   
!!$   pure subroutine set_name(this, name)
!!$      class(AbstractStateItemSpec), intent(inout) :: this
!!$      character(*), intent(in) :: name
!!$      this%name = name
!!$   end subroutine set_name
!!$   
!!$
!!$   pure function get_name(this) result(name)
!!$      character(:), allocatable :: name
!!$      class(AbstractStateItemSpec), intent(in) :: this
!!$      name = this%name
!!$   end function get_name
!!$   
!!$   pure subroutine set_ultimate_source_gc(this, ultimate_source_gc)
!!$      class(AbstractStateItemSpec), intent(inout) :: this
!!$      character(*), intent(in) :: ultimate_source_gc
!!$      this%ultimate_source_gc = ultimate_source_gc
!!$   end subroutine set_ultimate_source_gc
!!$   
!!$
!!$   pure function get_ultimate_source_gc(this) result(ultimate_source_gc)
!!$      character(:), allocatable :: ultimate_source_gc
!!$      class(AbstractStateItemSpec), intent(in) :: this
!!$      ultimate_source_gc = this%ultimate_source_gc
!!$   end function get_ultimate_source_gc
!!$   
!!$
   pure subroutine set_allocated(this, allocated)
      class(AbstractStateItemSpec), intent(inout) :: this
      logical, optional, intent(in) :: allocated

      if (present(allocated)) then
         this%allocated = allocated
      else
         this%allocated =  .true.
      end if

   end subroutine set_allocated

   pure logical function is_allocated(this)
      class(AbstractStateItemSpec), intent(in) :: this
      is_allocated = this%allocated
   end function is_allocated

   pure subroutine set_created(this, created)
      class(AbstractStateItemSpec), intent(inout) :: this
      logical, optional, intent(in) :: created

      if (present(created)) then
         this%created = created
      else
         this%created =  .true.
      end if

   end subroutine set_created

   pure logical function is_created(this)
      class(AbstractStateItemSpec), intent(in) :: this
      is_created = this%created
   end function is_created

   pure subroutine set_active(this, active)
      class(AbstractStateItemSpec), intent(inout) :: this
      logical, optional, intent(in) :: active

      if (present(active)) then
         this%active = active
      else
         this%active =  .true.
      end if

   end subroutine set_active

   pure logical function is_active(this)
      class(AbstractStateItemSpec), intent(in) :: this
      is_active = this%active
   end function is_active


end module mapl3g_AbstractStateItemSpec
