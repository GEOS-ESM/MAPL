module ud2f_CptrWrapper
   use, intrinsic :: iso_c_binding, only: c_ptr, C_NULL_PTR, c_associated
   implicit none
   private

   public :: CptrWrapper

!================================ CPTRWRAPPER ==================================
! Base class to wrap type(c_ptr) instances used for udunits2 objects that cannot 
! interface directly to fortran. Each extended class must provide a subroutine
! to free the memory associated with cptr_
   type, abstract :: CptrWrapper
      private
      type(c_ptr) :: cptr_ = C_NULL_PTR
   contains
      procedure :: get_cptr
      procedure :: set_cptr
      procedure :: is_free
      procedure :: free
      procedure(I_free_memory), deferred :: free_memory
   end type CptrWrapper

   abstract interface

      subroutine I_free_memory(this)
         import :: CptrWrapper
         class(CptrWrapper), intent(in) :: this
      end subroutine I_Free_Memory

   end interface

contains

   type(c_ptr) function get_cptr(this)
      class(CptrWrapper), intent(in) :: this

      get_cptr = this%cptr_

   end function get_cptr

   subroutine set_cptr(this, cptr)
      class(CptrWrapper), intent(inout) :: this
      type(c_ptr), intent(in) :: cptr
      this%cptr_ = cptr
   end subroutine set_cptr

   logical function is_free(this)
      class(CptrWrapper), intent(in) :: this

      is_free = .not. c_associated(this%cptr_)

   end function is_free

   ! Free up memory pointed to by cptr_ and set cptr_ to c_null_ptr
   subroutine free(this)
      class(CptrWrapper), intent(inout) :: this

      if(this%is_free()) return
      call this%free_memory()
      this%cptr_ = c_null_ptr

   end subroutine free

end module ud2f_CptrWrapper
