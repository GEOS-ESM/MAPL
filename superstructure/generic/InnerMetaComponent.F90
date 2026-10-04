#include "MAPL.h"

module mapl_InnerMetaComponent_mod
   use :: mapl_ErrorHandling_mod
   use :: mapl3_GenericGrid
   use esmf
   implicit none(type,external)
   private

   public :: InnerMetaComponent
   public :: get_inner_meta
   public :: attach_inner_meta
   public :: free_inner_meta
   ! Exported so that get_owning_gridcomp can probe for the inner meta
   ! directly via ESMF_InternalStateGet without going through the
   ! _GET_NAMED_PRIVATE_STATE macro (which would be circular).
   public :: INNER_META_LABEL

   type :: InnerMetaComponent
      private
      type(ESMF_GridComp) :: outer_gc
   contains
      procedure :: get_outer_gridcomp
   end type InnerMetaComponent

   interface InnerMetaComponent
      module procedure :: new_InnerMetaComponent
   end interface InnerMetaComponent

   character(len=*), parameter :: INNER_META_LABEL = "InnerMetaComponent Private State"
   ! Keep old name as an alias so existing internal uses compile unchanged.
   character(len=*), parameter :: INNER_META_PRIVATE_STATE = INNER_META_LABEL

   ! Wrapper type matching the layout produced by _DECLARE_WRAPPER(InnerMetaComponent).
   ! Defined here so that get_inner_meta_wrapper_ can use it directly via
   ! ESMF_InternalStateGet without going through the macro.
   type :: InnerMetaWrapper
      type(InnerMetaComponent), pointer :: ptr
   end type InnerMetaWrapper

contains

   function new_InnerMetaComponent(outer_gc) result(meta)
      type(InnerMetaComponent) :: meta
      type(ESMF_GridComp), intent(in) :: outer_gc

      meta%outer_gc = outer_gc

   end function new_InnerMetaComponent

   ! Internal helper: look up the InnerMetaComponent wrapper directly via
   ! ESMF without going through _GET_NAMED_PRIVATE_STATE.  This is needed
   ! because _GET_NAMED_PRIVATE_STATE calls mapl_get_owning_gridcomp, which
   ! in turn calls get_inner_meta -- creating infinite recursion.  All
   ! callers here operate on the primary user gridcomp or the outer-meta
   ! self_gridcomp, never on a mini gridcomp, so no redirect is needed.
   subroutine get_inner_meta_wrapper_(gc, w, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      type(InnerMetaWrapper), intent(out) :: w
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_InternalStateGet(gc, internalState=w, label=INNER_META_LABEL, rc=status)
      _ASSERT(status == ESMF_SUCCESS, &
           "Private state with name <" // INNER_META_LABEL // "> not found for this gridcomp.")
      _RETURN(_SUCCESS)
   end subroutine get_inner_meta_wrapper_

   function get_inner_meta(gridcomp, rc) result(inner_meta)
      type(InnerMetaComponent), pointer :: inner_meta
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(InnerMetaWrapper) :: w

      call get_inner_meta_wrapper_(gridcomp, w, _RC)
      inner_meta => w%ptr

      _RETURN(_SUCCESS)
   end function get_inner_meta

   subroutine attach_inner_meta(self_gc, outer_gc, rc)
      type(ESMF_GridComp), intent(inout) :: self_gc
      type(ESMF_GridComp), intent(in) :: outer_gc
      integer, optional, intent(out) :: rc

      type(InnerMetaWrapper) :: w
      integer :: status

      _SET_NAMED_PRIVATE_STATE(self_gc, InnerMetaComponent, INNER_META_LABEL)
      call get_inner_meta_wrapper_(self_gc, w, _RC)
      w%ptr = InnerMetaComponent(outer_gc)

      _RETURN(_SUCCESS)
   end subroutine attach_inner_meta

   subroutine free_inner_meta(gridcomp, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(InnerMetaWrapper) :: w

      call get_inner_meta_wrapper_(gridcomp, w, _RC)
      ! Pointer retrieved; caller responsible for deallocation if needed.

      _RETURN(_SUCCESS)
   end subroutine free_inner_meta

   function get_outer_gridcomp(this) result(gc)
      type(ESMF_GridComp) :: gc
      class(InnerMetaComponent), intent(in) :: this

      gc = this%outer_gc
   end function get_outer_gridcomp

end module mapl_InnerMetaComponent_mod
