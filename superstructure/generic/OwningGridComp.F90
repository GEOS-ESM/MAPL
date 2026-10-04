#include "MAPL.h"

! Provides mapl_get_owning_gridcomp, the indirection function used by
! _GET_NAMED_PRIVATE_STATE and _FREE_NAMED_PRIVATE_STATE to transparently
! redirect private-state lookups from a "mini" OpenMP sub-gridcomp to the
! primary user gridcomp that owns the user-visible private state.
!
! This is a small standalone module so that modules such as
! mapl_CouplerMetaComponent_mod can use it without taking a compile-time
! dependency on all of mapl_OuterMetaComponent_mod.
!
! Implementation note
! -------------------
! Only public interfaces are used: INNER_META_LABEL and
! InnerMetaComponent%get_outer_gridcomp() from mapl_InnerMetaComponent_mod,
! get_outer_meta() from mapl_OuterMetaComponent_mod, and the public
! get_user_gc_driver() / get_gridcomp() accessors.

module mapl_OwningGridComp_mod
   use mapl_InnerMetaComponent_mod, only: InnerMetaComponent, INNER_META_LABEL
   use mapl_OuterMetaComponent_mod, only: OuterMetaComponent, get_outer_meta
   use mapl_GriddedComponentDriver_mod, only: GriddedComponentDriver
   use mapl_ErrorHandling_mod
   use esmf
   implicit none(type,external)
   private

   public :: mapl_get_owning_gridcomp

contains

   ! Return the primary user gridcomp that owns user-visible private state.
   !
   ! For a normal (non-mini) gridcomp this is a cheap probe that returns gc
   ! unchanged.  For a mini gridcomp created by the OpenMP threading layer,
   ! it walks:
   !
   !   mini gc  --(ESMF INNER_META_LABEL)-->  InnerMetaComponent
   !            --(get_outer_gridcomp)-->      self_gridcomp
   !            --(get_outer_meta)-->          OuterMetaComponent
   !            --(get_user_gc_driver)-->      GriddedComponentDriver
   !            --(get_gridcomp)-->            primary user gridcomp
   !
   ! IMPORTANT: This function uses ESMF_InternalStateGet directly rather
   ! than the _GET_NAMED_PRIVATE_STATE macro to avoid infinite recursion
   ! (_GET_NAMED_PRIVATE_STATE calls this function for its redirect).
   ! Maximum recursion depth through get_outer_meta is 2; see comments below.
   function mapl_get_owning_gridcomp(gc, rc) result(owner)
      type(ESMF_GridComp) :: owner
      type(ESMF_GridComp), intent(inout) :: gc
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_GridComp) :: outer_gc
      type(OuterMetaComponent), pointer :: outer_meta
      type(GriddedComponentDriver), pointer :: user_driver

      ! Wrapper layout must match _DECLARE_WRAPPER(InnerMetaComponent).
      type :: InnerMetaWrapper
         type(InnerMetaComponent), pointer :: ptr
      end type InnerMetaWrapper

      type(InnerMetaWrapper) :: w

      ! Probe for an inner meta component via a direct ESMF call.
      ! A failure means gc has no inner meta; return it unchanged.
      call ESMF_InternalStateGet(gc, internalState=w, label=INNER_META_LABEL, rc=status)
      if (status /= ESMF_SUCCESS) then
         owner = gc
         _RETURN(_SUCCESS)
      end if

      ! Walk: inner meta -> outer_gc (the outer meta gridcomp, self_gridcomp).
      outer_gc = w%ptr%get_outer_gridcomp()

      ! Probe for an outer meta on outer_gc.  get_outer_meta expands
      ! _GET_NAMED_PRIVATE_STATE, which calls mapl_get_owning_gridcomp(outer_gc).
      ! That second call probes inner meta on outer_gc — which does NOT carry
      ! one (inner meta lives on user_gc, not self_gridcomp) — so it returns
      ! outer_gc immediately.  Recursion terminates at depth 2.
      outer_meta => get_outer_meta(outer_gc, rc=status)
      if (status /= ESMF_SUCCESS) then
         ! gc has inner meta but outer_gc has no outer meta: gc is not a
         ! mini gridcomp in a threaded context; return gc unchanged.
         owner = gc
         _RETURN(_SUCCESS)
      end if

      ! gc is a mini gridcomp: return the primary user gridcomp.
      ! Two-step to avoid chained function-result part-ref (not all compilers
      ! support this as a left-hand side expression).
      user_driver => outer_meta%get_user_gc_driver()
      owner = user_driver%get_gridcomp()

      _RETURN(_SUCCESS)
   end function mapl_get_owning_gridcomp

end module mapl_OwningGridComp_mod
