#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) create_subobjects_smod
   use mapl_InnerMetaComponent_mod, only: attach_inner_meta
   use mapl_OpenMP_Support_mod, only: make_subgeoms, make_substates, make_subgridcomps
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   ! Decompose the user gridcomp, its states and its geometry into
   ! `num_threads` "mini" components that each operate on a contiguous
   ! subset of the local geometry.
   !
   ! After creation, _GET_NAMED_PRIVATE_STATE calls on a mini gridcomp are
   ! transparently redirected to the primary user gridcomp via
   ! mapl_get_owning_gridcomp (see include/MAPL_private_state.h), so all
   ! threads share the same user private-state objects.  The mini gridcomps
   ! themselves carry only the InnerMetaComponent private state (added by
   ! attach_inner_meta below), which enables the redirect.
   !
   ! The count of ESMF internal-state labels on the user gridcomp at this
   ! point is recorded in this%user_gc_label_count.  activate_threading
   ! asserts the count is unchanged on subsequent activations, so that
   ! private state added after the first threaded run fails loudly.
   module subroutine create_subobjects(this, num_threads, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      integer, intent(in) :: num_threads
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: i, status
      type(MultiState) :: user_states
      type(ESMF_GridComp) :: user_gridcomp
      type(ESMF_Geom), allocatable :: subgeoms(:)
      type(ESMF_State), allocatable :: subimports(:), subexports(:), subinternals(:)
      type(ESMF_GridComp), allocatable :: subgridcomps(:)
      character(len=:), allocatable :: label_list(:)

      _ASSERT(num_threads >= 1, 'num_threads must be at least 1')
      _ASSERT(.not. allocated(this%subcomponents), 'sub objects have already been created')
      _ASSERT(allocated(this%geom), 'OpenMP threading requires a geometry')

      allocate(this%subcomponents(num_threads))

      user_states = this%user_gc_driver%get_states()

      subimports = make_substates(user_states%importState, num_threads, _RC)
      subexports = make_substates(user_states%exportState, num_threads, _RC)
      subinternals = make_substates(user_states%internalState, num_threads, _RC)
      subgeoms = make_subgeoms(this%geom, num_threads, _RC)

      user_gridcomp = this%user_gc_driver%get_gridcomp()
      subgridcomps = make_subgridcomps(user_gridcomp, this%run_entry_points, num_threads, _RC)

      ! Record how many ESMF internal-state labels exist on the user
      ! gridcomp now.  This snapshot is used by activate_threading to
      ! detect (and reject) private state that is added after this point.
      call ESMF_InternalStateGet(user_gridcomp, labelList=label_list, rc=status)
      _VERIFY(status)
      this%user_gc_label_count = size(label_list)

      do i = 1, num_threads
         this%subcomponents(i)%states%importState = subimports(i)
         this%subcomponents(i)%states%exportState = subexports(i)
         this%subcomponents(i)%states%internalState = subinternals(i)
         this%subcomponents(i)%geom = subgeoms(i)
         this%subcomponents(i)%gridcomp = subgridcomps(i)
         ! Each mini gridcomp must be able to find its (shared) outer meta
         ! component, just as the primary user gridcomp does.  The inner
         ! meta is the only private state that lives on the mini gridcomp
         ! itself; all user private state is resolved through
         ! mapl_get_owning_gridcomp at access time.
         call attach_inner_meta(this%subcomponents(i)%gridcomp, this%self_gridcomp, _RC)
      end do

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine create_subobjects

end submodule create_subobjects_smod
