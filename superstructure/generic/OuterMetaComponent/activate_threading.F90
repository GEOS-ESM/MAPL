#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) activate_threading_smod
   use mapl_GriddedComponentDriverMap_mod
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   ! Prepare this component (and its children) to be run on `num_threads`
   ! OpenMP threads.  Each component keeps a replica of its user gridcomp,
   ! states and geometry for each thread.  Sub objects are only created
   ! once - subsequent activations reuse them.
   !
   ! Guard: on the reuse path, assert that the primary user gridcomp still
   ! carries the same number of ESMF internal-state labels as when the
   ! subcomponents were first created.  If a user component adds private
   ! state after the first threaded run, the new label would not be
   ! reachable from the mini gridcomps (mapl_get_owning_gridcomp resolves
   ! to the primary user gridcomp, but only labels that existed at
   ! create_subobjects time are present there), and the GET macro would
   ! fail with a confusing "not found" message.  This assertion converts
   ! that silent semantic error into an early, descriptive failure.
   module recursive subroutine activate_threading(this, num_threads, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, intent(in) :: num_threads
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(GriddedComponentDriverMapIterator) :: iter
      type(GriddedComponentDriver), pointer :: child
      type(OuterMetaComponent), pointer :: child_meta
      type(ESMF_GridComp) :: child_outer_gc
      type(ESMF_GridComp) :: user_gridcomp
      character(len=:), allocatable :: label_list(:)
      integer :: current_label_count

      _ASSERT(num_threads >= 1, 'num_threads must be at least 1')

      if (.not. allocated(this%subcomponents)) then
         call this%create_subobjects(num_threads, _RC)
      else
         ! Subcomponents already exist: verify that no private state has been
         ! added to the user gridcomp since they were created.
         user_gridcomp = this%user_gc_driver%get_gridcomp()
         call ESMF_InternalStateGet(user_gridcomp, labelList=label_list, rc=status)
         _VERIFY(status)
         current_label_count = size(label_list)
         _ASSERT(current_label_count == this%user_gc_label_count, &
              'Private state was added to a user component after the first threaded run. ' // &
              'Private state must be registered in SetServices, before any threaded run occurs. ' // &
              'New labels will not be visible to mini gridcomps and _GET_NAMED_PRIVATE_STATE will fail.')
      end if
      _ASSERT(size(this%subcomponents) == num_threads, 'sub objects were created for a different number of threads')

      this%threading_active = .true.

      associate (e => this%children%end())
        iter = this%children%begin()
        do while (iter /= e)
           child => iter%second()
           child_outer_gc = child%get_gridcomp()
           child_meta => get_outer_meta(child_outer_gc, _RC)
           call child_meta%activate_threading(num_threads, _RC)
           call iter%next()
        end do
      end associate

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine activate_threading

end submodule activate_threading_smod
