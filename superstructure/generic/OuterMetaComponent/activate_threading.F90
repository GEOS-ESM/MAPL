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

      _ASSERT(num_threads >= 1, 'num_threads must be at least 1')

      if (.not. allocated(this%subcomponents)) then
         call this%create_subobjects(num_threads, _RC)
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
