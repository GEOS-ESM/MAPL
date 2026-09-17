#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) init_meta_smod
   use mapl_ErrorHandling_mod
   use pFlogger, only: logging
   implicit none(type,external)

contains

   ! NOTE: _Not_ an ESMF phase - this is initializing the object itself.
   ! Constructor (new_outer_meta) only copies basic parameters.  All
   ! other initialization is in this procedure.

   module subroutine init_meta(this, rc)
      class(OuterMetaComponent), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: user_gc_name

      user_gc_name = this%user_gc_driver%get_name(_RC)
      this%registry = StateRegistry(user_gc_name)

      ! docs/graph/spec/02-component-hierarchy.md REQ-HIER-002/004: every
      ! OuterComponent owns exactly one local ComponentGraph. Constructed
      ! here (not in new_outer_meta) to match the registry's construction
      ! pattern above - ComponentGraph() is non-trivial (seeds the default
      ! network id) so it must be called explicitly, not left to
      ! component-wise default initialization.
      this%local_graph = ComponentGraph()

      this%lgr => logging%get_logger(user_gc_name)

      _RETURN(_SUCCESS)

   end subroutine init_meta

end submodule init_meta_smod
