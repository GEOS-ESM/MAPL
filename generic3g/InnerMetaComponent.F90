#include "MAPL.h"

module mapl3g_InnerMetaComponent
   use :: mapl_ErrorHandling
   use :: mapl3_GenericGrid
   use :: mapl3g_ESMF_Interfaces, only: MAPL_UserCompGetInternalState
   use :: mapl3g_ESMF_Interfaces, only: MAPL_UserCompSetInternalState
   use esmf
   implicit none
   private

   public :: InnerMetaComponent
   public :: get_inner_meta
   public :: attach_inner_meta
   public :: free_inner_meta
   
   type :: InnerMetaComponent
      private
      type(ESMF_GridComp) :: outer_gc

      character(len=:), allocatable :: name
      type(ESMF_GridComp) :: self_gc ! mabye not needed?

      type(GenericGrid) :: generic_grid ! maybe should go to outer meta?

      real :: heartbeat
!!$      type(MAPL_SunOrbit) :: orbit
!!$      type(AlarmVector) :: alarms
!!$      type(DistributedProfiler) :: t_profiler
!!$      type(MaplGrid) :: grid

!!$      class(Logger), pointer :: lgr   ! Full compname:  "GCM.AGCM...."
   contains

      procedure :: get_outer_gridcomp
      
   end type InnerMetaComponent

   type :: InnerMetaWrapper
      type(InnerMetaComponent), pointer :: inner_meta
   end type InnerMetaWrapper

   interface InnerMetaComponent
      module procedure :: new_InnerMetaComponent
   end interface InnerMetaComponent

   character(len=*), parameter :: INNER_META_PRIVATE_STATE = "InnerMetaComponent Private State"

contains

   function new_InnerMetaComponent(self_gc, outer_gc) result(meta)
      type(InnerMetaComponent) :: meta
      type(ESMF_GridComp), intent(in) :: self_gc
      type(ESMF_GridComp), intent(in) :: outer_gc

      meta%self_gc = self_gc
      meta%outer_gc = outer_gc

   end function new_InnerMetaComponent

   function get_inner_meta(gridcomp, rc) result(inner_meta)
      type(InnerMetaComponent), pointer :: inner_meta
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status

      _GET_NAMED_PRIVATE_STATE(gridcomp, InnerMetaComponent, INNER_META_PRIVATE_STATE, inner_meta)
      
      _RETURN(_SUCCESS)
   end function get_inner_meta

   subroutine attach_inner_meta(self_gc, outer_gc, rc)
      type(ESMF_GridComp), intent(inout) :: self_gc
      type(ESMF_GridComp), intent(in) :: outer_gc
      integer, optional, intent(out) :: rc

      type(InnerMetaComponent), pointer :: inner_meta
      integer :: status

      _SET_NAMED_PRIVATE_STATE(self_gc, InnerMetaComponent, INNER_META_PRIVATE_STATE)
      _GET_NAMED_PRIVATE_STATE(self_gc, InnerMetaComponent, INNER_META_PRIVATE_STATE, inner_meta)
      inner_meta = InnerMetaComponent(self_gc, outer_gc)
      
      _RETURN(_SUCCESS)
   end subroutine attach_inner_meta

   subroutine free_inner_meta(gridcomp, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(InnerMetaWrapper) :: wrapper

      call MAPL_UserCompGetInternalState(gridcomp, INNER_META_PRIVATE_STATE, wrapper, status)
      _ASSERT(status==ESMF_SUCCESS, "OuterMetaComponent not created for this gridcomp")
      deallocate(wrapper%inner_meta)

      _RETURN(_SUCCESS)
   end subroutine free_inner_meta

   function get_outer_gridcomp(this) result(gc)
      type(ESMF_GridComp) :: gc
      class(InnerMetaComponent), intent(in) :: this

      gc = this%outer_gc
   end function get_outer_gridcomp

   subroutine set_outer_gridcomp(this, gc)
      type(ESMF_GridComp), intent(in) :: gc
      class(InnerMetaComponent), intent(inout) :: this

      this%outer_gc = gc
   end subroutine set_outer_gridcomp


end module mapl3g_InnerMetaComponent
   
