#include "MAPL_ErrLog.h"

module mapl3g_InnerMetaComponent
   use :: mapl_ErrorHandling
   use esmf
   implicit none
   private

   public :: InnerMetaComponent
   public :: get_inner_meta
   public :: attach_inner_meta
   public :: free_inner_meta
   
   type :: InnerMetaComponent
      private
      character(len=:), allocatable :: name
      type(ESMF_GridComp) :: self_gc ! mabye not needed?
      type(ESMF_GridComp) :: outer_gc

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
      type(InnerMetaWrapper) :: wrapper

      inner_meta => null()
      
      call ESMF_UserCompGetInternalState(gridcomp, INNER_META_PRIVATE_STATE, wrapper, status)
      _ASSERT(status==ESMF_SUCCESS, "InnerMetaComponent not found for this gridcomp.")
      inner_meta => wrapper%inner_meta
      
      _RETURN(_SUCCESS)
   end function get_inner_meta

   subroutine attach_inner_meta(self_gc, outer_gc, rc)
      type(ESMF_GridComp), intent(inout) :: self_gc
      type(ESMF_GridComp), intent(in) :: outer_gc
      type(InnerMetaComponent), target :: inner_meta
      integer, optional, intent(out) :: rc

      type(InnerMetaWrapper) :: wrapper
      integer :: status

             block
               character(ESMF_MAXSTR) :: name
               call ESMF_GridCompGet(self_gc, name=name, _RC)
               _HERE, '... attach inner meta for <',trim(name),'> '
             end block


      allocate(wrapper%inner_meta)
      wrapper%inner_meta = InnerMetaComponent(self_gc, outer_gc)
      call ESMF_UserCompSetInternalState(self_gc, INNER_META_PRIVATE_STATE, wrapper, status)
      _ASSERT(status==ESMF_SUCCESS, "Unable to set InnerMetaComponent for this gridcomp.")
      
      _RETURN(_SUCCESS)
   end subroutine attach_inner_meta

   subroutine free_inner_meta(gridcomp, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(InnerMetaWrapper) :: wrapper

      call ESMF_UserCompGetInternalState(gridcomp, INNER_META_PRIVATE_STATE, wrapper, status)
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
   
