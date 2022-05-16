#include "MAPL_ErrLog.h"

module mapl3g_InnerMetaComponent
   use :: esmf, only: ESMF_GridComp
   use :: esmf, only: ESMF_SUCCESS
   use :: mapl_ErrorHandling
   implicit none
   private

   public :: InnerMetaComponent
   public :: get_inner_meta
   public :: set_inner_meta

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
      
   end type InnerMetaComponent

   type :: InnerMetaWrapper
      type(InnerMetaComponent), pointer :: inner_meta
   end type InnerMetaWrapper

   character(len=*), parameter :: INNER_META_PRIVATE_STATE = "InnerMetaComponent Private State"

contains

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

   subroutine set_inner_meta(gridcomp, inner_meta, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(InnerMetaComponent), target :: inner_meta
      integer, optional, intent(out) :: rc

      integer :: status
      type(InnerMetaWrapper) :: wrapper

      wrapper%inner_meta => inner_meta
      call ESMF_UserCompSetInternalState(gridcomp, INNER_META_PRIVATE_STATE, wrapper, status)
      _ASSERT(status==ESMF_SUCCESS, "Unable to set InnerMetaComponent for this gridcomp.")
      
      _RETURN(_SUCCESS)
   end subroutine set_inner_meta


end module mapl3g_InnerMetaComponent
   
