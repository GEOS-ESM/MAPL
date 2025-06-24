#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module LocStreamFactoryMod

   use ESMF
   use MAPL_ErrorHandlingMod
   use MAPL_KeywordEnforcerMod
   use MAPL_Constants
   use, intrinsic :: iso_fortran_env, only: REAL32
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none
   private

   public :: LocStreamFactory

   type :: LocStreamFactory
      private
      real(kind=REAL64), allocatable :: lons(:)
      real(kind=REAL64), allocatable :: lats(:)
      contains
        procedure :: create_locstream
        procedure :: create_locstream_on_proc
        procedure :: destroy_locstream
   end type

   interface LocStreamFactory
      module procedure LocStreamFactory_from_arrays
   end interface LocStreamFactory

   contains

      function LocStreamFactory_from_arrays(lons,lats,unusable,rc) result(factory)
         type(LocStreamFactory) :: factory
         real(kind=REAL64), intent(in) :: lons(:)
         real(kind=REAL64), intent(in) :: lats(:)
         class (KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
         integer :: status

         __UNUSED_DUMMY(unusable)

         __ASSERT(size(lons)==size(lats),"Lats and Lons for locstream must be same size")
         allocate(factory%lons,source=lons,stat=status)
         __VERIFY(status)
         allocate(factory%lats,source=lats,stat=status)
         __VERIFY(status)
         __RETURN(__SUCCESS)
      end function LocStreamFactory_from_arrays

      function create_locstream(this,unusable,grid,rc) result(locstream)
         type(ESMF_LocStream) :: locstream
         class (LocStreamFactory) :: this
         class (KeywordEnforcer), optional, intent(in) :: unusable
         type(ESMF_Grid), optional :: grid
         integer, optional, intent(out) :: rc

         type(ESMF_VM) :: vm
         integer :: my_pet,local_count,status
         real(kind=REAL64), allocatable :: tlons(:),tlats(:)

         __UNUSED_DUMMY(unusable)
         call ESMF_VMGetCurrent(vm,rc=status)
         __VERIFY(status)
         call ESMF_VMGet(vm,localPet=my_pet,rc=status)
         __VERIFY(status)
         if (my_pet==0) then
            local_count = size(this%lons)
            allocate(tlons(size(this%lons)),source=this%lons,stat=status)
            __VERIFY(status)
            allocate(tlats(size(this%lats)),source=this%lats,stat=status)
            __VERIFY(status)
            tlons=tlons*MAPL_PI_R8/180.0d0
            tlats=tlats*MAPL_PI_R8/180.0d0
         else
            local_count = 0
            allocate(tlons(0),stat=status)
            __VERIFY(status)
            allocate(tlats(0),stat=status)
            __VERIFY(status)
         end if

         locstream = ESMF_LocStreamCreate(localCount=local_count,coordSys=ESMF_COORDSYS_SPH_RAD,rc=status)
         __VERIFY(status)
         call ESMF_LocStreamAddKey(locstream,keyName="ESMF:Lat",farray=tlats,datacopyflag=ESMF_DATACOPY_VALUE, &
                 keyUnits="Radians", keyLongName="Latitude",rc=status)
         __VERIFY(status)
         call ESMF_LocStreamAddKey(locstream,keyName="ESMF:Lon",farray=tlons,datacopyflag=ESMF_DATACOPY_VALUE, &
                 keyUnits="Radians", keyLongName="Longitude",rc=status)
         __VERIFY(status)

         if (present(grid)) then
            locstream = ESMF_LocStreamCreate(locstream,background=grid,rc=status)
            __VERIFY(status)
         end if
         __RETURN(__SUCCESS)
      end function create_locstream

      function create_locstream_on_proc (this,unusable,grid,rc) result(locstream)
         type(ESMF_LocStream) :: locstream
         class (LocStreamFactory) :: this
         class (KeywordEnforcer), optional, intent(in) :: unusable
         type(ESMF_Grid), optional :: grid
         integer, optional, intent(out) :: rc

         integer :: local_count,status
         real(kind=REAL64), allocatable :: tlons(:),tlats(:)

         local_count = size(this%lons)
         allocate(tlons(size(this%lons)),source=this%lons,stat=status)
         __VERIFY(status)
         allocate(tlats(size(this%lats)),source=this%lats,stat=status)
         __VERIFY(status)

         tlons=tlons*MAPL_PI_R8/180.0d0
         tlats=tlats*MAPL_PI_R8/180.0d0

         locstream = ESMF_LocStreamCreate(localCount=local_count,coordSys=ESMF_COORDSYS_SPH_RAD,__RC)
         call ESMF_LocStreamAddKey(locstream,keyName="ESMF:Lat",farray=tlats,datacopyflag=ESMF_DATACOPY_VALUE, &
                 keyUnits="Radians", keyLongName="Latitude",__RC)
         call ESMF_LocStreamAddKey(locstream,keyName="ESMF:Lon",farray=tlons,datacopyflag=ESMF_DATACOPY_VALUE, &
                 keyUnits="Radians", keyLongName="Longitude",__RC)

         if (present(grid)) then
            locstream = ESMF_LocStreamCreate(locstream,background=grid,__RC)
         end if
         __RETURN(__SUCCESS)
       end function create_locstream_on_proc

      subroutine destroy_locstream(this,locstream,rc)
        class (LocStreamFactory) :: this
        type(ESMF_LocStream) :: locstream
        integer, optional, intent(out) :: rc
        integer :: status

        if (allocated(this%lons)) deallocate (this%lons)
        if (allocated(this%lats)) deallocate (this%lats)
        call ESMF_LocStreamDestroy (locstream,noGarbage=.true.,__RC)

        __RETURN(__SUCCESS)
      end subroutine destroy_locstream


end module LocStreamFactoryMod
