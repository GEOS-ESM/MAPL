#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module LocStreamFactoryMod

   use ESMF
   use MAPL_ErrorHandlingMod
   use MAPL_KeywordEnforcerMod
   use MAPL_ConstantsMod
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
      
         _UNUSED_DUMMY(unusable)
 
         _ASSERT(size(lons)==size(lats),"Lats and Lons for locstream must be same size")
         allocate(factory%lons,source=lons,stat=status)
         _VERIFY(status)
         allocate(factory%lats,source=lats,stat=status)
         _VERIFY(status)
         _RETURN(_SUCCESS)
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

         _UNUSED_DUMMY(unusable) 
         call ESMF_VMGetCurrent(vm,rc=status)
         _VERIFY(status)
         call ESMF_VMGet(vm,localPet=my_pet,rc=status)
         _VERIFY(status)
         if (my_pet==0) then
            local_count = size(this%lons)
            allocate(tlons(size(this%lons)),source=this%lons,stat=status)
            _VERIFY(status)
            allocate(tlats(size(this%lats)),source=this%lats,stat=status)
            _VERIFY(status)
            tlons=tlons*MAPL_PI_R8/180.0d0
            tlats=tlats*MAPL_PI_R8/180.0d0
         else
            local_count = 0
            allocate(tlons(0),stat=status)
            _VERIFY(status)
            allocate(tlats(0),stat=status)
            _VERIFY(status)
         end if

         locstream = ESMF_LocStreamCreate(localCount=local_count,coordSys=ESMF_COORDSYS_SPH_RAD,rc=status)
         _VERIFY(status)
         call ESMF_LocStreamAddKey(locstream,keyName="ESMF:Lat",farray=tlats,datacopyflag=ESMF_DATACOPY_VALUE, &
                 keyUnits="Radians", keyLongName="Latitude",rc=status)
         _VERIFY(status)
         call ESMF_LocStreamAddKey(locstream,keyName="ESMF:Lon",farray=tlons,datacopyflag=ESMF_DATACOPY_VALUE, &
                 keyUnits="Radians", keyLongName="Longitude",rc=status)
         _VERIFY(status)

         if (present(grid)) then
            locstream = ESMF_LocStreamCreate(locstream,background=grid,rc=status)
            _VERIFY(status)
         end if
         _RETURN(_SUCCESS)
      end function create_locstream

end module LocStreamFactoryMod
