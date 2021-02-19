#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module LocStreamFactoryMod

   use ESMF
   use MAPL_ErrorHandlingMod
   use MAPL_KeywordEnforcerMod
   use MAPL_ConstantsMod
   use MAPL_BaseMod, only : MAPL_DecomposeDim
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
         integer :: my_pet,n_pets,status,offset
         real(kind=REAL64), allocatable :: tlons(:),tlats(:)
         integer, allocatable :: local_count(:)

         _UNUSED_DUMMY(unusable) 
         call ESMF_VMGetCurrent(vm,rc=status)
         _VERIFY(status)
         call ESMF_VMGet(vm,localPet=my_pet,peCount=n_pets,rc=status)
         _VERIFY(status)
         allocate(local_count(0:n_pets-1))
         call MAPL_DecomposeDim(size(this%lons),local_count,n_pets)
         _VERIFY(status)
         allocate(tlons(local_count(my_pet)))
         allocate(tlats(local_count(my_pet)))
         offset=0
         if (my_pet/=0) offset=sum(local_count(0:my_pet-1))
         tlons=this%lons(offset+1:offset+local_count(my_pet))
         tlats=this%lats(offset+1:offset+local_count(my_pet))
         tlons=tlons*MAPL_PI_R8/180.0d0
         tlats=tlats*MAPL_PI_R8/180.0d0

         locstream = ESMF_LocStreamCreate(localCount=local_count(my_pet),coordSys=ESMF_COORDSYS_SPH_RAD,rc=status)
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
