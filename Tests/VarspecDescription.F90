#define _success      0
#define _failure     1
#define _verify(A)   if(  A/=0) then; if(present(rc)) rc=A; PRINT *, Iam, __LINE__; return; endif
#define _assert(A)   if(.not.A) then; if(present(rc)) rc=_failure; PRINT *, Iam, __LINE__; return; endif
#define _return(A)   if(present(rc)) rc=A; return

module VarspecDescriptionMod
   use MAPL
   use ESMF
   use gFTL_StringVector
   implicit none
   private

   public :: VarspecDescription

   type, extends(MAPL_VarSpecType) :: VarspecDescription
   contains
      procedure :: addNewSpec
   end type VarspecDescription

   interface VarspecDescription
      module procedure new_VarspecDescriptionFromConfig
   end interface VarspecDescription

contains

   ! have to pass in the number of words on the config line because
   ! of a config bug, config really sucks
   function new_VarspecDescriptionFromConfig(cf,nwords,rc) result(VarspecDescr)
      type(VarspecDescription) :: VarspecDescr
      type(ESMF_Config), intent(inout) :: cf
      integer, intent(in) :: nwords
      logical :: lcomp
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = 'new_VarspecDescriptionFromConfig'
      integer :: status

      type(StringVector) :: svec
      integer :: i
      integer, pointer :: ungrid_ptr(:)
      character(ESMF_MAXSTR) :: tmpstring

      do i=1,nwords
         call ESMF_ConfigGetAttribute(cf,tmpstring,rc=status)
         _verify(status)
         if (trim(tmpstring)==',') cycle
         call svec%push_back(trim(tmpstring))
      enddo

      lcomp = (svec%size()==5 .or. svec%size()==6)
      _assert(lcomp)
      VarspecDescr%short_name = svec%at(1)
      VarspecDescr%long_name = svec%at(2)
      VarspecDescr%units = svec%at(3)
      tmpstring = svec%at(4)
      if (trim(tmpstring) == 'xy') then
         VarspecDescr%dims = MAPL_DimsHorzOnly
      else if (trim(tmpstring) == 'xyz') then
         VarspecDescr%dims = MAPL_DimsHorzVert
      else if (trim(tmpstring) == 'tileonly') then
         VarspecDescr%dims = MAPL_DimsTileOnly
      end if
      tmpstring = svec%at(5)
      if (trim(tmpstring) == 'none') then
         VarspecDescr%location = MAPL_VLocationNone
      else if (trim(tmpstring) == 'c') then
         VarspecDescr%location = MAPL_VLocationCenter
      else if (trim(tmpstring) == 'e') then
         VarspecDescr%location = MAPL_VLocationEdge
      end if
      if (svec%size() == 6) then
         tmpstring = svec%at(6)
         allocate(ungrid_ptr(1))
         read(tmpstring,*)ungrid_ptr(1)
         if (ungrid_ptr(1) > 0) VarspecDescr%ungridded_dims => ungrid_ptr
      end if


   end function new_VarspecDescriptionFromConfig

   subroutine addNewSpec(this,gc,specType,rc)
      class(VarspecDescription), intent(in) :: this
      type(ESMF_GridComp), intent(inout) :: gc
      character(*), intent(in) :: specType
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam = "addNewSpec"

      if (specType == "IMPORT") then
         call MAPL_AddImportSpec(GC, &
              SHORT_NAME = this%short_name, &
              LONG_NAME = this%long_name, &
              UNITS = this%units, &
              DIMS = this%dims, &
              VLOCATION = this%location, &
              !STAGGERING = this%staggering, &
              !ROTATION = this%rotation, &
              UNGRIDDED_DIMS = this%ungridded_dims, &
              RC = status)
      else if (specType == "EXPORT") then
         call MAPL_AddExportSpec(GC, &
              SHORT_NAME = this%short_name, &
              LONG_NAME = this%long_name, &
              UNITS = this%units, &
              DIMS = this%dims, &
              VLOCATION = this%location, &
              !STAGGERING = this%staggering, &
              !ROTATION = this%rotation, &
              UNGRIDDED_DIMS = this%ungridded_dims, &
              RC = status)
      else
         _return(_failure)
      end if
      _verify(status)

   end subroutine AddNewSpec

end module VarspecDescriptionMod


module VarspecDescriptionVectorMod
   use VarspecDescriptionMod

#define _type type (VarspecDescription)
#define _vector VarspecDescriptionVector
#define _iterator VarspecDescriptionVectorIterator
#include "templates/vector.inc"

end module VarspecDescriptionVectorMod

