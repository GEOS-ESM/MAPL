#define _SUCCESS      0
#define _FAILURE     1
#define _VERIFY(A)   if(  A/=0) then; if(present(rc)) rc=A; PRINT *, Iam, __LINE__; return; endif
#define _ASSERT(A)   if(.not.A) then; if(present(rc)) rc=_FAILURE; PRINT *, Iam, __LINE__; return; endif
#define _RETURN(A)   if(present(rc)) rc=A; return

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
      character(ESMF_MAXSTR) :: tmpstring

      do i=1,nwords
         call ESMF_ConfigGetAttribute(cf,tmpstring,rc=status)
         _VERIFY(status)
         if (trim(tmpstring)==',') cycle
         call svec%push_back(trim(tmpstring))
      enddo

      lcomp = (svec%size()==5 .or. svec%size()==7)
      _ASSERT(lcomp)
      VarspecDescr%short_name = svec%at(1)
      VarspecDescr%long_name = svec%at(2)
      VarspecDescr%units = svec%at(3)
      tmpstring = svec%at(4)
      if (trim(tmpstring) == 'xy') then
         VarspecDescr%dims = MAPL_DimsHorzOnly
      else if (trim(tmpstring) == 'xyz') then
         VarspecDescr%dims = MAPL_DimsHorzVert
      end if
      tmpstring = svec%at(5)
      if (trim(tmpstring) == 'none') then
         VarspecDescr%location = MAPL_VLocationNone
      else if (trim(tmpstring) == 'c') then
         VarspecDescr%location = MAPL_VLocationCenter
      else if (trim(tmpstring) == 'e') then
         VarspecDescr%location = MAPL_VLocationEdge
      end if

      if (svec%size() == 7) then
         tmpstring = svec%at(6)
         if (trim(tmpstring)== 'agrid') then
            VarspecDescr%staggering = MAPL_AGrid
         else if (trim(tmpstring)== 'cgrid') then
            VarspecDescr%staggering = MAPL_CGrid
         else if (trim(tmpstring)== 'dgrid') then
            VarspecDescr%staggering = MAPL_DGrid
         end if

         tmpstring = svec%at(7)
         if (trim(tmpstring)== 'grid_aligned') then
            VarspecDescr%rotation = MAPL_RotateCube
         else if (trim(tmpstring)== 'latlon_aligned') then
            VarspecDescr%rotation = MAPL_RotateLL
         end if

      else
         VarspecDescr%staggering = MAPL_AGrid
         VarspecDescr%rotation = MAPL_AGrid
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
              STAGGERING = this%staggering, &
              ROTATION = this%rotation, &
              RC = status)
      else if (specType == "EXPORT") then
         call MAPL_AddExportSpec(GC, &
              SHORT_NAME = this%short_name, &
              LONG_NAME = this%long_name, &
              UNITS = this%units, &
              DIMS = this%dims, &
              VLOCATION = this%location, &
              STAGGERING = this%staggering, &
              ROTATION = this%rotation, &
              RC = status)
      else
         _RETURN(_FAILURE)
      end if
      _VERIFY(status)

   end subroutine AddNewSpec

end module VarspecDescriptionMod


module VarspecDescriptionVectorMod
   use VarspecDescriptionMod

#define _type type (VarspecDescription)
#define _vector VarspecDescriptionVector
#define _iterator VarspecDescriptionVectorIterator
#include "templates/vector.inc"
   
end module VarspecDescriptionVectorMod

