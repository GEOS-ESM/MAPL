#include "MAPL_Generic.h"

module VarspecDescriptionMod
   use MAPL
   use ESMF
   use gFTL_StringVector
   use MAPL_Constants, only: MAPL_R4, MAPL_R8
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

      integer :: status

      type(StringVector) :: svec
      integer :: i
      integer, pointer :: ungrid_ptr(:)
      character(ESMF_MAXSTR) :: tmpstring

      do i=1,nwords
         call ESMF_ConfigGetAttribute(cf,tmpstring,rc=status)
         _VERIFY(status)
         if (trim(tmpstring)==',') cycle
         call svec%push_back(trim(tmpstring))
      enddo

      lcomp = (svec%size()==6 .or. svec%size()==7)
      _ASSERT(lcomp, 'Wrong number of columns in state descriptor')
      VarspecDescr%short_name = svec%at(1)
      
      ! Parse and validate precision (column 2)
      tmpstring = svec%at(2)
      if (trim(tmpstring) == 'R4') then
         VarspecDescr%precision = MAPL_R4
      else if (trim(tmpstring) == 'R8') then
         VarspecDescr%precision = MAPL_R8
      else
        _FAIL('Invalid precision "'// trim(tmpstring) // '": Must be either R4 or R8')

      end if
      
      VarspecDescr%long_name = svec%at(3)
      VarspecDescr%units = svec%at(4)
      tmpstring = svec%at(5)
      if (trim(tmpstring) == 'xy') then
         VarspecDescr%dims = MAPL_DimsHorzOnly
      else if (trim(tmpstring) == 'xyz') then
         VarspecDescr%dims = MAPL_DimsHorzVert
      else if (trim(tmpstring) == 'tileonly') then
         VarspecDescr%dims = MAPL_DimsTileOnly
      end if
      tmpstring = svec%at(6)
      if (trim(tmpstring) == 'none') then
         VarspecDescr%location = MAPL_VLocationNone
      else if (trim(tmpstring) == 'c') then
         VarspecDescr%location = MAPL_VLocationCenter
      else if (trim(tmpstring) == 'e') then
         VarspecDescr%location = MAPL_VLocationEdge
      end if
      if (svec%size() == 7) then
         tmpstring = svec%at(7)
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

      if (specType == "IMPORT") then
         call MAPL_AddImportSpec(GC, &
              SHORT_NAME = this%short_name, &
              LONG_NAME = this%long_name, &
              UNITS = this%units, &
              DIMS = this%dims, &
              VLOCATION = this%location, &
              PRECISION = this%precision, &
              !STAGGERING = this%staggering, &
              !ROTATION = this%rotation, &
              UNGRIDDED_DIMS = this%ungridded_dims, &
              _RC)
      else if (specType == "EXPORT") then
         call MAPL_AddExportSpec(GC, &
              SHORT_NAME = this%short_name, &
              LONG_NAME = this%long_name, &
              UNITS = this%units, &
              DIMS = this%dims, &
              VLOCATION = this%location, &
              PRECISION = this%precision, &
              !STAGGERING = this%staggering, &
              !ROTATION = this%rotation, &
              UNGRIDDED_DIMS = this%ungridded_dims, &
              _RC)
      else
         _RETURN(_FAILURE)
      end if

   end subroutine AddNewSpec

end module VarspecDescriptionMod


module VarspecDescriptionVectorMod
   use VarspecDescriptionMod

#define _type type (VarspecDescription)
#define _vector VarspecDescriptionVector
#define _iterator VarspecDescriptionVectorIterator
#include "templates/vector.inc"

end module VarspecDescriptionVectorMod

