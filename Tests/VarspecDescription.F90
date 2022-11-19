#define _SUCCESS      0
#define _FAILURE     1
#define _VERIFY(A)   if(  A/=0) then; if(present(rc)) rc=A; PRINT *, Iam, __LINE__; return; endif
#define _ASSERT(A)   if(.not.A) then; if(present(rc)) rc=_FAILURE; PRINT *, Iam, __LINE__; return; endif
#define _RETURN(A)   if(present(rc)) rc=A; return

module VarspecDescriptionMod
   use MAPL
   use ESMF
   use gFTL_StringVector
   use yaFYaml
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
   function new_VarspecDescriptionFromConfig(short_name,spec,rc) result(VarspecDescr)
      type(VarspecDescription) :: VarspecDescr
      character(len=*), intent(in) :: short_name
      class(YAML_Node), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=:), allocatable :: topology, levels, tempc

      VarspecDescr%short_name = short_name
      if (spec%has("LONG_NAME")) then
         tempc = spec%of("LONG_NAME")
         VarspecDescr%long_name = tempc
      else
         VarspecDescr%long_name = "unknown"
      end if
      if (spec%has("UNITS")) then
         tempc = spec%of("UNITS")
         VarspecDescr%units = tempc
      else
         VarspecDescr%units = "unknown"
      end if
      if (spec%has("TOPOLOGY")) then
         topology = spec%of("TOPOLOGY")
      else
         topology = "gridded"
      end if
      if (spec%has("LEVELS")) then
         levels = spec%of("LEVELS")
      else
         levels = "none"
      end if
      if (trim(topology)=="gridded") then
         if (levels == "none") then
            VarspecDescr%dims = MAPL_DimsHorzOnly
            VarspecDescr%location = MAPL_VLocationNone
         else if (levels == "center") then
            VarspecDescr%dims = MAPL_DimsHorzVert
            VarspecDescr%location = MAPL_VLocationCenter
         else if (levels == "edge") then
            VarspecDescr%dims = MAPL_DimsHorzVert
            VarspecDescr%location = MAPL_VLocationEdge
         end if
      end if 
      _RETURN(_SUCCESS)

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

