#include "MAPL_Generic.h"

module mapl_EsmfRegridder
   use mapl_RegridderParam
   use mapl_RegridderSpec
   use mapl_Regridder
   use mapl_RoutehandleParam
   use mapl_RoutehandleManager
   use mapl_DynamicMask
   use mapl_NullRegridder
   use mapl_ErrorHandlingMod
   use esmf
   implicit none
   private

   public :: EsmfRegridder
   public :: EsmfRegridderParam

   type, extends(RegridderParam) :: EsmfRegridderParam
      private
      type(RoutehandleParam) :: routehandle_param
      type(ESMF_Region_Flag) :: zeroregion
      type(ESMF_TermOrder_Flag) :: termorder
      logical :: checkflag
      type(DynamicMask), allocatable :: dyn_mask
   contains
      procedure :: equal_to
      procedure :: get_routehandle_param
   end type EsmfRegridderParam

   type, extends(Regridder) :: EsmfRegridder
      private
      type(ESMF_Routehandle) :: routehandle
      type(RegridderSpec) :: regridder_spec
   contains
      procedure :: regrid_scalar
   end type EsmfRegridder


   interface EsmfRegridderParam
      procedure :: new_EsmfRegridderParam_simple
      procedure :: new_EsmfRegridderParam
   end interface EsmfRegridderParam

   interface EsmfRegridder
      procedure :: new_EsmfRegridder
   end interface EsmfRegridder
   
contains

   function new_EsmfRegridderParam_simple(regridmethod, zeroregion, termorder, checkflag, dyn_mask) result(param)
      type(EsmfRegridderParam) :: param
      type(ESMF_RegridMethod_Flag), optional, intent(in) :: regridmethod
      type(ESMF_Region_Flag), optional, intent(in) :: zeroregion
      type(ESMF_TermOrder_Flag), optional, intent(in) :: termorder
      logical, optional, intent(in) :: checkflag
      type(DynamicMask), optional, intent(in) :: dyn_mask

      param%routehandle_param = RoutehandleParam(regridmethod=regridmethod)
      param = EsmfRegridderParam(RoutehandleParam(regridmethod=regridmethod), &
           zeroregion=zeroregion, termorder=termorder, checkflag=checkflag, dyn_mask=dyn_mask)
      
   end function new_EsmfRegridderParam_simple

   function new_EsmfRegridderParam(routehandle_param, zeroregion, termorder, checkflag, dyn_mask) result(param)
      type(EsmfRegridderParam) :: param
      type(RoutehandleParam), intent(in) :: routehandle_param
      type(ESMF_Region_Flag), optional, intent(in) :: zeroregion
      type(ESMF_TermOrder_Flag), optional, intent(in) :: termorder
      logical, optional, intent(in) :: checkflag
      type(DynamicMask), optional, intent(in) :: dyn_mask

      param%routehandle_param = routehandle_param

      param%zeroregion = ESMF_REGION_TOTAL
      if (present(zeroregion)) param%zeroregion = zeroregion

      if (present(dyn_mask)) then
         param%dyn_mask = dyn_mask
         param%termorder = ESMF_TERMORDER_SRCSEQ
      else
         param%termorder = ESMF_TERMORDER_FREE
      end if

      if (present(termorder)) param%termorder = termorder

      param%checkflag = .false.
      if (present(checkflag)) param%checkflag = checkflag
      
   end function new_EsmfRegridderParam


   function new_EsmfRegridder(routehandle, regridder_spec) result(regriddr)
      type(EsmfRegridder) :: regriddr
      type(ESMF_Routehandle), intent(in) :: routehandle
      type(RegridderSpec), intent(in) :: regridder_spec

      integer :: status

      regriddr%routehandle = routehandle
      regriddr%regridder_spec = regridder_spec

   end function new_EsmfRegridder

 
   subroutine regrid_scalar(this, f_in, f_out, rc)
      class(EsmfRegridder), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: f_in, f_out
      integer, optional, intent(out) :: rc

      integer :: status

      select type (q => this%regridder_spec%get_param())
      type is (EsmfRegridderParam)
         call regrid_scalar_safe(this%routehandle, q, f_in, f_out, rc)
      class default
         _FAIL('Invalid subclass of RegridderParam.')
      end select

      _RETURN(_SUCCESS)
   end subroutine regrid_scalar

   subroutine regrid_scalar_safe(routehandle, param, f_in, f_out, rc)
      type(ESMF_Routehandle), intent(inout) :: routehandle
      type(EsmfRegridderParam), intent(in) :: param
      type(ESMF_Field), intent(inout) :: f_in, f_out
      integer, optional, intent(out) :: rc
      
      integer :: status

      call ESMF_FieldRegrid(f_in, f_out, &
           routehandle=routehandle, &
           dynamicMask=param%dyn_mask%esmf_mask, &
           termorderflag=param%termorder, &
           zeroregion=param%zeroregion, &
           checkflag=param%checkflag, &
           _RC)

      _RETURN(_SUCCESS)
   end subroutine regrid_scalar_safe


   logical function equal_to(this, other)
      class(EsmfRegridderParam), intent(in) :: this
      class(RegridderParam), intent(in) :: other

      equal_to = .false.

      select type (q => other)
      type is (EsmfRegridderParam)
         if (.not. (this%routehandle_param ==  q%routehandle_param)) return
         if (.not. this%zeroregion == q%zeroregion) return
         if (.not. this%termorder == q%termorder) return
         if (this%checkflag .neqv. q%checkflag) return
         
         if (allocated(this%dyn_mask) .neqv. allocated(q%dyn_mask)) return
         if (this%dyn_mask /= q%dyn_mask) return
      class default
         return
      end select

      equal_to = .true.
   end function equal_to


   function get_routehandle_param(this) result(routehandle_param)
      class(EsmfRegridderParam), intent(in) :: this
      type(RoutehandleParam) :: routehandle_param

      routehandle_param = this%routehandle_param
   end function get_routehandle_param

end module mapl_EsmfRegridder
