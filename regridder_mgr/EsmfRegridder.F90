#include "MAPL.h"

module mapl3g_EsmfRegridder
   use mapl3g_RegridderParam
   use mapl3g_RegridderSpec
   use mapl3g_Regridder
   use mapl3g_RoutehandleParam
   use mapl3g_RoutehandleManager
   use mapl3g_DynamicMask
   use mapl3g_NullRegridder
   use mapl_ErrorHandlingMod
   use esmf
   implicit none
   private

   public :: EsmfRegridder
   public :: EsmfRegridderParam
   public :: make_EsmfRegridderParam

   type, extends(RegridderParam) :: EsmfRegridderParam
      private
      type(RoutehandleParam) :: routehandle_param
      type(ESMF_Region_Flag) :: zeroregion
      type(ESMF_TermOrder_Flag) :: termorder
      logical :: checkflag
      type(DynamicMask) :: dyn_mask
   contains
      procedure :: equal_to
      procedure :: get_routehandle_param
      procedure :: make_info
   end type EsmfRegridderParam

   type, extends(Regridder) :: EsmfRegridder
      private
      type(EsmfRegridderParam) :: regridder_param
      type(ESMF_Routehandle) :: routehandle
   contains
      procedure :: regrid_field
   end type EsmfRegridder


   interface EsmfRegridderParam
      procedure :: new_EsmfRegridderParam_simple
      procedure :: new_EsmfRegridderParam
   end interface EsmfRegridderParam

   interface EsmfRegridder
      procedure :: new_EsmfRegridder
   end interface EsmfRegridder

   interface make_EsmfRegridderParam
      procedure make_regridder_param_from_info
   end interface make_EsmfRegridderParam

   character(*), parameter :: KEY_ROUTEHANDLE = 'EsmfRouteHandle'
   
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
           zeroregion=zeroregion, termorder=termorder, checkflag=checkflag, &
           dyn_mask=dyn_mask)
      
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

   function new_EsmfRegridder(regridder_param, routehandle) result(regriddr)
      type(EsmfRegridder) :: regriddr
      type(EsmfRegridderParam), intent(in) :: regridder_param
      type(ESMF_Routehandle), intent(in) :: routehandle

      integer :: status

      regriddr%regridder_param = regridder_param
      regriddr%routehandle = routehandle

   end function new_EsmfRegridder

   subroutine regrid_field(this, f_in, f_out, rc)
      class(EsmfRegridder), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: f_in, f_out
      integer, optional, intent(out) :: rc
      
      integer :: status
      logical :: has_ungridded_dims
      logical :: has_dynamic_mask
      integer :: ub(ESMF_MAXDIM)
      type(ESMF_TypeKind_Flag) :: typekind
      type(ESMF_DynamicMask), allocatable :: mask

      call ESMF_FieldGet(f_in, ungriddedUBound=ub, typekind=typekind, _RC)
      has_ungridded_dims = any(ub > 1)

        associate(param => this%regridder_param)
        if (typekind == ESMF_TYPEKIND_R4) then
           has_dynamic_mask = allocated(param%dyn_mask%esmf_mask_r4)
           if (has_dynamic_mask) mask = param%dyn_mask%esmf_mask_r4
        elseif (typekind == ESMF_TYPEKIND_R8) then
           has_dynamic_mask = allocated(param%dyn_mask%esmf_mask_r8)
           if (has_dynamic_mask) mask = param%dyn_mask%esmf_mask_r8
        end if
        
        if (has_dynamic_mask .and. has_ungridded_dims) then
           call regrid_ungridded(this, mask, f_in, f_out, n=product(max(ub,1)), _RC)
           _RETURN(_SUCCESS)
        end if

        ! Otherwise
        call ESMF_FieldRegrid(f_in, f_out, &
             routehandle=this%routehandle, &
             termorderflag=param%termorder, &
             zeroregion=param%zeroregion, &
             checkflag=param%checkflag, &
             dynamicMask=mask, &
             _RC)
      end associate
      _RETURN(_SUCCESS)
   end subroutine regrid_field

   subroutine regrid_ungridded(this, mask, f_in, f_out, n, rc)
      class(EsmfRegridder), intent(inout) :: this
      type(ESMF_DynamicMask), intent(in) :: mask
      type(ESMF_Field), intent(inout) :: f_in, f_out
      integer, intent(in) :: n
      integer, optional, intent(out) :: rc 

      integer :: status
      integer :: k
      type(ESMF_Field) :: f_tmp_in, f_tmp_out

      do k = 1, n

         f_tmp_in = get_slice(f_in, k, _RC)
         f_tmp_out = get_slice(f_out, k, _RC)

         ! Can only call this if esmf_mask is allocated.
         associate (param => this%regridder_param)
           call ESMF_FieldRegrid(f_tmp_in, f_tmp_out, &
                routehandle=this%routehandle, &
                termorderflag=param%termorder, &
                zeroregion=param%zeroregion, &
                checkflag=param%checkflag, &
                dynamicMask=mask, &
                _RC)
         end associate

         call ESMF_FieldDestroy(f_tmp_in, nogarbage=.true., _RC)
         call ESMF_FieldDestroy(f_tmp_out, nogarbage=.true.,  _RC)
         
      end do
      
      _RETURN(_SUCCESS)

   contains

      function get_slice(f, k, rc) result(f_slice)
         type(ESMF_Field) :: f_slice
         type(ESMF_Field), intent(inout) :: f
         integer, intent(in) :: k
         integer, optional, intent(out) :: rc

         integer :: status
         real(kind=ESMF_KIND_R4), pointer :: x(:,:,:)
         real(kind=ESMF_KIND_R4), pointer :: x_slice(:,:)
         type(ESMF_Geom) :: geom
         type(ESMF_GeomType_Flag) :: geomtype
         type(ESMF_Grid) :: grid
         type(ESMF_Mesh) :: mesh
         type(ESMF_XGrid) :: xgrid
         type(ESMF_LocStream) :: locstream

         call ESMF_FieldGet(f, farrayptr=x, _RC)
         call ESMF_FieldGet(f, geomtype=geomtype, _RC)

         if (geomtype == ESMF_GEOMTYPE_GRID) then
            call ESMF_FieldGet(f, grid=grid, _RC)
            geom = ESMF_GeomCreate(grid, _RC)
         elseif (geomtype == ESMF_GEOMTYPE_MESH) then
            call ESMF_FieldGet(f, mesh=mesh, _RC)
            geom = ESMF_GeomCreate(mesh, _RC)
         elseif (geomtype == ESMF_GEOMTYPE_XGRID) then
            call ESMF_FieldGet(f, xgrid=xgrid, _RC)
            geom = ESMF_GeomCreate(xgrid, _RC)
         elseif (geomtype == ESMF_GEOMTYPE_LOCSTREAM) then
            call ESMF_FieldGet(f, locstream=locstream, _RC)
            geom = ESMF_GeomCreate(locstream, _RC)
         else
            _FAIL('Invalid geometry type.')
         end if

         x_slice => x(:,:,k)
         f_slice = ESMF_FieldCreate(geom, &
              datacopyflag=ESMF_DATACOPY_REFERENCE, &
              farrayptr=x_slice, _RC)

         call ESMF_GeomDestroy(geom, _RC)
         
         _RETURN(_SUCCESS)
      end function get_slice

   end subroutine regrid_ungridded

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

   function make_info(this, rc) result(info)
      type(esmf_Info) :: info
      class(EsmfRegridderParam), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_Info) :: rh_info

      info = esmf_InfoCreate(_RC)
      rh_info = this%routehandle_param%make_info(_RC)
      call esmf_InfoSet(info, key=KEY_ROUTEHANDLE, value=rh_info, _RC)
      call esmf_InfoDestroy(rh_info, _RC)

      call esmf_InfoPrint(info, _RC)

      _RETURN(_SUCCESS)
   end function make_info

   function make_regridder_param_from_info(info, rc) result(regridder_param)
      type(EsmfRegridderParam) :: regridder_param
      type(esmf_Info), intent(in) :: info
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_Info) :: rh_info
      type(RouteHandleParam) :: rh_param

      rh_info = esmf_InfoCreate(info, key=KEY_ROUTEHANDLE, _RC)
      rh_param = make_RouteHandleParam(rh_info, _RC)
      regridder_param = EsmfRegridderParam(rh_param)

      call esmf_InfoDestroy(rh_info, _RC)

      _RETURN(_SUCCESS)
   end function make_regridder_param_from_info

end module mapl3g_EsmfRegridder
