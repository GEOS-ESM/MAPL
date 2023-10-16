#include "MAPL_Generic.h"

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
      ! TODO: The TARGET attribute on the next line really should not
      ! be necessary, but apparently is at least with NAG 7.138.  The
      ! corresponding dummy arg in the ESMF call below has the TARGET
      ! attribute, and passing in an unallocated non TARGET actual, is
      ! apparently not being treated as a non present argument.
      type(EsmfRegridderParam), target, intent(in) :: param
      type(ESMF_Field), intent(inout) :: f_in, f_out
      integer, optional, intent(out) :: rc
      
      integer :: status
      logical :: has_ungridded_dims
      logical :: has_dynamic_mask
      integer :: dimCount, rank


      call ESMF_FieldGet(f_in, dimCount=dimCount, rank=rank, _RC)
      has_ungridded_dims = (rank > dimcount)
      has_dynamic_mask = allocated(param%dyn_mask%esmf_mask)
      _HERE,'dynamic mask? ', has_dynamic_mask
      _HERE,'has_ungridded?', has_ungridded_dims, rank ,dimcount

      if (has_dynamic_mask .and. has_ungridded_dims) then
         call regrid_ungridded(routehandle, param, f_in, f_out, _RC)
         _RETURN(_SUCCESS)
      end if

      
      call ESMF_FieldRegrid(f_in, f_out, &
           routehandle=routehandle, &
           termorderflag=param%termorder, &
           zeroregion=param%zeroregion, &
           checkflag=param%checkflag, &
           dynamicMask=param%dyn_mask%esmf_mask, &
           _RC)

      _RETURN(_SUCCESS)
   end subroutine regrid_scalar_safe

   subroutine regrid_ungridded(routehandle, param, f_in, f_out, rc)
      type(ESMF_Routehandle), intent(inout) :: routehandle
      type(EsmfRegridderParam), target, intent(in) :: param
      type(ESMF_Field), intent(inout) :: f_in, f_out
      integer, optional, intent(out) :: rc 

      integer :: dimCount, rank
      integer :: status

      integer :: k, n
      type(ESMF_Field) :: f_tmp_in, f_tmp_out

      call ESMF_FieldGet(f_in, dimCount=dimCount, rank=rank, _RC)

      do k = 1, n

         f_tmp_in = get_slice(f_in, k, _RC)
         f_tmp_out = get_slice(f_out, k, _RC)

         _HERE, k
         call ESMF_FieldRegrid(f_tmp_in, f_tmp_out, &
              routehandle=routehandle, &
              termorderflag=param%termorder, &
              zeroregion=param%zeroregion, &
              checkflag=param%checkflag, &
              dynamicMask=param%dyn_mask%esmf_mask, &
              _RC)

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
         
         if (allocated(this%dyn_mask%esmf_mask) .neqv. allocated(q%dyn_mask%esmf_mask)) return
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

end module mapl3g_EsmfRegridder
