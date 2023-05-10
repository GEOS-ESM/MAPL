module mapl3_EsmfRegridder
   use mapl_MaplRegridder
   implicit none
   private

   public :: EsmfRegridder
   public :: EsmfRegridderSpec

   type :: EsmfRegridderSpec
      private
      type(RouteHandleSpec) :: rh_spec
      type(ESMF_DynamicMask) :: dynamic_mask
      logical :: transpose
   end type EsmfRegridderSpec

   type, extends(MaplRegridder) :: EsmfRegridder
      private
      type(ESMF_Geom) :: geom_in, geom_out
      type(ESMF_RegridMethod_Flag) :: method

      ! deferred initialization
      type(ESMF_RouteHandle) :: routeHandle
      type(ESMF_DynamicMask) :: dynamic_mask
   contains
      procedure :: regrid_scalar
      procedure :: update

      ! Helper methods
      procedure :: set_dynamic_mask
   end type EsmfRegridder

   generic :: EsmfRegridder => new_EsmfRegridder
   generic :: EsmfRegridder => new_EsmfRegridder_from_spec
   
contains

   function new_EsmfRegridder(geom_in, geom_out, method, transpose) result(regridder)
      type(EsmfRegridder) :: regridder
      type(ESMF_Geom), intent(in) :: geom_in, geom_out
      type(ESMF_RegridMethod_Flag), intent(in) :: method

      rh_spec = ...
      call routehandle_manager%get_routeHandle(rh_spec, _RC)

      regridder%geom_in = geom_in
      regridder%geom_in = geom_out
      regridder%method = method

   end function new_EsmfRegridder

   ! Set route hadle, dynamic mask, etc.
   subroutine initialize(this, rc)

      call ESMF_FieldRegridStore(...)
      call this%set_dynamic_mask(_RC)
      
      _RETURN(_SUCCESS)
   end subroutine initialize

   subroutine regrid_scalar(this, f_in, f_out, rc)

      _ASSERT(FieldsAreConformable(x,y), ...)


      call this%update_geoms(f_in, f_out, _RC)
      

      call ESMF_FieldRegrid(src_field=f_in, dst_field=f_out, &
           routeHandle=this%routeHandle, &
           ... &
           )
      
   end subroutine regrid_scalar


   subroutine update_geoms(this, f_in, f_out, rc)

      call ESMF_FieldGet(f_in, geom=geom_in, _RC)
      call ESMF_FieldGet(f_out, geom=geom_out, _RC)

      new_id_in = MAPL_GetId(geom_in, _RC)
      new_id_out = MAPL_GetId(geom_out, _RC)

      _RETURN_IF( (new_id_in == this%id_in) .and. (new_id_out /= this%id_out ) )

      this%id_in = new_id_in
      this%id_out = new_id_out

      call ESMF_RouteHandleDestroy(this%route_handle, _RC)
      call this%create_route_handle(_RC)

      _RETURN(_SUCCESS)
   end subroutine update_geoms

end module mapl3_EsmfRegridder
