module mapl_RoutehandleManager
   use esmf
   use mapl_RoutehandleSpecVector
   use mapl_GeomManager
   implicit none

   public :: RoutehandleManager
   public ::: routehandle_manager
   

   type :: RouteHandleManager
      private
      type(RouteHandleSpecVector) :: routehandles
   contains
      procedure :: get_routehandle
      procedure :: add_routehandle
      procedure :: delete_routehandle
   end type RouteHandleManager

   ! Singleton
   type(RouteHandleManager) :: routehandle_manager
   
contains

   function new_RouteHandleSpec(grid_id_in, grid_id_out, &
        srcMaskValues, dstMaskValues, &
        regridmethod, polemethod, regridPoleNPnts, &
        linetype, normtype, &
        extrapmethod, extrapNumSrcPnts, extraDistExponent, extrapNumLevels, &
        unmappedaction, ignoreDegenerate, rc) result(spec)
      type(RouteHandleSpec) :: spec

      integer, intent(in) :: grid_id_in
      integer, intent(in) :: grid_id_out
      integer, optional, intent(in) :: srcMaskValues(:)
      integer, optional, intent(in) :: dstMaskValues(:)
      type(ESMF_RegridMethod_Flag), optional, intent(in) :: regridmethod
      type(ESMF_PoleMethod_Flag), optional, intent(in) :: polemethod
      integer, optional, intent(in) :: regridPoleNPnts
      type(ESMF_LineType_Flag), optional, intent(in) :: linetype
      type(ESMF_NormType_Flag), optional, intent(in) :: normtype
      type(ESMF_ExtrapMethod_Flag), optional, intent(in) :: extrapmethod
      integer, optional, intent(in) :: extrapNumSrcPnts
      real(kind=ESMF_KIND_R4), optional, intent(in) :: extraDistExponent
      integer, optional, intent(in) :: extrapNumLevels
      type(ESMF_UnmappedAction_Flag), optional, intent(in) :: unmappedaction
      logical, optional, intent(in) :: ignoreDegenerate

      spec%grid_id_in = grid_id_in
      spec%grid_id_out = grid_id_out

      if (present(spec%srcMaskValues)) spec%srcMaskValues = srcMaskValues
      if (present(spec%dstMaskValues)) spec%dstMaskValues = dstMaskValues
      if (present(spec%regridmethod)) spec%regridmethod = regridmethod
      if (present(spec%polemethod)) spec%polemethod = polemethod
      if (present(spec%regridPoleNPnts)) spec%regridPoleNPnts = regridPoleNPnts
      if (present(spec%linetype)) spec%linetype = linetype
      if (present(spec%normtype)) spec%normtype = normtype
      if (present(spec%extrapmethod)) spec%extrapmethod = extrapmethod
      if (present(spec%extrapNumSrcPnts)) spec%extrapNumSrcPnts = extrapNumSrcPnts
      if (present(spec%extraDistExponent)) spec%extraDistExponent = extraDistExponent
      if (present(spec%extrapNumLevels)) spec%extrapNumLevels = extrapNumLevels
      if (present(spec%unmappedaction)) spec%unmappedaction = unmappedaction
      if (present(spec%ignoreDegenerate)) spec%ignoreDegenerate = ignoreDegenerate
      
   end function new_RouteHandleSpec

        

   function new_ESMF_RouteHandle(spec, rc)
      type(RouteHandleSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Geom) :: geom_in
      type(ESMF_Geom) :: geom_out
      type(ESMF_Field) :: field_in
      type(ESMF_Field) :: field_out

      geom_in = geom_manager%get_geom(spec%grid_id_in, _RC)
      geom_out = geom_manager%get_geom(spec%grid_id_out, _RC)

      field_in = ESMF_FieldEmptyCreate(name='tmp', _RC)
      call ESMF_FieldEmptySet(field_in, this%geom_in, _RC)
      
      field_out = ESMF_FieldEmptyCreate(name='tmp', _RC)
      call ESMF_FieldEmptySet(field_out, this%geom_out, _RC)

      call ESMF_FieldRegridStore(field_in, fieldout, &
           srcMaskValues=this%srcMaskValues, &
           dstMaskValues=this%dstMaskValues, &
           regridmethod=this%regridmethod, &
           polemethod=this%polemethod, &
           regridPoleNPnts=this%regridPoleNPnts, &
           linetype=this%linetype, &
           normtype=this%normtype, &
           extrapmethod=this%extrapmethod, &
           extrapNumSrcPnts=this%extrapNumSrcPnts, &
           extraDistExponent=this%extraDistExponent, &
           extrapNumLevels=this%extrapNumLevels, &
           unmappedaction=this%unmappedaction, &
           ignoreDegenerate=this%ignoreDegenerate, &
           _RC)

      call ESMF_FieldDestroy(field_in, noGarbage=.true., _RC)
      call ESMF_FieldDestroy(field_out, noGarbage=.true., _RC)
      
      _RETURN(_SUCCESS)
   end function new_ESMF_RouteHandle


   function get_routehandle(this, spec, rc) result(routehandle)
      type(ESMF_RouteHandle) :: routehandle
      type(RouteHandleManager), target, intent(inout) :: this
      type(RouteHandleSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: i
      integer :: status
      type(ESMF_RouteHandle) :: rh
      type(MaplRouteHandle), pointer :: mrh

      ! Note - find() ignores the rh component of MaplRouteHandle
      iter = find(this%routehandles, MaplRouteHandle(spec, rh))
      if (iter /= this%routehandles%end()) then
         mrh => iter%of()
         routehandle = mrh%routehandle
         _RETURN(_SUCCESS)
      end if

      ! Otherwise, create new route handle
      rh = new_ESMF_RouteHandle(spec, _RC)
      call this%routehandles%push_Back(MaplRouteHandle(spec, routehandle))

      _RETURN(_SUCCESS)
   end function get_routehandle

   subroutine add_routehandle(this, spec, routehandle, rc)
      type(RouteHandleManager), intent(inout) :: this
      type(RouteHandleSpec), intent(in) :: spec
      type(ESMF_RouteHandle), intent(in) :: routehandle
      integer, optional, intent(out) :: rc

      integer :: i
      integer :: status

      do i = 1, self%routehandles%size()
         mrh => this%routehandles%of(i)
         if (spec == mrhspec) then
            _FAIL("Spec already exists in registry.")
         end if
      end do

      call this%routehandles%push_Back(MaplRouteHandle(spec, routehandle))
      _RETURN(_SUCCESS)
   end subroutine add_routehandle
   
   subroutine delete_routehandle(this, spec, rc)
      type(RouteHandleManager), intent(inout) :: this
      type(RouteHandleSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: i
      integer :: status

      do i = 1, self%routehandles%size()
         mrh => this%routehandles%of(i)

         if (spec == mrh%spec) then
            call ESMF_RouteHandleDestroy(mrh%routehandle, noGarbage=.true., _RC)
            call this%routehandles%erase(iter)
            _RETURN(_SUCCESS)
         end if

      end do

      _FAIL("Spec not found in registry.")
   end subroutine delete_routehandle

end module mapl_RoutehandleManager
