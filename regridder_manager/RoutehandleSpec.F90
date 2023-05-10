module mapl_RoutehandleSPec
   use esmf
   implicit none

   public :: RoutehandleSpec
   public :: operator(==)
   
   type :: RouteHandleSpec
      private
      ! Maybe grid id instead?
      integer :: grid_id_in
      integer :: grid_id_out

      ! Use allocatable attribute so that null() acts as non-present
      ! optional argument in new_ESMF_RouteHandle
      integer(kind=ESMF_KIND_I4),   allocatable :: srcMaskValues(:) => null()
      integer(kind=ESMF_KIND_I4),   allocatable  :: dstMaskValues(:) => null()
      type(ESMF_RegridMethod_Flag), allocatable :: regridmethod => null()
      type(ESMF_PoleMethod_Flag),   allocatable :: polemethod => null()
      integer, allocatable :: regridPoleNPnts
      type(ESMF_LineType_Flag), allocatable :: linetype
      type(ESMF_NormType_Flag), allocatable :: normtype
      type (ESMF_ExtrapMethod_Flag), allocatable :: extrapmethod
      integer, allocatable :: extrapNumSrcPnts
      real(kind=ESMF_KIND_R4), allocatable  :: extraDistExponent
      integer, allocatable :: extrapNumLevels
      type(ESMF_UnmappedAction_Flag), allocatable :: unmappedaction
      logical, allocatable :: ignoreDegenerate

      ! payload
      type(ESMF_RouteHandle) :: routehandle
   end type RouteHandleSpec

   interface operator(==)
      module procedure :: routehandle_spec_eq
   end interface operator(==)
   
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
      type(ESMF_Field) :: field_in
      type(ESMF_Field) :: field_out

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


   ! Ignore routehandle component itself.  
   logical function routehandle_spec_eq(a, b) result(eq)
      type(RouteHandleSpec), intent(in) :: a
      type(RouteHandleSpec), intent(in) :: b

      eq = a%grid_id_in == b%grid_id_in
      if (.not. eq) return
      eq = a%grid_id_out == b%grid_id_out
      if (.not. eq) return

      eq = same_mask_values(a%srcMaskValues, b%srcMaskValues)
      if (.not. eq) return
      eq = same_mask_values(a%dstMaskValues, b%dstMaskValues)
      if (.not. eq) return

      eq = same_regridmethod(a%regridmethod, b%regridmethod)
      if (.not. eq) return

      eq = same_polemethod(a%polemethod, b%polemethod)
      if (.not. eq) return

      eq = same_scalar_int(a%regridPoleNPnts, b%regridPoleNPnts)
      if (.not. eq) return

      eq = same_linetype(a%linetype, b%linetype)
      if (.not. eq) return

      eq = same_normtype(a%normtype, b%normtype)
      if (.not. eq) return

      eq = same_extrapmethod(a%extrapmethod, b%extrapmethod)
      if (.not. eq) return

      eq = same_scalar_int(a%extrapNumSrcPnts, b%extrapNumSrcPnts)
      if (.not. eq) return

      eq = same_scalar_real(a%extraDistExponent, b%extraDistExponent)
      if (.not. eq) return

      eq = same_scalar_int(a%extrapNumLevels, b%extrapNumLevels)
      if (.not. eq) return

      eq = same_unmappedaction(a%unmappedaction, b%unmappedaction)
      if (.not. eq) return

      eq = same_scalar_logical(a%ignoreDegenerate, b%ignoreDegenerate)
      if (.not. eq) return
      
   contains


      logical function same_mask_values(a, b) result(eq)
         integer, allocatable, intent(in) :: a(:)
         integer, allocatable, intent(in) :: b(:)

         eq = .false.
         if (allocated(a) .neqv. allocated(b)) return
         if (.not. allocated(a)) then ! trivial case
            eq = .true.
            return
         end if
         if (.not. (size(a) == size(b))) return
         eq = all(a == b)

      end function same_mask_values

      logical function same_regridmethod(a, b) result(eq)
         type(ESMF_RegridMethod_Flag), intent(in) :: a
         type(ESMF_RegridMethod_Flag), intent(in) :: b

         eq = .false.
         if (allocated(a) .neqv. allocated(b)) return
         eq = (a == b)

      end function same_regridmethod


      logical function same_polemethod(a, b) result(eq)
         type(ESMF_PoleMethod_Flag), intent(in) :: a
         type(ESMF_PoleMethod_Flag), intent(in) :: b

         eq = .false.
         if (allocated(a) .neqv. allocated(b)) return
         eq = (a == b)

      end function same_polemethod

      logical function same_scalar_int(a, b) result(eq)
         integer, intent(in) :: a
         integer, intent(in) :: b

         eq = .false.
         if (allocated(a) .neqv. allocated(b)) return
         eq = (a == b)

      end function same_scalar_int

      logical function same_linetype(a, b) result(eq)
         type(ESMF_LineType_Flag), intent(in) :: a
         type(ESMF_LineType_Flag), intent(in) :: b

         eq = .false.
         if (allocated(a) .neqv. allocated(b)) return
         eq = (a == b)

      end function same_linetype

      logical function same_normtype(a, b) result(eq)
         type(ESMF_NormType_Flag), intent(in) :: a
         type(ESMF_NormType_Flag), intent(in) :: b

         eq = .false.
         if (allocated(a) .neqv. allocated(b)) return
         eq = (a == b)

      end function same_normtype

      logical function same_extrapmethod(a, b) result(eq)
         type(ESMF_ExtrapMethod_Flag), intent(in) :: a
         type(ESMF_ExtrapMethod_Flag), intent(in) :: b

         eq = .false.
         if (allocated(a) .neqv. allocated(b)) return
         eq = (a == b)

      end function same_extrapmethod

      logical function same_scalar_real(a, b) result(eq)
         real, intent(in) :: a
         real, intent(in) :: b

         eq = .false.
         if (allocated(a) .neqv. allocated(b)) return
         eq = (a == b)

      end function same_scalar_real


      logical function same_unmappedaction(a, b) result(eq)
         type(ESMF_UnmappedAction_Flag), intent(in) :: a
         type(ESMF_UnmappedAction_Flag), intent(in) :: b

         eq = .false.
         if (allocated(a) .neqv. allocated(b)) return
         eq = (a == b)

      end function same_unmappedaction


      logical function same_scalar_logical(a, b) result(eq)
         logical, intent(in) :: a
         logical, intent(in) :: b

         eq = .false.
         if (allocated(a) .neqv. allocated(b)) return
         eq = (a == b)

      end function same_scalar_logical
      
   end function routehandle_spec_eq

end module mapl_RouteHandleManager
