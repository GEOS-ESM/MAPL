#include "MAPL.h"

module mapl3g_RoutehandleParam
   use esmf
   use mapl3g_Geom_API, only: MaplGeom, geom_manager, MAPL_SameGeom
   use mapl_ErrorHandlingMod
   implicit none
   private

   public :: RoutehandleParam
   public :: make_RouteHandle
   public :: make_RouteHandleParam
   public :: operator(==)

   ! If an argument to FieldRegridStore is optional _and_ has no default
   ! value, then we use the ALLOCATABLE attribute.  This allows us to
   ! treate the optional argument as not present in the call.
   type :: RoutehandleParam
      private

      ! Use allocatable attribute so that null() acts as non-present
      ! optional argument in new_ESMF_Routehandle
      integer(kind=ESMF_KIND_I4),   allocatable :: srcMaskValues(:)
      integer(kind=ESMF_KIND_I4),   allocatable  :: dstMaskValues(:)
      type(ESMF_RegridMethod_Flag) :: regridMethod
      type(ESMF_PoleMethod_Flag) :: polemethod
      integer, allocatable :: regridPoleNPnts
      type(ESMF_LineType_Flag) :: linetype
      type(ESMF_NormType_Flag) :: normtype
      type (ESMF_ExtrapMethod_Flag) :: extrapmethod
      integer :: extrapNumSrcPnts
      real(kind=ESMF_KIND_R4)  :: extrapDistExponent
      integer, allocatable :: extrapNumLevels
      type(ESMF_UnmappedAction_Flag) :: unmappedaction
      logical :: ignoreDegenerate
!#      integer :: srcTermProcessing
   contains
      procedure :: make_info
   end type RoutehandleParam

   interface make_RouteHandleParam
      procedure :: make_rh_param_from_info
   end interface make_RouteHandleParam

   interface make_RouteHandle
      procedure :: make_routehandle_from_param
   end interface make_RouteHandle

   interface operator(==)
      procedure :: equal_to
   end interface operator(==)
   
   type(ESMF_RegridMethod_Flag), parameter :: &
        CONSERVATIVE_METHODS(*) = [ESMF_REGRIDMETHOD_CONSERVE, ESMF_REGRIDMETHOD_CONSERVE_2ND]
   type(ESMF_RegridMethod_Flag), parameter :: &
        NONCONSERVATIVE_METHODS(*) = [ESMF_REGRIDMETHOD_BILINEAR, ESMF_REGRIDMETHOD_PATCH, ESMF_REGRIDMETHOD_NEAREST_STOD]

   interface RouteHandleParam
      procedure :: new_RoutehandleParam
   end interface RouteHandleParam

   character(*), parameter :: BILINEAR = 'bilinear'
   character(*), parameter :: CONSERVE = 'conserve'
   character(*), parameter :: KEY_REGRID_METHOD = 'regrid_method'
   

contains

   function new_RoutehandleParam( &
        srcMaskValues, dstMaskValues, &
        regridmethod, polemethod, regridPoleNPnts, &
        linetype, normtype, &
        extrapmethod, extrapNumSrcPnts, extrapDistExponent, extrapNumLevels, &
        unmappedaction, ignoreDegenerate, srcTermProcessing) result(param)
      type(RoutehandleParam) :: param

      integer, optional, intent(in) :: srcMaskValues(:)
      integer, optional, intent(in) :: dstMaskValues(:)
      type(ESMF_RegridMethod_Flag), optional, intent(in) :: regridmethod
      type(ESMF_PoleMethod_Flag), optional, intent(in) :: polemethod
      integer, optional, intent(in) :: regridPoleNPnts
      type(ESMF_LineType_Flag), optional, intent(in) :: linetype
      type(ESMF_NormType_Flag), optional, intent(in) :: normtype
      type(ESMF_ExtrapMethod_Flag), optional, intent(in) :: extrapmethod
      integer, optional, intent(in) :: extrapNumSrcPnts
      real(kind=ESMF_KIND_R4), optional, intent(in) :: extrapDistExponent
      integer, optional, intent(in) :: extrapNumLevels
      type(ESMF_UnmappedAction_Flag), optional, intent(in) :: unmappedaction
      logical, optional, intent(in) :: ignoreDegenerate
      integer, optional, intent(in) :: srcTermProcessing

      if (present(srcMaskValues)) param%srcMaskValues = srcMaskValues
      if (present(dstMaskValues)) param%dstMaskValues = dstMaskValues

      ! Simple ESMF defaults listed here. 
      param%regridmethod = ESMF_REGRIDMETHOD_BILINEAR
      param%normtype = ESMF_NORMTYPE_DSTAREA
      param%extrapmethod = ESMF_EXTRAPMETHOD_NONE
      param%extrapNumSrcPnts = 8
      param%extrapDistExponent = 2.0
      param%unmappedaction = ESMF_UNMAPPEDACTION_ERROR
      param%ignoreDegenerate = .false.

      if (present(regridmethod)) param%regridmethod = regridmethod

      ! Contingent ESMF defaults
      param%polemethod = get_default_polemethod(param%regridmethod)
      param%linetype = get_default_linetype(param%regridmethod)

      if (present(polemethod)) param%polemethod = polemethod
      if (present(regridPoleNPnts)) param%regridPoleNPnts = regridPoleNPnts
      if (present(linetype)) param%linetype = linetype
      if (present(normtype)) param%normtype = normtype
      if (present(extrapmethod)) param%extrapmethod = extrapmethod
      if (present(extrapNumSrcPnts)) param%extrapNumSrcPnts = extrapNumSrcPnts
      if (present(extrapDistExponent)) param%extrapDistExponent = extrapDistExponent
      if (present(extrapNumLevels)) param%extrapNumLevels = extrapNumLevels
      if (present(unmappedaction)) param%unmappedaction = unmappedaction
      if (present(ignoreDegenerate)) param%ignoreDegenerate = ignoreDegenerate
!#      if (present(srcTermProcessing)) param%srcTermProcessing = srcTermProcessing

   contains

      function get_default_polemethod(regridmethod) result(polemethod)
         type(ESMF_PoleMethod_Flag) :: polemethod
         type(ESMF_RegridMethod_Flag), intent(in) :: regridmethod
         integer :: i

         if (any([(regridmethod == CONSERVATIVE_METHODS(i), i=1, size(CONSERVATIVE_METHODS))])) then
            polemethod = ESMF_POLEMETHOD_NONE
         else
            polemethod = ESMF_POLEMETHOD_ALLAVG
         end if
            
      end function get_default_polemethod


      function get_default_linetype(regridmethod) result(linetype)
         type(ESMF_LineType_Flag) :: linetype
         type(ESMF_RegridMethod_Flag), intent(in) :: regridmethod
         integer :: i

         if (any([(regridmethod == CONSERVATIVE_METHODS(i), i= 1, size(CONSERVATIVE_METHODS))])) then
            linetype = ESMF_LINETYPE_GREAT_CIRCLE
         else
            linetype = ESMF_LINETYPE_CART
         end if
            
      end function get_default_linetype

      

   end function new_RoutehandleParam

   function make_routehandle_from_param(geom_in, geom_out, param, rc) result(routehandle)
      type(ESMF_Routehandle) :: routehandle
      type(ESMF_Geom), intent(in) :: geom_in
      type(ESMF_Geom), intent(in) :: geom_out
      type(RoutehandleParam), intent(in) :: param
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: field_in
      type(ESMF_Field) :: field_out

      integer :: srcTermProcessing=0

      field_in = ESMF_FieldEmptyCreate(name='tmp', _RC)
      call ESMF_FieldEmptySet(field_in, geom_in, _RC)
      call ESMF_FieldEmptyComplete(field_in, typekind=ESMF_TypeKind_R4, _RC)
      
      field_out = ESMF_FieldEmptyCreate(name='tmp', _RC)
      call ESMF_FieldEmptySet(field_out, geom_out, _RC)
      call ESMF_FieldEmptyComplete(field_out, typekind=ESMF_TypeKind_R4, _RC)

      call ESMF_FieldRegridStore(field_in, field_out, &
           srcMaskValues=param%srcMaskValues, &
           dstMaskValues=param%dstMaskValues, &
           regridmethod=param%regridmethod, &
           polemethod=param%polemethod, &
           regridPoleNPnts=param%regridPoleNPnts, &
           linetype=param%linetype, &
           normtype=param%normtype, &
           extrapmethod=param%extrapmethod, &
           extrapNumSrcPnts=param%extrapNumSrcPnts, &
           extrapDistExponent=param%extrapDistExponent, &
           extrapNumLevels=param%extrapNumLevels, &
           unmappedaction=param%unmappedaction, &
           ignoreDegenerate=param%ignoreDegenerate, &
           srcTermProcessing=srcTermProcessing, &
           routehandle=routehandle, &
           _RC)

      call ESMF_FieldDestroy(field_in, noGarbage=.true., _RC)
      call ESMF_FieldDestroy(field_out, noGarbage=.true., _RC)

      _RETURN(_SUCCESS)
   end function make_routehandle_from_param


   ! Ignore routehandle component itself.  
   logical function equal_to(a, b) result(eq)
      type(RoutehandleParam), intent(in) :: a
      type(RoutehandleParam), intent(in) :: b

      eq = same_mask_values(a%srcMaskValues, b%srcMaskValues)
      if (.not. eq) return

      eq = same_mask_values(a%dstMaskValues, b%dstMaskValues)
      if (.not. eq) return

      eq = a%regridmethod == b%regridmethod
      if (.not. eq) return

      eq = a%polemethod == b%polemethod
      if (.not. eq) return

      eq = same_scalar_int(a%regridPoleNPnts, b%regridPoleNPnts)
      if (.not. eq) return

      eq = a%linetype == b%linetype
      if (.not. eq) return

      eq = a%normtype == b%normtype
      if (.not. eq) return

      eq = a%extrapmethod == b%extrapmethod
      if (.not. eq) return

      eq = a%extrapNumSrcPnts == b%extrapNumSrcPnts
      if (.not. eq) return

      eq = a%extrapDistExponent == b%extrapDistExponent
      if (.not. eq) return

      eq = same_scalar_int(a%extrapNumLevels, b%extrapNumLevels)
      if (.not. eq) return

      eq = a%unmappedaction == b%unmappedaction
      if (.not. eq) return

      eq = a%ignoreDegenerate .eqv. b%ignoreDegenerate
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


      logical function same_scalar_int(a, b) result(eq)
         integer, allocatable, intent(in) :: a
         integer, allocatable, intent(in) :: b

         eq = .false.
         if (allocated(a) .neqv. allocated(b)) return

         eq = .true.
         if (.not. allocated(a)) return

         eq = (a == b)

      end function same_scalar_int

   end function equal_to

   function make_rh_param_from_info(info, rc) result(rh_param)
      type(RouteHandleParam) :: rh_param
      type(esmf_Info), intent(in) :: info
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: regrid_method_str
      type(esmf_RegridMethod_Flag), allocatable :: regrid_method
      logical :: is_present

      is_present = esmf_InfoIsPresent(info, key=KEY_REGRID_METHOD, _RC)
      if (is_present) then
         call esmf_InfoGetCharAlloc(info, key=KEY_REGRID_METHOD, value=regrid_method_str, _RC)
         select case(regrid_method_str)
         case(BILINEAR)
            regrid_method = ESMF_REGRIDMETHOD_BILINEAR
         case (CONSERVE)
            regrid_method = ESMF_REGRIDMETHOD_CONSERVE
         case default
            _FAIL('unsupported regrid method:: ' // regrid_method_str)
         end select
      end if

      rh_param = RouteHandleParam(regridMethod=regrid_method)

      _RETURN(_SUCCESS)
   end function make_rh_param_from_info

   function make_info(this, rc) result(info)
      type(esmf_Info) :: info
      class(RouteHandleParam), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: regrid_method_str
      type(esmf_RegridMethod_Flag), allocatable :: regrid_method
      logical :: is_present

      if (this%regridMethod == ESMF_REGRIDMETHOD_BILINEAR) then
         regrid_method_str = BILINEAR
      else if (this%regridMethod == ESMF_REGRIDMETHOD_CONSERVE) then
         regrid_method_str = CONSERVE
      else
         _FAIL('unsupported esmf regrid method')
      end if

      _HERE
      info = esmf_InfoCreate(_RC)
      call esmf_InfoSet(info, key=KEY_REGRID_METHOD, value=regrid_method_str, _RC)
      _HERE
      

      _RETURN(_SUCCESS)
   end function make_info

end module mapl3g_RoutehandleParam
