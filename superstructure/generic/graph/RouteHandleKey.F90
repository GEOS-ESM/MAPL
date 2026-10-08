#include "MAPL.h"

module mapl_RouteHandleKey_mod
   use esmf
   use mapl_NodeId_mod, only: NodeId
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: RouteHandleKey
   public :: operator(==)

   type :: RouteHandleKey
      private
      type(NodeId) :: source_geometry
      type(NodeId) :: destination_geometry
      type(ESMF_RegridMethod_Flag) :: regridmethod
      integer, allocatable :: srcMaskValues(:), dstMaskValues(:)
      type(ESMF_ExtrapMethod_Flag) :: extrapmethod
      integer :: extrapNumSrcPnts
      real(ESMF_KIND_R4) :: extrapDistExponent
      integer, allocatable :: extrapNumLevels
      type(ESMF_NormType_Flag) :: normtype
      type(ESMF_PoleMethod_Flag) :: polemethod
      integer, allocatable :: regridPoleNPnts
      type(ESMF_LineType_Flag) :: linetype
      type(ESMF_UnmappedAction_Flag) :: unmappedaction
      logical :: ignoreDegenerate
   contains
      procedure :: get_source_geometry => key_get_source_geometry
      procedure :: get_destination_geometry => key_get_destination_geometry
      procedure :: get_regridmethod => key_get_regridmethod
      procedure :: get_srcMaskValues => key_get_srcMaskValues
      procedure :: get_dstMaskValues => key_get_dstMaskValues
      procedure :: get_extrapmethod => key_get_extrapmethod
      procedure :: get_extrapNumSrcPnts => key_get_extrapNumSrcPnts
      procedure :: get_extrapDistExponent => key_get_extrapDistExponent
      procedure :: get_extrapNumLevels => key_get_extrapNumLevels
      procedure :: get_normtype => key_get_normtype
      procedure :: get_polemethod => key_get_polemethod
      procedure :: get_regridPoleNPnts => key_get_regridPoleNPnts
      procedure :: get_linetype => key_get_linetype
      procedure :: get_unmappedaction => key_get_unmappedaction
      procedure :: get_ignoreDegenerate => key_get_ignoreDegenerate
      procedure :: to_string => key_to_string
   end type RouteHandleKey

   interface RouteHandleKey
      module procedure new_RouteHandleKey
   end interface RouteHandleKey

   interface operator(==)
      module procedure key_equal
   end interface operator(==)

contains

   ! NOTE: every code= branch below, including the final failing
   ! else-branch, must leave the result "code" allocated. The error-macro
   ! expansion used at each call site (see MAPL_ErrLog.h) performs the
   ! assignment into the caller's "code" variable BEFORE the following
   ! status check and early return. If this function instead took the
   ! failing branch with its own result left unallocated, the caller's
   ! assignment would reference an unallocated allocatable-character
   ! function result - non-conforming (undefined outside an ALLOCATED
   ! query or DEALLOCATE) and, on Intel ifort (observed on bucy), a real
   ! SIGSEGV reading garbage length metadata, even though gfortran/NAG
   ! silently tolerate it. Pre-setting code to an empty string up front
   ! (before the failing branch can be reached) keeps the result always
   ! allocated regardless of outcome.
   function regrid_code(value, rc) result(code)
      type(ESMF_RegridMethod_Flag), intent(in) :: value
      integer, optional, intent(out) :: rc
      character(:), allocatable :: code
      integer :: status

      code = ''
      if (value == ESMF_REGRIDMETHOD_BILINEAR) then
         code = 'BILINEAR'
      else if (value == ESMF_REGRIDMETHOD_CONSERVE) then
         code = 'CONSERVE'
      else if (value == ESMF_REGRIDMETHOD_CONSERVE_2ND) then
         code = 'CONSERVE_2ND'
      else if (value == ESMF_REGRIDMETHOD_PATCH) then
         code = 'PATCH'
      else if (value == ESMF_REGRIDMETHOD_NEAREST_STOD) then
         code = 'NEAREST_STOD'
      else
         _FAIL('RouteHandleKey: unsupported regrid method')
      end if
      _RETURN(_SUCCESS)
   end function regrid_code

   function extrap_code(value, rc) result(code)
      type(ESMF_ExtrapMethod_Flag), intent(in) :: value
      integer, optional, intent(out) :: rc
      character(:), allocatable :: code
      integer :: status

      code = ''
      if (value == ESMF_EXTRAPMETHOD_NONE) then
         code = 'NONE'
      else
         _FAIL('RouteHandleKey: unsupported extrapolation method')
      end if
      _RETURN(_SUCCESS)
   end function extrap_code

   function norm_code(value, rc) result(code)
      type(ESMF_NormType_Flag), intent(in) :: value
      integer, optional, intent(out) :: rc
      character(:), allocatable :: code
      integer :: status

      code = ''
      if (value == ESMF_NORMTYPE_DSTAREA) then
         code = 'DSTAREA'
      else
         _FAIL('RouteHandleKey: unsupported normalization type')
      end if
      _RETURN(_SUCCESS)
   end function norm_code

   function pole_code(value, rc) result(code)
      type(ESMF_PoleMethod_Flag), intent(in) :: value
      integer, optional, intent(out) :: rc
      character(:), allocatable :: code
      integer :: status

      code = ''
      if (value == ESMF_POLEMETHOD_ALLAVG) then
         code = 'ALLAVG'
      else if (value == ESMF_POLEMETHOD_NONE) then
         code = 'NONE'
      else
         _FAIL('RouteHandleKey: unsupported pole method')
      end if
      _RETURN(_SUCCESS)
   end function pole_code

   function line_code(value, rc) result(code)
      type(ESMF_LineType_Flag), intent(in) :: value
      integer, optional, intent(out) :: rc
      character(:), allocatable :: code
      integer :: status

      code = ''
      if (value == ESMF_LINETYPE_GREAT_CIRCLE) then
         code = 'GREAT_CIRCLE'
      else
         _FAIL('RouteHandleKey: unsupported line type')
      end if
      _RETURN(_SUCCESS)
   end function line_code

   function unmapped_code(value, rc) result(code)
      type(ESMF_UnmappedAction_Flag), intent(in) :: value
      integer, optional, intent(out) :: rc
      character(:), allocatable :: code
      integer :: status

      code = ''
      if (value == ESMF_UNMAPPEDACTION_ERROR) then
         code = 'ERROR'
      else if (value == ESMF_UNMAPPEDACTION_IGNORE) then
         code = 'IGNORE'
      else
         _FAIL('RouteHandleKey: unsupported unmapped action')
      end if
      _RETURN(_SUCCESS)
   end function unmapped_code

   function new_RouteHandleKey(source_geometry, destination_geometry, srcMaskValues, dstMaskValues, &
      regridmethod, polemethod, regridPoleNPnts, linetype, normtype, extrapmethod, &
      extrapNumSrcPnts, extrapDistExponent, extrapNumLevels, unmappedaction, ignoreDegenerate) result(key)
      type(NodeId), intent(in) :: source_geometry, destination_geometry
      integer, optional, intent(in) :: srcMaskValues(:), dstMaskValues(:)
      type(ESMF_RegridMethod_Flag), optional, intent(in) :: regridmethod
      type(ESMF_PoleMethod_Flag), optional, intent(in) :: polemethod
      integer, optional, intent(in) :: regridPoleNPnts
      type(ESMF_LineType_Flag), optional, intent(in) :: linetype
      type(ESMF_NormType_Flag), optional, intent(in) :: normtype
      type(ESMF_ExtrapMethod_Flag), optional, intent(in) :: extrapmethod
      integer, optional, intent(in) :: extrapNumSrcPnts
      real(ESMF_KIND_R4), optional, intent(in) :: extrapDistExponent
      integer, optional, intent(in) :: extrapNumLevels
      type(ESMF_UnmappedAction_Flag), optional, intent(in) :: unmappedaction
      logical, optional, intent(in) :: ignoreDegenerate
      type(RouteHandleKey) :: key

      key%source_geometry = source_geometry
      key%destination_geometry = destination_geometry
      key%regridmethod = ESMF_REGRIDMETHOD_BILINEAR
      key%normtype = ESMF_NORMTYPE_DSTAREA
      key%extrapmethod = ESMF_EXTRAPMETHOD_NONE
      key%extrapNumSrcPnts = 8
      key%extrapDistExponent = 2.0_ESMF_KIND_R4
      key%unmappedaction = ESMF_UNMAPPEDACTION_ERROR
      key%ignoreDegenerate = .false.
      key%linetype = ESMF_LINETYPE_GREAT_CIRCLE
      if (present(regridmethod)) key%regridmethod = regridmethod
      if (key%regridmethod == ESMF_REGRIDMETHOD_CONSERVE .or. &
          key%regridmethod == ESMF_REGRIDMETHOD_CONSERVE_2ND) then
         key%polemethod = ESMF_POLEMETHOD_NONE
      else
         key%polemethod = ESMF_POLEMETHOD_ALLAVG
      end if
      if (present(srcMaskValues)) key%srcMaskValues = srcMaskValues
      if (present(dstMaskValues)) key%dstMaskValues = dstMaskValues
      if (present(polemethod)) key%polemethod = polemethod
      if (present(regridPoleNPnts)) key%regridPoleNPnts = regridPoleNPnts
      if (present(linetype)) key%linetype = linetype
      if (present(normtype)) key%normtype = normtype
      if (present(extrapmethod)) key%extrapmethod = extrapmethod
      if (present(extrapNumSrcPnts)) key%extrapNumSrcPnts = extrapNumSrcPnts
      if (present(extrapDistExponent)) key%extrapDistExponent = extrapDistExponent
      if (present(extrapNumLevels)) key%extrapNumLevels = extrapNumLevels
      if (present(unmappedaction)) key%unmappedaction = unmappedaction
      if (present(ignoreDegenerate)) key%ignoreDegenerate = ignoreDegenerate
   end function new_RouteHandleKey

   function key_to_string(this, rc) result(key_string)
      class(RouteHandleKey), intent(in) :: this
      integer, optional, intent(out) :: rc
      character(:), allocatable :: key_string
      character(:), allocatable :: rendering
      character(:), allocatable :: code
      integer :: status

      key_string = ''
      rendering = 'ROUTEHANDLE:' // this%source_geometry%to_string() // ':' // this%destination_geometry%to_string()
      code = regrid_code(this%regridmethod, _RC)
      rendering = rendering // ':' // code
      rendering = rendering // ':' // int_list(this%srcMaskValues) // ':' // int_list(this%dstMaskValues)
      code = extrap_code(this%extrapmethod, _RC)
      rendering = rendering // ':' // code
      rendering = rendering // ':' // int_value(this%extrapNumSrcPnts) // ':' // real_value(this%extrapDistExponent)
      rendering = rendering // ':' // scalar_alloc(this%extrapNumLevels)
      code = norm_code(this%normtype, _RC)
      rendering = rendering // ':' // code
      code = pole_code(this%polemethod, _RC)
      rendering = rendering // ':' // code
      rendering = rendering // ':' // scalar_alloc(this%regridPoleNPnts)
      code = line_code(this%linetype, _RC)
      rendering = rendering // ':' // code
      code = unmapped_code(this%unmappedaction, _RC)
      rendering = rendering // ':' // code
      rendering = rendering // ':' // merge('T', 'F', this%ignoreDegenerate)
      key_string = rendering
      _RETURN(_SUCCESS)
   end function key_to_string

   function int_value(value) result(text)
      integer, intent(in) :: value
      character(:), allocatable :: text
      character(32) :: buffer

      write(buffer, '(I0)') value
      text = trim(buffer)
   end function int_value

   function real_value(value) result(text)
      real(ESMF_KIND_R4), intent(in) :: value
      character(:), allocatable :: text
      character(64) :: buffer

      write(buffer, '(ES24.16E3)') value
      text = trim(adjustl(buffer))
   end function real_value

   function scalar_alloc(value) result(text)
      integer, allocatable, intent(in) :: value
      character(:), allocatable :: text

      if (allocated(value)) then
         text = 'A' // int_value(value)
      else
         text = 'U'
      end if
   end function scalar_alloc

   function int_list(value) result(text)
      integer, allocatable, intent(in) :: value(:)
      character(:), allocatable :: text
      integer :: i

      if (.not. allocated(value)) then
         text = 'U'
         return
      end if
      text = 'A'
      if (size(value) == 0) return
      text = 'A' // int_value(value(1))
      do i = 2, size(value)
         text = text // ',' // int_value(value(i))
      end do
   end function int_list

   logical function key_equal(left, right)
      type(RouteHandleKey), intent(in) :: left, right
      integer :: rc_left, rc_right
      key_equal = left%to_string(rc_left) == right%to_string(rc_right) .and. rc_left == 0 .and. rc_right == 0
   end function key_equal

   function key_get_source_geometry(this) result(value)
      class(RouteHandleKey), intent(in) :: this
      type(NodeId) :: value
      value = this%source_geometry
   end function key_get_source_geometry
   function key_get_destination_geometry(this) result(value)
      class(RouteHandleKey), intent(in) :: this
      type(NodeId) :: value
      value = this%destination_geometry
   end function key_get_destination_geometry
   function key_get_regridmethod(this) result(value)
      class(RouteHandleKey), intent(in) :: this
      type(ESMF_RegridMethod_Flag) :: value
      value = this%regridmethod
   end function key_get_regridmethod
   function key_get_srcMaskValues(this) result(value)
      class(RouteHandleKey), intent(in) :: this
      integer, allocatable :: value(:)
      if (allocated(this%srcMaskValues)) value = this%srcMaskValues
   end function key_get_srcMaskValues
   function key_get_dstMaskValues(this) result(value)
      class(RouteHandleKey), intent(in) :: this
      integer, allocatable :: value(:)
      if (allocated(this%dstMaskValues)) value = this%dstMaskValues
   end function key_get_dstMaskValues

   function key_get_extrapmethod(this) result(value)
      class(RouteHandleKey), intent(in) :: this
      type(ESMF_ExtrapMethod_Flag) :: value
      value = this%extrapmethod
   end function key_get_extrapmethod
   function key_get_extrapNumSrcPnts(this) result(value)
      class(RouteHandleKey), intent(in) :: this
      integer :: value
      value = this%extrapNumSrcPnts
   end function key_get_extrapNumSrcPnts
   function key_get_extrapDistExponent(this) result(value)
      class(RouteHandleKey), intent(in) :: this
      real(ESMF_KIND_R4) :: value
      value = this%extrapDistExponent
   end function key_get_extrapDistExponent
   function key_get_extrapNumLevels(this) result(value)
      class(RouteHandleKey), intent(in) :: this
      integer, allocatable :: value
      if (allocated(this%extrapNumLevels)) value = this%extrapNumLevels
   end function key_get_extrapNumLevels
   function key_get_normtype(this) result(value)
      class(RouteHandleKey), intent(in) :: this
      type(ESMF_NormType_Flag) :: value
      value = this%normtype
   end function key_get_normtype
   function key_get_polemethod(this) result(value)
      class(RouteHandleKey), intent(in) :: this
      type(ESMF_PoleMethod_Flag) :: value
      value = this%polemethod
   end function key_get_polemethod
   function key_get_regridPoleNPnts(this) result(value)
      class(RouteHandleKey), intent(in) :: this
      integer, allocatable :: value
      if (allocated(this%regridPoleNPnts)) value = this%regridPoleNPnts
   end function key_get_regridPoleNPnts
   function key_get_linetype(this) result(value)
      class(RouteHandleKey), intent(in) :: this
      type(ESMF_LineType_Flag) :: value
      value = this%linetype
   end function key_get_linetype
   function key_get_unmappedaction(this) result(value)
      class(RouteHandleKey), intent(in) :: this
      type(ESMF_UnmappedAction_Flag) :: value
      value = this%unmappedaction
   end function key_get_unmappedaction
   function key_get_ignoreDegenerate(this) result(value)
      class(RouteHandleKey), intent(in) :: this
      logical :: value
      value = this%ignoreDegenerate
   end function key_get_ignoreDegenerate

end module mapl_RouteHandleKey_mod
