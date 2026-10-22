#include "MAPL.h"

!------------------------------------------------------------------------------
! MAPL_StateItem_Flag: open-ended "variant" classification for
! GraphStateItem (REQ-SI-002b), distinct from ESMF's native
! ESMF_StateItem_Flag but mirroring its vocabulary as the "vanilla" default
! for each native kind: FIELD, FIELDBUNDLE, STATE, ROUTEHANDLE, NOTFOUND.
! Finer-grained roles refine a single native kind:
!   - FIELD refines to: GEOM
!   - FIELDBUNDLE refines to: VECTOR, BRACKET, VECTORBRACKET
!   - STATE refines to: VERTICALGRID
!   - ROUTEHANDLE has no finer refinement (already fully distinguished at
!     the native esmf_kind()/itemType() tier).
!
! Follows ESMF's flag-type style (a small derived type wrapping an integer
! code, compared via operator(==)/operator(/=)) so new variants can be added
! without disturbing client code that compares against named constants.
!
! This module also owns the ESMF_Info attachment/read-back for its values
! (MAPL_StateItemFlag_set_info/get_info), storing the flag's to_string() name
! rather than the internal integer code. mapl_StateItemVariantInfo_mod owns
! only the reserved key name and which host object's Info to use, and
! delegates the read/write of the value to this module.
!------------------------------------------------------------------------------
module mapl_StateItemFlag_mod
   use ESMF, only: ESMF_Info, ESMF_InfoSet, ESMF_InfoGetCharAlloc
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: MAPL_StateItem_Flag
   public :: operator(==)
   public :: operator(/=)
   public :: MAPL_StateItemFlag_set_info
   public :: MAPL_StateItemFlag_get_info

   public :: MAPL_STATEITEM_FIELD
   public :: MAPL_STATEITEM_FIELDBUNDLE
   public :: MAPL_STATEITEM_STATE
   public :: MAPL_STATEITEM_ROUTEHANDLE
   public :: MAPL_STATEITEM_NOTFOUND
   public :: MAPL_STATEITEM_GEOM
   public :: MAPL_STATEITEM_VECTOR
   public :: MAPL_STATEITEM_BRACKET
   public :: MAPL_STATEITEM_VECTORBRACKET
   public :: MAPL_STATEITEM_VERTICALGRID
   ! EXPRESSION item class (N:1 expression evaluation)
   public :: MAPL_STATEITEM_EXPRESSION

   type :: MAPL_StateItem_Flag
      private
      integer :: value = -1
   contains
      procedure :: to_string => flag_to_string
   end type MAPL_StateItem_Flag

   ! Vanilla defaults - one per native esmf_kind()/itemType() value.
   type(MAPL_StateItem_Flag), parameter :: MAPL_STATEITEM_FIELD       = MAPL_StateItem_Flag(0)
   type(MAPL_StateItem_Flag), parameter :: MAPL_STATEITEM_FIELDBUNDLE = MAPL_StateItem_Flag(1)
   type(MAPL_StateItem_Flag), parameter :: MAPL_STATEITEM_STATE       = MAPL_StateItem_Flag(2)
   type(MAPL_StateItem_Flag), parameter :: MAPL_STATEITEM_ROUTEHANDLE = MAPL_StateItem_Flag(3)
   type(MAPL_StateItem_Flag), parameter :: MAPL_STATEITEM_NOTFOUND    = MAPL_StateItem_Flag(4)

   ! Refinements.
   type(MAPL_StateItem_Flag), parameter :: MAPL_STATEITEM_GEOM          = MAPL_StateItem_Flag(10)
   type(MAPL_StateItem_Flag), parameter :: MAPL_STATEITEM_VECTOR        = MAPL_StateItem_Flag(11)
   type(MAPL_StateItem_Flag), parameter :: MAPL_STATEITEM_BRACKET       = MAPL_StateItem_Flag(12)
   type(MAPL_StateItem_Flag), parameter :: MAPL_STATEITEM_VECTORBRACKET = MAPL_StateItem_Flag(13)
   type(MAPL_StateItem_Flag), parameter :: MAPL_STATEITEM_VERTICALGRID  = MAPL_StateItem_Flag(14)
   ! EXPRESSION item class: next unused value after VERTICALGRID
   type(MAPL_StateItem_Flag), parameter :: MAPL_STATEITEM_EXPRESSION    = MAPL_StateItem_Flag(15)

   interface operator(==)
      module procedure flag_equal
   end interface operator(==)

   interface operator(/=)
      module procedure flag_not_equal
   end interface operator(/=)

contains

   pure logical function flag_equal(left, right) result(equal)
      type(MAPL_StateItem_Flag), intent(in) :: left, right

      equal = left%value == right%value
   end function flag_equal

   pure logical function flag_not_equal(left, right) result(not_equal)
      type(MAPL_StateItem_Flag), intent(in) :: left, right

      not_equal = .not. (left == right)
   end function flag_not_equal

   pure function flag_to_string(this) result(name)
      class(MAPL_StateItem_Flag), intent(in) :: this
      character(:), allocatable :: name

      select case (this%value)
      case (0);  name = 'FIELD'
      case (1);  name = 'FIELDBUNDLE'
      case (2);  name = 'STATE'
      case (3);  name = 'ROUTEHANDLE'
      case (4);  name = 'NOTFOUND'
      case (10); name = 'GEOM'
      case (11); name = 'VECTOR'
      case (12); name = 'BRACKET'
      case (13); name = 'VECTORBRACKET'
      case (14); name = 'VERTICALGRID'
      case (15); name = 'EXPRESSION'
      case default; name = 'UNKNOWN'
      end select
   end function flag_to_string

   function flag_from_string(name) result(flag)
      character(*), intent(in) :: name
      type(MAPL_StateItem_Flag) :: flag

      select case (trim(name))
      case ('FIELD');         flag = MAPL_STATEITEM_FIELD
      case ('FIELDBUNDLE');   flag = MAPL_STATEITEM_FIELDBUNDLE
      case ('STATE');         flag = MAPL_STATEITEM_STATE
      case ('ROUTEHANDLE');   flag = MAPL_STATEITEM_ROUTEHANDLE
      case ('NOTFOUND');      flag = MAPL_STATEITEM_NOTFOUND
      case ('GEOM');          flag = MAPL_STATEITEM_GEOM
      case ('VECTOR');        flag = MAPL_STATEITEM_VECTOR
      case ('BRACKET');       flag = MAPL_STATEITEM_BRACKET
      case ('VECTORBRACKET'); flag = MAPL_STATEITEM_VECTORBRACKET
      case ('VERTICALGRID');  flag = MAPL_STATEITEM_VERTICALGRID
      case ('EXPRESSION');    flag = MAPL_STATEITEM_EXPRESSION
      case default
         flag%value = -1
      end select
   end function flag_from_string

   ! Attaches this flag's name to the given ESMF_Info object under key,
   ! the sole read/write path for a MAPL_StateItem_Flag's ESMF_Info
   ! representation - callers (mapl_StateItemVariantInfo_mod) never see the
   ! internal integer code, only the flag value itself.
   subroutine MAPL_StateItemFlag_set_info(info, key, flag, rc)
      type(ESMF_Info), intent(inout) :: info
      character(*), intent(in) :: key
      type(MAPL_StateItem_Flag), intent(in) :: flag
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_InfoSet(info, key=key, value=flag%to_string(), _RC)
   end subroutine MAPL_StateItemFlag_set_info

   function MAPL_StateItemFlag_get_info(info, key, rc) result(flag)
      type(ESMF_Info), intent(in) :: info
      character(*), intent(in) :: key
      integer, optional, intent(out) :: rc
      type(MAPL_StateItem_Flag) :: flag

      integer :: status
      character(:), allocatable :: name

      call ESMF_InfoGetCharAlloc(info, key=key, value=name, _RC)
      flag = flag_from_string(name)
   end function MAPL_StateItemFlag_get_info

end module mapl_StateItemFlag_mod
