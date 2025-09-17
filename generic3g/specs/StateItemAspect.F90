#include "MAPL.h"
!-------------------------------------------------
! Table of allowed connections between (like) StateItemAspects
!-------------------------------------------------
!
! SRC^4   |  DST^4  | ALLOW   |  REQUIRE COUPLER
!---------|---------|---------|-------------------
! simple  | simple  |   Y     |   if (.not. match)
! simple  | mirror  |   Y     |   never
! simple  | timedep |   Y     |   always^2
!
! mirror  | simple  |   ?^1   |   never
! mirror  | mirror  |   N     |   N/A
! mirror  | timedep |   ?^1,3 |   never
!
! timedep | simple  |   Y     |   always^2
! timedep | mirror  |   Y     |   never
! timedep | timedep |   Y     |   always^2
!-------------------------------------------------
!
! Commments
!
! ^1: Cannot simultaneously mirror an export aspect to different
!     import aspects.  But would be useful for default values and
!     expressions (geom) Possibly becomes "not mirror" after first
!     connection, and subsequent ...
!
! ^2: Even if coincidental match at first.
!
! ^3: If we allow, then export must become time-dependent for
!     subsequent connections.  Otherwise, some other import might "agree" initially and
!     miss the need for a coupler in the general case.
!
! ^4: Neither SRC nor DST is permitted to be in INVALID status when
!     connecting.  However, a state item can still be connected so
!     long as the given invalid aspect is not in the coupling
!     order.
!-------------------------------------------------


module mapl3g_StateItemAspect
   use iso_fortran_env, only: INT64
   use mapl3g_AspectId
   use mapl_ErrorHandling
   use esmf

#define Key AspectId
#define Key_LT(a,b) (a) < (b)
#define T StateItemAspect
#define T_polymorphic
#define Map AspectMap
#define MapIterator AspectMapIterator
#define Pair AspectPair

#define USE_ALT_SET
!#include "shared/define_common_macros.inc"
#include "map/header.inc"
#include "map/public.inc"


   public :: StateItemAspect

   type, abstract :: StateItemAspect
      private
      logical :: mirror = .false.
      logical :: time_dependent = .false.
   contains
      ! Subclass must define these
      procedure(I_matches), deferred :: matches

      procedure(I_make_transform), deferred :: make_transform
      procedure :: connect_to_import
      procedure(I_connect_to_export), deferred :: connect_to_export
      procedure(I_get_aspect_id), deferred, nopass :: get_aspect_id

      procedure(I_supports_conversion_general), deferred :: supports_conversion_general
      procedure(I_supports_conversion_specific), deferred :: supports_conversion_specific
      generic :: supports_conversion => supports_conversion_general, supports_conversion_specific

      procedure, non_overridable :: can_connect_to
      procedure, non_overridable :: needs_extension_for

      procedure, non_overridable :: is_mirror
      procedure, non_overridable :: set_mirror
      procedure, non_overridable :: is_time_dependent
      procedure, non_overridable :: set_time_dependent

      procedure :: update_from_payload

   end type StateItemAspect

#include "map/specification.inc"

   abstract interface

      logical function I_matches(src, dst) result(matches)
         import :: StateItemAspect
         class(StateItemAspect), intent(in) :: src, dst
      end function I_matches

      logical function I_supports_conversion_general(src) result(supports_conversion)
         import :: StateItemAspect
         class(StateItemAspect), intent(in) :: src
      end function I_supports_conversion_general

      logical function I_supports_conversion_specific(src, dst) result(supports_conversion)
         import :: StateItemAspect
         class(StateItemAspect), intent(in) :: src
         class(StateItemAspect), intent(in) :: dst
      end function I_supports_conversion_specific

      function I_get_aspect_id() result(aspect_id)
         import StateItemAspect
         import AspectId
         type(AspectId) :: aspect_id
      end function I_get_aspect_id

      function I_make_transform(src, dst, other_aspects, rc) result(transform)
         use mapl3g_ExtensionTransform
         import :: StateItemAspect
         import :: AspectMap
         class(ExtensionTransform), allocatable :: transform
         class(StateItemAspect), intent(in) :: src
         class(StateItemAspect), intent(in) :: dst
         type(AspectMap), target, intent(in) :: other_aspects
         integer, optional, intent(out) :: rc
      end function I_make_transform

      subroutine I_connect_to_export(this, export, actual_pt, rc)
         use mapl3g_ActualConnectionPt
         import :: StateItemAspect
         class(StateItemAspect), intent(inout) :: this
         class(StateItemAspect), intent(in) :: export
         type(ActualConnectionPt), intent(in) :: actual_pt
         integer, optional, intent(out) :: rc
      end subroutine I_connect_to_export

end interface


contains


   subroutine update_from_payload(this, field, bundle, state, rc)
      class(StateItemAspect), intent(inout) :: this
      type(esmf_Field), optional, intent(in) :: field
      type(esmf_FieldBundle), optional, intent(in) :: bundle
      type(esmf_State), optional, intent(in) :: state
      integer, optional, intent(out) :: rc

      ! Default do nothing - override in subclasses.  When done
      ! make this just an interface.
      _RETURN(_SUCCESS)
   end subroutine update_from_payload

#include "map/procedures.inc"
#include "map/tail.inc"


   !-------------------------------------------
   ! Two aspects cann connect if and only if:
   ! (1) Same subclass
   ! (2) At least one is not mirror
   ! (3) Exact match or supports conversion
   !-------------------------------------------
   logical function can_connect_to(src, dst)
      class(StateItemAspect), intent(in) :: src, dst

      can_connect_to = .false.

      associate (num_mirror => count([src%is_mirror(), dst%is_mirror()]))
        select case (num_mirror)
        case (0)
           if (either_is_time_dependent(src, dst)) then
              ! Must expect to convert to unknown aspect value in the future.
              can_connect_to = src%supports_conversion()
              return
           end if
           can_connect_to = src%matches(dst)
           if (.not. can_connect_to) then
              can_connect_to = src%supports_conversion(dst)
           end if
        case (1)
           can_connect_to = .true.
        case (2)
           can_connect_to = .false. ! double mirror
        end select ! no need for default clause

      end associate

   end function can_connect_to

   logical function either_is_time_dependent(src, dst)
      class(StateItemAspect), intent(in) :: src, dst
      either_is_time_dependent = src%is_time_dependent() .or. dst%is_time_dependent()
   end function either_is_time_dependent

   logical function either_is_mirror(src, dst)
      class(StateItemAspect), intent(in) :: src, dst
      either_is_mirror = src%is_mirror() .or. dst%is_mirror()
   end function either_is_mirror

   !-------------------------------------------
   ! Note that if src is mirror - we do not "extend"
   ! rather the src aspect is actually modified (elsewhere)
   ! to be the dst aspect.
   !--------------------------------------------
   logical function needs_extension_for(src, dst)
      class(StateItemAspect), intent(in) :: src, dst

      if (dst%is_mirror()) then
         needs_extension_for = .false.
         return
      end if

      if (either_is_time_dependent(src, dst)) then
         needs_extension_for = .true.
         return
      end if

      ! Simple case
      needs_extension_for = .not. src%matches(dst)

   end function needs_extension_for

   logical function is_mirror(this)
      class(StateItemAspect), intent(in) :: this
      is_mirror = this%mirror
   end function is_mirror

   subroutine set_mirror(this, mirror)
      class(StateItemAspect), intent(inout) :: this
      logical, optional, intent(in) :: mirror
      if (present(mirror)) this%mirror = mirror
   end subroutine set_mirror

   logical function is_time_dependent(this)
      class(StateItemAspect), intent(in) :: this
      is_time_dependent = this%time_dependent
   end function is_time_dependent

   subroutine set_time_dependent(this, time_dependent)
      class(StateItemAspect), intent(inout) :: this
      logical, optional, intent(in) :: time_dependent
      if (present(time_dependent)) this%time_dependent = time_dependent
   end subroutine set_time_dependent

   ! Most subclasses have same behavior (NOOP) so we provide a base
   ! implementation. 
   subroutine connect_to_import(this, import, rc)
      class(StateItemAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: import
      integer, optional, intent(out) :: rc

      
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(import)
   end subroutine connect_to_import
   

#undef AspectPair
#undef AspectMapIterator
#undef AspectMap
#undef T_polymorphic
#undef T
#undef Key
#undef KEY_LT
end module mapl3g_StateItemAspect




