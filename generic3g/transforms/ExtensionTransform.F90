#include "MAPL.h"
module mapl3g_ExtensionTransform
   use mapl3g_TransformId
   use mapl3g_AspectId
   use mapl_ErrorHandling
   use ESMF
   implicit none
   private

   public :: ExtensionTransform
   public :: COUPLER_IMPORT_NAME
   public :: COUPLER_EXPORT_NAME

   type, abstract :: ExtensionTransform
   contains
      procedure(I_run), deferred :: initialize
      procedure(I_run), deferred :: update
      procedure :: runs_invalidate
      procedure :: invalidate
      procedure(I_get_transformId), deferred :: get_transformId
   end type ExtensionTransform


   abstract interface
      subroutine I_run(this, importState, exportState, clock, rc)
         use ESMF
         import ExtensionTransform
         class(ExtensionTransform), intent(inout) :: this
         type(ESMF_State) :: importState
         type(ESMF_State) :: exportState
         type(ESMF_Clock) :: clock
         integer, optional, intent(out) :: rc
      end subroutine I_run

      function I_get_transformId(this) result(id)
         import TransformId
         import ExtensionTransform
         class(ExtensionTransform), intent(in) :: this
         type(TransformId) :: id
      end function I_get_transformId
   end interface

   character(len=*), parameter :: COUPLER_IMPORT_NAME = 'import[1]'
   character(len=*), parameter :: COUPLER_EXPORT_NAME = 'export[1]'

contains

   ! This is a default no-op implementation of invalidate. Types derived from
   ! ExtensionTransform should overload it as needed.
   subroutine invalidate(this, importState, exportState, clock, rc)
      use ESMF
      class(ExtensionTransform), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine invalidate

   ! This is a default logical function that always return .FALSE.
   ! to determine if invalidate should run. Subclasses that run invalidate
   ! (override the invalidate subroutine nontrivially) need to implement
   ! a nontrivial override of this function.
   logical function runs_invalidate(this)
      class(ExtensionTransform), intent(in) :: this
      runs_invalidate = .FALSE.
   end function runs_invalidate

end module mapl3g_ExtensionTransform
