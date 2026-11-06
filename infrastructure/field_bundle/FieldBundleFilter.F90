#include "MAPL.h"

module mapl_FieldBundleFilter_mod

   use ESMF
   use mapl_FieldBundleGet_mod, only: FieldBundleGet
   use mapl_ErrorHandling_mod, only: MAPL_Return, MAPL_Verify

   implicit none(type, external)
   private

   public :: FieldBundleFilter
   public :: FieldPredicate

   interface FieldBundleFilter
      module procedure :: bundle_filter
   end interface

   ! Decides whether bundle_filter removes a field. Extensions can carry the
   ! data the decision needs as components.
   type, abstract :: FieldPredicate
   contains
      procedure(I_remove), deferred :: remove
   end type FieldPredicate

   abstract interface
      function I_remove(this, field, rc) result(remove)
         use ESMF
         import :: FieldPredicate
         class(FieldPredicate), intent(in) :: this
         type(ESMF_Field), intent(in) :: field
         integer, optional, intent(out) :: rc
         logical :: remove
      end function I_remove
   end interface

contains

   subroutine bundle_filter(bundle, predicate, rc)
      type(ESMF_FieldBundle), intent(inout) :: bundle
      class(FieldPredicate), intent(in) :: predicate
      integer, optional, intent(out) :: rc

      type(ESMF_Field), allocatable :: field_list(:)
      character(len=ESMF_MAXSTR) :: field_name
      integer :: idx, status
      logical :: remove

      call FieldBundleGet(bundle, fieldList=field_list, _RC) ! addorder
      do idx = 1, size(field_list)
         remove = predicate%remove(field_list(idx), _RC)
         if (remove) then
            call ESMF_FieldGet(field_list(idx), name=field_name, _RC)
            call ESMF_FieldBundleRemove(bundle, [field_name], _RC)
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine bundle_filter

end module mapl_FieldBundleFilter_mod
