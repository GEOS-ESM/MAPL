#include "MAPL.h"

module mapl_AbstractCovarianceKernel_mod

   use MAPL
   use ESMF

   implicit none(type, external)
   private

   public :: AbstractCovarianceKernel

   type, abstract :: AbstractCovarianceKernel
   contains
      procedure(I_advertise),  deferred :: advertise
      procedure(I_get_internal_field_prefixes), deferred :: get_internal_field_prefixes
      procedure(I_initialize), deferred :: initialize
      procedure(I_action),     deferred :: destroy
      procedure(I_action),     deferred :: reset
      procedure(I_update),     deferred :: update_r4
      procedure(I_update),     deferred :: update_r8
      procedure(I_compute),    deferred :: compute_r4
      procedure(I_compute),    deferred :: compute_r8
   end type AbstractCovarianceKernel

   abstract interface

      subroutine I_advertise(this, gridcomp, name, item_type, rc)
         import AbstractCovarianceKernel
         import esmf_GridComp
         import ESMF_StateItem_Flag
         class(AbstractCovarianceKernel), intent(inout) :: this
         type(esmf_GridComp), intent(inout) :: gridcomp
         character(*), intent(in) :: name
         type(ESMF_StateItem_Flag), intent(in) :: item_type
         integer, optional, intent(out) :: rc
      end subroutine I_advertise

      ! Returns the ordered list of internal-state name prefixes (e.g. 'mux_',
      ! 'muy_', 'c_' for Welford) that this kernel needs resolved and passed
      ! back to it (in the same order) via the internal_fields(:) argument of
      ! initialize/I_action/I_update/I_compute.
      function I_get_internal_field_prefixes(this) result(prefixes)
         import AbstractCovarianceKernel
         class(AbstractCovarianceKernel), intent(in) :: this
         character(len=16), allocatable :: prefixes(:)
      end function I_get_internal_field_prefixes

      subroutine I_initialize(this, gridcomp, f_x, f_y, counts_f, internal_fields, rc)
         import AbstractCovarianceKernel
         import esmf_GridComp
         import esmf_Field
         class(AbstractCovarianceKernel), intent(inout) :: this
         type(esmf_GridComp), intent(inout) :: gridcomp
         type(esmf_Field), intent(inout) :: f_x
         type(esmf_Field), intent(inout) :: f_y
         type(esmf_Field), intent(inout) :: counts_f
         type(esmf_Field), intent(inout) :: internal_fields(:)
         integer, optional, intent(out) :: rc
      end subroutine I_initialize

      subroutine I_action(this, gridcomp, internal_fields, rc)
         import AbstractCovarianceKernel
         import esmf_GridComp
         import esmf_Field
         class(AbstractCovarianceKernel), intent(inout) :: this
         type(esmf_GridComp), intent(inout) :: gridcomp
         type(esmf_Field), intent(inout) :: internal_fields(:)
         integer, optional, intent(out) :: rc
      end subroutine I_action

      subroutine I_update(this, gridcomp, f_x, f_y, counts_f, internal_fields, rc)
         import AbstractCovarianceKernel
         import esmf_GridComp
         import esmf_Field
         class(AbstractCovarianceKernel), intent(inout) :: this
         type(esmf_GridComp), intent(inout) :: gridcomp
         type(esmf_Field), intent(inout) :: f_x
         type(esmf_Field), intent(inout) :: f_y
         type(esmf_Field), intent(inout) :: counts_f
         type(esmf_Field), intent(inout) :: internal_fields(:)
         integer, optional, intent(out) :: rc
      end subroutine I_update

      subroutine I_compute(this, gridcomp, f_x, f_y, counts_f, cov_f, internal_fields, biased, rc)
         import AbstractCovarianceKernel
         import esmf_GridComp
         import esmf_Field
         class(AbstractCovarianceKernel), intent(inout) :: this
         type(esmf_GridComp), intent(inout) :: gridcomp
         type(esmf_Field), intent(inout) :: f_x
         type(esmf_Field), intent(inout) :: f_y
         type(esmf_Field), intent(inout) :: counts_f
         type(esmf_Field), intent(inout) :: cov_f
         type(esmf_Field), intent(inout) :: internal_fields(:)
         logical, intent(in) :: biased
         integer, optional, intent(out) :: rc
      end subroutine I_compute

   end interface

end module mapl_AbstractCovarianceKernel_mod
