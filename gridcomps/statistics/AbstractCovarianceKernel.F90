#include "MAPL.h"

module mapl_AbstractCovarianceKernel

   use MAPL
   use ESMF
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer

   implicit none(type, external)
   private

   public :: AbstractCovarianceKernel

   type, abstract :: AbstractCovarianceKernel
   contains
      procedure(I_advertise),  deferred :: advertise
      procedure(I_initialize), deferred :: initialize
      procedure(I_action),     deferred :: destroy
      procedure(I_action),     deferred :: reset
      procedure(I_update),     deferred :: update_r4
      procedure(I_update),     deferred :: update_r8
      procedure(I_compute),    deferred :: compute_r4
      procedure(I_compute),    deferred :: compute_r8
   end type AbstractCovarianceKernel

   abstract interface

      subroutine I_advertise(this, gridcomp, name, rc)
         import AbstractCovarianceKernel
         import esmf_GridComp
         class(AbstractCovarianceKernel), intent(inout) :: this
         type(esmf_GridComp), intent(inout) :: gridcomp
         character(*), intent(in) :: name
         integer, optional, intent(out) :: rc
      end subroutine I_advertise

      subroutine I_initialize(this, gridcomp, f_x, f_y, counts_f, rc)
         import AbstractCovarianceKernel
         import esmf_GridComp
         import esmf_Field
         class(AbstractCovarianceKernel), intent(inout) :: this
         type(esmf_GridComp), intent(inout) :: gridcomp
         type(esmf_Field), intent(inout) :: f_x
         type(esmf_Field), intent(inout) :: f_y
         type(esmf_Field), intent(inout) :: counts_f
         integer, optional, intent(out) :: rc
      end subroutine I_initialize

      subroutine I_action(this, gridcomp, f_x, rc)
         import AbstractCovarianceKernel
         import esmf_GridComp
         import esmf_Field
         class(AbstractCovarianceKernel), intent(inout) :: this
         type(esmf_GridComp), intent(inout) :: gridcomp
         type(esmf_Field), intent(inout) :: f_x
         integer, optional, intent(out) :: rc
      end subroutine I_action

      subroutine I_update(this, gridcomp, f_x, f_y, counts_f, rc)
         import AbstractCovarianceKernel
         import esmf_GridComp
         import esmf_Field
         class(AbstractCovarianceKernel), intent(inout) :: this
         type(esmf_GridComp), intent(inout) :: gridcomp
         type(esmf_Field), intent(inout) :: f_x
         type(esmf_Field), intent(inout) :: f_y
         type(esmf_Field), intent(inout) :: counts_f
         integer, optional, intent(out) :: rc
      end subroutine I_update

      subroutine I_compute(this, gridcomp, f_x, f_y, counts_f, cov_f, biased, rc)
         import AbstractCovarianceKernel
         import esmf_GridComp
         import esmf_Field
         class(AbstractCovarianceKernel), intent(inout) :: this
         type(esmf_GridComp), intent(inout) :: gridcomp
         type(esmf_Field), intent(inout) :: f_x
         type(esmf_Field), intent(inout) :: f_y
         type(esmf_Field), intent(inout) :: counts_f
         type(esmf_Field), intent(inout) :: cov_f
         logical, intent(in) :: biased
         integer, optional, intent(out) :: rc
      end subroutine I_compute

   end interface

end module mapl_AbstractCovarianceKernel
