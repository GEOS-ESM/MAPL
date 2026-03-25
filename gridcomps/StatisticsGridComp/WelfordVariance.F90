#include "MAPL.h"

module mapl3g_WelfordVariance
   use mapl3g_AbstractVariance, only: Variance
   use mapl3
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer

   implicit none(type,external)
   private

   public :: WelfordVariance

   type, extends(Variance) :: WelfordVariance 
      private
      type(ESMF_Field) :: mu
      type(ESMF_Field) :: m2
   contains
      procedure :: post_initialize
      procedure :: post_destroy
      procedure :: post_reset
      procedure :: update_r4
      procedure :: update_r8
      procedure :: compute_r4
      procedure :: compute_r8
   end type WelfordVariance 

   interface WelfordVariance
      module procedure new_WelfordVariance
   end interface WelfordVariance

contains

   function new_WelfordVariance(unusable, f, variance_field, alarm, biased) result(var) result(var)
      type(WelfordVariance) :: var
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Field), intent(in) :: f
      type(ESMF_Field), intent(in) :: variance_field
      type(ESMF_Alarm), intent(in) :: alarm
      character(len=*), optional, intent(in) :: biased

      call set_common(var, f=f, variance_field=variance_field, alarm=alarm, biased=biased)
      _UNUSED_DUMMY(unusable)

   end function new_WelfordVariance

   subroutine post_initialize(this, name, rc)
      class(WelfordVariance), intent(inout) :: this
      character(len=*), intent(in) :: name
      integer, optional, intent(out) :: rc
      integer :: status
     
      call MAPL_FieldClone(this%f, this%variance, _RC)
      call ESMF_FieldSet(this%variance, name='variance_'//name, _RC)
      call MAPL_FieldClone(this%f, this%mu, _RC)
      call MAPL_FieldClone(this%f, this%m2, _RC)
      _RETURN(_SUCCESS)

   end subroutine post_initialize

   subroutine post_destroy(this, rc)
      class(WelfordVariance), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status

      call ESMF_FieldDestroy(this%mu, _RC)
      call ESMF_FieldDestroy(this%m2, _RC)
      call ESMF_FieldDestroy(this%variance, _RC)
      _RETURN(_SUCCESS)

   end subroutine post_destroy

   subroutine post_reset(this, rc)
      class(WelfordVariance), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status

      call ESMF_FieldFill(this%mu, dataFillScheme='const', const1=0.d0, _RC)
      call ESMF_FieldFill(this%m2, dataFillScheme='const', const1=0.d0, _RC)
      call ESMF_FieldFill(this%variance, dataFillScheme='const', const1=0.d0, _RC)
      _RETURN(_SUCCESS)

   end subroutine post_reset

   subroutine update_r4(this, rc)
      class(WelfordVariance), intent(inout) :: this
      integer, intent(out) :: rc
      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: f(:), mu(:), m2(:)
      real(kind=ESMF_KIND_R4), allocatable(:) :: prev_mu(:)

      call MAPL_AssignFptr(this%f, f, _RC)
      call MAPL_AssignFptr(this%mu, mu, _RC)
      call MAPL_AssignFptr(this%m2, m2, _RC)
      allocate(prev_mu(size(mu)))

      where (f /= MAPL_UNDEF)
         this%counts = this%counts + 1
         prev_mu = mu
         mu = mu + (f - mu) / counts
         m2 = m2 + (f - prev_mu) * (f - mu)
      end where

      _RETURN(_SUCCESS)

   end subroutine update_r4

   subroutine update_r8(this, rc)
      class(Variance), intent(inout) :: this
      integer, intent(out) :: rc
      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: f(:), mu(:), m2(:)
      real(kind=ESMF_KIND_R4), allocatable(:) :: prev_mu(:)

      call MAPL_AssignFptr(this%f, f, _RC)
      call MAPL_AssignFptr(this%mu, mu, _RC)
      call MAPL_AssignFptr(this%m2, m2, _RC)
      allocate(prev_mu(size(mu)))

      where (f /= MAPL_UNDEF)
         this%counts = this%counts + 1
         prev_mu = mu
         mu = mu + (f - mu) / counts
         m2 = m2 + (f - prev_mu) * (f - mu)
      end where

      _RETURN(_SUCCESS)

   end subroutine update_r8

   subroutine compute_r4(this, rc)
      class(Variance), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: m2(:), var(:)
      integer :: counts_offset

      counts_offset = 1
      if(this%biased()) counts_offset = 0
      call MAPL_AssignFptr(this%m2, m2, _RC)
      call MAPL_AssignFptr(this%fvariance, var, _RC)

      where (this%counts > counts_offset)
         var = m2 / (this%counts - counts_offset)
      elsewhere
         var = MAPL_UNDEF
      end where

      _RETURN(_SUCCESS)

   end subroutine compute_r4

   subroutine compute_r8(this, rc)
      class(Variance), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: m2(:), var(:)
      integer :: counts_offset

      counts_offset = 1
      if(this%biased()) counts_offset = 0
      call MAPL_AssignFptr(this%m2, m2, _RC)
      call MAPL_AssignFptr(this%fvariance, var, _RC)

      where (this%counts > counts_offset)
         var = m2 / (this%counts - counts_offset)
      elsewhere
         var = MAPL_UNDEF
      end where

      _RETURN(_SUCCESS)

   end subroutine compute_r8

end module mapl3g_WelfordVariance
