#include "MAPL_Generic.h"

module MAPL_IdentityRegridderMod
   use MAPL_AbstractRegridderMod
   use MAPL_GridSpecMod
   use MAPL_RegridderSpec
   use MAPL_KeywordEnforcerMod
   use mapl_ErrorHandlingMod
   use mapl_RegridMethods
   use ESMF
   
   use, intrinsic :: iso_fortran_env, only: REAL32
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none
   private

   !----------------------------
   ! Note this module implements the singleton pattern.  The type is
   ! PRIVATE, which prevents other modules from creating objects of
   ! that type.  The identity_regridder() procedure is PUBLIC and
   ! returns a pointer to the singleton object.
   !----------------------------
   public :: identity_regridder

   integer, parameter :: NUM_DIMS = 2

   type, extends(AbstractRegridder) :: IdentityRegridder
      private
      logical :: initialized
   contains
      procedure :: initialize_subclass
      procedure :: regrid_scalar_2d_real32
      procedure :: regrid_scalar_3d_real32
      procedure :: regrid_vector_2d_real32
      procedure :: regrid_vector_3d_real32
   end type IdentityRegridder

   character(len=*), parameter :: MOD_NAME = 'MAPL_IdentityRegridder::'

   type (IdentityRegridder), save, target :: singleton
   
contains


   function identity_regridder() result(regridder)
      use ESMF
      type (IdentityRegridder), pointer :: regridder

      regridder => singleton
    end function identity_regridder


   subroutine regrid_scalar_2d_real32(this, q_in, q_out, rc)
      class (IdentityRegridder), intent(in) :: this
      real (kind=REAL32), intent(in) :: q_in(:,:)
      real (kind=REAL32), intent(out) :: q_out(:,:)
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME//'regrid_scalar_2d_real32'

      _UNUSED_DUMMY(this)

      q_out = q_in

      _RETURN(_SUCCESS)
      
   end subroutine regrid_scalar_2d_real32


   subroutine regrid_scalar_3d_real32(this, q_in, q_out, rc)
      use MAPL_CommsMod
      use MAPL_BaseMod

      class (IdentityRegridder), intent(in) :: this
      real (kind=REAL32), intent(in) :: q_in(:,:,:)
      real (kind=REAL32), intent(out) :: q_out(:,:,:)
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME//'regrid_scalar_2d_real32'

      _UNUSED_DUMMY(this)

      _ASSERT(size(q_in,3) == size(q_out,3), 'inconsistent array shape')

      q_out = q_in

      _RETURN(_SUCCESS)
      
   end subroutine regrid_scalar_3d_real32

   subroutine regrid_vector_2d_real32(this, u_in, v_in, u_out, v_out, rotate, rc)
      use MAPL_CommsMod
      use MAPL_BaseMod
      use, intrinsic :: iso_fortran_env, only: REAL32
      class (IdentityRegridder), intent(in) :: this
      real(kind=REAL32), intent(in) :: u_in(:,:)
      real(kind=REAL32), intent(in) :: v_in(:,:)
      real(kind=REAL32), intent(out) :: u_out(:,:)
      real(kind=REAL32), intent(out) :: v_out(:,:)
      logical, optional, intent(in) :: rotate
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME//'regrid_vector_3d_real32'

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(rotate)

      u_out = u_in
      v_out = v_in

     _RETURN(_SUCCESS)

   end subroutine regrid_vector_2d_real32


   subroutine regrid_vector_3d_real32(this, u_in, v_in, u_out, v_out, rotate, rc)
      use MAPL_CommsMod
      use MAPL_BaseMod
      use, intrinsic :: iso_fortran_env, only: REAL32
      class (IdentityRegridder), intent(in) :: this
      real(kind=REAL32), intent(in) :: u_in(:,:,:)
      real(kind=REAL32), intent(in) :: v_in(:,:,:)
      real(kind=REAL32), intent(out) :: u_out(:,:,:)
      real(kind=REAL32), intent(out) :: v_out(:,:,:)
      logical, optional, intent(in) :: rotate
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME//'regrid_vector_3d_real32'

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(rotate)

      _ASSERT(size(u_in,3) == size(u_out,3), 'inconsistent array shape')
      _ASSERT(size(v_in,3) == size(v_out,3), 'inconsistent array shape')
      _ASSERT(size(u_in,3) == size(v_in,3), 'inconsistent array shape')

      u_out = u_in
      v_out = v_in

     _RETURN(_SUCCESS)

   end subroutine regrid_vector_3d_real32

!$$
!$$   function clone(this)
!$$      class (AbstractRegridder), allocatable :: clone
!$$      class (IdentityRegridder), intent(in) :: this
!$$
!$$      ! We just need the type - not the details, so we copy an empty object.
!$$      type (IdentityRegridder) :: foo
!$$
!$$      allocate(clone, source=foo)
!$$
!$$   end function clone
!$$
   ! do nothing
   subroutine initialize_subclass(this, unusable, rc)
      use MAPL_KeywordEnforcerMod
      use MAPL_RegridderSpec
      use MAPL_BaseMod, only: MAPL_GridGet
      class (IdentityRegridder), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = 'initialize_subclass'

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(rc)

   end subroutine initialize_subclass
   
end module MAPL_IdentityRegridderMod
