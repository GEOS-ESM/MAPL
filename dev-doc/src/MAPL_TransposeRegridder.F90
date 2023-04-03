#include "MAPL_Generic.h"

module MAPL_TransposeRegridderMod
  use MAPL_AbstractRegridderMod
  use mapl_KeywordEnforcerMod
  use mapl_RegridderSpec
  use mapl_RegridMethods
  use mapl_ErrorHandlingMod
  use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
  use ESMF
  implicit none
  private

  public :: TransposeRegridder

  type, extends(AbstractRegridder) :: TransposeRegridder
     class (AbstractRegridder), pointer :: reference
  contains
      procedure :: initialize_subclass
      procedure :: regrid_scalar_2d_real32
      procedure :: regrid_scalar_2d_real64
      procedure :: regrid_scalar_3d_real32
      procedure :: regrid_scalar_3d_real64

      procedure :: regrid_vector_2d_real32
      procedure :: regrid_vector_2d_real64
      procedure :: regrid_vector_3d_real32
      procedure :: regrid_vector_3d_real64

      procedure :: regrid_esmf_fields_scalar
      procedure :: regrid_esmf_fields_vector

      procedure :: transpose_regrid_scalar_2d_real32
      procedure :: transpose_regrid_scalar_2d_real64
      procedure :: transpose_regrid_scalar_3d_real32
      procedure :: transpose_regrid_scalar_3d_real64

      procedure :: transpose_regrid_vector_2d_real32
      procedure :: transpose_regrid_vector_2d_real64
      procedure :: transpose_regrid_vector_3d_real32
      procedure :: transpose_regrid_vector_3d_real64

      procedure :: transpose_regrid_esmf_fields_scalar
      procedure :: transpose_regrid_esmf_fields_vector

      procedure :: get_spec
      procedure :: isTranspose
   end type TransposeRegridder

   interface TransposeRegridder
      module procedure new_TransposeRegridder
   end interface TransposeRegridder

   character(len=*), parameter :: MOD_NAME = 'MAPL_AbstractRegridder::'

contains

  function new_TransposeRegridder(reference) result(regridder)
     type (TransposeRegridder) :: regridder
     class (AbstractRegridder), target, intent(in) :: reference

     regridder%reference => reference

   end function new_TransposeRegridder


   subroutine initialize_subclass(this, unusable, rc)
     use MAPL_KeywordEnforcerMod
     class (TransposeRegridder), intent(inout) :: this
     class (KeywordEnforcer), optional, intent(in) :: unusable
     integer, optional, intent(out) :: rc

     _UNUSED_DUMMY(this)
     _UNUSED_DUMMY(unusable)

     ! This is a wrapper class and should not be directly
     ! initialized.
     _RETURN(_FAILURE)
   end subroutine initialize_subclass
         



   subroutine regrid_scalar_2d_real32(this, q_in, q_out, rc)
      class (TransposeRegridder), intent(in) :: this
      real(kind=REAL32), intent(in) :: q_in(:,:)
      real(kind=REAL32), intent(out) :: q_out(:,:)
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: Iam = MOD_NAME//'regrid_scalar_2d_real32'

      integer :: status
      call this%reference%transpose_regrid(q_in, q_out, rc=status)
      _RETURN(status)

   end subroutine regrid_scalar_2d_real32


   subroutine regrid_scalar_2d_real64(this, q_in, q_out, rc)
      class (TransposeRegridder), intent(in) :: this
      real(kind=REAL64), intent(in) :: q_in(:,:)
      real(kind=REAL64), intent(out) :: q_out(:,:)
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: Iam = MOD_NAME//'regrid_scalar_2d_real64'

      integer :: status
      call this%reference%transpose_regrid(q_in, q_out, rc=status)
      _RETURN(status)

   end subroutine regrid_scalar_2d_real64


   subroutine regrid_scalar_3d_real32(this, q_in, q_out, rc)
      class (TransposeRegridder), intent(in) :: this
      real(kind=REAL32), intent(in) :: q_in(:,:,:)
      real(kind=REAL32), intent(out) :: q_out(:,:,:)
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: Iam = MOD_NAME//'regrid_scalar_3d_real32'

      integer :: status
      call this%reference%transpose_regrid(q_in, q_out, rc=status)
      _RETURN(status)

   end subroutine regrid_scalar_3d_real32


   subroutine regrid_scalar_3d_real64(this, q_in, q_out, rc)
      class (TransposeRegridder), intent(in) :: this
      real(kind=REAL64), intent(in) :: q_in(:,:,:)
      real(kind=REAL64), intent(out) :: q_out(:,:,:)
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: Iam = MOD_NAME//'regrid_scalar_3d_real64'

      integer :: status
      call this%reference%transpose_regrid(q_in, q_out, rc=status)
      _RETURN(status)

   end subroutine regrid_scalar_3d_real64


   subroutine regrid_vector_2d_real32(this, u_in, v_in, u_out, v_out, rotate, rc)
      use, intrinsic :: iso_fortran_env, only: REAL32
      class (TransposeRegridder), intent(in) :: this
      real(kind=REAL32), intent(in) :: u_in(:,:)
      real(kind=REAL32), intent(in) :: v_in(:,:)
      real(kind=REAL32), intent(out) :: u_out(:,:)
      real(kind=REAL32), intent(out) :: v_out(:,:)
      logical, optional, intent(in) :: rotate
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: Iam = MOD_NAME//'regrid_vector_2d_real32'

      integer :: status

      _UNUSED_DUMMY(rotate)

      call this%reference%transpose_regrid(u_in, v_in, u_out, v_out, rc=status)
      _RETURN(status)

   end subroutine regrid_vector_2d_real32


   subroutine regrid_vector_2d_real64(this, u_in, v_in, u_out, v_out, rotate, rc)
      use, intrinsic :: iso_fortran_env, only: REAL64
      class (TransposeRegridder), intent(in) :: this
      real(kind=REAL64), intent(in) :: u_in(:,:)
      real(kind=REAL64), intent(in) :: v_in(:,:)
      real(kind=REAL64), intent(out) :: u_out(:,:)
      real(kind=REAL64), intent(out) :: v_out(:,:)
      logical, optional, intent(in) :: rotate
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: Iam = MOD_NAME//'regrid_vector_2d_real64'

      integer :: status

      _UNUSED_DUMMY(rotate)

      call this%reference%transpose_regrid(u_in, v_in, u_out, v_out, rc=status)
      _RETURN(status)

   end subroutine regrid_vector_2d_real64

   subroutine regrid_vector_3d_real32(this, u_in, v_in, u_out, v_out, rotate, rc)
      use, intrinsic :: iso_fortran_env, only: REAL32
      class (TransposeRegridder), intent(in) :: this
      real(kind=REAL32), intent(in) :: u_in(:,:,:)
      real(kind=REAL32), intent(in) :: v_in(:,:,:)
      real(kind=REAL32), intent(out) :: u_out(:,:,:)
      real(kind=REAL32), intent(out) :: v_out(:,:,:)
      logical, optional, intent(in) :: rotate
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: Iam = MOD_NAME//'regrid_vector_3d_real32'

      integer :: status

      _UNUSED_DUMMY(rotate)

      call this%reference%transpose_regrid(u_in, v_in, u_out, v_out, rotate=rotate, rc=status)
      _RETURN(status)

   end subroutine regrid_vector_3d_real32


   subroutine regrid_vector_3d_real64(this, u_in, v_in, u_out, v_out, rc)
      use, intrinsic :: iso_fortran_env, only: REAL64
      class (TransposeRegridder), intent(in) :: this
      real(kind=REAL64), intent(in) :: u_in(:,:,:)
      real(kind=REAL64), intent(in) :: v_in(:,:,:)
      real(kind=REAL64), intent(out) :: u_out(:,:,:)
      real(kind=REAL64), intent(out) :: v_out(:,:,:)
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: Iam = MOD_NAME//'regrid_vector_3d_real64'

      integer :: status
      call this%reference%transpose_regrid(u_in, v_in, u_out, v_out, rc=status)
      _RETURN(status)

   end subroutine regrid_vector_3d_real64


   subroutine regrid_esmf_fields_scalar(this, f_in, f_out, rc)
      use esmf, only: ESMF_TypeKind_Flag
      use esmf, only: ESMF_TYPEKIND_R4
      use esmf, only: ESMF_TYPEKIND_R8
      use esmf, only: ESMF_Field
      use esmf, only: ESMF_FieldGet
      class (TransposeRegridder), intent(in) :: this
      type (ESMF_Field), intent(in) :: f_in
      type (ESMF_Field), intent(in) :: f_out
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: Iam = MOD_NAME//'regrid_esmf_fields'

      integer :: status
      call this%reference%transpose_regrid(f_in, f_out, rc=status)
      _RETURN(status)
      
   end subroutine regrid_esmf_fields_scalar


   subroutine regrid_esmf_fields_vector(this, f_in, f_out, rc)
      use esmf, only: ESMF_TypeKind_Flag
      use esmf, only: ESMF_TYPEKIND_R4
      use esmf, only: ESMF_TYPEKIND_R8
      use esmf, only: ESMF_Field
      use esmf, only: ESMF_FieldGet
      integer, parameter :: NUM_DIM = 2
      class (TransposeRegridder), intent(in) :: this
      type (ESMF_Field), intent(in) :: f_in(NUM_DIM)
      type (ESMF_Field), intent(in) :: f_out(NUM_DIM)
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: Iam = MOD_NAME//'regrid_esmf_fields'

      integer :: status
      call this%reference%transpose_regrid(f_in, f_out, rc=status)
      _RETURN(status)

   end subroutine regrid_esmf_fields_vector


   ! Begin - transpose interfaces
   
   subroutine transpose_regrid_scalar_2d_real32(this, q_in, q_out, rc)
      use, intrinsic :: iso_fortran_env, only: REAL32
      class (TransposeRegridder), intent(in) :: this
      real(kind=REAL32), intent(in) :: q_in(:,:)
      real(kind=REAL32), intent(out) :: q_out(:,:)
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: Iam = MOD_NAME//'transpose_regrid_scalar_2d_real32'

      integer :: status
      call this%reference%regrid(q_in, q_out, rc=status)
      _RETURN(status)

   end subroutine transpose_regrid_scalar_2d_real32


   subroutine transpose_regrid_scalar_2d_real64(this, q_in, q_out, rc)
      use, intrinsic :: iso_fortran_env, only: REAL64
      class (TransposeRegridder), intent(in) :: this
      real(kind=REAL64), intent(in) :: q_in(:,:)
      real(kind=REAL64), intent(out) :: q_out(:,:)
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: Iam = MOD_NAME//'transpose_regrid_scalar_2d_real64'

      integer :: status
      call this%reference%regrid(q_in, q_out, rc=status)
      _RETURN(status)

   end subroutine transpose_regrid_scalar_2d_real64

   
   subroutine transpose_regrid_scalar_3d_real32(this, q_in, q_out, rc)
      use, intrinsic :: iso_fortran_env, only: REAL32
      class (TransposeRegridder), intent(in) :: this
      real(kind=REAL32), intent(in) :: q_in(:,:,:)
      real(kind=REAL32), intent(out) :: q_out(:,:,:)
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: Iam = MOD_NAME//'transpose_regrid_scalar_3d_real32'

      integer :: status
      call this%reference%regrid(q_in, q_out, rc=status)
      _RETURN(status)

   end subroutine transpose_regrid_scalar_3d_real32


   subroutine transpose_regrid_scalar_3d_real64(this, q_in, q_out, rc)
      use, intrinsic :: iso_fortran_env, only: REAL64
      class (TransposeRegridder), intent(in) :: this
      real(kind=REAL64), intent(in) :: q_in(:,:,:)
      real(kind=REAL64), intent(out) :: q_out(:,:,:)
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: Iam = MOD_NAME//'transpose_regrid_scalar_3d_real64'

      integer :: status
      call this%reference%regrid(q_in, q_out, rc=status)
      _RETURN(status)

   end subroutine transpose_regrid_scalar_3d_real64

   
   subroutine transpose_regrid_vector_2d_real32(this, u_in, v_in, u_out, v_out, rotate, rc)
      use, intrinsic :: iso_fortran_env, only: REAL32
      class (TransposeRegridder), intent(in) :: this
      real(kind=REAL32), intent(in) :: u_in(:,:)
      real(kind=REAL32), intent(in) :: v_in(:,:)
      real(kind=REAL32), intent(out) :: u_out(:,:)
      real(kind=REAL32), intent(out) :: v_out(:,:)
      logical, optional, intent(in) :: rotate
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME//'transpose_regrid_vector_2d_real32'

      integer :: status

      _UNUSED_DUMMY(rotate)

      call this%reference%regrid(u_in, v_in, u_out, v_out, rc=status)
      _RETURN(status)

   end subroutine transpose_regrid_vector_2d_real32


   subroutine transpose_regrid_vector_2d_real64(this, u_in, v_in, u_out, v_out, rotate, rc)
      use, intrinsic :: iso_fortran_env, only: REAL64
      class (TransposeRegridder), intent(in) :: this
      real(kind=REAL64), intent(in) :: u_in(:,:)
      real(kind=REAL64), intent(in) :: v_in(:,:)
      real(kind=REAL64), intent(out) :: u_out(:,:)
      real(kind=REAL64), intent(out) :: v_out(:,:)
      logical, optional, intent(in) :: rotate
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: Iam = MOD_NAME//'transpose_regrid_vector_2d_real64'

      integer :: status

      _UNUSED_DUMMY(rotate)

      call this%reference%regrid(u_in, v_in, u_out, v_out, rc=status)
      _RETURN(status)

   end subroutine transpose_regrid_vector_2d_real64


   subroutine transpose_regrid_vector_3d_real32(this, u_in, v_in, u_out, v_out, rotate, rc)
      use, intrinsic :: iso_fortran_env, only: REAL32
      class (TransposeRegridder), intent(in) :: this
      real(kind=REAL32), intent(in) :: u_in(:,:,:)
      real(kind=REAL32), intent(in) :: v_in(:,:,:)
      real(kind=REAL32), intent(out) :: u_out(:,:,:)
      real(kind=REAL32), intent(out) :: v_out(:,:,:)
      logical, optional, intent(in) :: rotate
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME//'transpose_regrid_vector_3d_real32'

      integer :: status

      _UNUSED_DUMMY(rotate)

      call this%reference%regrid(u_in, v_in, u_out, v_out, rotate=rotate, rc=status)
      _RETURN(status)

   end subroutine transpose_regrid_vector_3d_real32


   subroutine transpose_regrid_vector_3d_real64(this, u_in, v_in, u_out, v_out, rc)
      use, intrinsic :: iso_fortran_env, only: REAL64
      class (TransposeRegridder), intent(in) :: this
      real(kind=REAL64), intent(in) :: u_in(:,:,:)
      real(kind=REAL64), intent(in) :: v_in(:,:,:)
      real(kind=REAL64), intent(out) :: u_out(:,:,:)
      real(kind=REAL64), intent(out) :: v_out(:,:,:)
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: Iam = MOD_NAME//'transpose_regrid_vector_3d_real64'

      integer :: status
      call this%reference%regrid(u_in, v_in, u_out, v_out, rc=status)
      _RETURN(status)

   end subroutine transpose_regrid_vector_3d_real64


   subroutine transpose_regrid_esmf_fields_scalar(this, f_in, f_out, rc)
      use esmf, only: ESMF_TypeKind_Flag
      use esmf, only: ESMF_TYPEKIND_R4
      use esmf, only: ESMF_TYPEKIND_R8
      use esmf, only: ESMF_Field
      use esmf, only: ESMF_FieldGet
      class (TransposeRegridder), intent(in) :: this
      type (ESMF_Field), intent(in) :: f_in
      type (ESMF_Field), intent(in) :: f_out
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: Iam = MOD_NAME//'transpose_regrid_esmf_fields'

      integer :: status
      call this%reference%regrid(f_in, f_out, rc=status)
      _RETURN(status)

   end subroutine transpose_regrid_esmf_fields_scalar


   subroutine transpose_regrid_esmf_fields_vector(this, f_in, f_out, rc)
      use esmf, only: ESMF_TypeKind_Flag
      use esmf, only: ESMF_TYPEKIND_R4
      use esmf, only: ESMF_TYPEKIND_R8
      use esmf, only: ESMF_Field
      use esmf, only: ESMF_FieldGet
      integer, parameter :: NUM_DIM = 2
      class (TransposeRegridder), intent(in) :: this
      type (ESMF_Field), intent(in) :: f_in(NUM_DIM)
      type (ESMF_Field), intent(in) :: f_out(NUM_DIM)
      integer, optional, intent(out) :: rc
      
      character(len=*), parameter :: Iam = MOD_NAME//'transpose_regrid_esmf_fields'

      integer :: status
      call this%reference%regrid(f_in, f_out, rc=status)
      _RETURN(status)

   end subroutine transpose_regrid_esmf_fields_vector

   function get_spec(this) result(spec)
      type (RegridderSpec) :: spec
      class (TransposeRegridder), intent(in) :: this
      spec = this%reference%get_spec()
   end function get_spec

   function isTranspose(this) result(amTranspose)
      logical :: amTranspose
      class (TransposeRegridder), intent(in) :: this
      _UNUSED_DUMMY(this)
      amTranspose = .true.
   end function isTranspose
  
end module MAPL_TransposeRegridderMod
