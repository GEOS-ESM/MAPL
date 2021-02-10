#include "MAPL_Generic.h"

module MAPL_AbstractRegridderMod
   use MAPL_BaseMod, only: MAPL_UNDEF
   use mapl_RegridderSpec
   use mapl_KeywordEnforcerMod
   use ESMF
   use MAPL_MemUtilsMod
   use MAPL_ExceptionHandling
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   implicit none
   private
   
   public :: AbstractRegridder

   type, abstract :: AbstractRegridder
      private
      type (RegridderSpec) :: spec
      real :: undef_value = MAPL_UNDEF
      logical :: has_undef_value_ = .false.
   contains
      procedure :: clone

      procedure, nopass :: supports
      procedure :: initialize_base
      procedure(initialize_subclass), deferred :: initialize_subclass
      generic :: initialize => initialize_base, initialize_subclass
      procedure :: get_spec
      procedure :: set_spec
      procedure :: isTranspose
      
      procedure :: regrid_scalar_2d_real32
      procedure :: regrid_scalar_2d_real64
      procedure :: regrid_scalar_3d_real32
      procedure :: regrid_scalar_3d_real64

      procedure :: regrid_vector_2d_real32
      procedure :: regrid_vector_2d_real64
      procedure :: regrid_vector_3d_real32
      procedure :: regrid_vector_3d_real64

      ! ESMF interface is full generic and simply unpacks into the other
      ! interfaces.
      procedure :: regrid_esmf_fields_scalar
      procedure :: regrid_esmf_fields_vector
      
      ! Generic overload
      generic :: regrid => regrid_esmf_fields_scalar
      generic :: regrid => regrid_esmf_fields_vector
      generic :: regrid => regrid_scalar_2d_real32
      generic :: regrid => regrid_scalar_2d_real64
      generic :: regrid => regrid_scalar_3d_real32
      generic :: regrid => regrid_scalar_3d_real64
      generic :: regrid => regrid_vector_2d_real32
      generic :: regrid => regrid_vector_2d_real64
      generic :: regrid => regrid_vector_3d_real32
      generic :: regrid => regrid_vector_3d_real64


      ! Transpose methods
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

      ! Generic overload
      generic :: transpose_regrid => transpose_regrid_esmf_fields_scalar
      generic :: transpose_regrid => transpose_regrid_esmf_fields_vector
      generic :: transpose_regrid => transpose_regrid_scalar_2d_real32
      generic :: transpose_regrid => transpose_regrid_scalar_2d_real64
      generic :: transpose_regrid => transpose_regrid_scalar_3d_real32
      generic :: transpose_regrid => transpose_regrid_scalar_3d_real64
      generic :: transpose_regrid => transpose_regrid_vector_2d_real32
      generic :: transpose_regrid => transpose_regrid_vector_2d_real64
      generic :: transpose_regrid => transpose_regrid_vector_3d_real32
      generic :: transpose_regrid => transpose_regrid_vector_3d_real64


      ! Setters/getters
      procedure :: set_undef_value
      procedure :: get_undef_value
      procedure :: clear_undef_value
      procedure :: has_undef_value

   end type AbstractRegridder


   abstract interface

      subroutine initialize_subclass(this, unusable, rc)
         use MAPL_KeywordEnforcerMod
         use MAPL_RegridderSpec
         import AbstractRegridder
         implicit none
         class (AbstractRegridder), intent(inout) :: this
         class (KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end subroutine initialize_subclass
         
   end interface

   character(len=*), parameter :: MOD_NAME = 'MAPL_AbstractRegridder::'

contains


   subroutine regrid_scalar_2d_real32(this, q_in, q_out, rc)
      class (AbstractRegridder), intent(in) :: this
      real(kind=REAL32), intent(in) :: q_in(:,:)
      real(kind=REAL32), intent(out) :: q_out(:,:)
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME//'regrid_scalar_2d_real32'
      _ASSERT(.false., 'unimplemented - must override in subclass')
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(q_in)
      q_out = 0
      _RETURN(_FAILURE)

   end subroutine regrid_scalar_2d_real32


   subroutine regrid_scalar_2d_real64(this, q_in, q_out, rc)
      class (AbstractRegridder), intent(in) :: this
      real(kind=REAL64), intent(in) :: q_in(:,:)
      real(kind=REAL64), intent(out) :: q_out(:,:)
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: Iam = MOD_NAME//'regrid_scalar_2d_real64'

      _ASSERT(.false., 'unimplemented - must override in subclass')
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(q_in)
      q_out = 0
      _RETURN(_FAILURE)

   end subroutine regrid_scalar_2d_real64


   subroutine regrid_scalar_3d_real32(this, q_in, q_out, rc)
      class (AbstractRegridder), intent(in) :: this
      real(kind=REAL32), intent(in) :: q_in(:,:,:)
      real(kind=REAL32), intent(out) :: q_out(:,:,:)
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: Iam = MOD_NAME//'regrid_scalar_3d_real32'

      _ASSERT(.false., 'unimplemented - must override in subclass')
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(q_in)
      q_out = 0
      _RETURN(_FAILURE)

   end subroutine regrid_scalar_3d_real32


   subroutine regrid_scalar_3d_real64(this, q_in, q_out, rc)
      class (AbstractRegridder), intent(in) :: this
      real(kind=REAL64), intent(in) :: q_in(:,:,:)
      real(kind=REAL64), intent(out) :: q_out(:,:,:)
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: Iam = MOD_NAME//'regrid_scalar_3d_real64'

      _ASSERT(.false., 'unimplemented - must override in subclass')
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(q_in)
      q_out = 0
      _RETURN(_FAILURE)

   end subroutine regrid_scalar_3d_real64


   subroutine regrid_vector_2d_real32(this, u_in, v_in, u_out, v_out, rotate, rc)
      class (AbstractRegridder), intent(in) :: this
      real(kind=REAL32), intent(in) :: u_in(:,:)
      real(kind=REAL32), intent(in) :: v_in(:,:)
      real(kind=REAL32), intent(out) :: u_out(:,:)
      real(kind=REAL32), intent(out) :: v_out(:,:)
      logical, optional, intent(in) :: rotate
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: Iam = MOD_NAME//'regrid_vector_2d_real32'

      _ASSERT(.false., 'unimplemented - must override in subclass')
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(u_in)
      _UNUSED_DUMMY(v_in)
      _UNUSED_DUMMY(rotate)
      u_out = 0
      v_out = 0
      _RETURN(_FAILURE)

   end subroutine regrid_vector_2d_real32


   subroutine regrid_vector_2d_real64(this, u_in, v_in, u_out, v_out, rotate, rc)
      class (AbstractRegridder), intent(in) :: this
      real(kind=REAL64), intent(in) :: u_in(:,:)
      real(kind=REAL64), intent(in) :: v_in(:,:)
      real(kind=REAL64), intent(out) :: u_out(:,:)
      real(kind=REAL64), intent(out) :: v_out(:,:)
      logical, optional, intent(in) :: rotate
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: Iam = MOD_NAME//'regrid_vector_2d_real64'

      _ASSERT(.false., 'unimplemented - must override in subclass')
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(u_in)
      _UNUSED_DUMMY(v_in)
      _UNUSED_DUMMY(rotate)
      u_out = 0
      v_out = 0
      _RETURN(_FAILURE)

   end subroutine regrid_vector_2d_real64

   subroutine regrid_vector_3d_real32(this, u_in, v_in, u_out, v_out, rotate, rc)
      class (AbstractRegridder), intent(in) :: this
      real(kind=REAL32), intent(in) :: u_in(:,:,:)
      real(kind=REAL32), intent(in) :: v_in(:,:,:)
      real(kind=REAL32), intent(out) :: u_out(:,:,:)
      real(kind=REAL32), intent(out) :: v_out(:,:,:)
      logical, optional, intent(in) :: rotate
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: Iam = MOD_NAME//'regrid_vector_3d_real32'

      _ASSERT(.false., 'unimplemented - must override in subclass')
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(u_in)
      _UNUSED_DUMMY(v_in)
      _UNUSED_DUMMY(rotate)
      u_out = 0
      v_out = 0
      _RETURN(_FAILURE)

   end subroutine regrid_vector_3d_real32


   subroutine regrid_vector_3d_real64(this, u_in, v_in, u_out, v_out, rc)
      class (AbstractRegridder), intent(in) :: this
      real(kind=REAL64), intent(in) :: u_in(:,:,:)
      real(kind=REAL64), intent(in) :: v_in(:,:,:)
      real(kind=REAL64), intent(out) :: u_out(:,:,:)
      real(kind=REAL64), intent(out) :: v_out(:,:,:)
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: Iam = MOD_NAME//'regrid_vector_3d_real64'

      _ASSERT(.false., 'unimplemented - must override in subclass')
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(u_in)
      _UNUSED_DUMMY(v_in)
      u_out = 0
      v_out = 0
      _RETURN(_FAILURE)

   end subroutine regrid_vector_3d_real64


   subroutine regrid_esmf_fields_scalar(this, f_in, f_out, rc)
      use esmf, only: ESMF_TypeKind_Flag
      use esmf, only: ESMF_TYPEKIND_R4
      use esmf, only: ESMF_TYPEKIND_R8
      use esmf, only: ESMF_Field
      use esmf, only: ESMF_FieldGet
      class (AbstractRegridder), intent(in) :: this
      type (ESMF_Field), intent(in) :: f_in
      type (ESMF_Field), intent(in) :: f_out
      integer, optional, intent(out) :: rc
      
      character(len=*), parameter :: Iam = MOD_NAME//'regrid_esmf_fields'
      integer :: rank_in
      type (ESMF_TypeKind_Flag) :: typekind_in
      integer :: rank_out
      type (ESMF_TypeKind_Flag) :: typekind_out
      integer :: status

      call ESMF_FieldGet(f_in, rank=rank_in, typekind=typekind_in, rc=status)
      _VERIFY(status)
      call ESMF_FieldGet(f_out, rank=rank_out, typekind=typekind_out, rc=status)
      _VERIFY(status)

      _ASSERT(rank_in == rank_out, 'in-out rank mismatch')
      _ASSERT(typekind_in%dkind == typekind_out%dkind, 'in-out typekind mismatch')

      select case (rank_in)

      case (2)

         select case (typekind_in%dkind)

         case (ESMF_TYPEKIND_R4%dkind)

            block
              real(REAL32), pointer :: q_in(:,:), q_out(:,:)
              
              call ESMF_FieldGet(f_in , 0, q_in, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_out , 0, q_out, rc=status)
              _VERIFY(status)
              call this%regrid(q_in, q_out, rc=status)
              _VERIFY(status)
            end block

         case (ESMF_TYPEKIND_R8%dkind)

            block
              real(REAL64), pointer :: q_in(:,:), q_out(:,:)
              
              call ESMF_FieldGet(f_in , 0, q_in, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_out , 0, q_out, rc=status)
              _VERIFY(status)
              call this%regrid(q_in, q_out, rc=status)
              _VERIFY(status)
            end block

         case default
            _ASSERT(.false., 'unsupported typekind')
         end select

      case (3)

         select case (typekind_in%dkind)
         case (ESMF_TYPEKIND_R4%dkind)
            block
              real(REAL32), pointer :: q_in(:,:,:), q_out(:,:,:)

              call ESMF_FieldGet(f_in , 0, q_in, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_out , 0, q_out, rc=status)
              _VERIFY(status)
              _ASSERT(size(q_in,3) == size(q_out,3), 'array bounds mismatch')
              call this%regrid(q_in, q_out,rc=status)
              _VERIFY(status)
            end block

         case (ESMF_TYPEKIND_R8%dkind)

            block
              real(REAL64), pointer :: q_in(:,:,:), q_out(:,:,:)

              call ESMF_FieldGet(f_in , 0, q_in, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_out , 0, q_out, rc=status)
              _VERIFY(status)
              _ASSERT(size(q_in,3) == size(q_out,3), 'array bounds mismatch')
              call this%regrid(q_in, q_out, rc=status)
              _VERIFY(status)
            end block
         case default ! unsupported type/kind
            _ASSERT(.false., 'unsupported type kind')
         end select
      
      case default ! unsupported rank
         _ASSERT(.false., 'unsupported rank')
      end select

      _RETURN(_SUCCESS)
      
   end subroutine regrid_esmf_fields_scalar


   subroutine regrid_esmf_fields_vector(this, f_in, f_out, rc)
      use esmf, only: ESMF_TypeKind_Flag
      use esmf, only: ESMF_TYPEKIND_R4
      use esmf, only: ESMF_TYPEKIND_R8
      use esmf, only: ESMF_Field
      use esmf, only: ESMF_FieldGet
      integer, parameter :: NUM_DIM = 2
      class (AbstractRegridder), intent(in) :: this
      type (ESMF_Field), intent(in) :: f_in(NUM_DIM)
      type (ESMF_Field), intent(in) :: f_out(NUM_DIM)
      integer, optional, intent(out) :: rc
      
      character(len=*), parameter :: Iam = MOD_NAME//'regrid_esmf_fields'
      integer :: rank_in(NUM_DIM)
      type (ESMF_TypeKind_Flag) :: typekind_in(NUM_DIM)
      integer :: rank_out(NUM_DIM)
      type (ESMF_TypeKind_Flag) :: typekind_out(NUM_DIM)
      integer :: status
      integer :: d

      do d = 1, NUM_DIM
         call ESMF_FieldGet(f_in(d), rank=rank_in(d), typekind=typekind_in(d), rc=status)
         _VERIFY(status)
         call ESMF_FieldGet(f_out(d), rank=rank_out(d), typekind=typekind_out(d), rc=status)
         _VERIFY(status)
      end do

      ! Check consistent type/kind/rank for all arguments
      _ASSERT(all(rank_in == rank_in(1)), 'rank mismatch across input vector components')
      _ASSERT(all(typekind_in%dkind == typekind_in(1)%dkind), 'typekind mismatch across input vector components')
      _ASSERT(all(rank_out == rank_out(1)),'rank mismatch across output vectory components')
      _ASSERT(all(typekind_out%dkind == typekind_out(1)%dkind),'typekind mismatch across output vector coomponents')

      _ASSERT(rank_in(1) == rank_out(1), 'in-out rank mismatch')
      _ASSERT(typekind_in(1)%dkind == typekind_out(1)%dkind,'in-out typekind mismatch')

      select case (rank_in(1))

      case (2)

         select case (typekind_in(1)%dkind)

         case (ESMF_TYPEKIND_R4%dkind)

            block
              real(REAL32), pointer :: u_in(:,:), v_in(:,:)
              real(REAL32), pointer :: u_out(:,:), v_out(:,:)
              
              call ESMF_FieldGet(f_in(1) , 0, u_in, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_in(2) , 0, v_in, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_out(1) , 0, u_out, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_out(2) , 0, v_out, rc=status)
              _VERIFY(status)
              call this%regrid(u_in, v_in, u_out, v_out, rc=status)
              _VERIFY(status)
            end block

         case (ESMF_TYPEKIND_R8%dkind)

            block
              real(REAL64), pointer :: u_in(:,:), v_in(:,:)
              real(REAL64), pointer :: u_out(:,:), v_out(:,:)
              
              call ESMF_FieldGet(f_in(1) , 0, u_in, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_in(2) , 0, v_in, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_out(1) , 0, u_out, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_out(2) , 0, v_out, rc=status)
              _VERIFY(status)
              call this%regrid(u_in, v_in, u_out, v_out, rc=status)
              _VERIFY(status)
            end block

         case default ! unsupported typekind
            _ASSERT(.false., 'unsupported typekind')
         end select

      case (3)

         select case (typekind_in(1)%dkind)
         case (ESMF_TYPEKIND_R4%dkind)
            block
              real(REAL32), pointer :: u_in(:,:,:), v_in(:,:,:)
              real(REAL32), pointer :: u_out(:,:,:), v_out(:,:,:)
              
              call ESMF_FieldGet(f_in(1) , 0, u_in, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_in(2) , 0, v_in, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_out(1) , 0, u_out, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_out(2) , 0, v_out, rc=status)
              _VERIFY(status)
              call this%regrid(u_in, v_in, u_out, v_out, rc=status)
              _VERIFY(status)
            end block

         case (ESMF_TYPEKIND_R8%dkind)

            block
              real(REAL64), pointer :: u_in(:,:,:), v_in(:,:,:)
              real(REAL64), pointer :: u_out(:,:,:), v_out(:,:,:)
              
              call ESMF_FieldGet(f_in(1) , 0, u_in, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_in(2) , 0, v_in, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_out(1) , 0, u_out, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_out(2) , 0, v_out, rc=status)
              _VERIFY(status)
              call this%regrid(u_in, v_in, u_out, v_out, rc=status)
              _VERIFY(status)
            end block

         case default ! unsupported type/kind
            _ASSERT(.false., 'unsupported type-kind')
         end select
      
      case default ! unsupported rank
         _ASSERT(.false., 'unsupported rank')
      end select

      _RETURN(_SUCCESS)
      

   end subroutine regrid_esmf_fields_vector


   ! Begin - transpose interfaces
   
   subroutine transpose_regrid_scalar_2d_real32(this, q_in, q_out, rc)
      class (AbstractRegridder), intent(in) :: this
      real(kind=REAL32), intent(in) :: q_in(:,:)
      real(kind=REAL32), intent(out) :: q_out(:,:)
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME//'transpose_regrid_scalar_2d_real32'
      _ASSERT(.false., 'unimplemented - must override in subclass')
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(q_in)
      q_out = 0
      _RETURN(_FAILURE)
   end subroutine transpose_regrid_scalar_2d_real32


   subroutine transpose_regrid_scalar_2d_real64(this, q_in, q_out, rc)
      class (AbstractRegridder), intent(in) :: this
      real(kind=REAL64), intent(in) :: q_in(:,:)
      real(kind=REAL64), intent(out) :: q_out(:,:)
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME//'transpose_regrid_scalar_2d_real64'
      _ASSERT(.false., 'unimplemented - must override in subclass')
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(q_in)
      q_out = 0
      _RETURN(_FAILURE)
   end subroutine transpose_regrid_scalar_2d_real64

   
   subroutine transpose_regrid_scalar_3d_real32(this, q_in, q_out, rc)
      class (AbstractRegridder), intent(in) :: this
      real(kind=REAL32), intent(in) :: q_in(:,:,:)
      real(kind=REAL32), intent(out) :: q_out(:,:,:)
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME//'transpose_regrid_scalar_3d_real32'
      _ASSERT(.false., 'unimplemented - must override in subclass')
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(q_in)
      q_out = 0
      _RETURN(_FAILURE)

   end subroutine transpose_regrid_scalar_3d_real32


   subroutine transpose_regrid_scalar_3d_real64(this, q_in, q_out, rc)
      class (AbstractRegridder), intent(in) :: this
      real(kind=REAL64), intent(in) :: q_in(:,:,:)
      real(kind=REAL64), intent(out) :: q_out(:,:,:)
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME//'transpose_regrid_scalar_3d_real64'
      _ASSERT(.false., 'unimplemented - must override in subclass')
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(q_in)
      q_out = 0
      _RETURN(_FAILURE)

   end subroutine transpose_regrid_scalar_3d_real64

   
   subroutine transpose_regrid_vector_2d_real32(this, u_in, v_in, u_out, v_out, rotate, rc)
      class (AbstractRegridder), intent(in) :: this
      real(kind=REAL32), intent(in) :: u_in(:,:)
      real(kind=REAL32), intent(in) :: v_in(:,:)
      real(kind=REAL32), intent(out) :: u_out(:,:)
      real(kind=REAL32), intent(out) :: v_out(:,:)
      logical, optional, intent(in) :: rotate
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME//'transpose_regrid_vector_2d_real32'

      _ASSERT(.false., 'unimplemented - must override in subclass')
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(u_in)
      _UNUSED_DUMMY(v_in)
      _UNUSED_DUMMY(u_out)
      _UNUSED_DUMMY(v_out)
      _UNUSED_DUMMY(rotate)
      u_out = 0
      v_out = 0
      _RETURN(_FAILURE)
      
   end subroutine transpose_regrid_vector_2d_real32


   subroutine transpose_regrid_vector_2d_real64(this, u_in, v_in, u_out, v_out, rotate, rc)
      class (AbstractRegridder), intent(in) :: this
      real(kind=REAL64), intent(in) :: u_in(:,:)
      real(kind=REAL64), intent(in) :: v_in(:,:)
      real(kind=REAL64), intent(out) :: u_out(:,:)
      real(kind=REAL64), intent(out) :: v_out(:,:)
      logical, optional, intent(in) :: rotate
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME//'transpose_regrid_vector_2d_real64'

      _ASSERT(.false., 'unimplemented - must override in subclass')
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(u_in)
      _UNUSED_DUMMY(v_in)
      _UNUSED_DUMMY(u_out)
      _UNUSED_DUMMY(v_out)
      _UNUSED_DUMMY(rotate)
      u_out = 0
      v_out = 0
      _RETURN(_FAILURE)
      
   end subroutine transpose_regrid_vector_2d_real64


   subroutine transpose_regrid_vector_3d_real32(this, u_in, v_in, u_out, v_out, rotate, rc)
      class (AbstractRegridder), intent(in) :: this
      real(kind=REAL32), intent(in) :: u_in(:,:,:)
      real(kind=REAL32), intent(in) :: v_in(:,:,:)
      real(kind=REAL32), intent(out) :: u_out(:,:,:)
      real(kind=REAL32), intent(out) :: v_out(:,:,:)
      logical, optional, intent(in) :: rotate
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME//'transpose_regrid_vector_3d_real32'

      _ASSERT(.false., 'unimplemented - must override in subclass')
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(u_in)
      _UNUSED_DUMMY(v_in)
      _UNUSED_DUMMY(u_out)
      _UNUSED_DUMMY(v_out)
      _UNUSED_DUMMY(rotate)
      u_out = 0
      v_out = 0
      _RETURN(_FAILURE)
      
   end subroutine transpose_regrid_vector_3d_real32


   subroutine transpose_regrid_vector_3d_real64(this, u_in, v_in, u_out, v_out, rc)
      class (AbstractRegridder), intent(in) :: this
      real(kind=REAL64), intent(in) :: u_in(:,:,:)
      real(kind=REAL64), intent(in) :: v_in(:,:,:)
      real(kind=REAL64), intent(out) :: u_out(:,:,:)
      real(kind=REAL64), intent(out) :: v_out(:,:,:)
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME//'transpose_regrid_vector_3d_real64'

      _ASSERT(.false., 'unimplemented - must override in subclass')
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(u_in)
      _UNUSED_DUMMY(v_in)
      _UNUSED_DUMMY(u_out)
      _UNUSED_DUMMY(v_out)
      u_out = 0
      v_out = 0
      _RETURN(_FAILURE)
      
   end subroutine transpose_regrid_vector_3d_real64


   subroutine transpose_regrid_esmf_fields_scalar(this, f_in, f_out, rc)
      use esmf, only: ESMF_TypeKind_Flag
      use esmf, only: ESMF_TYPEKIND_R4
      use esmf, only: ESMF_TYPEKIND_R8
      use esmf, only: ESMF_Field
      use esmf, only: ESMF_FieldGet
      class (AbstractRegridder), intent(in) :: this
      type (ESMF_Field), intent(in) :: f_in
      type (ESMF_Field), intent(in) :: f_out
      integer, optional, intent(out) :: rc
      
      character(len=*), parameter :: Iam = MOD_NAME//'transpose_regrid_esmf_fields'
      integer :: rank_in
      type (ESMF_TypeKind_Flag) :: typekind_in
      integer :: rank_out
      type (ESMF_TypeKind_Flag) :: typekind_out
      integer :: status

      call ESMF_FieldGet(f_in, rank=rank_in, typekind=typekind_in, rc=status)
      _VERIFY(status)
      call ESMF_FieldGet(f_out, rank=rank_out, typekind=typekind_out, rc=status)
      _VERIFY(status)

      _ASSERT(rank_in == rank_out, 'in-out rank mismatch')
      _ASSERT(typekind_in%dkind == typekind_out%dkind, 'in-out typekind mismatch')

      select case (rank_in)

      case (2)

         select case (typekind_in%dkind)

         case (ESMF_TYPEKIND_R4%dkind)

            block
              real(REAL32), pointer :: q_in(:,:), q_out(:,:)
              
              call ESMF_FieldGet(f_in , 0, q_in, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_out , 0, q_out, rc=status)
              _VERIFY(status)
              call this%transpose_regrid(q_in, q_out, rc=status)
              _VERIFY(status)
            end block

         case (ESMF_TYPEKIND_R8%dkind)

            block
              real(REAL64), pointer :: q_in(:,:), q_out(:,:)
              
              call ESMF_FieldGet(f_in , 0, q_in, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_out , 0, q_out, rc=status)
              _VERIFY(status)
              call this%transpose_regrid(q_in, q_out, rc=status)
              _VERIFY(status)
            end block

         case default ! unsupported typekind
            _ASSERT(.false., 'unsupported typekind')
         end select

      case (3)

         select case (typekind_in%dkind)
         case (ESMF_TYPEKIND_R4%dkind)
            block
              real(REAL32), pointer :: q_in(:,:,:), q_out(:,:,:)

              call ESMF_FieldGet(f_in , 0, q_in, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_out , 0, q_out, rc=status)
              _VERIFY(status)
              _ASSERT(size(q_in,3) == size(q_out,3), 'in-out shape mismatch')
              call this%transpose_regrid(q_in, q_out,rc=status)
              _VERIFY(status)
            end block

         case (ESMF_TYPEKIND_R8%dkind)

            block
              real(REAL64), pointer :: q_in(:,:,:), q_out(:,:,:)

              call ESMF_FieldGet(f_in , 0, q_in, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_out , 0, q_out, rc=status)
              _VERIFY(status)
              _ASSERT(size(q_in,3) == size(q_out,3), 'in-out shape mismatch')
              call this%transpose_regrid(q_in, q_out, rc=status)
              _VERIFY(status)
            end block
         case default ! unsupported type/kind
            _ASSERT(.false., 'unsupported typekind')
         end select
      
      case default ! unsupported rank
         _ASSERT(.false., 'unsupported rank')
      end select

      _RETURN(_SUCCESS)
      
   end subroutine transpose_regrid_esmf_fields_scalar


   subroutine transpose_regrid_esmf_fields_vector(this, f_in, f_out, rc)
      use esmf, only: ESMF_TypeKind_Flag
      use esmf, only: ESMF_TYPEKIND_R4
      use esmf, only: ESMF_TYPEKIND_R8
      use esmf, only: ESMF_Field
      use esmf, only: ESMF_FieldGet
      integer, parameter :: NUM_DIM = 2
      class (AbstractRegridder), intent(in) :: this
      type (ESMF_Field), intent(in) :: f_in(NUM_DIM)
      type (ESMF_Field), intent(in) :: f_out(NUM_DIM)
      integer, optional, intent(out) :: rc
      
      character(len=*), parameter :: Iam = MOD_NAME//'transpose_regrid_esmf_fields'
      integer :: rank_in(NUM_DIM)
      type (ESMF_TypeKind_Flag) :: typekind_in(NUM_DIM)
      integer :: rank_out(NUM_DIM)
      type (ESMF_TypeKind_Flag) :: typekind_out(NUM_DIM)
      integer :: status
      integer :: d

      do d = 1, NUM_DIM
         call ESMF_FieldGet(f_in(d), rank=rank_in(d), typekind=typekind_in(d), rc=status)
         _VERIFY(status)
         call ESMF_FieldGet(f_out(d), rank=rank_out(d), typekind=typekind_out(d), rc=status)
         _VERIFY(status)
      end do

      ! Check consistent type/kind/rank for all arguments
      _ASSERT(all(rank_in == rank_in(1)), 'rank mismatch across input vector components')
      _ASSERT(all(typekind_in%dkind == typekind_in(1)%dkind), 'typekind mismatch across input vector components')
      _ASSERT(all(rank_out == rank_out(1)),'rank mismatch across output vectory components')
      _ASSERT(all(typekind_out%dkind == typekind_out(1)%dkind),'typekind mismatch across output vector coomponents')

      _ASSERT(rank_in(1) == rank_out(1), 'in-out rank mismatch')
      _ASSERT(typekind_in(1)%dkind == typekind_out(1)%dkind,'in-out typekind mismatch')

      select case (rank_in(1))

      case (2)

         select case (typekind_in(1)%dkind)

         case (ESMF_TYPEKIND_R4%dkind)

            block
              real(REAL32), pointer :: u_in(:,:), v_in(:,:)
              real(REAL32), pointer :: u_out(:,:), v_out(:,:)
              
              call ESMF_FieldGet(f_in(1) , 0, u_in, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_in(2) , 0, v_in, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_out(1) , 0, u_out, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_out(2) , 0, v_out, rc=status)
              _VERIFY(status)
              call this%transpose_regrid(u_in, v_in, u_out, v_out, rc=status)
              _VERIFY(status)
            end block

         case (ESMF_TYPEKIND_R8%dkind)

            block
              real(REAL64), pointer :: u_in(:,:), v_in(:,:)
              real(REAL64), pointer :: u_out(:,:), v_out(:,:)
              
              call ESMF_FieldGet(f_in(1) , 0, u_in, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_in(2) , 0, v_in, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_out(1) , 0, u_out, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_out(2) , 0, v_out, rc=status)
              _VERIFY(status)
              call this%transpose_regrid(u_in, v_in, u_out, v_out, rc=status)
              _VERIFY(status)
            end block

         case default ! unsupported typekind
            _ASSERT(.false., 'unsupported typekind')
         end select

      case (3)

         select case (typekind_in(1)%dkind)
         case (ESMF_TYPEKIND_R4%dkind)
            block
              real(REAL32), pointer :: u_in(:,:,:), v_in(:,:,:)
              real(REAL32), pointer :: u_out(:,:,:), v_out(:,:,:)
              
              call ESMF_FieldGet(f_in(1) , 0, u_in, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_in(2) , 0, v_in, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_out(1) , 0, u_out, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_out(2) , 0, v_out, rc=status)
              _VERIFY(status)
              call this%transpose_regrid(u_in, v_in, u_out, v_out, rc=status)
              _VERIFY(status)
            end block

         case (ESMF_TYPEKIND_R8%dkind)

            block
              real(REAL64), pointer :: u_in(:,:,:), v_in(:,:,:)
              real(REAL64), pointer :: u_out(:,:,:), v_out(:,:,:)
              
              call ESMF_FieldGet(f_in(1) , 0, u_in, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_in(2) , 0, v_in, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_out(1) , 0, u_out, rc=status)
              _VERIFY(status)
              call ESMF_FieldGet(f_out(2) , 0, v_out, rc=status)
              _VERIFY(status)
              call this%transpose_regrid(u_in, v_in, u_out, v_out, rc=status)
              _VERIFY(status)
            end block

         case default ! unsupported type/kind
            _ASSERT(.false., 'unsupported typekind')
         end select
      
      case default ! unsupported rank
         _ASSERT(.false., 'unsupported rank')
      end select

      _RETURN(_SUCCESS)
      

   end subroutine transpose_regrid_esmf_fields_vector


   subroutine set_undef_value(this, undef_value)
      class (AbstractRegridder), intent(inout) :: this
      real :: undef_value

      this%undef_value = undef_value
      this%has_undef_value_ = .true.

   end subroutine set_undef_value


   function get_undef_value(this) result(undef_value)
      real :: undef_value
      class (AbstractRegridder), intent(in) :: this

      undef_value = this%undef_value

   end function get_undef_value


   subroutine clear_undef_value(this)
      class (AbstractRegridder), intent(inout) :: this

      this%has_undef_value_ = .false.

   end subroutine clear_undef_value


   logical function has_undef_value(this)
      class (AbstractRegridder), intent(in) :: this

      has_undef_value = this%has_undef_value_

   end function has_undef_value


   function get_spec(this) result(spec)
      type (RegridderSpec) :: spec
      class (AbstractRegridder), intent(in) :: this
      spec = this%spec
   end function get_spec
   
   subroutine set_spec(this, spec)
      class(AbstractRegridder), intent(inout) :: this
      type(RegridderSpec), intent(in) :: spec
      this%spec = spec
   end subroutine set_spec

   function isTranspose(this) result(amTranspose)
      logical :: amTranspose
      class (AbstractRegridder), intent(in) :: this
      _UNUSED_DUMMY(this)
      amTranspose = .false.
   end function isTranspose

   subroutine initialize_base(this, spec, unusable, rc)
      use MAPL_KeywordEnforcerMod
      use MAPL_RegridderSpec
      class (AbstractRegridder), intent(inout) :: this
      type (RegridderSpec), intent(in) :: spec
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME//'initialize_base'
      _UNUSED_DUMMY(unusable)

      this%spec = spec
      ! Do the rest in the child type
      call this%initialize(rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)

   end subroutine initialize_base
         
   function clone(this)
      class (AbstractRegridder), allocatable :: clone
      class (AbstractRegridder), intent(in) :: this

      allocate(clone, source=this)

   end function clone

   ! Ideally this would be a DEFERRED method, but it is not needed for
   ! the custom legacy regridders and providing a default fail, avoids
   ! preserves backward compatibility for those.
   logical function supports(spec, unusable, rc)
      type(RegridderSpec), intent(in) :: spec
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(spec)
      _UNUSED_DUMMY(unusable)

      _FAIL('unimplemented')
      _RETURN(_SUCCESS)
   end function supports

end module MAPL_AbstractRegridderMod
