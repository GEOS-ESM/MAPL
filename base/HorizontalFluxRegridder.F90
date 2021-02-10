#include "MAPL_Generic.h"
#include "unused_dummy.H"

module mapl_HorizontalFluxRegridder
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   use ESMF
   use mapl_AbstractRegridderMod
   use mapl_RegridderSpec
   use mapl_RegridMethods
   use mapl_KeywordEnforcerMod
   use mapl_ErrorHandlingMod
   use mapl_BaseMod
   implicit none
   private

   public :: HorizontalFluxRegridder

   type, extends(AbstractRegridder) :: HorizontalFluxRegridder
   contains
      procedure, nopass :: supports
      procedure :: initialize_subclass
      ! Note scalar regridding intentionally not supported.
      procedure :: regrid_vector_2d_real32
      procedure :: regrid_vector_2d_real64
      procedure :: regrid_vector_3d_real32
      procedure :: regrid_vector_3d_real64
      
!!$      procedure :: transpose_regrid_vector_2d_real32
!!$      procedure :: transpose_regrid_vector_3d_real32
   end type HorizontalFluxRegridder
   
contains

   logical function supports(spec, unusable, rc)
      type(RegridderSpec), intent(in) :: spec
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: counts_in(2)
      integer :: counts_out(2)
      integer :: status

      
      _UNUSED_DUMMY(unusable)
      
      supports = (spec%regrid_method == REGRID_METHOD_CONSERVE_HFLUX)
      if (.not. supports) return

      call MAPL_GridGet(spec%grid_in, localCellCountPerDim=counts_in, __RC__)
      call MAPL_GridGet(spec%grid_out, localCellCountPerDim=counts_out, __RC__)

      supports = all(mod(counts_in, counts_out) == 0) .or. all(mod(counts_out, counts_in) == 0)

      _FAIL('unimplemented')
      _RETURN(_SUCCESS)
   end function supports

   subroutine initialize_subclass(this, unusable, rc)
     use MAPL_KeywordEnforcerMod
     use MAPL_RegridderSpec
     use MAPL_BaseMod, only: MAPL_grid_interior
     class (HorizontalFluxRegridder), intent(inout) :: this
     class (KeywordEnforcer), optional, intent(in) :: unusable
     integer, optional, intent(out) :: rc

     _FAIL('unimplemented')
     _RETURN(_SUCCESS)
  end subroutine initialize_subclass

   subroutine regrid_vector_2d_real32(this, u_in, v_in, u_out, v_out, rotate, rc)
      class (HorizontalFluxRegridder), intent(in) :: this
      real, intent(in) :: u_in(:,:)
      real, intent(in) :: v_in(:,:)
      real, intent(out) :: u_out(:,:)
      real, intent(out) :: v_out(:,:)
      logical, optional, intent(in) :: rotate
      integer, optional, intent(out) :: rc

      _RETURN(ESMF_SUCCESS)
   end subroutine regrid_vector_2d_real32

   subroutine regrid_vector_2d_real64(this, u_in, v_in, u_out, v_out, rotate, rc)
      class (HorizontalFluxRegridder), intent(in) :: this
      real(REAL64), intent(in) :: u_in(:,:)
      real(REAL64), intent(in) :: v_in(:,:)
      real(REAL64), intent(out) :: u_out(:,:)
      real(REAL64), intent(out) :: v_out(:,:)
      logical, optional, intent(in) :: rotate
      integer, optional, intent(out) :: rc

      _RETURN(ESMF_SUCCESS)
   end subroutine regrid_vector_2d_real64

   
   subroutine regrid_vector_3d_real32(this, u_in, v_in, u_out, v_out, rotate, rc)
      class (HorizontalFluxRegridder), intent(in) :: this
      real, intent(in) :: u_in(:,:,:)
      real, intent(in) :: v_in(:,:,:)
      real, intent(out) :: u_out(:,:,:)
      real, intent(out) :: v_out(:,:,:)
      logical, optional, intent(in) :: rotate
      integer, optional, intent(out) :: rc

      _RETURN(ESMF_SUCCESS)
   end subroutine regrid_vector_3d_real32

   subroutine regrid_vector_3d_real64(this, u_in, v_in, u_out, v_out, rc)
      class (HorizontalFluxRegridder), intent(in) :: this
      real(REAL64), intent(in) :: u_in(:,:,:)
      real(REAL64), intent(in) :: v_in(:,:,:)
      real(REAL64), intent(out) :: u_out(:,:,:)
      real(REAL64), intent(out) :: v_out(:,:,:)
      integer, optional, intent(out) :: rc

      _RETURN(ESMF_SUCCESS)
   end subroutine regrid_vector_3d_real64

   
end module mapl_HorizontalFluxRegridder
