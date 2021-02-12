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
   use mapl_ErrorHandlingMod
   implicit none
   private

   public :: HorizontalFluxRegridder

   type, extends(AbstractRegridder) :: HorizontalFluxRegridder
      private
      integer :: resolution_ratio = -1
      integer :: im_in, jm_in
      integer :: im_out, jm_out
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

      integer :: counts_in(5)
      integer :: counts_out(5)
      integer :: status

      
      _UNUSED_DUMMY(unusable)
      
      supports = (spec%regrid_method == REGRID_METHOD_CONSERVE_HFLUX)
      if (.not. supports) return

      call MAPL_GridGet(spec%grid_in, localCellCountPerDim=counts_in, __RC__)
      call MAPL_GridGet(spec%grid_out, localCellCountPerDim=counts_out, __RC__)

      supports = all(mod(counts_in(1:2), counts_out(1:2)) == 0) .or. all(mod(counts_out, counts_in) == 0)

      _RETURN(_SUCCESS)
   end function supports

   subroutine initialize_subclass(this, unusable, rc)
     use MAPL_KeywordEnforcerMod
     use MAPL_RegridderSpec
     use MAPL_BaseMod, only: MAPL_grid_interior
     class (HorizontalFluxRegridder), intent(inout) :: this
     class (KeywordEnforcer), optional, intent(in) :: unusable
     integer, optional, intent(out) :: rc

     type(RegridderSpec) :: spec

     type(ESMF_Grid) :: grid_in, grid_out
     integer :: counts(5)
     integer :: status
     
     spec = this%get_spec()

     associate (grid_in => spec%grid_in, grid_out => spec%grid_out)

       associate (IM_in => this%IM_in, JM_in => this%JM_in, IM_out => this%IM_out, JM_out => this%JM_out)
         
         call MAPL_GridGet(spec%grid_in, localCellCountPerDim=counts, __RC__)
         IM_in = counts(1)
         JM_in = counts(2)
         
         call MAPL_GridGet(spec%grid_out, localCellCountPerDim=counts, __RC__)
         IM_out = counts(1)
         JM_out = counts(2)

         _ASSERT(mod(IM_in, IM_out) == 0, 'grids not nested')
         _ASSERT(mod(JM_in, JM_out) == 0, 'grids not nested')
         _ASSERT((IM_in / IM_out) == (JM_in / JM_out), 'inconsistent aspect ratio')
         
         this%resolution_ratio = (IM_in / IM_out)
       end associate
     end associate
     
     _RETURN(_SUCCESS)
  end subroutine initialize_subclass

   subroutine regrid_vector_2d_real32(this, u_in, v_in, u_out, v_out, rotate, rc)
      class (HorizontalFluxRegridder), intent(in) :: this
      real(kind=REAL32), intent(in) :: u_in(:,:)
      real(kind=REAL32), intent(in) :: v_in(:,:)
      real(kind=REAL32), intent(out) :: u_out(:,:)
      real(kind=REAL32), intent(out) :: v_out(:,:)
      logical, optional, intent(in) :: rotate
      integer, optional, intent(out) :: rc

      integer :: i, j, ii, jj
      real(kind=REAL32) :: m_x, m_y


      _ASSERT(size(u_in,1) == this%IM_in, 'mismatch in IM for input')
      _ASSERT(size(v_in,1) == this%IM_in, 'mismatch in IM for input')
      _ASSERT(size(u_in,2) == this%JM_in, 'mismatch in IM for input')
      _ASSERT(size(v_in,2) == this%JM_in, 'mismatch in IM for input')
      _ASSERT(size(u_out,1) == this%IM_out, 'mismatch in IM for input')
      _ASSERT(size(v_out,1) == this%IM_out, 'mismatch in IM for input')
      _ASSERT(size(u_out,2) == this%JM_out, 'mismatch in IM for input')
      _ASSERT(size(v_out,2) == this%JM_out, 'mismatch in IM for input')

      associate (N => this%resolution_ratio)
        associate (IM => size(u_out,1), JM => size(u_out,2))

          ! aggregate y-fluxes
          do j = 1, JM
             jj = 1 + (j-1)*N
             do i  = 1, IM
                m_y = 0
                do ii = 1 + (i-1)*N, i*N
                   m_y = m_y + v_in(ii,j)
                end do
                v_out(i,j) = m_y
             end do
          end do
          
          ! aggregate x-fluxes
          do i = 1, IM
             ii = 1 + (i-1)*N
             do j  = 1, JM
                m_x = 0
                do jj = 1 + (j-1)*N, j*N
                   m_x = m_x + u_in(i,jj)
                end do
                u_out(i,j) = m_x
             end do
          end do

        end associate
      end associate

      
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

      integer :: i, j, ii, jj
      real(REAL64) :: m_x, m_y
      
      _ASSERT(size(u_in,1) == this%IM_in, 'mismatch in IM for input')
      _ASSERT(size(v_in,1) == this%IM_in, 'mismatch in IM for input')
      _ASSERT(size(u_in,2) == this%JM_in, 'mismatch in IM for input')
      _ASSERT(size(v_in,2) == this%JM_in, 'mismatch in IM for input')
      _ASSERT(size(u_out,1) == this%IM_out, 'mismatch in IM for input')
      _ASSERT(size(v_out,1) == this%IM_out, 'mismatch in IM for input')
      _ASSERT(size(u_out,2) == this%JM_out, 'mismatch in IM for input')
      _ASSERT(size(v_out,2) == this%JM_out, 'mismatch in IM for input')


      associate (N => this%resolution_ratio)
        associate (IM => size(u_out,1), JM => size(u_out,2))

          ! aggregate y-fluxes
          do j = 1, JM
             jj = 1 + (j-1)*N
             do i  = 1, IM
                m_y = 0
                do ii = 1 + (i-1)*N, i*N
                   m_y = m_y + v_in(ii,j)
                end do
                v_out(i,j) = m_y
             end do
          end do
          
          ! aggregate x-fluxes
          do i = 1, IM
             ii = 1 + (i-1)*N
             do j  = 1, JM
                m_x = 0
                do jj = 1 + (j-1)*N, j*N
                   m_x = m_x + u_in(i,jj)
                end do
                u_out(i,j) = m_x
             end do
          end do

        end associate
      end associate

      _RETURN(ESMF_SUCCESS)
   end subroutine regrid_vector_2d_real64

   
   subroutine regrid_vector_3d_real32(this, u_in, v_in, u_out, v_out, rotate, rc)
      class (HorizontalFluxRegridder), intent(in) :: this
      real(kind=REAL32), intent(in) :: u_in(:,:,:)
      real(kind=REAL32), intent(in) :: v_in(:,:,:)
      real(kind=REAL32), intent(out) :: u_out(:,:,:)
      real(kind=REAL32), intent(out) :: v_out(:,:,:)
      logical, optional, intent(in) :: rotate
      integer, optional, intent(out) :: rc

      integer :: k, status

      _ASSERT(size(u_in,3) == size(u_out,3), 'mismatch in number of levels')
      _ASSERT(size(v_in,3) == size(v_out,3), 'mismatch in number of levels')
      _ASSERT(size(u_in,3) == size(v_in,3), 'mismatch in number of levels')

      associate (LM => size(u_in,3))
        
        do k = 1, LM
           call this%regrid(u_in(:,:,k),v_in(:,:,k), u_out(:,:,k), v_out(:,:,k), rotate, rc=status)
           _VERIFY(status)
        end do

      end associate

      _RETURN(ESMF_SUCCESS)
   end subroutine regrid_vector_3d_real32

   subroutine regrid_vector_3d_real64(this, u_in, v_in, u_out, v_out, rc)
      class (HorizontalFluxRegridder), intent(in) :: this
      real(REAL64), intent(in) :: u_in(:,:,:)
      real(REAL64), intent(in) :: v_in(:,:,:)
      real(REAL64), intent(out) :: u_out(:,:,:)
      real(REAL64), intent(out) :: v_out(:,:,:)
      integer, optional, intent(out) :: rc

      integer :: k, status

      _ASSERT(size(u_in,3) == size(u_out,3), 'mismatch in number of levels')
      _ASSERT(size(v_in,3) == size(v_out,3), 'mismatch in number of levels')
      _ASSERT(size(u_in,3) == size(v_in,3), 'mismatch in number of levels')

      associate (LM => size(u_in,3))
        
        do k = 1, LM
           call this%regrid(u_in(:,:,k),v_in(:,:,k), u_out(:,:,k), v_out(:,:,k), rc=status)
           _VERIFY(status)
        end do

      end associate

      _RETURN(ESMF_SUCCESS)
   end subroutine regrid_vector_3d_real64

   
end module mapl_HorizontalFluxRegridder
