#include "MAPL_Generic.h"
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_EtaHybridVerticalCoordinate
   use, intrinsic :: ISO_FORTRAN_ENV, only: REAL64, REAL32
   use ESMF
   use ESMFL_Mod
   use MAPL_ErrorHandlingMod
   use MAPL_KeywordEnforcerMod
   use pFIO
   implicit none
   private

   public :: EtaHybridVerticalCoordinate


   type :: EtaHybridVerticalCoordinate
      private
      real(kind=REAL64), allocatable :: ak(:)
      real(kind=REAL64), allocatable :: bk(:)
      integer :: num_levels = 0
      real(kind=REAL64) :: ref_pressure
   contains
      procedure :: get_eta_r8
      procedure :: get_eta_r4
      procedure :: get_pressure_levels_r8
      procedure :: get_pressure_levels_r8_3d
      procedure :: get_pressure_levels_r4
      procedure :: get_pressure_levels_r4_3d
      generic :: get_eta =>get_eta_r8, get_eta_r4
      generic :: get_pressure_levels=>get_pressure_levels_r8,   get_pressure_levels_r4, &
                                      get_pressure_levels_r8_3d,get_pressure_levels_r4_3d
   end type EtaHybridVerticalCoordinate

   interface newEtaHybridVerticalCoordinate
      module procedure new_EtaHybridVerticalCoordinate_by_ak_bk
      module procedure new_EtaHybridVerticalCoordinate_by_cfg
   end interface

   real(kind=REAL64), parameter :: DEFAULT_REFERENCE_PRESSURE = 98400.d0 ! (Pa) default reference pressure

contains


   function new_EtaHybridVerticalCoordinate_by_ak_bk(ak, bk, unused, ref_pressure, rc) result(grid)
      type (EtaHybridVerticalCoordinate) :: grid
      real(kind=REAL64), intent(in) :: ak(:)
      real(kind=REAL64), intent(in) :: bk(:)
      class(KeywordEnforcer), optional, intent(in) :: unused
      real(kind=REAL64),optional, intent(in) :: ref_pressure
      integer, optional, intent(inout) :: rc

      _ASSERT(size(ak) >= 2, 'size of ak should be >=2')
      _ASSERT(size(ak) == size(bk), ' size of ak should be the same as that of bk')

      grid%ak = ak
      grid%bk = bk
      grid%num_levels = size(ak) - 1

      if (present(ref_pressure)) then
         grid%ref_pressure = ref_pressure
      else
         grid%ref_pressure = DEFAULT_REFERENCE_PRESSURE
      end if
      
   end function new_EtaHybridVerticalCoordinate_by_ak_bk

   function new_EtaHybridVerticalCoordinate_by_cfg(config, unused, rc) result(grid)
      type (EtaHybridVerticalCoordinate) :: grid
      type (ESMF_Config) :: config
      class (KeywordEnforcer), optional, intent(in) :: unused
      integer, optional, intent(inout) :: rc
      real(kind=REAL64), allocatable :: ak(:)
      real(kind=REAL64), allocatable :: bk(:)
      integer :: status
      integer :: k, num_levels
      real(kind=REAL64) :: ptop, pint, ref_pressure
      character(len=32) :: data_label
 
      call ESMF_ConfigGetAttribute(config, num_levels,label='NUM_LEVELS:', __RC__ )
      call ESMF_ConfigGetAttribute(config, ref_pressure,label='REF_PRESSURE:', default = DEFAULT_REFERENCE_PRESSURE, __RC__ )

      data_label = "ak-bk:"

      allocate(ak(num_levels+1), bk(num_levels+1))

      call ESMF_ConfigFindLabel(config, trim(data_label), __RC__ )

      ! get ak and bk
      do k = 1, num_levels+1
         call ESMF_ConfigNextLine(config, __RC__ ) 
         call ESMF_ConfigGetAttribute(config, ak(k), __RC__ )
         call ESMF_ConfigGetAttribute(config, bk(k), __RC__ )
      enddo

      grid = EtaHybridVerticalCoordinate(ak, bk, ref_pressure=ref_pressure)

      deallocate(ak, bk)
   end function new_EtaHybridVerticalCoordinate_by_cfg

   subroutine get_eta_r8(this, ptop, pint, ak, bk, unused,rc)
      class(EtaHybridVerticalCoordinate), intent(in) :: this
      real(kind=REAL64), intent(out) :: ak(:)
      real(kind=REAL64), intent(out) :: bk(:)
      real(kind=REAL64), intent(out) :: ptop ! model top (Pa)
      real(kind=REAL64), intent(out) :: pint ! transition to p (Pa)
      class(KeywordEnforcer), optional, intent(in) :: unused
      integer, optional, intent(out) :: rc
      integer :: num_levels, k, ks

     _ASSERT(this%num_levels == size(ak) - 1 ,"size vertical grid should be consistent")      

     ak = this%ak
     bk = this%bk
     do k = 1, num_levels+1
        if (num_levels == 1) then
           ks = 1
           exit
        endif

        if ( bk(k) > 0.0d0) then
           ks = k -2
           exit
        endif
     enddo
     ptop = this%ak(1)
     pint = this%ak(ks+1)

     _RETURN(_SUCCESS)

   end subroutine get_eta_r8

   subroutine get_eta_r4(this, ptop, pint, ak, bk, unused,rc)
      class(EtaHybridVerticalCoordinate), intent(in) :: this
      real(kind=REAL32), intent(out) :: ak(:)
      real(kind=REAL32), intent(out) :: bk(:)
      real(kind=REAL32), intent(out) :: ptop ! model top (Pa)
      real(kind=REAL32), intent(out) :: pint ! transition to p (Pa)
      class(KeywordEnforcer), optional, intent(in) :: unused
      integer, optional, intent(out) :: rc

      real(kind=REAL64), allocatable :: ak8(:)
      real(kind=REAL64), allocatable :: bk8(:)
      real(kind=REAL64) :: ptop8 ! model top (Pa)
      real(kind=REAL64) :: pint8 ! transition to p (Pa)
      integer :: num_levels     

      num_levels = this%num_levels
      allocate(ak8(num_levels+1))
      allocate(bk8(num_levels+1))

      call this%get_eta(ptop8, pint8, ak8, bk8)

      ak = real(ak8, kind=REAL32)
      bk = real(bk8, kind=REAL32)
     
      ptop = ptop8
      pint = pint8

      deallocate(ak8,bk8)

     _RETURN(_SUCCESS)
   end subroutine get_eta_r4

   subroutine get_pressure_levels_r8(this, pressure_levels, unused, reference_pressure, rc)
      class(EtaHybridVerticalCoordinate), intent(in) :: this
      real(kind=REAL64), intent(out) :: pressure_levels(:)
      class(KeywordEnforcer), optional, intent(in) :: unused
      real(kind=REAL64), optional, intent(in) :: reference_pressure
      integer, optional, intent(out) :: rc
      real(kind=REAL64) :: p0
      integer :: k, num_levels

      num_levels = this%num_levels
      _ASSERT(size(pressure_levels) == num_levels, 'incorrect array size for pressure_levels dummy argument')

      if (present(reference_pressure)) then
         p0 = reference_pressure
      else
         p0 = DEFAULT_REFERENCE_PRESSURE 
      end if

      pressure_levels(1) = this%ak(1) + 0.50d0 * dpref_(1,p0)

      do k = 2, num_levels
         pressure_levels(k) = pressure_levels(k-1) + 0.5d0 * (dpref_(k-1, p0) + dpref_(k,p0))
      end do

      Pressure_levels = pressure_levels/100.0d0

   contains
      real(kind=REAL64) function dpref_ (k,pbot)
         integer k
         real(kind=REAL64) pbot
         dpref_   = ( this%ak(k+1) - this%ak(k) ) + &
                    ( this%bk(k+1) - this%bk(k) ) * pbot
      end function dpref_

   end subroutine get_pressure_levels_r8

   subroutine get_pressure_levels_r8_3d(this, pressure_levels, unused, reference_pressure, rc)
      class(EtaHybridVerticalCoordinate), intent(in) :: this
      real(kind=REAL64), intent(out) :: pressure_levels(:,:,:)
      class(KeywordEnforcer), optional, intent(in) :: unused
      real(kind=REAL64), optional, intent(in) :: reference_pressure(:,:)
      integer, optional, intent(out) :: rc
      integer :: i,j, isize, jsize
      
      isize = size(pressure_levels,2)
      jsize = size(pressure_levels,3)

      do j = 1, jsize
         do i = 1, isize
           call this%get_pressure_levels(pressure_levels(:,i,j), reference_pressure = reference_pressure(i,j))
         enddo
      enddo
   end subroutine

   subroutine get_pressure_levels_r4(this, pressure_levels, unused, reference_pressure, rc)
      class(EtaHybridVerticalCoordinate), intent(in) :: this
      real(kind=REAL32), intent(out) :: pressure_levels(:)
      class(KeywordEnforcer), optional, intent(in) :: unused
      real(kind=REAL32), optional, intent(in) :: reference_pressure
      integer, optional, intent(out) :: rc
      real(kind=REAL64) :: p0
      integer :: k, num_levels
      real(kind=REAL64), allocatable :: plevels(:)
      character(len=*), parameter :: Iam="get_pressure_levels"

      num_levels = this%num_levels
      _ASSERT(size(pressure_levels) == num_levels, 'incorrect array size for pressure_levels dummy argument')

      if (present(reference_pressure)) then
         p0 = reference_pressure
      else
         p0 = DEFAULT_REFERENCE_PRESSURE
      end if

      allocate(plevels(num_levels))

      call this%get_pressure_levels(plevels, reference_pressure=p0)

      pressure_levels = real(plevels,kind=REAL32)
     
      deallocate(plevels)

   end subroutine get_pressure_levels_r4

   subroutine get_pressure_levels_r4_3d(this, pressure_levels, unused, reference_pressure, rc)
      class(EtaHybridVerticalCoordinate), intent(in) :: this
      real(kind=REAL32), intent(out) :: pressure_levels(:,:,:)
      class(KeywordEnforcer), optional, intent(in) :: unused
      real(kind=REAL32), optional, intent(in) :: reference_pressure(:,:)
      integer, optional, intent(out) :: rc
      integer :: i,j
      
      do j = 1, size(pressure_levels,3)
         do i = 1, size(pressure_levels,2)
           call this%get_pressure_levels(pressure_levels(:,i,j), reference_pressure = reference_pressure(i,j))
         enddo
      enddo
   end subroutine

end module MAPL_EtaHybridVerticalCoordinate
