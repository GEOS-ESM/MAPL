#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_VerticalGrid
   use, intrinsic :: ISO_FORTRAN_ENV, only: REAL64, REAL32
   use ESMF
   use ESMFL_Mod
   use MAPL_ErrorHandlingMod
   use MAPL_KeywordEnforcerMod
   use pFIO
   implicit none
   private

   public :: VerticalGrid


   type :: VerticalGrid
      private
      real(kind=REAL64), allocatable :: ak(:)
      real(kind=REAL64), allocatable :: bk(:)
      integer :: ks
      integer :: num_levels = 0
      logical :: use_sigma_levels = .false.
      logical :: use_ncep_levels  = .false.
   contains
      procedure :: set_eta_r8
      procedure :: set_eta_r4
      procedure :: get_pressure_levels_r8
      procedure :: get_pressure_levels_r4
      generic :: set_eta =>set_eta_r8, set_eta_r4
      generic :: get_pressure_levels=>get_pressure_levels_r8, get_pressure_levels_r4
   end type VerticalGrid

   interface newVerticalGrid
      module procedure new_VerticalGrid_by_ak_bk
      module procedure new_VerticalGrid_by_cfg
   end interface

   real(kind=REAL64), parameter :: DEFAULT_REFENCE_PRESSURE = 98400.d0 ! Pa

contains


   function new_VerticalGrid_by_ak_bk(ak, bk, ks, unused, use_sigma_levels, use_ncep_levels, rc) result(grid)
      type (VerticalGrid) :: grid
      real(kind=REAL64), intent(in) :: ak(:)
      real(kind=REAL64), intent(in) :: bk(:)
      integer, intent(in) :: ks
      class(KeywordEnforcer), optional, intent(in) :: unused
      logical, optional,intent(in) :: use_sigma_levels
      logical, optional,intent(in) :: use_ncep_levels
      integer, optional, intent(inout) :: rc

      character(len=*), parameter :: Iam="new_VerticalGrid_by_ak_bk"

      _ASSERT(size(ak) >= 2, 'size of ak should be >=2')
      _ASSERT(size(ak) == size(bk), ' size of ak should be the same as that of bk')

      grid%ak = ak
      grid%bk = bk
      grid%ks = ks

      if (present(use_sigma_levels)) then
         grid%use_sigma_levels = use_sigma_levels
      else
         grid%use_sigma_levels = .false.
      end if

      if (present(use_ncep_levels)) then
         grid%use_ncep_levels = use_ncep_levels
      else
         grid%use_ncep_levels = .false.
      end if

      grid%num_levels = size(ak) - 1
      
   end function new_VerticalGrid_by_ak_bk

   function new_VerticalGrid_by_cfg(config, unused, reference_pressure, rc) result(grid)
      type (VerticalGrid) :: grid
      type (ESMF_Config) :: config
      class (KeywordEnforcer), optional, intent(in) :: unused
      real(kind=REAL64), optional, intent(in) :: reference_pressure
      integer, optional, intent(inout) :: rc
      logical :: use_sigma_levels
      logical :: use_ncep_levels
      real(kind=REAL64), allocatable :: ak(:)
      real(kind=REAL64), allocatable :: bk(:)

      integer :: k,ks, num_levels
      real(kind=REAL64) :: ptop, pint
      character(len=32) :: data_label
      character(len=*), parameter :: Iam="new_VerticalGrid_by_cfg"
 
      call ESMF_ConfigGetAttribute(config, num_levels,label='NUM_LEVELS:', default = 0, rc=rc)
      call ESMF_ConfigGetAttribute(config, use_sigma_levels, label='USE_SIGMA_LEVELS:', default=.false., rc=rc)
      call ESMF_ConfigGetAttribute(config, use_ncep_levels, label='USE_NCEP_LEVELS:',   default=.false., rc=rc)

      data_label = "levels_"//i_to_string(num_levels)//":"
      if(use_sigma_levels) then
         _ASSERT(num_levels==64, "sigma only 64 levels")
         data_label = "sigma_levels_64:"
         _ASSERT( .not. use_ncep_levels, "64 .or. 72")
      endif

      if(use_ncep_levels) then
         _ASSERT(num_levels==72, "ncep_gmao 72 levels")
         data_label = "ncep_levels_72:"
      endif
#ifdef _BETA3P1_N_EARLIER_
      _ASSERT( .not. use_ncep_levels, " not ncep grid")
      _ASSERT( .not. use_sigma_levels, " not sigma grid")
      _ASSERT(num_levels==72, " _BETA3P1_N_EARLIER_ is defines")
      eta_lable = "BETA3P1_levels_72:"
#endif

      allocate(ak(num_levels+1), bk(num_levels+1))

      call ESMF_ConfigFindLabel(config, trim(data_label), rc=rc)

      ! get ak and bk
      do k = 1, num_levels+1
         call ESMF_ConfigNextLine(config, rc=rc) 
         call ESMF_ConfigGetAttribute(config, ak(k), rc=rc)
         call ESMF_ConfigGetAttribute(config, bk(k), rc=rc)
      enddo
      ! the last row is ks for pint = ak(ks+1)
      call ESMF_ConfigNextLine(config, rc=rc) 
      call ESMF_ConfigGetAttribute(config, ks, rc=rc)

      grid = VerticalGrid(ak, bk, ks, use_sigma_levels, use_ncep_levels)

   end function new_VerticalGrid_by_cfg

   subroutine set_eta_r8(this, km, ks, ptop, pint, ak, bk, unused,rc)
      class(VerticalGrid), intent(in) :: this
      integer, intent(in)  :: km
      integer, intent(out) :: ks
      real(kind=REAL64), intent(out) :: ak(:)
      real(kind=REAL64), intent(out) :: bk(:)
      real(kind=REAL64), intent(out) :: ptop ! model top (Pa)
      real(kind=REAL64), intent(out) :: pint ! transition to p (Pa)
      class(KeywordEnforcer), optional, intent(in) :: unused
      integer, optional, intent(out) :: rc
    
     _ASSERT(km == size(ak)-1 ,"size ak should be consistent")      
     _ASSERT(km == size(bk)-1 ,"size ak should be consistent")      
     _ASSERT(km == this%num_levels,"size vertical grid should be consistent")      

     ak = this%ak
     bk = this%bk
     ks = this%ks
     ptop = this%ak(1)
     pint = this%ak(ks+1)

     _RETURN(_SUCCESS)

   end subroutine set_eta_r8

   subroutine set_eta_r4(this, km, ks, ptop, pint, ak, bk, unused,rc)
      class(VerticalGrid), intent(in) :: this
      integer, intent(in)  :: km
      integer, intent(out) :: ks
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
    
     _ASSERT(km == size(ak)-1 ,"size ak should be consistent")      
     _ASSERT(km == size(bk)-1 ,"size ak should be consistent")      
     _ASSERT(km == this%num_levels,"size vertical grid should be consistent")      

     allocate(ak8(km+1))
     allocate(bk8(km+1))

     call this%set_eta(km, ks, ptop8, pint8, ak8, bk8)

     ak = real(ak8, kind=REAL32)
     bk = real(bk8, kind=REAL32)
     ptop = ak(1)
     pint = ak(ks+1)

     deallocate(ak8,bk8)

     _RETURN(_SUCCESS)
   end subroutine set_eta_r4

   subroutine get_pressure_levels_r8(this, pressure_levels, unused, reference_pressure, rc)
      class(VerticalGrid), intent(in) :: this
      real(kind=REAL64), intent(out) :: pressure_levels(:)
      class(KeywordEnforcer), optional, intent(in) :: unused
      real(kind=REAL64), optional, intent(in) :: reference_pressure
      integer, optional, intent(out) :: rc
      real(kind=REAL64) :: p0
      integer :: k, n_levels
      character(len=*), parameter :: Iam="get_pressure_levels"

      n_levels = this%num_levels
      _ASSERT(size(pressure_levels) == n_levels, 'incorrect array size for pressure_levels dummy argument')

      if (present(reference_pressure)) then
         p0 = reference_pressure
      else
         p0 = DEFAULT_REFENCE_PRESSURE
      end if

      pressure_levels(1) = this%ak(1) + 0.50d0 * dpref_(1,p0)

      do k = 2, n_levels
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

   subroutine get_pressure_levels_r4(this, pressure_levels, unused, reference_pressure, rc)
      class(VerticalGrid), intent(in) :: this
      real(kind=REAL32), intent(out) :: pressure_levels(:)
      class(KeywordEnforcer), optional, intent(in) :: unused
      real(kind=REAL32), optional, intent(in) :: reference_pressure
      integer, optional, intent(out) :: rc
      real(kind=REAL64) :: p0
      integer :: k, n_levels
      real(kind=REAL64), allocatable :: plevels(:)
      character(len=*), parameter :: Iam="get_pressure_levels"

      n_levels = this%num_levels
      _ASSERT(size(pressure_levels) == n_levels, 'incorrect array size for pressure_levels dummy argument')

      if (present(reference_pressure)) then
         p0 = reference_pressure
      else
         p0 = DEFAULT_REFENCE_PRESSURE
      end if

      allocate(plevels(n_levels))

      call get_pressure_levels_r8(this, plevels, reference_pressure=p0)

      pressure_levels = real(plevels,kind=REAL32)

   end subroutine get_pressure_levels_r4

end module MAPL_VerticalGrid
