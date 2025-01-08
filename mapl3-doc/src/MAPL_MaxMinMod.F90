!------------------------------------------------------------------------------
!               Global Modeling and Assimilation Office (GMAO)                !
!                    Goddard Earth Observing System (GEOS)                    !
!                                 MAPL Component                              !
!------------------------------------------------------------------------------
!>
!### MODULE: `MAPL_MaxMinMod`
!
! Author: GMAO SI-Team
!
! `MAPL_MaxMinMod` --- Global Max/Min of Arrays
!
! This module implements functions for calculating/printing out the global min/max
! of fortran arrays. Derived from GEOS-4 pmaxmin() functions.
!
   module MAPL_MaxMinMod


! !USES:

      Use ESMF
      Use MAPL_CommsMod
      Use mpi

      implicit None

! !PUBLIC MEMBER FUNCTIONS:
!
      private
      public  MAPL_MaxMin

      interface MAPL_MaxMin

         module procedure pmaxmin3d_r4
         module procedure pmaxmin2d_r4
         module procedure pmaxmin1d_r4

         module procedure pmaxmin3d_r8
         module procedure pmaxmin2d_r8
         module procedure pmaxmin1d_r8

      end interface MAPL_MaxMin

CONTAINS

  subroutine pmaxmin3d_r4 ( qname, a, pmin, pmax, fac )
      implicit none
      character(len=*),             intent(in)  :: qname        ! label to print
      real(ESMF_KIND_R4),           intent(in)  :: a(:,:,:)     ! input array
      real(ESMF_KIND_R4), optional, intent(in)  :: fac          ! multiplication factor
      real(ESMF_KIND_R4), optional, intent(out) :: pmax, pmin   ! min/max value
!                         ---
      integer im, jt
      im = size(a,1) * size(a,2)
      jt = size(a,3)
      call pmaxmin2d_r4 ( qname, reshape(a,(/ im, jt /)), pmin, pmax, fac )
    end subroutine pmaxmin3d_r4

    subroutine pmaxmin2d_r4 ( qname, a, pmin_, pmax_, fac_ )

      implicit none
      character(len=*),             intent(in)  :: qname        ! label to print
      real(ESMF_KIND_R4),           intent(in)  :: a(:,:)       ! input array
      real(ESMF_KIND_R4), optional, intent(in)  :: fac_         ! multiplication factor
      real(ESMF_KIND_R4), optional, intent(out) :: pmax_, pmin_ ! min/max value
!                                           ---

      real(ESMF_KIND_R4) :: pmax, pmin, fac

      integer :: i, j, two=2

      real pm1(2)
      real pm_res(2)
      type(ESMF_VM) :: vm

      character(len=16) :: name
!NOTE: the current version does not trap error conditions returned in status
      integer :: status
      integer :: comm
      logical :: has_nans
      logical :: has_nans_local
      character(len=:), allocatable :: buf

      if ( present(fac_) ) then
         fac = fac_
      else
         fac = 1.0
      end if

      call ESMF_VmGetCurrent(vm=vm, rc=status)

      has_nans_local = any(a /= a)

      pmin = minval(a, mask=(a==a))
      pmax = maxval(a, mask=(a==a))

      pm1(1) = pmax
      pm1(2) = -pmin
      call MAPL_CommsAllReduceMax(vm, sendbuf=pm1, recvbuf=pm_res, cnt=two, RC=status)
      pmax=pm_res(1)
      pmin=-pm_res(2)

      if ( present(pmax_) ) pmax_ = pmax
      if ( present(pmin_) ) pmin_ = pmin

      call ESMF_VmGet(VM, mpicommunicator=comm, rc=status)
      call MPI_Reduce(has_nans_local, has_nans, 1, MPI_LOGICAL, MPI_LOR, 0, comm, status)

      if ( fac /= 0.0 ) then  ! trick to prevent printing
         if ( MAPL_am_I_root(vm) ) then
            name = '            '
            name(1:len(qname)) = qname
            buf = ""
            if (has_nans) then
               buf = " has NaN"
            end if
            write(*,*) name, ' max = ', pmax*fac, ' min = ', pmin*fac, buf
            return
         end if
      end if

      return

    end subroutine pmaxmin2d_r4

    subroutine pmaxmin1d_r4 ( qname, a, pmin, pmax, fac )
      implicit none
      character(len=*),             intent(in)  :: qname        ! label to print
      real(ESMF_KIND_R4),           intent(in)  :: a(:)         ! input array
      real(ESMF_KIND_R4), optional, intent(in)  :: fac          ! multiplication factor
      real(ESMF_KIND_R4), optional, intent(out) :: pmax, pmin   ! min/max value

      integer :: im, jt
      im = size(a)
      jt = 1
      call pmaxmin2d_r4 ( qname, reshape(a,(/ im, jt /)), pmin, pmax, fac )
    end subroutine pmaxmin1d_r4

!---

  subroutine pmaxmin3d_r8 ( qname, a, pmin, pmax, fac )
      implicit none
      character(len=*),             intent(in)  :: qname        ! label to print
      real(ESMF_KIND_R8),           intent(in)  :: a(:,:,:)     ! input array
      real(ESMF_KIND_R8), optional, intent(in)  :: fac          ! multiplication factor
      real(ESMF_KIND_R8), optional, intent(out) :: pmax, pmin   ! min/max value
!                         ---
      real(ESMF_KIND_R4) :: pmin_r4, pmax_r4, fac_r4
      if ( present(fac) ) then
         fac_r4 = fac
      else
         fac_r4 = 1.0
      end if
      call pmaxmin3d_r4 ( qname, real(a,kind=ESMF_KIND_R4), pmin_r4, pmax_r4, fac_r4 )
      if ( present(pmin) ) pmin = pmin_r4
      if ( present(pmax) ) pmax = pmax_r4
   end subroutine pmaxmin3d_r8

  subroutine pmaxmin2d_r8 ( qname, a, pmin, pmax, fac )
      implicit none
      character(len=*),             intent(in)  :: qname        ! label to print
      real(ESMF_KIND_R8),           intent(in)  :: a(:,:)     ! input array
      real(ESMF_KIND_R8), optional, intent(in)  :: fac          ! multiplication factor
      real(ESMF_KIND_R8), optional, intent(out) :: pmax, pmin   ! min/max value
!                         ---
      real(ESMF_KIND_R4) :: pmin_r4, pmax_r4, fac_r4
      if ( present(fac) ) then
         fac_r4 = fac
      else
         fac_r4 = 1.0
      end if
      call pmaxmin2d_r4 ( qname, real(a,kind=ESMF_KIND_R4), pmin_r4, pmax_r4, fac_r4 )
      if ( present(pmin) ) pmin = pmin_r4
      if ( present(pmax) ) pmax = pmax_r4
   end subroutine pmaxmin2d_r8

  subroutine pmaxmin1d_r8 ( qname, a, pmin, pmax, fac )
      implicit none
      character(len=*),             intent(in)  :: qname        ! label to print
      real(ESMF_KIND_R8),           intent(in)  :: a(:)         ! input array
      real(ESMF_KIND_R8), optional, intent(in)  :: fac          ! multiplication factor
      real(ESMF_KIND_R8), optional, intent(out) :: pmax, pmin   ! min/max value
!                         ---
      real(ESMF_KIND_R4) :: pmin_r4, pmax_r4, fac_r4
      if ( present(fac) ) then
         fac_r4 = fac
      else
         fac_r4 = 1.0
      end if
      call pmaxmin1d_r4 ( qname, real(a,kind=ESMF_KIND_R4), pmin_r4, pmax_r4, fac_r4 )
      if ( present(pmin) ) pmin = pmin_r4
      if ( present(pmax) ) pmax = pmax_r4
   end subroutine pmaxmin1d_r8


 end module MAPL_MaxMinMod
