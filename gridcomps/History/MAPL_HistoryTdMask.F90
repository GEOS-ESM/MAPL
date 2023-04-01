#include "MAPL_Generic.h"
module MAPL_TimeDependentMask
  use ESMF
  public
  type :: TimeDependentMask
     logical, allocatable :: mask(:,:)    ! for CS and LL
     character(len=ESMF_MAXPATHLEN) :: mask_file_header
     type(ESMF_Time) :: obs_start_time
     type(ESMF_Time) :: obs_end_time
     type(ESMF_TimeInterval) :: obs_interval
     type(ESMF_Time) :: mask_start
     type(ESMF_Time) :: mask_end
     type(ESMF_Time) :: mask_freq
   contains
     procedure :: get_mask
  end type TimeDependentMask

  
  interface TimeDependentMask
     procedure new_TimeDependentMask
  end interface TimeDependentMask

contains

  function new_TimeDependentMask(mask_setup) result(tdmask)
    ! convert mask start_time, end_time to ESMF time
    ! read in
    type(TimeDependentMask), intent(out) :: tdmask
    character(len=ESMF_MAXPATHLEN), intent(in) :: mask_setup
    integer :: nx, ny


    
    ! allocate mask dimension from obs
    nx=10; ny=10
    allocate(tdmask%mask(nx,ny))
    mask(:,:)=.F.

    
    return
  end function new_TimeDependentMask
  
  subroutine get_mask(this, time_span, grid, rc)
    type(TimeDependentMask), intent(inout) :: this
    type(ESMF_Time), intent (in) :: time_span(2)
    type(ESMF_grid), intent (in) :: grid  ! CS or LL from model/bundle
    integer, optional, intent(out) :: rc

    integer :: status
    integer, allocatable :: COUNTS(:)
    integer :: IM, JM, LM, IM_WORLD, JM_WORLD

    character(len=ESMF_MAXPATHLEN) :: s1, s2, s3, s4
    
    ! s1. get esmf grid dim, default mask=.F.
    call ESMF_GridGet(grid, DistGrid=disgrid, dimCount=dimCount, _RC)
    call ESMF_DistGridGet(distgrid, deLayout=LAYOUT, _RC)
    call EMSF_VmGetCurrent(VM, _RC)
    call ESMF_VmGet(VM, localPet=myid, petCount=ndes, _RC)
    
    call ESMF_GridGet(grid, localCellCountPerDim=COUNTS, _RC)
    IM= COUNTS(1)
    JM= COUNTS(2)
    LM= COUNTS(3)

    call ESMF_GridGet(grid, globalCellCountPerDim=COUNTS, _RC)
    IM_WORLD= COUNTS(1)
    JM_WORLD= CONNTS(2)

    allocate(this%mask(IM, JM))
    this%mask(:,:)=.F.
    
    ! s2. read in a series of swath files within time_span
    !     - parse ob filename, dir, freq. from hist-input
    !     - read in lon/lat obs. data
    read(mask_setup,*) s1, s2, s3, s4


    ! s3. find index [loc/global] via bisect for CS/LL
    !
    !
    

    

    
  end subroutine get_mask



    subroutine split_string (string, mark, length_mx, &
       mxseg, nseg, str_piece, jstatus)
    implicit none
    integer,           intent (in) :: length_mx
    character (len=length_mx), intent (in) :: string
    character (len=1), intent (in) :: mark
    integer,           intent (in) :: mxseg
    integer,           intent (out):: nseg
    character (len=length_mx), intent (out):: str_piece(mxseg)
    integer,           intent (out):: jstatus
    INTEGER                        :: len1, l, lh, lw
    integer                        :: iseg
    integer,           allocatable :: ipos(:)
    !
    !   xxxx_yy_zz_uu_vv
    !       ^  ^  ^  ^
    !       |  |  |  |
    !       marker
    !
    len1 = LEN_TRIM( string )
    allocate (ipos(len1))
    iseg=0
    do l=1, len1
       if (mark .eq. string(l:l)) then
          iseg=iseg+1
          ipos(iseg)=l
  !        write(6,*) 'match!', l
  !        write(6,*) 'ipos ', iseg, ' = ', l
       endif
    enddo
    if (iseg.eq.0 .or. iseg.gt.mxseg-1) then
       call error ('split_string', 'find nseg .eq.0 or > 4', 1)
       jstatus=1   ! fail
       return
    else
       jstatus=0   ! success
    endif
    nseg=iseg
    !
    !
    str_piece(:)=''
    lw=1    ! lw, lh: two index positions
    do l=1, nseg
       lh=ipos(l)-1
       do iseg=lw, lh
          str_piece(l)=trim(str_piece(l))//string(iseg:iseg)
       enddo
       lw=ipos(l)+1
    enddo
    if (lw.le.len1) then
       lh=len1
       do iseg=lw, lh
          str_piece(l)=trim(str_piece(l))//string(iseg:iseg)
       enddo
    endif
    nseg=nseg+1  ! must add one bc of eggs and '_'
    if (nseg.gt.mxseg) then
       call error ('split_string', 'nseg exceeds mx', 1)
    endif
    str_piece(nseg+1:mxseg)='void'
    return
  end subroutine split_string
  

  subroutine error(insubroutine, message, ierr )
    character (len=*), intent (in) :: insubroutine
    character (len=*), intent (in) :: message
    integer, intent (in) :: ierr
    !
    write (6, 11)
    write (6, 12)  trim(insubroutine), trim(message), ierr
    write (6, 11)
    stop
11  format ('**====================**')
12  format (2x, a, 4x, a, 4x, "ierr =", i4)
    return
  end subroutine error
  
end module MAPL_TimeDependentMask

!
!Q1.  what is template in ExtData, how to read in multiple nc files
!Q2.  II, JJ index on each processor?
!!!
!!!CO2_GridComp_ExtData.rc
!!!
!!!Y F%y4-%m2-%d2t12:00:00 none     none     emco2ff       ExtData/PIESA/sfc/bian.co2.x288_y181_t12.2001.nc
!!!CO2_NEP         'kg C m-2 s-1'      Y Y F%y4-%m2-%d2t12:00:00 none     none     emco2nep      ExtData/PIESA/sfc/bian.co2.x288_y181_t12.2001.nc
!!!CO2_OCN         'kg C m-2 s-1'      Y Y F%y4-%m2-%d2t12:00:00 none     none     emco2ocn      ExtData/PIESA/sfc/bian.co2.x288_y181_t12.2001.nc
!!!
!!!CO2_CMS_BIOMASS 'kg C m-2 s-1'      N Y F%y4-%m2-%d2t12:00:00 none     none     biomass       ExtData/PIESA/sfc/CMS/em.daily.1x1.25.%y4.nc
!!!CO2_CMS_FF      'kg C m-2 s-1'      N Y F%y4-%m2-%d2t12:00:00 none     none     emco2ff       ExtData/PIESA/sfc/CMS/ORNL_TRANSCOM.co2_ff_nep_ocn.x288_y181_t12.%y4.nc
!!!CO2_CMS_NEP     'kg C m-2 s-1'      N Y  P0000-00-00T03:00    none     none     emco2nep      ExtData/PIESA/sfc/CMS/casa.3hr.1x1.25.%y4.nc
!!!CO2_CMS_OCN     'kg C m-2 s-1'      N Y F%y4-%m2-%d2t12:00:00 none     none     emco2ocn      ExtData/PIESA/sfc/CMS/NOBM_fco2.daily.1x1.25.%y4.nc
!!!
!!!CO2_regionMask  NA                  N v   -                   none     none     REGION_MASK   ExtData/PIESA/sfc/ARCTAS.region_mask.x540_y361.2008.nc
!!!#---------------+-------------------+-+-+---------------------+--------+--------+-------------+----------------------
!!!%%~
!!!
!!!
!
!
!
!
!
!
!
!!!MAPL_HistoryTdMask.F90
!!!! In History
!!!!  - New option to specify a time-dependent mask:   template
!!!!    (up where we define grids and such)
!!!!  - Add way to specify that the named mask is used in a collection
!!!!  - During run step
!!!!      - check if collection needs mask
!!!!      - if so use get_mask() to get mask
!!!!      - WHERE (.not. mask) T = undef
!!!!
!!!
!!!module mapl_TimeDependentMask
!!!   implicit none
!!!
!!!   type :: TimeDependentMask
!!!   contains
!!!      procedure :: mask
!!!   end type TimeDependentMask
!!!
!!!contains
!!!
!!!   function new_TimeDependentMask(template) result(mask)
!!!      character(*), intent(in) :: template
!!!   end function new_TimeDependentMask
!!!
!!!   subroutine get_mask(this, time_interval, grid, mask, rc)
!!!      class(TimeDependentMask), intent(inout) :: this
!!!      type(ESMF_Time), intent(in) :: time_interval(2)
!!!      type(ESMF_Grid), intent(inout) :: grid
!!!      logical, intent(out) :: mask(:,:)
!!!      integer, optional, intent(in) :: rc
!!!
!!!      ! (0) Set mask(I,J) = .false.
!!!
!!!
!!!
!!!
!!!
!!!t(in) :: time_interval(2)
!!!type(ESMF_Grid), intent(inout) :: grid
!!!logical, intent(out) :: mask(:,:)
!!!integer, optional, intent(in) :: rc
!!!
!!!! (0) Set mask(I,J) = .false.
!!!
!!!type(ESMF_Grid), intent(inout) :: grid
!!!logical, intent(out) :: mask(:,:)
!!!integer, optional, intent(in) :: rc
!!!
!!!! (0) Set mask(I,J) = .false.~
!!!
!!!
!!!      ! (1) Figure out which files in template overlap time interval
!!!      ! Similar logic exists in ExtData
!!!
!!!      ! (2) For each file with overlap
!!!      !         Determine subset (lat,lon) that is in time window
!!!      ! (3) For each (lat,lon) determine (I,J,FACE)
!!!      !     if proc has (I,J,FACE) set mask(I,J) = .true.
!!!      !     Logic is grid dependent. (unfortunate)
!!!
!!!      !   MAPL/base/Base_Base_implementation.F90
!!!      !    module subroutine MAPL_GetGlobalHorzIJIndex(npts,II,JJ,lon,lat,lonR8,latR8,Grid, rc)
!!!
!!!
!!!   end subroutine get_mask
!!!
!!!      ! In history - after regrid step
!!!      if (collection_is_masked) then
!!!         call td_mask%get_mask(..., mask,_RC)
!!!      end if
!!!
!!!end module mapl_TimeDependentMask
!
