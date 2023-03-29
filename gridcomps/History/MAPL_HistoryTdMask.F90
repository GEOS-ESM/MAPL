module MAPL_TimeDependentMask
  public
  type :: TimeDependentMask
     logical, allocatable :: mask(:,:)    ! for CS and LL
     type(ESMF_Time) :: mask_start
     type(ESMF_Time) :: mask_end
     type(ESMF_Time) :: mask_freq
     character(len=ESMF_MAXPATHLEN) :: mask_file_header
   contains
     procedure :: get_mask
  end type TimeDependentMask
  type(ESMF_Time) :: obs_start_time
  type(ESMF_Time) :: obs_end_time
  type(ESMF_TimeInterval) :: obs_interval
  type(ESMF_Time) :: bracket_obs_start
  type(ESMF_Time) :: bracket_obs_end

  
  interface TimeDependentMask
     procedure new_TimeDependentMask
  end interface TimeDependentMask

contains

  subroutine new_TimeDependentMask()
    ! convert mask start_time, end_time to ESMF time
    ! read in
  end subroutine new_TimeDependentMask
  
  subroutine get_mask(this, bundle, time_span, grid, rc)
    type(TimeDependentMask), intent(inout) :: this
    type(ESMF_FieldBundle), intent (in) :: bundle    ! input bundle
    type(ESMF_Time), intent (in) :: time_span(2)
    type(ESMF_grid), intent (in) :: grid
    integer, optional, intent(out) :: rc

    type(ESMF_grid) :: grid_in   ! CS or LL from model/bundle
    integer :: status
    integer, allocatable :: COUNTS(:)
    integer :: IM, JM, LM, IM_WORLD, JM_WORLD
    
    this%mask = 1

    ! s1. get esmf grid dim, default mask=.F.
    call ESMF_FieldBundleGet(bundle, grid=grid_in, _RC)
    call ESMF_GridGet(grid_in, DistGrid=disgrid, dimCount=dimCount, _RC)
    call ESMF_DistGridGet(distgrid, deLayout=LAYOUT, _RC)
    call EMSF_VmGetCurrent(VM, _RC)
    call ESMF_VmGet(VM, localPet=myid, petCount=ndes, _RC)
    
    call ESMF_GridGet(grid_in, localCellCountPerDim=COUNTS, _RC)
    IM= COUNTS(1)
    JM= COUNTS(2)
    LM= COUNTS(3)

    call ESMF_GridGet(grid_in, globalCellCountPerDim=COUNTS, _RC)
    IM_WORLD= COUNTS(1)
    JM_WORLD= CONNTS(2)


    this%mask = 1


    
  end subroutine get_mask
end module MAPL_TimeDependentMask



!!MAPL_HistoryTdMask.F90
!!! In History
!!!  - New option to specify a time-dependent mask:   template
!!!    (up where we define grids and such)
!!!  - Add way to specify that the named mask is used in a collection
!!!  - During run step
!!!      - check if collection needs mask
!!!      - if so use get_mask() to get mask
!!!      - WHERE (.not. mask) T = undef
!!!
!!
!!module mapl_TimeDependentMask
!!   implicit none
!!
!!   type :: TimeDependentMask
!!   contains
!!      procedure :: mask
!!   end type TimeDependentMask
!!
!!contains
!!
!!   function new_TimeDependentMask(template) result(mask)
!!      character(*), intent(in) :: template
!!   end function new_TimeDependentMask
!!
!!   subroutine get_mask(this, time_interval, grid, mask, rc)
!!      class(TimeDependentMask), intent(inout) :: this
!!      type(ESMF_Time), intent(in) :: time_interval(2)
!!      type(ESMF_Grid), intent(inout) :: grid
!!      logical, intent(out) :: mask(:,:)
!!      integer, optional, intent(in) :: rc
!!
!!      ! (0) Set mask(I,J) = .false.
!!
!!
!!
!!
!!
!!t(in) :: time_interval(2)
!!type(ESMF_Grid), intent(inout) :: grid
!!logical, intent(out) :: mask(:,:)
!!integer, optional, intent(in) :: rc
!!
!!! (0) Set mask(I,J) = .false.
!!
!!type(ESMF_Grid), intent(inout) :: grid
!!logical, intent(out) :: mask(:,:)
!!integer, optional, intent(in) :: rc
!!
!!! (0) Set mask(I,J) = .false.~
!!
!!
!!      ! (1) Figure out which files in template overlap time interval
!!      ! Similar logic exists in ExtData
!!
!!      ! (2) For each file with overlap
!!      !         Determine subset (lat,lon) that is in time window
!!      ! (3) For each (lat,lon) determine (I,J,FACE)
!!      !     if proc has (I,J,FACE) set mask(I,J) = .true.
!!      !     Logic is grid dependent. (unfortunate)
!!
!!      !   MAPL/base/Base_Base_implementation.F90
!!      !    module subroutine MAPL_GetGlobalHorzIJIndex(npts,II,JJ,lon,lat,lonR8,latR8,Grid, rc)
!!
!!
!!   end subroutine get_mask
!!
!!      ! In history - after regrid step
!!      if (collection_is_masked) then
!!         call td_mask%get_mask(..., mask,_RC)
!!      end if
!!
!!end module mapl_TimeDependentMask
!!
!!CO2_GridComp_ExtData.rc
!!
!!Y F%y4-%m2-%d2t12:00:00 none     none     emco2ff       ExtData/PIESA/sfc/bian.co2.x288_y181_t12.2001.nc
!!CO2_NEP         'kg C m-2 s-1'      Y Y F%y4-%m2-%d2t12:00:00 none     none     emco2nep      ExtData/PIESA/sfc/bian.co2.x288_y181_t12.2001.nc
!!CO2_OCN         'kg C m-2 s-1'      Y Y F%y4-%m2-%d2t12:00:00 none     none     emco2ocn      ExtData/PIESA/sfc/bian.co2.x288_y181_t12.2001.nc
!!
!!CO2_CMS_BIOMASS 'kg C m-2 s-1'      N Y F%y4-%m2-%d2t12:00:00 none     none     biomass       ExtData/PIESA/sfc/CMS/em.daily.1x1.25.%y4.nc
!!CO2_CMS_FF      'kg C m-2 s-1'      N Y F%y4-%m2-%d2t12:00:00 none     none     emco2ff       ExtData/PIESA/sfc/CMS/ORNL_TRANSCOM.co2_ff_nep_ocn.x288_y181_t12.%y4.nc
!!CO2_CMS_NEP     'kg C m-2 s-1'      N Y  P0000-00-00T03:00    none     none     emco2nep      ExtData/PIESA/sfc/CMS/casa.3hr.1x1.25.%y4.nc
!!CO2_CMS_OCN     'kg C m-2 s-1'      N Y F%y4-%m2-%d2t12:00:00 none     none     emco2ocn      ExtData/PIESA/sfc/CMS/NOBM_fco2.daily.1x1.25.%y4.nc
!!
!!CO2_regionMask  NA                  N v   -                   none     none     REGION_MASK   ExtData/PIESA/sfc/ARCTAS.region_mask.x540_y361.2008.nc
!!#---------------+-------------------+-+-+---------------------+--------+--------+-------------+----------------------
!!%%~
!!
!!
