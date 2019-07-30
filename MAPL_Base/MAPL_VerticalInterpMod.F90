#include "MAPL_ErrLog.h"
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: linearVerticalInterpolation_mod
!
      module linearVerticalInterpolation_mod

      use ESMF
      use MAPL_BaseMod
      use MAPL_ConstantsMod, only: MAPL_KAPPA, MAPL_RGAS, MAPL_CP, MAPL_GRAV
      use MAPL_ErrorHandlingMod
      use, intrinsic :: iso_fortran_env, only: REAL64
!
      implicit none
!
      private
!
! !PUBLIC MEMBER FUNCTIONS:

     public  :: vertInterpolation_pressKappa
!
! !DESCRIPTION:
! Contains routines for linear vertical interpolation in the pressure
! to the KAPPA formulation.
!
!EOP
!------------------------------------------------------------------------------
CONTAINS
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: vertInterpolation_pressKappa
!
! !INTERFACE:
!
      subroutine vertInterpolation_pressKappa (fModel, fPres, ps, plevs, &
                                  undef, phis, phis_units, rc)
!
! !INPUT PARAMETERS:
      type(ESMF_Field), intent(INOUT) :: fPres  
      type(ESMF_Field), intent(INOUT) :: ps  
      real,    intent(IN) :: plevs(:)        ! pressure levels
      real,    intent(IN) :: undef                ! undefined value
      type(ESMF_Field), optional, intent(INOUT) :: PHIS   ! Surface Geopotential Heights
      character(len=*), optional, intent(in) :: phis_units  ! units of phis
!
! !OUTPUT PARAMETERS:
      type(ESMF_Field), intent(INOUT) :: fModel  
      integer, optional, intent(OUT) :: rc
!
! !DESCRIPTION:
!  Perform a linear vertical interpolation from pressure levels to model levels
!  in the pressure kappa formulation.
!  It is assumed that we have a top to bottom ordering of the vertical levels.
!
! !LOCAL VARIABLES:
      integer :: im, jm, lmMod, lmPres
      integer :: i, j, L, k, k2, k3, dims(3)
      integer, allocatable :: kbeg(:,:),kend(:,:)
      real    :: vdef, vsurf, TH
      real,allocatable :: pl_mod(:,:,:),ple_mod(:,:,:)
      character(len=1) :: vartype
      real(REAL64), allocatable :: ak(:),bk(:)
      integer :: status
      character(len=ESMF_MAXSTR) :: Iam = "MAPL_VerticalInterp"
      real    :: gfactor
      type(ESMF_Grid) :: grid
      real, pointer   :: vMod(:,:,:), vPres(:,:,:), vPS(:,:), vPHIS(:,:)
      character(ESMF_MAXSTR) :: vname, units
!
!EOP
!------------------------------------------------------------------------------
!BOC

      ! get dimensions, allocate
      call ESMF_FieldGet(fModel,grid=grid,rc=status)
      _VERIFY(STATUS)
      call ESMF_AttributeGet(fModel,name='UNITS',value=units,rc=status)
      _VERIFY(STATUS)
      call ESMF_AttributeGet(fModel,name='LONG_NAME',value=vname,rc=status)
      _VERIFY(STATUS)
      vname = ESMF_UtilStringLowerCase(vname,rc=status)
      call MAPL_GridGet(grid, localCellCountPerDim=dims,rc=status)
      _VERIFY(STATUS)
      im = dims(1)
      jm = dims(2)
      lmMod = dims(3)
      allocate(ak(lmMod+1),stat=status)
      _VERIFY(STATUS)
      allocate(bk(lmMod+1),stat=status)
      _VERIFY(STATUS)
      allocate(pl_mod(im,jm,lmMod),stat=status)
      _VERIFY(STATUS)
      allocate(ple_mod(im,jm,lmMod+1),stat=status)
      _VERIFY(STATUS)
      allocate(kbeg(im,jm),stat=status)
      _VERIFY(STATUS)
      allocate(kend(im,jm),stat=status)
      _VERIFY(STATUS)

      call ESMF_FieldGet(fPres,grid=grid,rc=status)
      _VERIFY(STATUS)
      call MAPL_GridGet(grid, localCellCountPerDim=dims,rc=status)
      _VERIFY(STATUS)
      lmPres = dims(3)

      ! given PS, get AK, BK from the grid to get ple
      call ESMF_FieldGet(PS,0,farrayPtr=vPS,rc=status)
      _VERIFY(STATUS)
      call ESMF_FieldGet(PS,grid=grid,rc=status)
      _VERIFY(STATUS)
      call ESMF_AttributeGet(grid,name="GridAK",valuelist=ak,rc=status)
      _VERIFY(STATUS)
      call ESMF_AttributeGet(grid,name="GridBK",valuelist=bk,rc=status)
      _VERIFY(STATUS)
      do i=1,lmmod+1
         ple_mod(:,:,i)=ak(i)+bk(i)*vPS(:,:)
      enddo
      do i=1,lmmod
         pl_mod(:,:,i)=(ple_mod(:,:,i)+ple_mod(:,:,i+1))/2.0
      enddo
      
      call ESMF_FieldGet(fModel,0,farrayPtr=vMod,rc=status)
      _VERIFY(STATUS)
      call ESMF_FieldGet(fPres,0,farrayPtr=vPres,rc=status)
      _VERIFY(STATUS)

      !---------------------------------------------------------------
      ! For each grid point (i,j), determine the range of model levels
      ! (kbeg to kend) that will be used for the main interpolation
      ! formula.
      !---------------------------------------------------------------
      call searchVerticalLocations(pl_mod, plevs, kbeg, kend, im, jm, lmmod, lmpres)

      !-------------------------------
      ! Compute trapped interpolations
      !-------------------------------
    
      DO j = 1, jm
         DO i = 1, im
            DO L = kbeg(i,j), kend(i,j)
               SEARCH_LEVELS : DO k = 2, lmpres
                  IF (plevs(k)   .GE. pl_mod(i,j,L) .AND. &
                      plevs(k-1) .LT. pl_mod(i,j,L) ) THEN
                     IF ( (vpres(i,j,k) .NE. undef) .AND. (vpres(i,j,k-1) .NE. undef) ) THEN
                        ! Apply the pressure to the kappa linear interpolation
                        vmod(i,j,L) = interpVal(vpres(i,j,k), vpres(i,j,k-1), pl_mod(i,j,L), &
                                                plevs(k), plevs(k-1), MAPL_KAPPA)
                     ELSEIF ( (vpres(i,j,k) .NE. undef) .AND. (vpres(i,j,k-1) .EQ. undef) ) THEN
                        vmod(i,j,L) = vpres(i,j,k)
                     ELSEIF ( (vpres(i,j,k) .EQ. undef) .AND. (vpres(i,j,k-1) .NE. undef) ) THEN
                        vmod(i,j,L) = vpres(i,j,k-1)
                     ELSEIF ( (vpres(i,j,k) .EQ. undef) .AND. (vpres(i,j,k-1) .EQ. undef) ) THEN
                        ! Search for the first non undefined value
                        vdef = undef
                        ! First: search by going toward the surface
                        SEARCH_DEF3: DO k3 = k+1,lmpres
                           IF (vpres(i,j,k3) .NE. undef) THEN
                              vdef = vpres(i,j,k3)
                              EXIT SEARCH_DEF3
                           END IF
                        END DO SEARCH_DEF3

                        IF (vdef .EQ. undef) THEN
                        ! Second: search by going toward the top
                           SEARCH_DEF4: DO k3 = k-2, 1, -1
                              IF (vpres(i,j,k3) .NE. undef) THEN
                                 vdef = vpres(i,j,k3)
                                 EXIT SEARCH_DEF4
                              END IF
                           END DO SEARCH_DEF4
                        END IF
                        if (vdef == undef) then
                           _RETURN(ESMF_FAILURE)
                        end if
                        vmod(i,j,L) = vdef
                     END IF
                     EXIT SEARCH_LEVELS
                  END IF
               ENDDO SEARCH_LEVELS
            ENDDO
         ENDDO
      ENDDO

      !--------------------------------------
      ! Do extrapolation on levels below kbeg
      ! (toward the top of the atmosphere)
      !--------------------------------------
      DO j = 1, jm
         DO i = 1, im
            vdef = vpres(i,j,1)
            IF (vdef .EQ. undef) THEN
               ! Search for the first non undefined value
               SEARCH_DEF1: DO k = 2, lmpres
                  IF (vpres(i,j,k) .NE. undef) THEN
                     vdef = vpres(i,j,k)
                     EXIT SEARCH_DEF1
                  END IF
               END DO SEARCH_DEF1
            END IF
            DO L = 1, kbeg(i,j) -1
               vmod(i,j,L) = vdef
            ENDDO
         ENDDO
      ENDDO
      
      !--------------------------------------
      ! Do extrapolation on levels above kend
      ! (toward the surface) get var type
      !--------------------------------------
      ! = 'W' for winds ( U & V)
      ! = 'T' for temperature
      ! = 'H' for height                        
      ! = 'O' for any other variable
      if (index(vname,"wind")/=0) then
         varType = 'W'
      else if (index(vname,"temperature")/=0) then
         varType = 'T'
      else if (index(vname,"height")/=0) then
         varType = 'H'
         _ASSERT(present(PHIS),'needs informative message')
         call ESMF_FieldGet(PHIS,0,farrayPtr=vPHIS,rc=status)
         _VERIFY(STATUS)
         _ASSERT(present(phis_units),'needs informative message')
         if (trim(units)=="m" .and. trim(phis_units)=="m+2 s-2") then
            gfactor = MAPL_GRAV
         else
            gfactor = 1.0
         end if   
      else
         varType = 'O'
      end if                  
      DO j = 1, jm
         DO i = 1, im
            vdef = vpres(i,j,lmpres)
            IF (vdef .EQ. undef) THEN
               ! Search for the first non undefined value
               SEARCH_DEF2: DO k2 = lmpres-1, 1, -1
                  IF (vpres(i,j,k2) .NE. undef) THEN
                     vdef = vpres(i,j,k2)
                     EXIT SEARCH_DEF2
                  END IF
               END DO SEARCH_DEF2
               DO L = kend(i,j)+1, lmmod
                  IF (varType .EQ. 'W') THEN
                     vsurf = 0.0
                     vmod(i,j,L) = interpVal(vsurf, vdef, pl_mod(i,j,L), &
                                                  plevs(lmpres), plevs(k2), MAPL_KAPPA)
                     !vmod(i,j,L) = interpVal(vdef, vsurf, pl_mod(i,j,L), &
                     !                        plevs(k2), plevs(lmpres),  MAPL_KAPPA)
                  ELSE IF (varType .EQ. 'T') THEN
                     ! Compute the potential temperature at pressure level k2
                     TH = vdef / ( (plevs(k2)*1.00E-05)**(MAPL_RGAS/MAPL_CP) )
                     ! Reverse back to the temperature at model level L
                     vmod(i,j,L) = TH * ( (pl_mod(i,j,L)*1.00E-05)**(MAPL_RGAS/MAPL_CP) )
                  ELSE IF (varType .EQ. 'H') THEN
                     vsurf = vPHIS(i,j)/gfactor
                     vmod(i,j,L) = interpVal(vsurf, vdef, pl_mod(i,j,L), &
                                                  plevs(lmpres), plevs(k2), MAPL_KAPPA)
                  ELSE
                     vmod(i,j,L) = vdef
                  END IF
               ENDDO
            ELSE
               DO L = kend(i,j)+1, lmmod
                  vmod(i,j,L) = vpres(i,j,lmpres)
               ENDDO
            END IF
         ENDDO
      ENDDO
      
      deallocate(ak)
      deallocate(bk)
      deallocate(pl_mod)
      deallocate(ple_mod)
      deallocate(kbeg)
      deallocate(kend)

      _RETURN(ESMF_SUCCESS)

      end subroutine vertInterpolation_pressKappa
!EOC
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: interpVal
! 
! !INTERFACE
!
      FUNCTION interpVal (var1, var2, ple, plev1, plev2, KAP) result(val)
!
! !INPUT PARAMETERS:
      real :: var1      ! variable (pressure level) value at level k
      real :: var2      ! variable (pressure level) value at level k-1
      real :: ple       ! model pressure at current level
      real :: plev1     ! pressure value at level k
      real :: plev2     ! pressure value at level k-1
      real :: KAP       ! coefficient KAPPA
!      
! ! RETURNED VALUE:
      real :: val
!
! !DESCRIPTION:
! Main vertical interpolation formula:
!
!  \begin{eqnarray*}
!  \alpha_{L} & = & \frac{ \alpha_{k-1}P_{L}^{\kappa} - \alpha_{k}P_{L}^{\kappa} + 
!                   \alpha_{k}P_{k-1}^{\kappa}  - \alpha_{k-1}P_{k}^{\kappa} }
!                   { P_{k-1}^{\kappa} - P_{k}^{\kappa} }
!  \end{eqnarray*}
!
! In this particular case, we have:
!
!  \begin{eqnarray*}
!  val & = & \frac{ var2 \times ple^{KAP}    - var1 \times ple^{KAP} + 
!                   var1 \times plev2^{KAP}  - var2 \times plev1^{KAP} }
!                   { plev2^{KAP} - plevs^{KAP} }
!  \end{eqnarray*}
!
!EOP
!------------------------------------------------------------------------------
!BOC

      val = ( var1*(ple**KAP - plev2**KAP) - &
              var2*(ple**KAP - plev1**KAP) ) / (plev1**KAP - plev2**KAP)

      END FUNCTION interpVal
!EOC
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: searchVerticalLocations
!
! !INTERFACE
!
      subroutine searchVerticalLocations(pl_mod, plevs, kbeg, kend, &
                                    im, jm, lmmod, lmpres)
!
! !INPUT PARAMETERS:
      integer, intent(IN) :: im                  ! number of longitude grid points
      integer, intent(IN) :: jm                  ! number of latitude  grid points
      integer, intent(IN) :: lmmod               ! number of model vertical levels
      integer, intent(IN) :: lmpres              ! number of pressure vertical levels
      real,    intent(IN) :: pl_mod(im,jm,lmmod) ! 3D mid pressure on model levels
      real,    intent(IN) :: plevs(lmpres)       ! pressure levels
!
! !OUTPUT PARAMETERS:
      integer, intent(OUT) :: kbeg(im,jm)        ! starting location
      integer, intent(OUT) :: kend(im,jm)        ! ending location
!
! !DESCRIPTION:
!  For each horizontal grid point, search the last pressure level that
!  is below lowest model levels and the first pressure level that is above the
!  last model level. This allows to speed up the calculations while doing
!  interpolations.
!
! !LOCAL VARIABLES:
      integer :: i, j, L, k

!EOP
!------------------------------------------------------------------------------
!BOC
      !-----------------------------------------------------------------------
      ! Find the starting location.
      ! The kbeg in each column is the index of model level data which has the
      ! smaller pressure (highest level) that is trapped by pressure levels.
      !-----------------------------------------------------------------------

      kbeg(:,:) = 1
      DO j = 1, jm
         DO i = 1, im
            model_pressure_beg : DO L = 2, lmmod
               !pressure_search_beg : DO k = lmpres, 2, -1
               pressure_search_beg : DO k = 2, lmpres
                  IF ( (pl_mod(i,j,L) .LE. plevs(k  )) .AND. &
                       (pl_mod(i,j,L) .GT. plevs(k-1))) THEN
                     kbeg(i,j) = L
                     EXIT model_pressure_beg
                  ELSE IF (pl_mod(i,j,L) .LT. plevs(k-1)) THEN
                     EXIT pressure_search_beg
                  END IF
               ENDDO pressure_search_beg
            ENDDO model_pressure_beg
         ENDDO
      ENDDO

      !------------------------------------------------------------------------
      ! Find the ending location.
      ! The kend in each column is the index of model level data which has the
      ! largest pressure (lowest level) that is trapped by pressure levels.
      !------------------------------------------------------------------------

      kend(:,:) = lmmod
      DO j = 1, jm
         DO i = 1, im
            model_pressure_end : DO L = lmmod, 2, -1
               !pressure_search_end : DO k = 2, lmpres
               pressure_search_end : DO k = lmpres, 2, -1
                  IF ( (pl_mod(i,j,L) .LE. plevs(k  )) .AND. &
                       (pl_mod(i,j,L) .GT. plevs(k-1))) THEN
                     kend(i,j) = L
                     EXIT model_pressure_end
                  !ELSE IF (pl_mod(i,j,L) .LT. plevs(k-1)) THEN
                  ELSE IF (pl_mod(i,j,L) .GT. plevs(k)) THEN
                     EXIT pressure_search_end
                  END IF
               ENDDO pressure_search_end
            ENDDO model_pressure_end
         ENDDO
      ENDDO

      return

      end subroutine searchVerticalLocations
!EOC
!------------------------------------------------------------------------------


      end module linearVerticalInterpolation_mod
