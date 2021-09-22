#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_Base

  !BOP
  !
  ! !MODULE: MAPL_BaseMod --- A Collection of Assorted MAPL Utilities

  ! !USES:
  !
  use ESMF, only: ESMF_MAXSTR
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit NONE
  private

  ! !PUBLIC MEMBER FUNCTIONS:
  !
  public MAPL_AllocateCoupling    ! Atanas: please provide 1-line for each
  public MAPL_FieldAllocCommit
  !public MAPL_FieldF90Deallocate
  public MAPL_ClimInterpFac
  !public MAPL_ConnectCoupling
  public MAPL_DecomposeDim
  public MAPL_MakeDecomposition
  public MAPL_FieldCreate
  public MAPL_FieldCreateEmpty
  public MAPL_FieldGetTime
  public MAPL_FieldSetTime
  public MAPL_GRID_INTERIOR
  public MAPL_IncYMD
  public MAPL_Interp_Fac
  public MAPL_LatLonGridCreate   ! Creates regular Lat/Lon ESMF Grids
  public MAPL_Nhmsf
  public MAPL_NSECF
  public MAPL_Nsecf2
  public MAPL_PackTime
  public MAPL_PackDateTime
  public MAPL_RemapBounds
  public MAPL_Tick
  public MAPL_TimeStringGet
  public MAPL_UnpackTime
  public MAPL_UnpackDateTime
  public MAPL_RmQualifier
  public MAPL_AttributeSet
  public MAPL_SetPointer
  public MAPL_FieldCopyAttributes
  public MAPL_StateAdd
  public MAPL_FieldBundleAdd
  public MAPL_FieldBundleGet
  public MAPL_FieldDestroy
  public MAPL_FieldBundleDestroy
  public MAPL_GetHorzIJIndex
  public MAPL_GenGridName
  public MAPL_GenXYOffset
  public MAPL_GeosNameNew
  public MAPL_Communicator
  public MAPL_BundleCreate
  public MAPL_FieldCopy
  public MAPL_Leap
  public MAPL_GridGetCorners
  public MAPL_GridGetInterior
  public MAPL_TrimString
  public MAPL_FieldSplit
  public MAPL_GetCorrectedPhase


  real,    public, parameter :: MAPL_UNDEF              = 1.0e15  


  character(len=ESMF_MAXSTR), public, parameter :: MAPL_StateItemOrderList = 'MAPL_StateItemOrderList'
  character(len=ESMF_MAXSTR), public, parameter :: MAPL_BundleItemOrderList = 'MAPL_BundleItemOrderList'

  type ::  MAPL_Communicator
     integer :: comm
     integer :: rank
     integer :: root
     integer :: size
     !logical :: am_i_root
  end type MAPL_Communicator

#ifdef __PROTEX__

  !DESCRIPTION:

  The module {\tt MAPL\_Base} provides a collection assorted
  utilities and constants used throughout the MAPL Library.


#endif

  !EOP
  !----------------------------------------------------------------------

  interface MAPL_FieldCreate
     module procedure MAPL_FieldCreateRename
     module procedure MAPL_FieldCreateNewgrid
     module procedure MAPL_FieldCreateR4
  end interface MAPL_FieldCreate

  interface MAPL_FieldGetTime
     module procedure MAPL_GetFieldTimeFromField
     module procedure MAPL_GetFieldTimeFromState
  end interface MAPL_FieldGetTime

  interface MAPL_FieldSetTime
     module procedure MAPL_SetFieldTimeFromField
     module procedure MAPL_SetFieldTimeFromState
  end interface MAPL_FieldSetTime

  interface MAPL_RemapBounds
     module procedure MAPL_RemapBoundsFull_3dr4
     module procedure MAPL_RemapBounds_3dr4
     module procedure MAPL_RemapBounds_3dr8
  end interface MAPL_RemapBounds

  interface MAPL_AttributeSet
     module procedure MAPL_StateAttSetI4
     module procedure MAPL_BundleAttSetI4
     module procedure MAPL_FieldAttSetI4
  end interface MAPL_AttributeSet

  interface MAPL_SetPointer
     module procedure MAPL_SetPointer2DR4
     module procedure MAPL_SetPointer3DR4
  end interface MAPL_SetPointer

  interface MAPL_StateAdd
     module procedure MAPL_StateAddField
     module procedure MAPL_StateAddBundle
  end interface MAPL_StateAdd

  interface MAPL_FieldBundleAdd
     module procedure MAPL_FieldBundleAddField
  end interface MAPL_FieldBundleAdd

  interface MAPL_FieldBundleGet
     module procedure MAPL_FieldBundleGetByIndex
  end interface MAPL_FieldBundleGet


  interface
     module subroutine MAPL_AllocateCoupling(field, rc)
       use ESMF, only: ESMF_Field
       type(ESMF_Field),  intent(INOUT) :: field
       integer, optional, intent(  OUT) :: rc             
     end subroutine MAPL_AllocateCoupling

     module subroutine MAPL_FieldAllocCommit(field, dims, location, typekind, &
          hw, ungrid, default_value, rc)
       use ESMF, only: ESMF_Field
       type(ESMF_Field),               intent(INOUT) :: field
       integer,                        intent(IN   ) :: dims            
       integer,                        intent(IN   ) :: location            
       integer,                        intent(IN   ) :: typekind
       integer,                        intent(IN   ) :: hw !halowidth
       integer,              optional, intent(IN   ) :: ungrid(:)
       real,                 optional, intent(IN   ) :: default_value
       integer,              optional, intent(  OUT) :: rc             
     end subroutine MAPL_FieldAllocCommit

     module subroutine MAPL_FieldF90Deallocate(field, rc)
       use ESMF, only: ESMF_Field
       type(ESMF_Field),  intent(INOUT) :: field
       integer, optional, intent(  OUT) :: rc             
     end subroutine MAPL_FieldF90Deallocate

     module subroutine MAPL_SetPointer2DR4(state, ptr, name, rc)
       use ESMF, only: ESMF_State
       type(ESMF_State),               intent(INOUT) :: state
       real,                           pointer       :: ptr(:,:)
       character(len=*),               intent(IN   ) :: name
       integer,              optional, intent(  OUT) :: rc             
     end subroutine MAPL_SetPointer2DR4

     module subroutine MAPL_SetPointer3DR4(state, ptr, name, rc)
       use ESMF, only: ESMF_State
       type(ESMF_State),               intent(INOUT) :: state
       real,                           pointer       :: ptr(:,:,:)
       character(len=*),               intent(IN   ) :: name
       integer,              optional, intent(  OUT) :: rc             
     end subroutine MAPL_SetPointer3DR4

     module subroutine MAPL_DecomposeDim ( dim_world,dim,NDEs, unusable, symmetric, min_DE_extent )
       use MAPL_KeywordEnforcerMod

       integer, intent(in) :: dim_world, NDEs
       integer, intent(out) :: dim(0:NDEs-1)
       class (KeywordEnforcer), optional, intent(in) :: unusable
       logical, intent(in), optional :: symmetric
       integer, optional, intent(in) :: min_DE_extent
     end subroutine MAPL_DecomposeDim

     module subroutine MAPL_MakeDecomposition(nx, ny, unusable, reduceFactor, rc)
       use MAPL_KeywordEnforcerMod
       integer, intent(out) :: nx
       integer, intent(out) :: ny
       class (KeywordEnforcer), optional, intent(in) :: unusable
       integer, optional, intent(in) :: reduceFactor
       integer, optional, intent(out) :: rc
     end subroutine MAPL_MakeDecomposition

     module subroutine MAPL_Interp_Fac (TIME0, TIME1, TIME2, FAC1, FAC2, RC)
       use ESMF, only: ESMF_Time
       !------------------------------------------------------------        

       !  PURPOSE:
       !  ========
       !
       !    Compute interpolation factors, fac, to be used 
       !    in the calculation of the instantaneous boundary 
       !    conditions, ie:
       !
       !     q(i,j) = fac1*q1(i,j) + (1.-fac1)*q2(i,j)
       !
       !    where:
       !     q(i,j)  => Boundary Data valid    at time0
       !     q1(i,j) => Boundary Data centered at time1
       !     q2(i,j) => Boundary Data centered at time2

       !  INPUT:
       !  ======
       !    time0    : Time of current timestep
       !    time1    : Time of boundary data 1 
       !    time2    : Time of boundary data 2 

       !  OUTPUT:
       !  =======
       !     fac1    : Interpolation factor for Boundary Data 1
       !
       ! ------------------------------------------------------------        
       !               GODDARD LABORATORY FOR ATMOSPHERES            
       ! ------------------------------------------------------------        

       type(ESMF_Time),   intent(in ) :: TIME0, TIME1, TIME2
       real,              intent(out) :: FAC1
       real,    optional, intent(out) :: FAC2
       integer, optional, intent(out) :: RC
     end subroutine MAPL_Interp_Fac

     module subroutine MAPL_ClimInterpFac (CLOCK,I1,I2,FAC, RC)
       use ESMF, only: ESMF_Clock

       !------------------------------------------------------------        

       type(ESMF_CLOCK),  intent(in ) :: CLOCK
       integer,           intent(OUT) :: I1, I2
       real,              intent(out) :: FAC
       integer, optional, intent(out) :: RC
     end subroutine MAPL_ClimInterpFac


     module subroutine MAPL_TimeStringGet(TIMESTRING,YY,MM,DD,H,M,S)
       character(len=*),  intent (IN ) :: TIMESTRING
       integer, optional, intent (OUT) :: YY
       integer, optional, intent (OUT) :: MM
       integer, optional, intent (OUT) :: DD
       integer, optional, intent (OUT) :: H
       integer, optional, intent (OUT) :: M
       integer, optional, intent (OUT) :: S
     end subroutine MAPL_TimeStringGet


     module subroutine MAPL_UnpackTime(TIME,IYY,IMM,IDD)
       integer, intent (IN ) :: TIME
       integer, intent (OUT) :: IYY
       integer, intent (OUT) :: IMM
       integer, intent (OUT) :: IDD
     end subroutine MAPL_UnpackTime


     module subroutine MAPL_PackTime(TIME,IYY,IMM,IDD)
       integer, intent (OUT) :: TIME
       integer, intent (IN ) :: IYY
       integer, intent (IN ) :: IMM
       integer, intent (IN ) :: IDD
     end subroutine MAPL_PackTime


     module subroutine MAPL_PackDateTime(date_time, yy, mm, dd, h, m, s)
       integer, intent(in) :: yy, mm, dd, h, m, s
       integer, intent(out) :: date_time(:)
     end subroutine MAPL_PackDateTime


     module subroutine MAPL_UnpackDateTime(date_time, yy, mm, dd, h, m, s)
       integer, intent(in) :: date_time(:)
       integer, intent(out) :: yy, mm, dd, h, m, s
     end subroutine MAPL_UnpackDateTime


     integer module function MAPL_nsecf(nhms)
       integer, intent(in) :: nhms
     end function MAPL_nsecf

     module subroutine MAPL_tick (nymd,nhms,ndt)
       integer nymd,nhms,ndt
     end subroutine MAPL_tick

     integer module function MAPL_nsecf2 (nhhmmss,nmmdd,nymd)
       integer nhhmmss,nmmdd,nymd
     end function MAPL_nsecf2

     integer module function MAPL_nhmsf (nsec)
       implicit none
       integer  nsec
     end function MAPL_nhmsf

     ! A year is a leap year if
     ! 1) it is divible by 4, and 
     ! 2) it is not divisible by 100, unless
     ! 3) it is also divisible by 400.
     logical module function MAPL_LEAP(NY)
       integer, intent(in) :: NY
     end function MAPL_LEAP


     integer module function MAPL_incymd (NYMD,M)                                                  
       integer nymd,m
     end function MAPL_incymd


     module subroutine MAPL_PICKEM(II,JJ,IM,JM,COUNT)
       integer, intent(IN ) :: IM, JM, COUNT
       integer, intent(OUT) :: II(COUNT), JJ(COUNT)
     end subroutine MAPL_PICKEM


     module subroutine MAPL_GetFieldTimeFromField ( FIELD, TIME, RC )
       use ESMF, only: ESMF_Field, ESMF_Time
       type(ESMF_Field),        intent(INOUT) :: FIELD ! ALT: IN
       type(ESMF_Time),         intent(  OUT) :: TIME
       integer, optional,       intent(  OUT) :: RC
     end subroutine MAPL_GetFieldTimeFromField

     ! ------------------------------------------------------------------------------

     module subroutine  MAPL_SetFieldTimeFromField (FIELD, TIME, RC )
       use ESMF, only: ESMF_Field, ESMF_Time
       type(ESMF_FIELD),        intent(INOUT) :: FIELD
       type(ESMF_TIME),         intent(INOUT) :: TIME !ALT: IN
       integer, optional,       intent(  OUT) :: RC
     end subroutine  MAPL_SetFieldTimeFromField


     module subroutine  MAPL_GetFieldTimeFromState ( STATE, Fieldname, TIME, RC )
       use ESMF, only: ESMF_State, ESMF_Time
       type(ESMF_STATE),        intent(IN   ) :: STATE
       character(len=*),        intent(IN   ) :: Fieldname
       type(ESMF_Time),         intent(  OUT) :: TIME
       integer, optional,       intent(  OUT) :: RC
     end subroutine  MAPL_GetFieldTimeFromState

     ! ------------------------------------------------------------------------------

     module subroutine  MAPL_SetFieldTimeFromState ( STATE, Fieldname, TIME, RC )
       use ESMF, only: ESMF_State, ESMF_Time
       type(ESMF_STATE),        intent(INOUT) :: STATE
       character(len=*),        intent(IN   ) :: Fieldname
       type(ESMF_Time),         intent(INOUT) :: TIME !ALT: IN
       integer, optional,       intent(  OUT) :: RC
     end subroutine  MAPL_SetFieldTimeFromState


     module function MAPL_FieldCreateRename(FIELD, NAME, DoCopy, RC) RESULT(F)
       use ESMF, only: ESMF_Field
       type (ESMF_Field), intent(INOUT) :: FIELD !ALT: IN
       character(len=*),  intent(IN   ) :: NAME
       logical, optional, intent(IN   ) :: DoCopy
       integer, optional, intent(  OUT) :: RC
       type (ESMF_Field)                :: F
     end function MAPL_FieldCreateRename

     module function MAPL_FieldCreateNewgrid(FIELD, GRID, LM, NEWNAME, RC) RESULT(F)
       use ESMF, only: ESMF_Field, ESMF_Grid
       type (ESMF_Field), intent(INOUT) :: FIELD !ALT: intent(IN)
       type (ESMF_Grid),  intent(INout) :: GRID
       integer, optional, intent(IN   ) :: lm
       character(len=*), optional, intent(IN) :: newName
       integer, optional, intent(  OUT) :: RC
       type (ESMF_Field)                :: F
     end function MAPL_FieldCreateNewgrid

     module function MAPL_FieldCreateR4(FIELD, RC) RESULT(F)
       use ESMF, only: ESMF_Field
       type (ESMF_Field), intent(INOUT) :: FIELD !ALT: IN
       integer, optional, intent(  OUT) :: RC
       type (ESMF_Field)                :: F
     end function MAPL_FieldCreateR4

     module function MAPL_FieldCreateEmpty(NAME, GRID, RC) RESULT(FIELD)
       use ESMF, only: ESMF_Field, ESMF_Grid
       character(len=*),  intent(IN   ) :: NAME
       type (ESMF_Grid),  intent(INout) :: GRID
       integer, optional, intent(  OUT) :: RC
       type (ESMF_Field)                :: FIELD
     end function MAPL_FieldCreateEmpty

     module subroutine MAPL_FieldCopyAttributes(FIELD_IN, FIELD_OUT, RC)
       use ESMF, only: ESMF_Field
       type (ESMF_Field), intent(INOUT) :: FIELD_IN !ALT: intent(in)
       type (ESMF_Field), intent(INOUT) :: FIELD_OUT
       integer, optional, intent(  OUT) :: RC
     end subroutine MAPL_FieldCopyAttributes

     module subroutine MAPL_FieldCopy(from, to, RC)
       use ESMF, only: ESMF_Field
       type (ESMF_Field), intent(INOUT) :: FROM !ALT: IN
       type (ESMF_Field), intent(INOUT) :: TO !ALT: OUT
       integer, optional, intent(  OUT) :: RC
     end subroutine MAPL_FieldCopy

     !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     module function MAPL_RemapBounds_3dr4(A, LB1, LB2, LB3) result(ptr)
       integer,      intent(IN) :: LB1, LB2, LB3
       real, target, intent(IN) :: A(LB1:,LB2:,LB3:)
       real, pointer            :: ptr(:,:,:)
     end function MAPL_RemapBounds_3dr4

     !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     module function MAPL_RemapBounds_3dr8(A, LB1, LB2, LB3) result(ptr)
       integer,      intent(IN) :: LB1, LB2, LB3
       real(kind=REAL64), target, intent(IN) :: A(LB1:,LB2:,LB3:)
       real(kind=REAL64), pointer            :: ptr(:,:,:)
     end function MAPL_RemapBounds_3dr8

     !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

     !BOP

     ! !IROUTINE: MAPL_LatLonGridCreate --- Create regular Lat/Lon Grid
     !
     ! !INTERFACE:

     module function MAPL_LatLonGridCreate (Name, vm,                 &
          Config, ConfigFile,       &
          Nx, Ny,                   &
          IM_World, BegLon, DelLon, &
          JM_World, BegLat, DelLat, &
          LM_World,                 &
          rc)                       &
          result(Grid)
       use ESMF, only: ESMF_VM, ESMF_Config, ESMF_Grid

       ! !INPUT PARAMETERS:

       character(len=*),            intent(in)  :: Name
       type (ESMF_VM),    OPTIONAL, target,     &
            intent(in)  :: VM


       !   There are 3 possibilities to provide the coordinate information:

       ! 1) Thru Config object:
       type(ESMF_Config), OPTIONAL, target,     & 
            intent(in)  :: Config 

       ! 2) Thru a resource file:
       character(len=*),  OPTIONAL, intent(in)  :: ConfigFile


       ! 3) Thru argument list:
       integer,           OPTIONAL, intent(in)  :: Nx, Ny          ! Layout
       integer,           OPTIONAL, intent(in)  :: IM_World        ! Zonal 
       real,              OPTIONAL, intent(in)  :: BegLon, DelLon  ! in degrees

       integer,           OPTIONAL, intent(in)  :: JM_World        ! Meridional
       real,              OPTIONAL, intent(in)  :: BegLat, DelLat  ! in degrees

       integer,           OPTIONAL, intent(in)  :: LM_World        ! Vertical

       ! !OUTPUT PARAMETERS:

       type (ESMF_Grid)                         :: Grid  ! Distributed grid
       integer,           OPTIONAL, intent(out) :: rc    ! return code
#ifdef ___PROTEX___

       !DESCRIPTION: 

       This routine creates a distributed ESMF grid where the horizontal
       coordinates are regular longitudes and latitudes. The grid is 
       created on the user specified {\bf VM}, or on the current VM if the user 
       does not specify one. The layout and the coordinate information can
       be provided with a {\tt ESMF\_Config attribute}, a resource file name
       or specified through the argument list.

       \subsubsection*{Using resource files}

       The {\bf resource file} {\tt ConfigFile} has a syntax similar to a GrADS
       control file.  Here is an example defining a typical GEOS-5 1x1.25
       grid with 72 layers:
       %
       \begin{verbatim}
       GDEF: LatLon 
       IDEF: 32  
       JDEF: 16  
       LDEF:  1  
       XDEF: 288 LINEAR -180. 1.25
       YDEF: 181 LINEAR -90. 1.
       ZDEF:  72 LINEAR 1 1
       \end{verbatim}
       %
       More generally, 
       \begin{verbatim}
       GDEF: LatLon 
       IDEF: Nx 
       JDEF: Ny
       LDEF: Nz
       XDEF: IM_World XCoordType BegLon, DelLon
       YDEF: JM_World YCoordType BegLat, DelLat
       ZDEF: LM_World ZCoordType 1        1
       \end{verbatim}
       The attribute {\bf GDEF} must always be {\tt LatLon} for  Lat/Lon grids. 
       The remaining parameters are:
       \bd
       \item[Nx] is the number of processors used to decompose the X dimension
       \item[Ny] is the number of processors used to decompose the Y dimension
       \item[Nz] is the number of processors used to decompose the Z dimension;
       must be 1 for now.          
       \item[IM\_World] is the number of longitudinal grid points;  if {\tt IM\_World=0} then
       the grid has no zonal dimension.
       \item[XCoordType] must be set to LINEAR
       \item[BegLon] is the longitude (in degrees) of the {\em center} of the first 
       gridbox
       \item[DelLon] is the constant mesh size (in degrees); if {\tt DelLon<1} then a
       global grid is assumed.
       %
       \item[JM\_World] is the number of meridional grid points  if {\tt JM\_World=0} then
       the grid has no meridional dimension.
       \item[YCoordType] must be set to LINEAR
       \item[BegLat] is the latitude (in degrees) of the {\em center} of the first 
       gridbox
       \item[DelLat] is the constant mesh size (in degrees); if {\tt DelLat<1} then a
       global grid is assumed.
       %
       \item[LM\_World] is the number of vertical grid points; if {\tt LM\_World=0} then the grid has no
       vertical dimension.
       \ed
       As of this writing, only the size of the vertical grid ({\tt LM\_World})
       needs to be specified.

       \subsubsection*{Passing an ESMF Config}

       The {\bf ESMF\_Config} object {\tt Config}, when specified, must
       contain the same information as the resource file above.

       subsubsection*{Providing parameters explicitly through the argument list}

       Alternatively, one can specify coordinate information in the argument
       list; their units and meaning is as in the resource file above. In
       this case you must specify at least {\tt Nx, Ny, IM\_World, JM\_World,} and 
       {\tt LM\_World}. The other parameters have default values
       \bd
       \item[BegLon] defaults to -180. (the date line)
       \item[DelLon] defaults to -1. (meaning a global grid)
       \item[BegLat] defaults to -90. (the south pole)
       \item[DelLat] deaults to -1. (meaning a global grid)
       \ed

       \subsubsection*{Restrictions}

       The current implementation imposes the following 
       restrictions:
       \begin{enumerate}
       \item Only uniform longitude/latitude grids are supported (no Gaussian grids).
       \item Only 2D Lon-Lat or 3D Lon-Lat-Lev grids are currently supported 
       (no Lat-Lev or Lon-Lev grids supprted yet).
       \item No vertical decomposition yet ({\tt Nz=1}).
       \end{enumerate}

       \subsubsection*{Future enhancements}

       The {\tt IDEF/JDEF/LDEF} records in the resource file should be
       extended as to allow specification of a more general distribution.
       For consistency with the {\tt XDEF/YDEF/ZDEF} records a similar 
       syntax could be adopted. For example,
       %
       \begin{verbatim}
       IDEF 4   LEVELS  22 50 50 22 
       XDEF 144 LINEAR -180 2.5 
       \end{verbatim}
       would indicate that longitudes would be decomposed in 4 PETs,
       with the first PET having 22 grid points, the second 50 gridpoints,
       and so on. 

#endif
     end function MAPL_LatLonGridCreate

     !............................................................................

     module subroutine MAPL_GRID_INTERIOR(GRID,I1,IN,J1,JN)
       use ESMF, only: ESMF_Grid
       type (ESMF_Grid), intent(IN) :: grid
       integer, intent(OUT)         :: I1, IN, J1, JN
     end subroutine MAPL_GRID_INTERIOR

     module subroutine MAPL_GridGetCorners(grid,gridCornerLons, gridCornerLats, RC)
       use ESMF, only: ESMF_Grid, ESMF_KIND_R8
       type (ESMF_Grid), intent(INOUT) :: GRID
       real(ESMF_KIND_R8), intent(INOUT) :: gridCornerLons(:,:)
       real(ESMF_KIND_R8), intent(INOUT) :: gridCornerLats(:,:)
       integer, optional, intent(  OUT) :: RC

     end subroutine MAPL_GridGetCorners

     !............................................................................


     !
     ! Note: The routine below came from ESMFL; it has been moved here to
     !       avoid circular dependencies (Arlindo).
     !
     module subroutine MAPL_GridGetInterior(GRID,I1,IN,J1,JN)
       use ESMF, only: ESMF_Grid
       type (ESMF_Grid), intent(IN) :: grid
       integer, intent(OUT)         :: I1, IN, J1, JN
     end subroutine MAPL_GridGetInterior

     !.......................................................................

     module function MAPL_RmQualifier(str, del) result(new)

       character(len=*),           intent(in)  :: str
       character(len=*), optional, intent(in)  :: del ! optional delimiter

       character(len=len(str)) :: new
     end function MAPL_RmQualifier



     module function MAPL_StrUpCase(str) result(new)
       character(len=*), intent(IN) :: str
       character(len=len(str))      :: new
     end function MAPL_StrUpCase

     module function MAPL_StrDnCase(str) result(new)
       character(len=*), intent(IN) :: str
       character(len=len(str))      :: new
     end function MAPL_StrDnCase


     ! ========================================
     recursive module subroutine MAPL_StateAttSetI4(STATE, NAME, VALUE, RC)
       use ESMF, only: ESMF_State
       type(ESMF_State),                 intent(INOUT) :: STATE
       character(len=*),                 intent(IN   ) :: NAME
       integer,                          intent(IN   ) :: VALUE
       integer, optional,                intent(  OUT) :: RC
     end subroutine MAPL_StateAttSetI4

     ! ========================================
     module subroutine MAPL_BundleAttSetI4(BUNDLE, NAME, VALUE, RC)
       use ESMF, only: ESMF_FieldBundle
       type(ESMF_FieldBundle),           intent(INOUT) :: BUNDLE
       character(len=*),                 intent(IN   ) :: NAME
       integer,                          intent(IN   ) :: VALUE
       integer, optional,                intent(  OUT) :: RC
     end subroutine MAPL_BundleAttSetI4

     ! ========================================
     module subroutine MAPL_FieldAttSetI4(FIELD, NAME, VALUE, RC)
       use ESMF, only: ESMF_Field
       type(ESMF_Field),                 intent(INOUT) :: FIELD
       character(len=*),                 intent(IN   ) :: NAME
       integer,                          intent(IN   ) :: VALUE
       integer, optional,                intent(  OUT) :: RC
     end subroutine MAPL_FieldAttSetI4
     ! ========================================

     module subroutine MAPL_FieldDestroy(Field,RC)
       use ESMF, only: ESMF_Field
       type(ESMF_Field),          intent(INOUT) :: Field
       integer, optional,         intent(OUT  ) :: RC
     end subroutine MAPL_FieldDestroy

     module subroutine MAPL_FieldBundleDestroy(Bundle,RC)
       use ESMF, only: ESMF_FieldBundle
       type(ESMF_FieldBundle),    intent(INOUT) :: Bundle
       integer, optional,         intent(OUT  ) :: RC
     end subroutine MAPL_FieldBundleDestroy

     module subroutine MAPL_StateAddField(State, Field, RC)
       use ESMF, only: ESMF_State, ESMF_Field
       type(ESMF_State),  intent(inout) :: State
       type(ESMF_Field),  intent(in   ) :: Field
       integer, optional, intent(  out) :: rc
     end subroutine MAPL_StateAddField

     module subroutine MAPL_StateAddBundle(State, Bundle, RC)
       use ESMF, only: ESMF_State, ESMF_FieldBundle
       type(ESMF_State),  intent(inout) :: State
       type(ESMF_FieldBundle),  intent(in   ) :: Bundle
       integer, optional, intent(  out) :: rc

     end subroutine MAPL_StateAddBundle

     module subroutine MAPL_FieldBundleAddField(Bundle, Field, multiflag, RC)
       use ESMF, only: ESMF_Field, ESMF_FieldBundle
       type(ESMF_FieldBundle),  intent(inout) :: Bundle
       type(ESMF_Field),  intent(in   ) :: Field
       logical, optional, intent(in   ) :: multiflag
       integer, optional, intent(  out) :: rc
     end subroutine MAPL_FieldBundleAddField

     module subroutine MAPL_FieldBundleGetByIndex(Bundle, fieldIndex, Field, RC)
       use ESMF, only: ESMF_Field, ESMF_FieldBundle
       type(ESMF_FieldBundle),  intent(INout) :: Bundle
       integer,           intent(in   ) :: fieldIndex
       type(ESMF_Field),  intent(INout   ) :: Field
       integer, optional, intent(  out) :: rc
     end subroutine MAPL_FieldBundleGetByIndex

     !BOPI
     !  !IROUTINE: MAPL_GetHorzIJIndex -- Get indexes on destributed ESMF grid for an arbitary lat and lon

     !  !INTERFACE:
     module subroutine MAPL_GetHorzIJIndex(npts,II,JJ,lon,lat,lonR8,latR8,Grid, rc)
       use ESMF, only: ESMF_KIND_R8, ESMF_GRid
       implicit none
       !ARGUMENTS:
       integer,                      intent(in   ) :: npts ! number of points in lat and lon arrays
       integer,                      intent(inout) :: II(npts) ! array of the first index for each lat and lon
       integer,                      intent(inout) :: JJ(npts) ! array of the second index for each lat and lon
       real, optional,               intent(in   ) :: lon(npts) ! array of longitudes in radians
       real, optional,               intent(in   ) :: lat(npts) ! array of latitudes in radians
       real(ESMF_KIND_R8), optional, intent(in   ) :: lonR8(npts) ! array of longitudes in radians
       real(ESMF_KIND_R8), optional, intent(in   ) :: latR8(npts) ! array of latitudes in radians
       type(ESMF_Grid),    optional, intent(inout) :: Grid ! ESMF grid
       integer,            optional, intent(out  ) :: rc  ! return code
     end subroutine MAPL_GetHorzIJIndex

     module subroutine MAPL_GenGridName(im, jm, lon, lat, xyoffset, gridname, geos_style)
       integer :: im, jm
       character (len=*) :: gridname
       real, optional    :: lon(:), lat(:)
       integer, optional :: xyoffset
       logical,  optional :: geos_style
     end subroutine MAPL_GenGridName

     module function MAPL_GenXYOffset(lon, lat) result(xy)
       real        :: lon(:), lat(:)
       integer     :: xy
     end function MAPL_GenXYOffset

     module subroutine MAPL_GeosNameNew(name)
       character(len=*) :: name
     end subroutine MAPL_GeosNameNew

     ! From a grid and a list of fields create an allocated ESMF bundle with
     ! these fields. By Default variables will be 3D at the center location
     ! unless 2 optional arguements are passed in. Can also pass in a list
     ! of long names and units if desired
     module function MAPL_BundleCreate(name,grid,fieldNames,is2D,isEdge,long_names,units,rc) result(B)
       use ESMF, only: ESMF_Grid, ESMF_FieldBundle
       character(len=*),           intent(in   ) :: name
       type(ESMF_Grid),            intent(inout) :: grid
       character(len=*),           intent(in   ) :: fieldNames(:)
       logical, optional,          intent(in   ) :: is2D(:)
       logical, optional,          intent(in   ) :: isEdge(:)
       character(len=*), optional, intent(in   ) :: long_names(:)
       character(len=*), optional, intent(in   ) :: units(:)
       integer, optional, intent(out  ) :: rc
       type(ESMF_FieldBundle) :: B
     end function MAPL_BundleCreate


     module function MAPL_TrimString(istring,rc) result(ostring)
       character(len=*), intent(in) :: istring
       integer, optional, intent(out) :: rc

       character(len=:), allocatable :: ostring
     end function MAPL_TrimString

     module subroutine MAPL_FieldSplit(field, fields, aliasName, rc)
       use ESMF, only: ESMF_Field
       type(ESMF_Field),          intent(IN   ) :: field
       type(ESMF_Field), pointer, intent(  out) :: fields(:)
       character(len=*), optional, intent(in  ) :: aliasName
       integer, optional,         intent(  out) :: rc
     end subroutine MAPL_FieldSplit

     module function MAPL_GetCorrectedPhase(gc,rc) result(phase)
       use ESMF, only: ESMF_GridComp
       type(ESMF_GridComp), intent(inout) :: gc
       integer, optional, intent(out) :: rc
       integer :: phase
     end function
  end interface

contains

  ! NAG and Intel need inconsistent workarounds for this function if moved
  ! into the submodule.  So keeping it here.

  function MAPL_RemapBoundsFull_3dr4(A,I1,IM,J1,JM,L1,LM)
    integer,      intent(IN) :: I1,IM,J1,JM,L1,LM
    real, target, intent(IN) :: A(I1:IM,J1:JM,L1:LM)
    real, pointer            :: MAPL_RemapBoundsFull_3dr4(:,:,:)

    MAPL_RemapBoundsFull_3dr4 => A
  end function MAPL_RemapBoundsFull_3dr4

end module MAPL_Base

module MAPL_BaseMod
  use MAPL_Base
  use MAPL_RangeMod, only:   MAPL_Range
  use mapl_MaplGrid, only: MAPL_GridGet, MAPL_DistGridGet, MAPL_GetImsJms, MAPL_GridHasDE
  use MAPL_Constants
   



end module MAPL_BaseMod


