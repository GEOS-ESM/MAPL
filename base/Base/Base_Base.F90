!------------------------------------------------------------------------------
!               Global Modeling and Assimilation Office (GMAO)                !
!                    Goddard Earth Observing System (GEOS)                    !
!                                 MAPL Component                              !
!------------------------------------------------------------------------------
!
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"
!
!>
!### MODULE: `MAPL_Base`
!
! Author: GMAO SI-Team
!
! The module `MAPL_Base` provides a collection assorted
! utilities and constants used throughout the MAPL Library.
!
module MAPL_Base

  ! !USES:
  !
  use ESMF, only: ESMF_MAXSTR, ESMF_PIN_FLAG, ESMF_PIN_DE_TO_SSI_CONTIG
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit NONE
  private

  ! !PUBLIC MEMBER FUNCTIONS:
  !
  public MAPL_FieldCreate
  public MAPL_FieldGetTime
  public MAPL_FieldSetTime
  public MAPL_GRID_INTERIOR
  public MAPL_FieldCopyAttributes
  public MAPL_StateAdd
  public MAPL_FieldBundleAdd
  public MAPL_FieldBundleGet
  public MAPL_FieldBundleDestroy
  public MAPL_GetHorzIJIndex
  public MAPL_Reverse_Schmidt
  public MAPL_GenGridName
  public MAPL_GridGetCorners
  public MAPL_GridGetInterior
  public MAPL_FieldSplit
  public MAPL_PinFlagSet
  public MAPL_PinFlagGet
  public MAPL_AllocateCoupling
  public MAPL_StateItemOrderList
  public MAPL_BundleItemOrderList


  !----------------------------------------------------------------------

  character(len=ESMF_MAXSTR), parameter :: MAPL_StateItemOrderList  = 'MAPL_StateItemOrderList'
  character(len=ESMF_MAXSTR), parameter :: MAPL_BundleItemOrderList = 'MAPL_BundleItemOrderList'

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

  ! Note: The routine below came from ESMFL; it has been moved here to
  !       avoid circular dependencies (Arlindo).
  interface  MAPL_GridGetInterior
    module procedure MAPL_Grid_Interior
  end interface


  interface
     module subroutine MAPL_AllocateCoupling(field, rc)
       use ESMF, only: ESMF_Field
       type(ESMF_Field),  intent(INOUT) :: field
       integer, optional, intent(  OUT) :: rc
     end subroutine MAPL_AllocateCoupling


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

     module subroutine MAPL_FieldCopyAttributes(FIELD_IN, FIELD_OUT, RC)
       use ESMF, only: ESMF_Field
       type (ESMF_Field), intent(INOUT) :: FIELD_IN !ALT: intent(in)
       type (ESMF_Field), intent(INOUT) :: FIELD_OUT
       integer, optional, intent(  OUT) :: RC
     end subroutine MAPL_FieldCopyAttributes

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

     module subroutine MAPL_FieldBundleDestroy(Bundle,NoGarbage,RC)
       use ESMF, only: ESMF_FieldBundle
       type(ESMF_FieldBundle),    intent(INOUT) :: Bundle
       logical, optional,         intent(IN   ) :: NoGarbage
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

     module subroutine MAPL_GetHorzIJIndex(npts,II,JJ,lon,lat,lonR8,latR8,Grid, rc)
       use ESMF, only: ESMF_KIND_R8, ESMF_GRid
       implicit none
       integer,                      intent(in   ) :: npts
       integer,                      intent(inout) :: II(npts)
       integer,                      intent(inout) :: JJ(npts)
       real, optional,               intent(in   ) :: lon(npts)
       real, optional,               intent(in   ) :: lat(npts)
       real(ESMF_KIND_R8), optional, intent(in   ) :: lonR8(npts)
       real(ESMF_KIND_R8), optional, intent(in   ) :: latR8(npts)
       type(ESMF_Grid),    optional, intent(inout) :: Grid
       integer,            optional, intent(out  ) :: rc
     end subroutine MAPL_GetHorzIJIndex

     module subroutine MAPL_Reverse_Schmidt(Grid, stretched, npts,lon,lat,lonR8,latR8, lonRe, latRe, rc)
       use ESMF, only: ESMF_KIND_R8, ESMF_GRid
       implicit none
       type(ESMF_Grid),              intent(inout) :: Grid
       logical,                      intent(out  ) :: stretched
       integer,                      intent(in   ) :: npts
       real, optional,               intent(in   ) :: lon(npts)
       real, optional,               intent(in   ) :: lat(npts)
       real(ESMF_KIND_R8), optional, intent(in   ) :: lonR8(npts)
       real(ESMF_KIND_R8), optional, intent(in   ) :: latR8(npts)
       real(ESMF_KIND_R8), optional, intent(out  ) :: lonRe(npts)
       real(ESMF_KIND_R8), optional, intent(out  ) :: latRe(npts)
       integer,            optional, intent(out  ) :: rc
     end subroutine MAPL_Reverse_Schmidt

     module subroutine MAPL_GenGridName(im, jm, lon, lat, xyoffset, gridname, geos_style)
       integer :: im, jm
       character (len=*) :: gridname
       real, optional    :: lon(:), lat(:)
       integer, optional :: xyoffset
       logical,  optional :: geos_style
     end subroutine MAPL_GenGridName

     module subroutine MAPL_FieldSplit(field, fields, aliasName, rc)
       use ESMF, only: ESMF_Field
       type(ESMF_Field),          intent(IN   ) :: field
       type(ESMF_Field), pointer, intent(  out) :: fields(:)
       character(len=*), optional, intent(in  ) :: aliasName
       integer, optional,         intent(  out) :: rc
     end subroutine MAPL_FieldSplit

     module subroutine MAPL_FieldAllocCommit(field, dims, location, typekind, &
          hw, ungrid, default_value, rc)
       use ESMF, only: ESMF_Field
       type(ESMF_Field),               intent(INOUT) :: field
       integer,                        intent(IN   ) :: dims
       integer,                        intent(IN   ) :: location
       integer,                        intent(IN   ) :: typekind
       integer,                        intent(IN   ) :: hw
       integer,              optional, intent(IN   ) :: ungrid(:)
       real,                 optional, intent(IN   ) :: default_value
       integer,              optional, intent(  OUT) :: rc
     end subroutine MAPL_FieldAllocCommit

     module subroutine MAPL_TimeStringGet(TIMESTRING,YY,MM,DD,H,M,S)
       character(len=*),  intent(IN ) :: TIMESTRING
       integer, optional, intent(OUT) :: YY
       integer, optional, intent(OUT) :: MM
       integer, optional, intent(OUT) :: DD
       integer, optional, intent(OUT) :: H
       integer, optional, intent(OUT) :: M
       integer, optional, intent(OUT) :: S
     end subroutine MAPL_TimeStringGet

     module function MAPL_FieldCreateEmpty(NAME, GRID, RC) RESULT(FIELD)
       use ESMF, only: ESMF_Field, ESMF_Grid
       character(len=*),  intent(IN   ) :: NAME
       type(ESMF_Grid),   intent(INout) :: GRID
       integer, optional, intent(  OUT) :: RC
       type(ESMF_Field)                 :: FIELD
     end function MAPL_FieldCreateEmpty

     module subroutine MAPL_FieldCopy(from, to, RC)
       use ESMF, only: ESMF_Field
       type(ESMF_Field), intent(INOUT) :: from
       type(ESMF_Field), intent(INOUT) :: to
       integer, optional, intent(  OUT) :: RC
     end subroutine MAPL_FieldCopy

     module subroutine MAPL_GetGlobalHorzIJIndex(npts,II,JJ,lon,lat,lonR8,latR8,Grid, rc)
       use ESMF, only: ESMF_KIND_R8, ESMF_Grid
       implicit none
       integer,                      intent(in   ) :: npts
       integer,                      intent(inout) :: II(npts)
       integer,                      intent(inout) :: JJ(npts)
       real, optional,               intent(in   ) :: lon(npts)
       real, optional,               intent(in   ) :: lat(npts)
       real(ESMF_KIND_R8), optional, intent(in   ) :: lonR8(npts)
       real(ESMF_KIND_R8), optional, intent(in   ) :: latR8(npts)
       type(ESMF_Grid),    optional, intent(inout) :: Grid
       integer,            optional, intent(out  ) :: rc
     end subroutine MAPL_GetGlobalHorzIJIndex

  end interface

  type(ESMF_Pin_Flag), protected :: pinflag_global = ESMF_PIN_DE_TO_SSI_CONTIG

contains

  subroutine MAPL_PinFlagSet(pinflag)
    type(ESMF_PIN_FLAG), intent(in) :: pinflag
    pinflag_global = pinflag
  end subroutine MAPL_PinFlagSet

  function MAPL_PinFlagGet() result(pinflag)
    type(ESMF_PIN_FLAG) :: pinflag

    pinflag = pinflag_global
  end function MAPL_PinFlagGet
  
end module MAPL_Base

module MAPL_BaseMod
  use MAPL_Base
  use MAPL_RangeMod, only:   MAPL_Range
  use mapl_MaplGrid, only: MAPL_GridGet, MAPL_DistGridGet, MAPL_GetImsJms, MAPL_GridHasDE
  use MAPL_Constants




end module MAPL_BaseMod


