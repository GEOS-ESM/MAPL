!------------------------------------------------------------------------------
!               Global Modeling and Assimilation Office (GMAO)                !
!                    Goddard Earth Observing System (GEOS)                    !
!                                 MAPL Component                              !
!------------------------------------------------------------------------------
!
#include "MAPL_Generic.h"
#include "unused_dummy.H"
!
!>
!### MODULE: `MAPL_HistoryGridCompMod`
!
! Author: GMAO SI-Team
!
! `MAPL_HistoryGridCompMod` contains the `Initialize`, `Run` and `Finalize` methods for `History`.
! The three methods are called at the level of CAP.
!
  module MAPL_HistoryGridCompMod
!
! !USES:
!
  use ESMF
  use ESMFL_Mod
  use MAPL_BaseMod
  use MAPL_VarSpecMiscMod
  use MAPL_Constants
  use MAPL_IOMod
  use MAPL_CommsMod
  use MAPL_GenericMod
  use MAPL_LocStreamMod
  use MAPL_CFIOMod
  use MAPL_GenericCplCompMod
  use MAPL_NewArthParserMod
  use MAPL_SortMod
  use MAPL_ShmemMod
  use MAPL_StringGridMapMod
  use MAPL_GridManagerMod
  use MAPL_ConfigMod
  use, intrinsic :: iso_fortran_env, only: INT64
  use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
  use MAPL_HistoryCollectionMod, only: HistoryCollection, FieldSet, HistoryCollectionGlobalAttributes
  use MAPL_HistoryCollectionVectorMod, only: HistoryCollectionVector
  use MAPL_StringFieldSetMapMod, only: StringFieldSetMap
  use MAPL_StringFieldSetMapMod, only: StringFieldSetMapIterator
  use MAPL_ExceptionHandling
  use MAPL_VerticalDataMod
  use MAPL_TimeDataMod
  use mapl_RegridMethods
  use MAPL_GriddedIOitemVectorMod
  use MAPL_GriddedIOitemMod
  use pFIO_ClientManagerMod, only: o_Clients
  use MAPL_DownbitMod
  use pFIO_ConstantsMod
  use HistoryTrajectoryMod
  use StationSamplerMod
  use MaskSamplerGeosatMod
  use MAPL_StringTemplate
  use regex_module
  use MAPL_TimeUtilsMod, only: is_valid_time, is_valid_date
  use gFTL_StringStringMap
  !use ESMF_CFIOMOD
  use MAPL_EpochSwathMod

  use pflogger, only: Logger, logging
  use mpi

  implicit none
  private

! !PUBLIC MEMBER FUNCTIONS:

  public SetServices

  type :: SpecWrapper
     type (MAPL_VarSpec),              pointer :: SPEC(:)
  end type SpecWrapper

  type :: ExchangeRegridType
     type(MAPL_LocStreamXform) :: XFORM
     type(MAPL_LocStreamXform) :: XFORMntv
     type(MAPL_LocStream)      :: LocIn
     type(MAPL_LocStream)      :: LocOut
     type(MAPL_LocStream)      :: LocNative
     type(ESMF_State)          :: state_out
     integer                   :: ntiles_in
     integer                   :: ntiles_out
!ALT: this will not be needed when we modify LocStream to take vm instead of layout
     character(len=ESMF_MAXSTR)     :: tilefile
     character(len=ESMF_MAXSTR)     :: gridname
     logical                        :: noxform
     logical                        :: ontiles
     integer                        :: regridType
  end type ExchangeRegridType

  type :: ExchangeRegrid
     type(ExchangeRegridType), pointer :: PTR
  end type ExchangeRegrid

  type :: HISTORY_STATE
     type (HistoryCollection),        pointer :: list(:)       => null()
     type(HistoryCollectionVector) :: collections
     type (ExchangeRegrid),      pointer :: Regrid(:)     => null()
!     character(len=ESMF_MAXSTR), pointer :: GCNameList(:) => null()
!     type (ESMF_GridComp),       pointer :: gcs(:)        => null()
     type (ESMF_State),          pointer :: GIM(:)        => null()
     type (ESMF_State),          pointer :: GEX(:)        => null()
     type (ESMF_CplComp),        pointer :: CCS(:)        => null()
     type (ESMF_State),          pointer :: CIM(:)        => null()
     type (ESMF_State),          pointer :: CEX(:)        => null()
     type (ESMF_TimeInterval),   pointer :: STAMPOFFSET(:) => null()
     logical,                    pointer :: LCTL(:)       => null()
     logical,                    pointer :: average(:)    => null()
     type (SpecWrapper),         pointer :: SRCS(:)       => null()
     type (SpecWrapper),         pointer :: DSTS(:)       => null()
     type (StringGridMap)                :: output_grids
     type (StringFieldSetMap)            :: field_sets
     character(len=ESMF_MAXSTR)          :: expsrc
     character(len=ESMF_MAXSTR)          :: expid
     character(len=ESMF_MAXSTR)          :: expdsc
     type(HistoryCollectionGlobalAttributes) :: global_atts
     integer                             :: CoresPerNode, mype, npes
     integer                             :: AvoidRootNodeThreshold
     integer                             :: version
     logical                             :: fileOrderAlphabetical
     logical                             :: integer_time
     integer                             :: collectionWriteSplit
     integer                             :: serverSizeSplit
     logical                             :: allow_overwrite
     logical                             :: file_weights
  end type HISTORY_STATE

  type HISTORY_wrap
     type (HISTORY_STATE), pointer :: PTR
  end type HISTORY_wrap

  type HISTORY_ExchangeListType
     integer(kind=INT64), pointer                  :: lsaddr_ptr(:) => null()
  end type HISTORY_ExchangeListType

  type HISTORY_ExchangeListWrap
     type(HISTORY_ExchangeListType), pointer :: PTR
  end type HISTORY_ExchangeListWrap

  integer, parameter :: MAPL_G2G = 1
  integer, parameter :: MAPL_T2G = 2
  integer, parameter :: MAPL_T2G2G = 3

  public HISTORY_ExchangeListWrap

  type(samplerHQ) :: Hsampler

  INTERFACE
     MODULE SUBROUTINE SetServices ( gc, rc )
        type(ESMF_GridComp), intent(inout) :: gc     !! composite gridded component
        integer, intent(out), optional     :: rc     !! return code
     END SUBROUTINE
  END INTERFACE

  INTERFACE
     MODULE SUBROUTINE Initialize ( gc, import, dumexport, clock, rc )
        type(ESMF_GridComp), intent(inout)    :: gc        !! composite gridded component
        type(ESMF_State),       intent(inout) :: import    !! import state
        type(ESMF_State),       intent(inout) :: dumexport !! export state
        type(ESMF_Clock),       intent(inout) :: clock     !! the clock
        integer, intent(out), OPTIONAL        :: rc        !! Error code:
     END SUBROUTINE
  END INTERFACE

  INTERFACE
     MODULE SUBROUTINE Run( gc, import, export, clock, rc )
        type(ESMF_GridComp),    intent(inout) :: gc
        type(ESMF_State),       intent(inout) :: import    
        type(ESMF_State),       intent(inout) :: export    
        type(ESMF_Clock),       intent(inout) :: clock
        integer, optional,      intent(  out) :: rc
     END SUBROUTINE
  END INTERFACE


  INTERFACE
     MODULE SUBROUTINE RecordRestart ( gc, import, export, clock, rc )
        type(ESMF_GridComp), intent(inout)    :: gc     !! composite gridded component
        type(ESMF_State),       intent(inout) :: import !! import state
        type(ESMF_State),       intent(  out) :: export !! export state
        type(ESMF_Clock),       intent(inout) :: clock  !! the clock
        integer, intent(out), OPTIONAL        :: rc     !! Error code:
                                                        !! = 0 all is well
                                                        !! otherwise, error
     END SUBROUTINE
  END INTERFACE


  INTERFACE
     MODULE SUBROUTINE Finalize ( gc, import, export, clock, rc )
        type(ESMF_GridComp), intent(inout)    :: gc     !! composite gridded component
        type(ESMF_State),       intent(inout) :: import !! import state
        type(ESMF_State),       intent(  out) :: export !! export state
        type(ESMF_Clock),       intent(inout) :: clock  !! the clock
        
        integer, intent(out), OPTIONAL        :: rc     !! Error code:
                                                        !! = 0 all is well
                                                        !! otherwise, error
     END SUBROUTINE
  END INTERFACE

  INTERFACE
     MODULE SUBROUTINE checkIfStateHasField(state, input_fieldName, hasField, rc)
       type(ESMF_State), intent(in) :: state ! export state
       character(len=*), intent(in) :: input_fieldName
       logical, intent(out)         :: hasField
       integer, intent(out), optional :: rc ! Error code:
     END SUBROUTINE
  END INTERFACE

  INTERFACE
     MODULE SUBROUTINE CopyStateItems(src, dst, rc)
       type(ESMF_State), intent(in) :: src
       type(ESMF_State), intent(inout) :: dst
       integer, optional, intent(out) :: rc
     END SUBROUTINE
  END INTERFACE

  INTERFACE
     MODULE FUNCTION get_acc_offset(current_time,ref_time,rc) result(acc_offset)
        integer :: acc_offset
        type(ESMF_Time), intent(in) :: current_time
        integer, intent(in) :: ref_time
        integer, optional, intent(out) :: rc
     END FUNCTION
  END INTERFACE

  INTERFACE
     MODULE SUBROUTINE get_DateStamp (clock, DateStamp, offset, rc)
       type (ESMF_Clock)                   :: clock
       character(len=ESMF_MAXSTR),optional :: DateStamp
       type(ESMF_TimeInterval),   optional :: offset
       integer, optional                   :: rc
     END SUBROUTINE
  END INTERFACE

  INTERFACE
     MODULE SUBROUTINE Get_Tdim (list, clock, tdim)
       type (HistoryCollection),  intent(IN ) :: list 
       type (ESMF_Clock),    intent(IN ) :: clock
       integer,              intent(OUT) :: tdim
     END SUBROUTINE
  END INTERFACE

  INTERFACE
     MODULE SUBROUTINE MAPL_GradsCtlWrite ( clock, state,list,fname,expid,expdsc,output_grids,rc )             
        type(ESMF_Clock),  intent(inout) :: clock
        type(ESMF_State)                 :: state               
        type(HistoryCollection)               :: list
        character(len=*)                 :: expid
        character(len=*)                 :: expdsc
        character(len=*)                 :: fname               
        type(StringGridMap), intent(in)  :: output_grids
        integer, optional, intent(out)   :: rc
     END SUBROUTINE
  END INTERFACE

  INTERFACE
     MODULE subroutine regen_rcx_for_obs_platform (config, nlist, list, rc)
       type(ESMF_Config), intent(inout)       :: config
       integer, intent(in)                    :: nlist
       type(HistoryCollection), pointer       :: list(:)
       integer, intent(inout), optional :: rc
     END SUBROUTINE
  END INTERFACE

  INTERFACE
     MODULE SUBROUTINE RegridTransform(STATE_IN, XFORM, STATE_OUT, LS_IN, LS_OUT, NTILES_IN, NTILES_OUT, RC)
       type (ESMF_State)        , intent(IN   ) :: STATE_IN
       type (ESMF_State)        , intent(INOUT) :: STATE_OUT
       type(MAPL_LocStreamXform), intent(IN   ) :: XFORM
       type(MAPL_LocStream)     , intent(IN   ) :: LS_IN, LS_OUT
       integer                  , intent(IN   ) :: NTILES_IN, NTILES_OUT
       integer, optional        , intent(  OUT) :: RC
     END SUBROUTINE
  END INTERFACE

  INTERFACE
     MODULE SUBROUTINE RegridTransformT2G2G(STATE_IN, XFORM, XFORMntv, STATE_OUT, LS_IN, LS_OUT, LS_NTV, NTILES_IN, NTILES_OUT, RC)
       type (ESMF_State)        , intent(IN   ) :: STATE_IN
       type (ESMF_State)        , intent(INOUT) :: STATE_OUT
       type(MAPL_LocStreamXform), intent(IN   ) :: XFORM, XFORMntv
       type(MAPL_LocStream)     , intent(IN   ) :: LS_IN, LS_OUT, LS_NTV
       integer                  , intent(IN   ) :: NTILES_IN, NTILES_OUT
       integer, optional        , intent(  OUT) :: RC
     END SUBROUTINE
  END INTERFACE

  INTERFACE
     MODULE SUBROUTINE RegridTransformT2G(STATE_IN, XFORM, STATE_OUT, LS_OUT, NTILES_OUT, RC)
       type (ESMF_State)        , intent(IN   ) :: STATE_IN
       type (ESMF_State)        , intent(INOUT) :: STATE_OUT
       type(MAPL_LocStreamXform), optional, intent(IN   ) :: XFORM
       type(MAPL_LocStream)     , intent(IN   ) :: LS_OUT
       integer                  , intent(IN   ) :: NTILES_OUT
       integer, optional        , intent(  OUT) :: RC
     END SUBROUTINE
  END INTERFACE

  INTERFACE
     MODULE SUBROUTINE MAPL_RunExpression(state,fields,tmpfields,rewrite,nfield,rc)
        type (ESMF_State),  intent(in)    :: state
        character(len=*), intent(in):: fields(:,:),tmpfields(:)
        logical, intent(inout) :: rewrite(:)
        integer, intent(in):: nfield 
        integer, optional, intent(out) :: rc
     END SUBROUTINE
  END INTERFACE

  INTERFACE
     MODULE SUBROUTINE MAPL_SetExpression(nfield,fields,tmpfields,rewrite,nPExtraFields, &
              ExtraFields,ExtraGridComp,ExpState,rc)
         integer,intent(in)::nfield
         character(len=*),  intent(inout) :: fields(:,:)
         character(len=*),  intent(inout) :: tmpfields(:)
         logical,           intent(inout) :: rewrite(:)
         integer,           intent(inout) :: nPExtraFields
         character(len=*), pointer, intent(inout) :: ExtraFields(:)
         character(len=*), pointer, intent(inout) :: ExtraGridComp(:)
         type(ESMF_State),  intent(inout) :: ExpState
         integer, optional, intent(out  ) :: rc
     END SUBROUTINE
  END INTERFACE

  INTERFACE
     MODULE SUBROUTINE shavebits( state, list, rc)
       type(ESMF_state), intent(inout) :: state
       type (HistoryCollection), intent(in) :: list
       integer, optional, intent(out):: rc
     END SUBROUTINE
  END INTERFACE

  INTERFACE
     MODULE SUBROUTINE MAPL_StateDestroy(State, RC)
        type(ESMF_State), intent(inout) :: state
        integer, optional,intent(  out) :: rc
     END SUBROUTINE
  END INTERFACE

  INTERFACE
     MODULE SUBROUTINE MAPL_StateGet(state,name,field,force_field,rc)
       type(ESMF_State), intent(in) :: state
       character(len=*), intent(in) :: name
       type(ESMF_Field), intent(inout) :: field
       logical, optional, intent(in) :: force_field
       integer, optional, intent(out  ) :: rc
     END SUBROUTINE
  END INTERFACE

end module MAPL_HistoryGridCompMod
