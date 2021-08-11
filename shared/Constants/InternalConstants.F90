module MAPL_InternalConstantsMod

   use, intrinsic :: iso_fortran_env, only: REAL64, REAL32

   implicit none
!=============================================================================
!BOP

! !MODULE: -- A container module for MAPL internal constants

! !PUBLIC VARIABLES:
   integer,parameter            :: MAPL_R8                      = selected_real_kind(12) ! 8 byte real
   integer,parameter            :: MAPL_R4                      = selected_real_kind( 6) ! 4 byte real
   integer,parameter            :: MAPL_RN                      = kind(1.0)              ! native real
   integer,parameter            :: MAPL_I8                      = selected_int_kind (13) ! 8 byte integer
   integer,parameter            :: MAPL_I4                      = selected_int_kind ( 6) ! 4 byte integer
   integer,parameter            :: MAPL_IN                      = kind(1)                ! native integer

   integer,parameter            :: MAPL_UNDEFINED_INTEGER       = 1-huge(1)
   real,parameter               :: MAPL_UNDEFINED_REAL          = huge(1.)
   real(kind=REAL64), parameter :: MAPL_UNDEFINED_REAL64        = huge(1.d0)
   character(len=*), parameter  :: MAPL_UNDEFINED_CHAR          = '**'
   character(len=*), parameter  :: MAPL_GRID_NAME_DEFAULT       = 'UNKNOWN'
   character(len=*), parameter  :: MAPL_GRID_FILE_NAME_DEFAULT  = 'UNKNOWN'
   character(len=*), parameter  :: MAPL_CF_COMPONENT_SEPARATOR  = '.'

   enum, bind(c)
      enumerator MAPL_TimerModeOld
      enumerator MAPL_TimerModeRootOnly
      enumerator MAPL_TimerModeMax
      enumerator MAPL_TimerModeMinMax
   endenum

   enum, bind(c)
      enumerator :: MAPL_UseStarrQsat = 1
      enumerator MAPL_UseGoffGratchQsat
      enumerator MAPL_UseMurphyKoopQsat
      enumerator MAPL_UseCAMQsat
   endenum

   enum, bind(c)
      enumerator MAPL_Unknown
      enumerator MAPL_IsGather
      enumerator MAPL_IsScatter
   endenum

   integer, parameter :: MAPL_TileNameLength = 128

   integer, parameter :: MAPL_NoShm=255

   enum, bind(c)
      enumerator MAPL_SUCCESS
      enumerator MAPL_FILE_NOT_FOUND
   endenum

   enum, bind(c)
      enumerator :: MAPL_DimTopoEdge = -1
      enumerator MAPL_DimTopoCyclic
      enumerator MAPL_DimTopoCenter
   endenum

   integer, parameter :: MAPL_CplUNKNOWN        = 0  !Not used
   integer, parameter :: MAPL_CplSATISFIED      = 1
   integer, parameter :: MAPL_CplNEEDED         = 2  !not used
   integer, parameter :: MAPL_CplNOTNEEDED      = 4  !not used
   integer, parameter :: MAPL_FriendlyVariable  = 8
   integer, parameter :: MAPL_FieldItem         = 8
   integer, parameter :: MAPL_BundleItem        = 16
   integer, parameter :: MAPL_StateItem         = 32
   integer, parameter :: MAPL_NoRestart         = 64  !not used

   enum, bind(c)
      enumerator MAPL_Write2Disk
      enumerator MAPL_Write2RAM
   endenum

   enum, bind(c)
      enumerator MAPL_VLocationNone
      enumerator MAPL_VLocationEdge
      enumerator MAPL_VLocationCenter
   endenum

   enum, bind(c)
      enumerator MAPL_DimsUnknown
      enumerator MAPL_DimsVertOnly
      enumerator MAPL_DimsHorzOnly
      enumerator MAPL_DimsHorzVert
      enumerator MAPL_DimsTileOnly
      enumerator MAPL_DimsTileTile
      enumerator MAPL_DimsNone
   endenum

   enum, bind(c)
      enumerator :: MAPL_ScalarField = 1
      enumerator MAPL_VectorField
   endenum

   enum, bind(c)
      enumerator MAPL_CplAverage
      enumerator MAPL_CplMin
      enumerator MAPL_CplMax
      enumerator MAPL_CplAccumulate
      enumerator MAPL_MinMaxUnknown   ! This was defined to MAPL_CplAverage before and is not used
   endenum

   enum, bind(c)
      enumerator :: MAPL_AttrGrid = 1
      enumerator MAPL_AttrTile
   endenum

   enum, bind(c)
      enumerator MAPL_Uninitialized
      enumerator MAPL_InitialDefault
      enumerator MAPL_InitialRestart
   endenum

   enum, bind(c)
      enumerator :: MAPL_DuplicateEntry = -99
      enumerator :: MAPL_ConnUnknown = -1
      enumerator MAPL_Self
      enumerator MAPL_Import
      enumerator MAPL_Export
   endenum

   enum, bind(c)
      enumerator :: MAPL_FirstPhase = 1
      enumerator MAPL_SecondPhase
      enumerator MAPL_ThirdPhase
      enumerator MAPL_FourthPhase
      enumerator MAPL_FifthPhase
   endenum

   integer, parameter :: MAPL_Ocean              = 0  
   integer, parameter :: MAPL_Lake               = 19 
   integer, parameter :: MAPL_LandIce            = 20 
   integer, parameter :: MAPL_Land               = 100
   integer, parameter :: MAPL_Vegetated          = 101
   integer, parameter :: MAPL_NumVegTypes        = 6 

   enum, bind(c)
      enumerator MAPL_AGrid
      enumerator MAPL_CGrid
      enumerator MAPL_DGrid
   endenum 

   enum, bind(c)
      enumerator MAPL_RotateLL
      enumerator MAPL_RotateCube
   endenum

   enum, bind(c)
      enumerator MAPL_HorzTransOrderBinning
      enumerator MAPL_HorzTransOrderBilinear
      enumerator :: MAPL_HorzTransOrderFraction = 98
      enumerator MAPL_HorzTransOrderSample
   endenum

   enum, bind(c)
      enumerator MAPL_RestartOptional
      enumerator MAPL_RestartSkip
      enumerator MAPL_RestartRequired
      enumerator MAPL_RestartBootstrap
      enumerator MAPL_RestartSkipInitial
   endenum

!EOP

end module MAPL_InternalConstantsMod
