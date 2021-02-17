module mapl_Enumerators
   implicit none
   private

   integer, public, parameter :: MAPL_CplUNKNOWN        = 0
   integer, public, parameter :: MAPL_CplSATISFIED      = 1
   integer, public, parameter :: MAPL_CplNEEDED         = 2
   integer, public, parameter :: MAPL_CplNOTNEEDED      = 4
   integer, public, parameter :: MAPL_FriendlyVariable  = 8
   integer, public, parameter :: MAPL_FieldItem         = 8
   integer, public, parameter :: MAPL_BundleItem        = 16
   integer, public, parameter :: MAPL_StateItem         = 32
   integer, public, parameter :: MAPL_NoRestart         = 64

   integer, public, parameter :: MAPL_Write2Disk        = 0
   integer, public, parameter :: MAPL_Write2RAM         = 1

   integer, public, parameter :: MAPL_VLocationNone   = 0
   integer, public, parameter :: MAPL_VLocationEdge   = 1
   integer, public, parameter :: MAPL_VLocationCenter = 2

   integer, public, parameter :: MAPL_DimsUnknown     = 0
   integer, public, parameter :: MAPL_DimsVertOnly    = 1
   integer, public, parameter :: MAPL_DimsHorzOnly    = 2
   integer, public, parameter :: MAPL_DimsHorzVert    = 3
   integer, public, parameter :: MAPL_DimsTileOnly    = 4
   integer, public, parameter :: MAPL_DimsTileTile    = 5
   integer, public, parameter :: MAPL_DimsNone        = 6

   integer, public, parameter :: MAPL_ScalarField     = 1
   integer, public, parameter :: MAPL_VectorField     = 2


   integer, public, parameter :: MAPL_CplAverage      = 0
   integer, public, parameter :: MAPL_CplMin          = 1
   integer, public, parameter :: MAPL_CplMax          = 2
   integer, public, parameter :: MAPL_CplAccumulate   = 3
   integer, public, parameter :: MAPL_MinMaxUnknown   = MAPL_CplAverage

   integer, public, parameter :: MAPL_AttrGrid        = 1
   integer, public, parameter :: MAPL_AttrTile        = 2

   integer, public, parameter :: MAPL_UnInitialized  = 0
   integer, public, parameter :: MAPL_InitialDefault  = 1
   integer, public, parameter :: MAPL_InitialRestart  = 2

   integer, public, parameter :: MAPL_DuplicateEntry  = -99
   integer, public, parameter :: MAPL_Self = 0 
   integer, public, parameter :: MAPL_Import = 1
   integer, public, parameter :: MAPL_Export = 2
   integer, public, parameter :: MAPL_ConnUnknown = -1
   integer, public, parameter :: MAPL_FirstPhase   = 1
   integer, public, parameter :: MAPL_SecondPhase  = MAPL_FirstPhase+1
   integer, public, parameter :: MAPL_ThirdPhase   = MAPL_FirstPhase+2
   integer, public, parameter :: MAPL_FourthPhase  = MAPL_FirstPhase+3
   integer, public, parameter :: MAPL_FifthPhase   = MAPL_FirstPhase+4

   integer, public, parameter :: MAPL_Ocean              = 0
   integer, public, parameter :: MAPL_Lake               = 19
   integer, public, parameter :: MAPL_LandIce            = 20
   integer, public, parameter :: MAPL_Land               = 100
   integer, public, parameter :: MAPL_Vegetated          = 101

   integer, public, parameter :: MAPL_NumVegTypes        = 6

   integer, public, parameter :: MAPL_AGrid = 0
   integer, public, parameter :: MAPL_CGrid = 1
   integer, public, parameter :: MAPL_DGrid = 2

   integer, public, parameter :: MAPL_RotateLL = 0
   integer, public, parameter :: MAPL_RotateCube = 1


   integer, public, parameter :: MAPL_HorzTransOrderBinning  = 0
   integer, public, parameter :: MAPL_HorzTransOrderBilinear = 1
   integer, public, parameter :: MAPL_HorzTransOrderFraction = 98
   integer, public, parameter :: MAPL_HorzTransOrderSample   = 99

   integer, public, parameter :: MAPL_RestartOptional = 0
   integer, public, parameter :: MAPL_RestartSkip = 1
   integer, public, parameter :: MAPL_RestartRequired = 2
   integer, public, parameter :: MAPL_RestartBootstrap = 3
   integer, public, parameter :: MAPL_RestartSkipInitial = 4

end module mapl_Enumerators
