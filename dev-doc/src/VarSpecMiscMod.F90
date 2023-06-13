#include "MAPL_ErrLog.h"

!=============================================================================
!BOP

! !MODULE: MAPL_VarSpecMiscMod -- A class for manipulation variable specifications.

! !INTERFACE:

module MAPL_VarSpecMiscMod

! !USES:

   use ESMF
   use pFlogger
   use MAPL_Constants
   use MAPL_ExceptionHandling
   use mapl_VariableSpecification
   use mapl_VarSpecVector
   use mapl_VarConnVector
   use MAPL_VarSpecTypeMod
   use MAPL_VarSpecMod
   use MAPL_VarSpecPtrMod
   use MAPL_VarConnPoint
   use MAPL_VarConnType
   use MAPL_VarConn
! !PUBLIC MEMBER FUNCTIONS:

implicit none
private


! re export
   public :: MAPL_VarSpecType
   public :: MAPL_VarSpec
   public :: MAPL_VarSpecPtr
   public :: VarConnPoint
   public :: VarConnType

   public :: MAPL_VarSpecCreateInList
   public :: MAPL_VarSpecAddToList
   public :: MAPL_VarSpecSet
   public :: MAPL_VarSpecGet
   public :: MAPL_VarSpecDestroy
   public :: MAPL_VarSpecAddChildName
   public :: MAPL_VarSpecReconnect
   public :: MAPL_VarSpecGetIndex
   public :: MAPL_VarSpecAddRefToList
   public :: MAPL_VarSpecPrint
   public :: MAPL_VarSpecPrintCSV
   public :: operator(==)

!EOP

contains





 
end module MAPL_VarSpecMiscMod
