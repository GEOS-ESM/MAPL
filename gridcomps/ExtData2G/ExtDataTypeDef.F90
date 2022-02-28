module MAPL_ExtDataTypeDef
   use ESMF
   use MAPL_GriddedIOItemMod
   use MAPL_ExtDataBracket
   use MAPL_ExtDataPointerUpdate
   use MAPL_ExtDataAbstractFileHandler
   use MAPL_FileMetadataUtilsMod
   implicit none

   public PrimaryExport
   public DerivedExport
   public BracketingFields

  integer, parameter         :: MAPL_ExtDataNullFrac      = -9999

  type BracketingFields
     ! fields to store endpoints for interpolation of a vector pair
     type(ExtDataBracket) :: comp1
     type(ExtDataBracket) :: comp2
     ! if vertically interpolating vector fields
     type(ExtDataBracket) :: auxiliary1
     type(ExtDataBracket) :: auxiliary2
  end type BracketingFields

  type PrimaryExport
     character(len=ESMF_MAXSTR)   :: name
     character(len=ESMF_MAXSTR)   :: units=''
     integer                      :: Trans
     character(len=ESMF_MAXSTR)   :: var
     character(len=ESMF_MAXPATHLEN)   :: file ! remove

     logical                      :: isConst
     real                         :: Const !remove
     integer                      :: vartype ! MAPL_FieldItem or MAPL_BundleItem

     class(ExtDataAbstractFileHandler), allocatable :: filestream

     ! if primary export represents a pair of vector fields
     logical                      :: isVector
     type(BracketingFields)       :: modelGridFields

     ! names of the two vector components in the gridded component where import is declared
     character(len=ESMF_MAXSTR)   :: vcomp1, vcomp2
     ! the corresponding names of the two vector components on file
     character(len=ESMF_MAXSTR)   :: fcomp1, fcomp2
     type(GriddedIOitem)          :: fileVars

     integer                      :: pfioCollection_id
     integer                      :: iclient_collection_id

     logical                      :: ExtDataAlloc
     integer                      :: FracVal = MAPL_ExtDataNullFrac
     ! do we have to do vertical interpolation
     logical                      :: do_VertInterp = .false.
     logical                      :: do_Fill = .false.
     type(FileMetadataUtils)      :: file_metadata
     integer                      :: LM
     real, allocatable            :: levs(:)
     character(len=4)             :: importVDir = "down"
     character(len=4)             :: fileVDir = "down"
     character(len=ESMF_MAXSTR)   :: levUnit
     logical                      :: havePressure = .false.
     type(ExtDataPointerUpdate) :: update_freq

     ! new stuff
     logical                      :: cycling
     logical                      :: persist_closest
     type(ESMF_Time), allocatable :: source_time(:)
  end type PrimaryExport
  
  type DerivedExport
     character(len=ESMF_MAXSTR)     :: name
     character(len=ESMF_MAXPATHLEN) :: expression
     logical                        :: ExtDataAlloc
     logical                        :: masking
     type(ExtDataPointerUpdate)     :: update_freq
  end type DerivedExport


end module  MAPL_ExtDataTypeDef
