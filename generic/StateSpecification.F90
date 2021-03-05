#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module mapl_StateSpecification
   use ESMF
   use pFlogger
   use mapl_Enumerators
   use mapl_ErrorHandlingMod
   use mapl_VarSpecVector
   use mapl_VarSpecMod
   use mapl_VariableSpecification
   implicit none

   private

   public :: StateSpecification
   public MAPL_VarSpecCreateInListNew
   public :: MAPL_VarSpecAddRefToList

   type :: StateSpecification
      type(VarSpecVector) :: var_specs
      type(MAPL_VarSpec), pointer :: old_var_specs(:) => null()
   contains
      procedure :: update_vector
      procedure :: update_legacy
   end type StateSpecification

interface MAPL_VarSpecAddRefToList
   module procedure MAPL_VarSpecAddRefFromItemNew
end interface

contains

   ! Possible memory leak.  Eliminate after refactoring
   subroutine update_vector(this)
      class(StateSpecification), target, intent(inout) :: this

      integer :: i, n

      call this%var_specs%clear()
      
      n = size(this%old_var_specs)
      do i = 1, n
         call this%var_specs%push_back(this%old_var_specs(i))
      end do
         
   end subroutine update_vector

   subroutine update_legacy(this)
      class(StateSpecification), target, intent(inout) :: this
      
      integer :: i, n

      if (associated(this%old_var_specs)) then
         do i = 1, size(this%old_var_specs)
            nullify(this%old_var_specs(i)%specPtr)
         end do
         deallocate(this%old_var_specs)
      endif
      n = this%var_specs%size()
      allocate(this%old_var_specs(n))
      do i = 1, n
         this%old_var_specs(i) = this%var_specs%of(i)
      end do
      
   end subroutine update_legacy

   function to_VarSpecPtr(var_spec_vector) result(spec_arr_ptr)
      type(MAPL_VarSpec), pointer :: spec_arr_ptr(:)
      type(VarSpecVector), target, intent(in) :: var_spec_vector

      integer :: i, n


      n = var_spec_vector%size()
      allocate(spec_arr_ptr(n))

      do i = 1, n
         spec_arr_ptr(i) = var_spec_vector%of(i)
      end do

   end function to_VarSpecPtr


  subroutine MAPL_VarSpecCreateInListNew(SPEC, SHORT_NAME, LONG_NAME,     &
                             UNITS,  Dims, VLocation, FIELD, BUNDLE, STATE, &
                             NUM_SUBTILES, &
                             STAT, ACCMLT_INTERVAL, COUPLE_INTERVAL, OFFSET, &
                             DEFAULT, FRIENDLYTO, &
                             HALOWIDTH, PRECISION, &
                             RESTART, &
                             ATTR_RNAMES, ATTR_INAMES, &
                             ATTR_RVALUES, ATTR_IVALUES, &
                             UNGRIDDED_DIMS, &
                             UNGRIDDED_UNIT, &
                             UNGRIDDED_NAME, &
                             UNGRIDDED_COORDS, &
                             FIELD_TYPE, &
                             STAGGERING, &
                             ROTATION,   & 
                             GRID, &
                                                                   RC  )

    type (StateSpecification), intent(inout):: SPEC
    character (len=*)               , intent(IN)      :: SHORT_NAME
    character (len=*)  , optional   , intent(IN)      :: LONG_NAME
    character (len=*)  , optional   , intent(IN)      :: UNITS
    character (len=*)  , optional   , intent(IN)      :: FRIENDLYTO
    integer            , optional   , intent(IN)      :: DIMS
    integer            , optional   , intent(IN)      :: VLOCATION
    integer            , optional   , intent(IN)      :: NUM_SUBTILES
    integer            , optional   , intent(IN)      :: ACCMLT_INTERVAL
    integer            , optional   , intent(IN)      :: COUPLE_INTERVAL
    integer            , optional   , intent(IN)      :: OFFSET
    integer            , optional   , intent(IN)      :: STAT
    real               , optional   , intent(IN)      :: DEFAULT
    type(ESMF_Field)   , optional   , intent(IN), target :: FIELD
    type(ESMF_FieldBundle)  , optional   , intent(IN), target :: BUNDLE
    type(ESMF_State)   , optional   , intent(IN), target :: STATE
    integer            , optional   , intent(IN)      :: HALOWIDTH
    integer            , optional   , intent(IN)      :: PRECISION
    integer            , optional   , intent(IN)      :: RESTART
    character (len=*)  , optional   , intent(IN)      :: ATTR_INAMES(:)
    character (len=*)  , optional   , intent(IN)      :: ATTR_RNAMES(:)
    integer            , optional   , intent(IN)      :: ATTR_IVALUES(:)
    real               , optional   , intent(IN)      :: ATTR_RVALUES(:)
    integer            , optional   , intent(IN)      :: UNGRIDDED_DIMS(:)
    character (len=*)  , optional   , intent(IN)      :: UNGRIDDED_UNIT
    character (len=*)  , optional   , intent(IN)      :: UNGRIDDED_NAME
    real               , optional   , intent(IN)      :: UNGRIDDED_COORDS(:)
    integer            , optional   , intent(IN)      :: FIELD_TYPE
    integer            , optional   , intent(IN)      :: STAGGERING
    integer            , optional   , intent(IN)      :: ROTATION
    type(ESMF_Grid)    , optional   , intent(IN)      :: GRID
    integer            , optional   , intent(OUT)     :: RC



    integer                               :: STATUS

    type (MAPL_VarSpec)         :: TMP

    integer                    :: usableDIMS
    integer                    :: usableVLOC
    integer                    :: usableACCMLT
    integer                    :: usableCOUPLE
    integer                    :: usableOFFSET
    integer                    :: usableSTAT
    integer                    :: usableNUM_SUBTILES
    integer                    :: usableHALOWIDTH
    integer                    :: usablePRECISION
    integer                    :: usableFIELD_TYPE
    integer                    :: usableSTAGGERING
    integer                    :: usableROTATION
    integer                    :: usableRESTART
    character(len=ESMF_MAXSTR) :: usableLONG
    character(len=ESMF_MAXSTR) :: usableUNIT
    character(len=ESMF_MAXSTR) :: usableFRIENDLYTO
    character(len=ESMF_MAXSTR), pointer :: usableATTR_INAMES(:) => NULL()
    character(len=ESMF_MAXSTR), pointer :: usableATTR_RNAMES(:) => NULL()
    integer                   , pointer :: usableATTR_IVALUES(:) => NULL()
    real                      , pointer :: usableATTR_RVALUES(:) => NULL()
    integer                   , pointer :: usableUNGRIDDED_DIMS(:) => null()
    real                       :: usableDEFAULT
    type(ESMF_Grid)            :: usableGRID
    type(ESMF_Field), pointer  :: usableFIELD => null()
    type(ESMF_FieldBundle), pointer :: usableBUNDLE => null()
    type(ESMF_State), pointer  :: usableSTATE => null()
    character(len=ESMF_MAXSTR) :: useableUngrd_Unit
    character(len=ESMF_MAXSTR) :: useableUngrd_Name
    real                      , pointer :: usableUNGRIDDED_COORDS(:) => NULL()

    INTEGER :: I
    integer :: szINAMES, szRNAMES, szIVALUES, szRVALUES
    integer :: szUNGRD
    logical :: defaultProvided


      if(present(STAT)) then
       usableSTAT=STAT
      else
       usableSTAT=MAPL_FieldItem !ALT: not sure if needs special attn for bundles
      endif
    
      if(present(ACCMLT_INTERVAL)) then
       usableACCMLT=ACCMLT_INTERVAL
      else
       usableACCMLT=0
      endif
    
      if(present(COUPLE_INTERVAL)) then
       usableCOUPLE=COUPLE_INTERVAL
      else
       usableCOUPLE=0
      endif
    
      if(present(OFFSET)) then
       usableOFFSET=OFFSET
      else
       usableOFFSET=0
      endif
    
      if(present(LONG_NAME)) then
       usableLONG=LONG_NAME
      else
       usableLONG=SHORT_NAME
      endif

      if(present(UNITS)) then
       usableUNIT=UNITS
      else
       usableUNIT=""
      endif

      if(present(FRIENDLYTO)) then
       usableFRIENDLYTO=FRIENDLYTO
       if (LEN(TRIM(FRIENDLYTO)) /= 0) then 
          usableSTAT = ior(usableSTAT,MAPL_FriendlyVariable)
       end if
      else
       usableFRIENDLYTO=""
      endif

      if(present(DIMS)) then
       usableDIMS=DIMS
      else
       usableDIMS=MAPL_DimsUnknown
      endif

      if(present(VLOCATION)) then
       usableVLOC=VLOCATION
      else
       usableVLOC=MAPL_VLocationNone
      endif

      if(present(NUM_SUBTILES)) then
       usableNUM_SUBTILES=NUM_SUBTILES
      else
       usableNUM_SUBTILES=0
      endif

      if(present(DEFAULT)) then
         defaultProvided=.true.
         usableDEFAULT=DEFAULT
      else
         defaultProvided=.false.
         usableDEFAULT=0.0 ! ALT: this could be NaN
!         usableDEFAULT=Z'7F800001' ! DSK: set to NaN, dies in FV Init
!         usableDEFAULT=-999. ! DSK
      endif

      if (present(FIELD_TYPE)) then
         usableFIELD_TYPE=FIELD_TYPE
      else
         usableFIELD_TYPE=MAPL_ScalarField 
      endif

      if (present(STAGGERING)) then
         usableSTAGGERING=STAGGERING
      else
         usableSTAGGERING=MAPL_AGrid
      endif

      if (present(ROTATION)) then
         usableROTATION=ROTATION
      else
         usableROTATION=MAPL_RotateLL
      endif

      if(present(GRID)) then
         usableGRID=GRID
      else
!         usableGRID = ESMF_GridEmptyCreate(RC=STATUS)
!         _VERIFY(STATUS)
!         call ESMF_GridDestroy(usableGRID) !ALT we do not need RC

         ! Initialize this grid object as invalid
         usableGrid%this = ESMF_NULL_POINTER
      endif

      if(present(FIELD)) then
         usableFIELD=>FIELD
      else
         allocate(usableFIELD, STAT=STATUS)
         _VERIFY(STATUS)
!         usableFIELD = ESMF_FieldEmptyCreate(NAME=SHORT_NAME,RC=STATUS)
!         _VERIFY(STATUS)
!         call ESMF_FieldDestroy(usableFIELD) !ALT we do not need RC

         ! Initialize this field object as invalid
         usableField%ftypep => NULL()
      endif

      if(present(BUNDLE)) then
         usableBUNDLE=>BUNDLE
      else
         allocate(usableBUNDLE, STAT=STATUS)
         _VERIFY(STATUS)
!         usableBUNDLE = ESMF_FieldBundleCreate(NAME=SHORT_NAME,RC=STATUS)
!         _VERIFY(STATUS)
!         call ESMF_FieldBundleDestroy(usableBUNDLE) !ALT we do not need RC

         ! Initialize this fieldBundle object as invalid
         usableBundle%this => NULL()
      endif

      if(present(STATE)) then
         usableSTATE=>STATE
      else
         allocate(usableSTATE, STAT=STATUS)
         _VERIFY(STATUS)
!         usableSTATE = ESMF_StateCreate(NAME=SHORT_NAME,RC=STATUS)
!         _VERIFY(STATUS)
!         call ESMF_StateDestroy(usableSTATE) !ALT we do not need RC

         ! Initialize this state object as invalid
         usableState%statep => NULL()
      endif

      if(present(HALOWIDTH)) then
       usableHALOWIDTH=HALOWIDTH
      else
       usableHALOWIDTH=0
      endif

      if(present(RESTART)) then
       usableRESTART=RESTART
      else
       usableRESTART=MAPL_RestartOptional ! default
      endif

      if(present(PRECISION)) then
       usablePRECISION=PRECISION
      else
       usablePRECISION=kind(0.0) ! default "real" kind
      endif

! Sanity checks
      if (usablePRECISION /= ESMF_KIND_R4 .AND. usablePRECISION /= ESMF_KIND_R8) then
         ! only those 2 values are allowed
         _RETURN(ESMF_FAILURE) 
      end if

      szRNAMES = 0
      if (present(ATTR_RNAMES)) then
         szRNAMES = size(ATTR_RNAMES)
         allocate(usableATTR_RNAMES(szRNAMES), stat=status)
         _VERIFY(STATUS)
         usableATTR_RNAMES = ATTR_RNAMES
      end if

      szINAMES = 0
      if (present(ATTR_INAMES)) then
         szINAMES = size(ATTR_INAMES)
         allocate(usableATTR_INAMES(szINAMES), stat=status)
         _VERIFY(STATUS)
         usableATTR_INAMES = ATTR_INAMES
      end if

      szRVALUES = 0
      if (present(ATTR_RVALUES)) then
         szRVALUES = size(ATTR_RVALUES)
         allocate(usableATTR_RVALUES(szRVALUES), stat=status)
         _VERIFY(STATUS)
         usableATTR_RVALUES = ATTR_RVALUES
      end if

      szIVALUES = 0
      if (present(ATTR_IVALUES)) then
         szIVALUES = size(ATTR_INAMES)
         allocate(usableATTR_IVALUES(szIVALUES), stat=status)
         _VERIFY(STATUS)
         usableATTR_IVALUES = ATTR_IVALUES
      end if
      _ASSERT(szIVALUES == szINAMES,'needs informative message')
      _ASSERT(szRVALUES == szRNAMES,'needs informative message')

      szUNGRD = 0
      if (present(UNGRIDDED_DIMS)) then
         szUNGRD = size(UNGRIDDED_DIMS)
         allocate(usableUNGRIDDED_DIMS(szUNGRD), stat=status)
         _VERIFY(STATUS)
         usableUNGRIDDED_DIMS = UNGRIDDED_DIMS
      else
         NULLIFY(usableUNGRIDDED_DIMS)
      end if

      if (present(UNGRIDDED_UNIT)) then
         useableUngrd_Unit = UNGRIDDED_UNIT
      else
         useableUngrd_Unit = "level" ! ALT: we are changing the default from "N/A" to "level" to make GrADS happy
      end if
      if (present(UNGRIDDED_NAME)) then
         useableUngrd_NAME = UNGRIDDED_NAME
      else
         useableUngrd_NAME = "N/A"
      end if

      szUNGRD = 0
      if (present(UNGRIDDED_COORDS)) then
         szUNGRD = size(UNGRIDDED_COORDS)
         allocate(usableUNGRIDDED_COORDS(szUNGRD), stat=status)
         _VERIFY(STATUS)
         usableUNGRIDDED_COORDS = UNGRIDDED_COORDS
      end if

      I = spec%var_specs%size()
      allocate(tmp%specptr)
      
      TMP%SPECPTR%SHORT_NAME =  SHORT_NAME
      TMP%SPECPTR%LONG_NAME  =  usableLONG
      TMP%SPECPTR%UNITS      =  usableUNIT
      TMP%SPECPTR%DIMS       =  usableDIMS
      TMP%SPECPTR%LOCATION   =  usableVLOC
      TMP%SPECPTR%NUM_SUBTILES = usableNUM_SUBTILES
      TMP%SPECPTR%STAT       =  usableSTAT
      TMP%SPECPTR%ACCMLT_INTERVAL  =  usableACCMLT
      TMP%SPECPTR%COUPLE_INTERVAL  =  usableCOUPLE
      TMP%SPECPTR%OFFSET     =  usableOFFSET
      TMP%SPECPTR%LABEL      =  0
      TMP%SPECPTR%DEFAULT    =  usableDEFAULT
      TMP%SPECPTR%defaultProvided = defaultProvided
      TMP%SPECPTR%FIELD      => usableFIELD
      TMP%SPECPTR%BUNDLE     => usableBUNDLE
      TMP%SPECPTR%STATE      => usableSTATE
      TMP%SPECPTR%GRID       =  usableGRID
      TMP%SPECPTR%FRIENDLYTO =  usableFRIENDLYTO
      TMP%SPECPTR%HALOWIDTH  =  usableHALOWIDTH
      TMP%SPECPTR%RESTART    =  usableRESTART
      TMP%SPECPTR%PRECISION  =  usablePRECISION
      TMP%SPECPTR%FIELD_TYPE =  usableFIELD_TYPE
      TMP%SPECPTR%UNGRIDDED_UNIT =  useableUngrd_Unit
      TMP%SPECPTR%UNGRIDDED_NAME =  useableUngrd_Name
      TMP%SPECPTR%STAGGERING =  usableSTAGGERING
      TMP%SPECPTR%ROTATION =  usableROTATION
      TMP%SPECPTR%doNotAllocate    =  .false.
      TMP%SPECPTR%alwaysAllocate   =  .false.
      if(associated(usableATTR_IVALUES)) then
         TMP%SPECPTR%ATTR_IVALUES  =>  usableATTR_IVALUES
      else
         NULLIFY(TMP%SPECPTR%ATTR_IVALUES)
      endif
      if(associated(usableATTR_RVALUES)) then
         TMP%SPECPTR%ATTR_RVALUES  =>  usableATTR_RVALUES
      else
         NULLIFY(TMP%SPECPTR%ATTR_RVALUES)
      endif
      if(associated(usableUNGRIDDED_DIMS)) then
         TMP%SPECPTR%UNGRIDDED_DIMS  =>  usableUNGRIDDED_DIMS
      else
         NULLIFY(TMP%SPECPTR%UNGRIDDED_DIMS)
      endif
      if(associated(usableUNGRIDDED_COORDS)) then
         TMP%SPECPTR%UNGRIDDED_COORDS  =>  usableUNGRIDDED_COORDS
      else
         NULLIFY(TMP%SPECPTR%UNGRIDDED_COORDS)
      endif
      if(associated(usableATTR_RNAMES)) then
         TMP%SPECPTR%ATTR_RNAMES=>  usableATTR_RNAMES
      else
         NULLIFY(TMP%SPECPTR%ATTR_RNAMES)
      endif
      if(associated(usableATTR_INAMES)) then
         TMP%SPECPTR%ATTR_INAMES=>  usableATTR_INAMES
      else
         NULLIFY(TMP%SPECPTR%ATTR_INAMES)
      endif

      call spec%var_specs%push_back(tmp)
      call spec%update_legacy()

      _RETURN(ESMF_SUCCESS)

   end subroutine MAPL_VarSpecCreateInListNew

  subroutine MAPL_VarSpecAddRefFromItemNew(SPEC, ITEM, ALLOW_DUPLICATES, RC)

    type (StateSpecification)        :: SPEC
    type (MAPL_VarSpec ), intent(IN )     :: ITEM
    logical, optional   , intent(IN)      :: ALLOW_DUPLICATES
    integer, optional   , intent(OUT)     :: RC



    integer                               :: STATUS

    integer                               :: I
    logical                               :: usableALLOW_DUPLICATES
    class(Logger), pointer :: lgr
     

    if(present(ALLOW_DUPLICATES)) then
       usableALLOW_DUPLICATES=ALLOW_DUPLICATES
    else
       usableALLOW_DUPLICATES=.FALSE.
    endif


    if(.not.associated(ITEM%SPECPtr)) then
       _RETURN(ESMF_FAILURE)
    endif
    if (spec%var_specs%size() > 0) then
       if (.not. usableALLOW_DUPLICATES) then
          I = MAPL_VarSpecGetIndex(SPEC%old_var_specs, ITEM, RC=STATUS)
          _VERIFY(STATUS)
          if(I /= -1) then
             if (SPEC%old_var_specs(I) == ITEM) THEN
                if(present(RC)) then
                   RC=MAPL_DuplicateEntry
                end if
                return
             else
                lgr => logging%get_logger('MAPL.GENERIC')
                call lgr%error("Duplicate SHORT_NAME %a with different attributes.", trim(ITEM%SPECPtr%short_name))
                call MAPL_VarSpecPrint(ITEM)
                call MAPL_VarSpecPrint(SPEC%old_var_specs(I))
                _RETURN(ESMF_FAILURE)
             end if
          endif
       end if
    end if
      call spec%var_specs%push_back(item)
      call spec%update_legacy()

      _RETURN(ESMF_SUCCESS)


   end subroutine MAPL_VarSpecAddRefFromItemNew


end module mapl_StateSpecification
