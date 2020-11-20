   
!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!
#include "MAPL_Generic.h"
   
MODULE ExtDataUtRoot_GridCompMod
!
! !USES:
!
      use ESMF
      use MAPL
      use VarspecDescriptionMod
      use VarspecDescriptionVectorMod
      use netcdf
      use gFTL_StringStringMap
      !use m_set_eta, only: set_eta
      use, intrinsic :: iso_fortran_env, only: REAL64

      IMPLICIT NONE
      PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:

      PUBLIC SetServices

      type :: timeVar
         type(ESMF_Time) :: refTime
         character(len=10) :: timeUnits
         integer :: climYear
      contains
         procedure :: initTime
         procedure :: evaluateTime
      end type timeVar

      type :: SyntheticFieldSupport
         type(ESMF_Field) :: time2d
         type(StringStringMap) :: fillDefs
         character(len=ESMF_MAXSTR) :: runMode
         type(timeVar) :: tFunc
      end type SyntheticFieldSupport

      type :: SyntheticFieldSupportWrapper
         type(SyntheticFieldSupport), pointer :: ptr => null()
      end type SyntheticFieldSupportWrapper

      character(len=*), parameter :: runModeGenerateExports = "GenerateExports"
      character(len=*), parameter :: runModeCompareImports = "CompareImports"
      character(len=*), parameter :: runModeFillExportFromImport = "FillExportsFromImports"
      character(len=*), parameter :: runModeFillImport = "FillImport"
      character(len=*), parameter :: wrap_name = "SyntheticFieldWrapperName"

      include "mpif.h"

   contains

!BOP

! !IROUTINE: SetServices -- Sets ESMF services for this component

! !INTERFACE:

      subroutine SetServices ( GC, RC )

! !ARGUMENTS:

         type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
         integer,             intent(  OUT) :: RC  ! return code

! !DESCRIPTION:  The SetServices registers the Radiation component

!EOP

!=============================================================================
!
! ErrLog Variables

         character(len=ESMF_MAXSTR)              :: IAm
         integer                                 :: STATUS
         character(len=ESMF_MAXSTR)              :: COMP_NAME

         type(ESMF_Config)          :: cf
         type(SyntheticFieldSupportWrapper) :: synthWrap
         type(SyntheticFieldSupport), pointer :: synth

!=============================================================================

! Begin...

! Get my name and set-up traceback handle
! ---------------------------------------

         Iam = 'SetServices'
         call ESMF_GridCompGet( GC, NAME=COMP_NAME, CONFIG=CF, RC=STATUS )
         _VERIFY(STATUS)
         Iam = trim(COMP_NAME) // "::" // Iam

!   Set the Initialize, Run, Finalize entry points
!   ----------------------------------------------
         call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_INITIALIZE,  Initialize_, RC=status)
         _VERIFY(STATUS)
         call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN,   Run_, RC=status)
         _VERIFY(STATUS)

         allocate(synth)
         synthWrap%ptr => synth
         call ESMF_UserCompSetInternalState(gc,wrap_name,synthWrap,status)
         _VERIFY(status)


!   Add Import/Export
!   -----------------
         call AddState(GC,CF,"IMPORT",rc=status)
         _VERIFY(status)
         call AddState(GC,CF,"EXPORT",rc=status)
         _VERIFY(status)
         call MAPL_AddInternalSpec(GC,&
               short_name='time', &
               long_name='na' , &
               units = 'na', &
               dims = MAPL_DimsHorzOnly, &
               vlocation = MAPL_VLocationCenter, rc=status)

!   Generic Set Services
!   --------------------
         call MAPL_GenericSetServices ( GC, rc=status)
         _VERIFY(STATUS)

         _RETURN(ESMF_SUCCESS)

      end subroutine SetServices

!BOP
!
! !IROUTINE:  Initialize_ --- Initialize RUT
!
! !INTERFACE:
!

      SUBROUTINE Initialize_ ( GC, IMPORT, EXPORT, CLOCK, rc )

! !USES:

         implicit NONE

! !INPUT PARAMETERS:

         type(ESMF_Clock),  intent(inout) :: CLOCK     ! The clock

! !OUTPUT PARAMETERS:

         type(ESMF_GridComp), intent(inout) :: GC      ! Grid Component
         type(ESMF_State), intent(inout) :: IMPORT     ! Import State
         type(ESMF_State), intent(inout) :: EXPORT     ! Export State
         integer, intent(out)            :: rc         ! Error return code:
!  0 - all is well
!  1 - 

! !DESCRIPTION: This is a simple ESMF wrapper.
!
! !REVISION HISTORY:
!
!  29 Sep 2011  Anton Darmenov   Cloned from the ExtData GC code
!
!EOP
!-------------------------------------------------------------------------


         type(ESMF_Config)           :: CF          ! Universal Config 
         character(len=ESMF_MAXSTR)  :: Iam
         integer                     :: status
         character(len=ESMF_MAXSTR)  :: comp_name

         !real(REAL64) :: ptop, pint
         !real(REAL64), allocatable :: ak(:),bk(:)
         integer :: im,jm,lm,nx,ny,nrows, ncolumn,i
         !integer :: ls
         type(ESMF_Grid) :: grid
         type(ESMF_Time) :: currTime
         type(SyntheticFieldSupportWrapper) :: synthWrap
         type(SyntheticFieldSupport), pointer :: synth => null()
         character(len=ESMF_MaxStr) :: key, keyVal

!  Get my name and set-up traceback handle
!  ---------------------------------------
         Iam = "Initialize_"
         call ESMF_GridCompGet( GC, name=comp_name, config=CF, rc=status )
         _VERIFY(STATUS)
         Iam = trim(comp_name) // '::' // trim(Iam)

         call ESMF_UserCompGetInternalState(gc,wrap_name,synthWrap,status)
         _VERIFY(status)
         synth => synthWrap%ptr
         call ESMF_ClockGet(Clock,currTime=currTime,rc=status)
         _VERIFY(STATUS)

         call ESMF_ConfigGetDim(cf,nrows,ncolumn,label="FILL_DEF::",rc=status)
         if (status==ESMF_SUCCESS) then
            call ESMF_ConfigFindLabel(cf,label="FILL_DEF::",rc=status)
            _VERIFY(status)
            do i=1,nrows
               call ESMF_ConfigNextLine(cf,rc=status)
               _VERIFY(status)
               call ESMF_ConfigGetAttribute(cf,value=key,rc=status)
               call ESMF_ConfigGetAttribute(cf,value=keyVal,rc=status)
               call synth%fillDefs%insert(trim(key),trim(keyVal))
            enddo
         end if
         call synth%tFunc%initTime(cf,currTime,rc=status)
         _VERIFY(status)

         call ESMF_ConfigGetAttribute(cf,value=synth%runMode,label="RUN_MODE:",rc=status)
         _VERIFY(status)
!  Create grid for this GC
!  ------------------------

!_VERIFY(STATUS)
         call ESMF_ConfigGetAttribute(cf, value=NX, &
               Label="NX:", rc=status)
         call ESMF_ConfigGetAttribute(cf, value=NY, &
               Label="NY:", rc=status)
         call ESMF_ConfigGetAttribute(cf, value=IM, &
               Label="IM:", rc=status)
         call ESMF_ConfigGetAttribute(cf, value=JM, &
               Label="JM:", rc=status)
         call ESMF_ConfigGetAttribute(cf, value=LM, &
               Label="LM:", default=72, rc=status)
         _VERIFY(STATUS)
         !if (jm == 6*im) call GetWeights_init(6,1,IM,IM,LM,NX,NY,.true.,.false.,MPI_COMM_WORLD)

         call MAPL_GridCreate(GC, rc=status)
         _VERIFY(STATUS)
         call ESMF_GridCompGet(GC, grid=grid, rc=status)
         _VERIFY(STATUS)
         !allocate(ak(lm+1),stat=status)
         !_VERIFY(STATUS)
         !allocate(bk(lm+1),stat=status)
         !_VERIFY(STATUS)
         !call set_eta(lm,ls,ptop,pint,ak,bk)
         !call ESMF_AttributeSet(grid,name='GridAK', itemCount=LM+1, &
               !valuelist=ak,rc=status)
         !_VERIFY(STATUS)
         !call ESMF_AttributeSet(grid,name='GridBK', itemCount=LM+1, &
               !valuelist=bk,rc=status)
         !_VERIFY(STATUS)

!  Initialize MAPL Generic
!  -----------------------
         call MAPL_GenericInitialize ( GC, IMPORT, EXPORT, clock, RC=status)
         _VERIFY(STATUS)
         call ForceAllocation(Export,rc=status)
         _VERIFY(STATUS)

!  All done
!  --------
         _RETURN(ESMF_SUCCESS)

      END SUBROUTINE Initialize_

      SUBROUTINE Run_ ( GC, IMPORT, EXPORT, CLOCK, rc )

! !USES:

         implicit NONE

! !INPUT PARAMETERS:

         type(ESMF_Clock),  intent(inout) :: CLOCK     ! The clock

! !OUTPUT PARAMETERS:

         type(ESMF_GridComp), intent(inout)  :: GC     ! Grid Component
         type(ESMF_State), intent(inout) :: IMPORT     ! Import State
         type(ESMF_State), intent(inout) :: EXPORT     ! Export State
         integer, intent(out) ::  rc                   ! Error return code:

         type (ESMF_GridComp),      allocatable  :: GCS(:)
         type (ESMF_State),         allocatable  :: GIM(:)
         type (ESMF_State),         allocatable  :: GEX(:)

         character(len=ESMF_MAXSTR)    :: Iam
         integer                       :: STATUS
         type(MAPL_MetaComp), pointer :: MAPL
         character(len=ESMF_MAXSTR)    :: comp_name
         type(SyntheticFieldSupportWrapper) :: synthWrap
         type(SyntheticFieldSupport), pointer :: synth => null()

! Local derived type aliases

         type(ESMF_State) :: internal
         type(ESMF_Config) :: cf
         type(ESMF_Time) :: currTime

!  Get my name and set-up traceback handle
!  ---------------------------------------
         Iam = "Run_"
         call ESMF_GridCompGet( GC, name=comp_name, rc=status )
         _VERIFY(STATUS)
         Iam = trim(comp_name) // '::' // trim(Iam)

         call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS )
         _VERIFY(STATUS)
         call MAPL_Get(MAPL, childrens_gridcomps=GCS, &
              childrens_import_states =GIM, childrens_export_states=GEX, rc=status)
         _VERIFY(STATUS)
         call MAPL_Get ( MAPL, internal_esmf_state=internal, cf=cf, RC=STATUS )
         _VERIFY(STATUS)
         call ESMF_ClockGet(Clock,currTime=currTime,rc=status)
         _VERIFY(STATUS)

         call ESMF_UserCompGetInternalState(gc,wrap_name,synthWrap,status)
         _VERIFY(status)
         synth => synthWrap%ptr

         select case (trim(synth%runMode))

         case(RunModeGenerateExports)

            call FillState(internal,export,currTime,synth,__RC__) 

         case(runModecompareImports)
            call FillState(internal,export,currTime,synth,__RC__)
            call CompareState(import,export,0.001,__RC__) 

         case(runModeFillImport)
! Nothing to do, we are just letting ExtData run

         case(runModeFillExportFromImport)
            call CopyState(import,export,__RC__)

         end select

!  --------
         _RETURN(ESMF_SUCCESS)

      END SUBROUTINE Run_

   subroutine AddState(gc,cf,stateType,rc)
      type(ESMF_GridComp), intent(inout) :: gc
      type(ESMF_Config), intent(inout) :: cf
      character(len=*), intent(in) :: stateType
      integer, intent(out), optional :: rc

      character(len=*), parameter :: Iam = __FILE__//'::AddState'
      integer :: status

      type(VarspecDescriptionVector) :: VarspecVec
      type(VarspecDescriptionVectorIterator) :: Iter
      type(VarspecDescription) :: VarspecDescr
      type(VarspecDescription), pointer :: VarspecPtr
      integer :: nrows,ncolumn,i

      if (trim(stateType) == 'IMPORT') then
         call ESMF_ConfigGetDim(cf,nrows,ncolumn,label="IMPORT_STATE::",rc=status)
         if (status==ESMF_SUCCESS) then
            call ESMF_ConfigFindLabel(cf,label="IMPORT_STATE::",rc=status)
            do i=1,nrows
               call ESMF_ConfigNextLine(cf,rc=status)
               _VERIFY(status)
               VarspecDescr = VarspecDescription(CF,ncolumn,rc)
               _VERIFY(status)
               call VarspecVec%push_back(VarspecDescr)
            enddo
         end if
      end if
      if (trim(stateType) == 'EXPORT') then
         call ESMF_ConfigGetDim(cf,nrows,ncolumn,label="EXPORT_STATE::",rc=status)
         if (status==ESMF_SUCCESS) then
         call ESMF_ConfigFindLabel(cf,label="EXPORT_STATE::",rc=status)
            do i=1,nrows
               call ESMF_ConfigNextLine(cf,rc=status)
               _VERIFY(status)
               VarspecDescr = VarspecDescription(CF,ncolumn,rc)
               _VERIFY(status)
               call VarspecVec%push_back(VarspecDescr)
            enddo
         endif
      end if
      iter = VarspecVec%begin()
      do while (iter /= VarspecVec%end())
         VarspecPtr => iter%get()
         call VarspecPtr%addNewSpec(gc,stateType,rc=status)
         _VERIFY(status)
         call iter%next()
      end do

   end subroutine AddState

   subroutine initTime(this,cf,currTime,rc)
      class(timeVar), intent(inout) :: this
      type(ESMF_Config), intent(inout) :: cf
      type(ESMF_Time), intent(inout) :: currTime
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = 'initTime'
      integer :: status
      logical :: isPresent

      integer :: datetime(2), yy,mm,dd,mn,hh,ss

      call ESMF_ConfigFindLabel(cf,'REF_TIME:',isPresent=isPresent,rc=status)
      if (isPresent) then
         call ESMF_ConfigGetAttribute(cf,datetime,label='REF_TIME:',rc=status)
         _VERIFY(status)
         YY =     datetime(1)/10000
         MM = mod(datetime(1),10000)/100
         DD = mod(datetime(1),100)
         HH =     datetime(2)/10000
         MN = mod(datetime(2),10000)/100
         SS = mod(datetime(2),100)
         call ESMF_TimeSet(this%refTime,yy=yy,mm=mm,dd=dd,h=hh,m=mn,s=ss,rc=status)
         _VERIFY(status)
      else
         this%refTime=currTime
      end if
      call ESMF_ConfigGetAttribute(cf,this%timeUnits,label='TIME_UNITS:',default='days',rc=status)
      _VERIFY(status)

      call ESMF_ConfigGetAttribute(cf,this%climYear,label='CLIM_YEAR:',default=-1,rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)

   end subroutine initTime

   function evaluateTime(this,currTime,rc) result(dt)
      class(timeVar), intent(in) :: this
      type(ESMF_Time), intent(inout) :: currTime
      integer, optional, intent(out) :: rc
      real(REAL64) :: dt

      character(len=*), parameter :: Iam='evaluateTime'
      integer :: status

      type(ESMF_TimeInterval) :: timeInterval, yearInterval
      integer :: ycurr,yint

      if (this%climYear > 0) then
         call ESMF_TimeGet(currTime,yy=ycurr,rc=status)
         _VERIFY(status)
         yint=this%climYear-ycurr
         call ESMF_TimeIntervalSet(yearInterval,yy=yint,rc=status)
         _VERIFY(status)
         currTime = currTime+yearInterval
      end if
      timeInterval = currTime - this%refTime
      status=ESMF_FAILURE
      select case(trim(this%timeUnits))
      case ('days')
         call ESMF_TimeIntervalGet(timeInterval,d_r8=dt,rc=status)
      case ('hours')
         call ESMF_TimeIntervalGet(timeInterval,h_r8=dt,rc=status)
      case ('minutes')
         call ESMF_TimeIntervalGet(timeInterval,m_r8=dt,rc=status)
      case ('seconds')
         call ESMF_TimeIntervalGet(timeInterval,s_r8=dt,rc=status)
      end select
      _VERIFY(status)

   end function evaluateTime

   subroutine CopyState(inState,outState,rc)

      type(ESMF_State), intent(inout) :: inState
      type(ESMF_State), intent(inout) :: outState
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam=__FILE__//"::CopyState"

      integer                             :: I
      real, pointer                       :: IMptr3(:,:,:) => null()
      real, pointer                       :: Exptr3(:,:,:) => null()
      real, pointer                       :: IMptr2(:,:) => null()
      real, pointer                       :: Exptr2(:,:) => null()
      integer :: itemcountIn,itemCountOut,rank
      character(len=ESMF_MAXSTR), allocatable :: inNameList(:)
      character(len=ESMF_MAXSTR), allocatable :: outNameList(:)
      type(ESMF_Field) :: expf,impf

      call ESMF_StateGet(inState,itemcount=itemCountIn,__RC__)
      allocate(InNameList(itemCountIn),stat=status)
      _VERIFY(status)
      call ESMF_StateGet(inState,itemNameList=InNameList,__RC__)

      call ESMF_StateGet(outState,itemcount=ItemCountOut,__RC__)
      allocate(outNameList(ItemCountOut),stat=status)
      _VERIFY(status)
      call ESMF_StateGet(outState,itemNameList=outNameList,__RC__)

      _ASSERT(itemCountIn == itemCountOut,'needs informative message')
      call ESMF_StateGet(inState,itemNameList=inNameList,__RC__)
      do i=1,itemCountIn
         call ESMF_StateGet(inState,trim(inNameList(i)),impf,__RC__)
         call ESMF_StateGet(outState,trim(outNameList(i)),expf,__RC__)
         call ESMF_FieldGet(impf,rank=rank,__RC__)
         if (rank==2) then
            call MAPL_GetPointer(inState,IMptr2,inNameList(i),__RC__)
            call MAPL_GetPointer(outState,Exptr2,inNameList(i),alloc=.true.,__RC__)
            EXptr2=IMptr2
         else if (rank==3) then
            call MAPL_GetPointer(inState,IMptr3,inNameList(i),__RC__)
            call MAPL_GetPointer(outState,EXptr3,inNameList(i),alloc=.true.,__RC__)
            EXptr3=IMptr3
         end if
      end do
      deallocate(inNameList,outNameList) 
      _RETURN(ESMF_SUCCESS)

   end subroutine CopyState

   subroutine FillState(inState,outState,time,Synth,rc)

      type(ESMF_State), intent(inout) :: inState
      type(ESMF_State), intent(inout) :: outState
      type(ESMF_Time),  intent(Inout) :: time
      type(SyntheticFieldSupport) :: synth
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam=__FILE__//"::FillState"
      integer                             :: I
      real, pointer                       :: Exptr2(:,:) => null()
      integer :: itemcount
      character(len=ESMF_MAXSTR), allocatable :: outNameList(:)
      type(ESMF_Field) :: expf,farray(1)
      type(ESMF_State) :: pstate
      character(len=:), pointer :: fexpr

      call ESMF_StateGet(outState,itemcount=itemCount,__RC__)
      allocate(outNameList(itemCount),stat=status)
      _VERIFY(status)
      call ESMF_StateGet(outState,itemNameList=outNameList,__RC__)

      call MAPL_GetPointer(inState,exPtr2,'time',rc=status)
      _VERIFY(status)
      exPtr2=synth%tFunc%evaluateTime(Time,rc=status)
      _VERIFY(status)
      call ESMF_StateGet(inState,'time',farray(1),rc=status)
      _VERIFY(status)
      pstate = ESMF_StateCreate(rc=status)
      _VERIFY(status)
      call ESMF_StateAdd(pstate,farray,rc=status)
      _VERIFY(status)

      do i=1,itemCount
         call ESMF_StateGet(outState,trim(outNameList(i)),expf,rc=status)
         _VERIFY(status)
         fexpr => synth%fillDefs%at(trim(outNameList(i)))
         call MAPL_StateEval(pstate,fexpr,expf,rc=status)
         _VERIFY(status)
      enddo

      _RETURN(ESMF_SUCCESS)

   end subroutine FillState

   subroutine CompareState(State1,State2,tol,rc)
      type(ESMF_State), intent(inout) :: State1
      type(ESMF_State), intent(inout) :: State2
      real, intent(in)                :: tol
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam=__FILE__//"::CompareState"
      integer                             :: ii,i,j,k
      real, pointer                       :: ptr3_1(:,:,:) => null()
      real, pointer                       :: ptr3_2(:,:,:) => null()
      real, pointer                       :: ptr2_1(:,:) => null()
      real, pointer                       :: ptr2_2(:,:) => null()
      integer :: itemcount,rank1,rank2,lb(3),ub(3)
      character(len=ESMF_MAXSTR), allocatable :: NameList(:)
      logical, allocatable :: foundDiff(:)
      type(ESMF_Field) :: Field1,Field2
    
      call ESMF_StateGet(State1,itemcount=itemCount,__RC__)
         allocate(NameList(itemCount),stat=status)
         _VERIFY(status)
         allocate(foundDiff(itemCount),stat=status)
         _VERIFY(status)
         call ESMF_StateGet(State1,itemNameList=NameList,__RC__)
         do ii=1,itemCount
            call ESMF_StateGet(State1,trim(nameList(ii)),field1,__RC__)
            call ESMF_StateGet(State2,trim(nameList(ii)),field2,__RC__)
            call ESMF_FieldGet(field1,rank=rank1,__RC__)
            call ESMF_FieldGet(field1,rank=rank2,__RC__)
            _ASSERT(rank1==rank2,'needs informative message')
            foundDiff(ii)=.false.
            if (rank1==2) then
               call MAPL_GetPointer(state1,ptr2_1,trim(nameList(ii)),__RC__)
               call MAPL_GetPointer(state2,ptr2_2,trim(nameList(ii)),__RC__)
               do i=1,size(ptr2_1,1) 
                  do j=1,size(ptr2_1,2)
                     if (abs(ptr2_1(i,j)-ptr2_2(i,j)) .gt. tol) then
                        foundDiff(ii)=.true.
                        exit
                     end if
                  enddo
               enddo
            else if (rank1==3) then
               call MAPL_GetPointer(state1,ptr3_1,trim(nameList(ii)),__RC__)
               call MAPL_GetPointer(state2,ptr3_2,trim(nameList(ii)),__RC__)
               lb=lbound(ptr3_1)
               ub=ubound(ptr3_1) 
               do i=1,size(ptr3_1,1) 
                  do j=1,size(ptr3_1,2) 
                     do k=lb(3),ub(3)
                        if (abs(ptr3_1(i,j,k)-ptr3_2(i,j,k)) .gt. tol) then
                           foundDiff(ii)=.true.
                           exit
                        end if
                     enddo
                  enddo
               enddo
            end if
            if (foundDiff(ii)) then 
               _ASSERT(.false.,'found difference when compare state')
            end if
         enddo
         
         _RETURN(ESMF_SUCCESS)

      end subroutine CompareState

      subroutine ForceAllocation(state,rc)
         type(ESMF_State), intent(inout) :: state
         integer, optional, intent(out) :: rc
       
         integer :: status
         character(len=*), parameter :: Iam=__FILE__//"::ForceAllocation"
  
         real, pointer :: ptr3d(:,:,:)
         real, pointer :: ptr2d(:,:)
         integer       :: ii
         integer :: itemcount,dims
         character(len=ESMF_MAXSTR), allocatable :: NameList(:)
         type (ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
         type(ESMF_Field) :: Field

         call ESMF_StateGet(State,itemcount=itemCount,__RC__)
         allocate(NameList(itemCount),stat=status)
         _VERIFY(status)
         allocate(itemTypeList(itemCount),stat=status)
         _VERIFY(status)
         call ESMF_StateGet(State,itemNameList=NameList,itemTypeList=itemTypeList,__RC__)
         if (itemCount == 0) then
            _RETURN(ESMF_SUCCESS)
         end if
         do ii=1,itemCount
            if (itemTypeList(ii)==ESMF_STATEITEM_FIELD) then
               call ESMF_StateGet(State,trim(nameList(ii)),field,__RC__)
               call ESMF_AttributeGet(field,name='DIMS',value=dims,__RC__)
               if (dims==MAPL_DimsHorzOnly) then
                  call MAPL_GetPointer(state,ptr2d,trim(nameList(ii)),alloc=.true.,__RC__)
               else if (dims==MAPL_DimsHorzVert) then
                  call MAPL_GetPointer(state,ptr3d,trim(nameList(ii)),alloc=.true.,__RC__)
               end if
            end if
         enddo
         _RETURN(ESMF_SUCCESS)

      end subroutine ForceAllocation

end module ExtDataUtRoot_GridCompMod

