   
!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!
#include "MAPL_Generic.h"
   
MODULE ExtDataUtRoot_GridCompMod
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

      PUBLIC SetServices

      type :: timeVar
         type(ESMF_Time) :: refTime
         character(len=10) :: timeUnits
         integer :: climYear
         logical :: have_offset
         integer :: update_ref_time
         type(ESMF_TimeInterval) :: update_offset
      contains
         procedure :: init_time
         procedure :: evaluate_time
         procedure :: set_time_for_date
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

   contains

      subroutine SetServices ( GC, RC )

! !ARGUMENTS:

         type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
         integer,             intent(  OUT) :: RC  ! return code

         integer                                 :: STATUS
         character(len=ESMF_MAXSTR)              :: COMP_NAME

         type(ESMF_Config)          :: cf
         type(SyntheticFieldSupportWrapper) :: synthWrap
         type(SyntheticFieldSupport), pointer :: synth

         call ESMF_GridCompGet( GC, NAME=COMP_NAME, CONFIG=CF, _RC )

         call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_INITIALIZE,  Initialize_, _RC)
         call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN,   Run_, _RC)

         allocate(synth)
         synthWrap%ptr => synth
         call ESMF_UserCompSetInternalState(gc,wrap_name,synthWrap,status)
         _VERIFY(status)

         call AddState(GC,CF,"IMPORT",_RC)
         call AddState(GC,CF,"EXPORT",_RC)
         call MAPL_AddInternalSpec(GC,&
               short_name='time', &
               long_name='na' , &
               units = 'na', &
               dims = MAPL_DimsHorzOnly, &
               vlocation = MAPL_VLocationNone, _RC)
         call MAPL_AddInternalSpec(GC,&
               short_name='lats', &
               long_name='na' , &
               units = 'na', &
               dims = MAPL_DimsHorzOnly, &
               vlocation = MAPL_VLocationNone, _RC)
         call MAPL_AddInternalSpec(GC,&
               short_name='lons', &
               long_name='na' , &
               units = 'na', &
               dims = MAPL_DimsHorzOnly, &
               vlocation = MAPL_VLocationNone, _RC)
         call MAPL_AddInternalSpec(GC,&
               short_name='i_index', &
               long_name='na' , &
               units = 'na', &
               dims = MAPL_DimsHorzOnly, &
               vlocation = MAPL_VLocationNone, _RC)
         call MAPL_AddInternalSpec(GC,&
               short_name='j_index', &
               long_name='na' , &
               units = 'na', &
               dims = MAPL_DimsHorzOnly, &
               vlocation = MAPL_VLocationNone, _RC)
         call MAPL_AddInternalSpec(GC,&
               short_name='doy', &
               long_name='day_since_start_of_year' , &
               units = 'na', &
               dims = MAPL_DimsHorzOnly, &
               vlocation = MAPL_VLocationNone, _RC)

         call MAPL_GenericSetServices ( GC, _RC)

         _RETURN(ESMF_SUCCESS)

      end subroutine SetServices

      SUBROUTINE Initialize_ ( GC, IMPORT, EXPORT, CLOCK, rc )

         implicit NONE

         type(ESMF_Clock),  intent(inout) :: CLOCK     ! The clock

         type(ESMF_GridComp), intent(inout) :: GC      ! Grid Component
         type(ESMF_State), intent(inout) :: IMPORT     ! Import State
         type(ESMF_State), intent(inout) :: EXPORT     ! Export State
         integer, intent(out)            :: rc         ! Error return code:

         type(ESMF_Config)           :: CF          ! Universal Config 
         integer                     :: status
         character(len=ESMF_MAXSTR)  :: comp_name

         !real(REAL64) :: ptop, pint
         !real(REAL64), allocatable :: ak(:),bk(:)
         integer :: nrows, ncolumn,i
         !integer :: ls
         type(ESMF_Grid) :: grid
         type(ESMF_Time) :: currTime
         type(SyntheticFieldSupportWrapper) :: synthWrap
         type(SyntheticFieldSupport), pointer :: synth => null()
         character(len=ESMF_MaxStr) :: key, keyVal

         call ESMF_GridCompGet( GC, name=comp_name, config=CF, _RC )

         call ESMF_UserCompGetInternalState(gc,wrap_name,synthWrap,status)
         _VERIFY(status)
         synth => synthWrap%ptr
         call ESMF_ClockGet(Clock,currTime=currTime,_RC)

         call ESMF_ConfigGetDim(cf,nrows,ncolumn,label="FILL_DEF::",rc=status)
         if (status==ESMF_SUCCESS) then
            call ESMF_ConfigFindLabel(cf,label="FILL_DEF::",_RC)
            do i=1,nrows
               call ESMF_ConfigNextLine(cf,_RC)
               call ESMF_ConfigGetAttribute(cf,value=key,_RC)
               call ESMF_ConfigGetAttribute(cf,value=keyVal,_RC)
               call synth%fillDefs%insert(trim(key),trim(keyVal))
            enddo
         end if
         call synth%tFunc%init_time(cf,currTime,_RC)

         call ESMF_ConfigGetAttribute(cf,value=synth%runMode,label="RUN_MODE:",_RC)

         call MAPL_GridCreate(GC, _RC)
         call ESMF_GridCompGet(GC, grid=grid, _RC)
         !allocate(ak(lm+1),stat=status)
         !allocate(bk(lm+1),stat=status)
         !call set_eta(lm,ls,ptop,pint,ak,bk)
         !call ESMF_AttributeSet(grid,name='GridAK', itemCount=LM+1, &
               !valuelist=ak,_RC)
         !call ESMF_AttributeSet(grid,name='GridBK', itemCount=LM+1, &
               !valuelist=bk,_RC)

         call MAPL_GenericInitialize ( GC, IMPORT, EXPORT, clock, _RC)
         call ForceAllocation(Export,_RC)

         _RETURN(ESMF_SUCCESS)

      END SUBROUTINE Initialize_

      SUBROUTINE Run_ ( GC, IMPORT, EXPORT, CLOCK, rc )

         implicit NONE

         type(ESMF_Clock),  intent(inout) :: CLOCK     ! The clock

         type(ESMF_GridComp), intent(inout)  :: GC     ! Grid Component
         type(ESMF_State), intent(inout) :: IMPORT     ! Import State
         type(ESMF_State), intent(inout) :: EXPORT     ! Export State
         integer, intent(out) ::  rc                   ! Error return code:

         type (ESMF_GridComp),      allocatable  :: GCS(:)
         type (ESMF_State),         allocatable  :: GIM(:)
         type (ESMF_State),         allocatable  :: GEX(:)

         integer                       :: STATUS
         type(MAPL_MetaComp), pointer :: MAPL
         character(len=ESMF_MAXSTR)    :: comp_name
         type(SyntheticFieldSupportWrapper) :: synthWrap
         type(SyntheticFieldSupport), pointer :: synth => null()

         type(ESMF_State) :: internal
         type(ESMF_Config) :: cf
         type(ESMF_Time) :: currTime
         real(ESMF_KIND_R8),pointer :: ptrR8(:,:)
         real, pointer :: ptrR4(:,:)
         type(ESMF_Grid) :: grid

         call ESMF_GridCompGet( GC, name=comp_name, _RC )

         call MAPL_GetObjectFromGC ( GC, MAPL, _RC )
         call MAPL_Get(MAPL, childrens_gridcomps=GCS, &
              childrens_import_states =GIM, childrens_export_states=GEX, _RC)
         call MAPL_Get ( MAPL, internal_esmf_state=internal, cf=cf, _RC )
         call ESMF_ClockGet(Clock,currTime=currTime,_RC)

         call ESMF_UserCompGetInternalState(gc,wrap_name,synthWrap,status)
         _VERIFY(status)
         synth => synthWrap%ptr
         call ESMF_GridCompGet(GC,grid=grid,_RC)
         call MAPL_GetPointer(internal,ptrR4,'lons',_RC)
         call ESMF_GridGetCoord (Grid, coordDim=1, localDE=0, &
                           staggerloc=ESMF_STAGGERLOC_CENTER, &
                           farrayPtr=ptrR8, _RC)
         ptrR4=ptrR8
         call MAPL_GetPointer(internal,ptrR4,'lats',_RC)
         call ESMF_GridGetCoord (Grid, coordDim=2, localDE=0, &
                           staggerloc=ESMF_STAGGERLOC_CENTER, &
                           farrayPtr=ptrR8, _RC) 
         ptrR4=ptrR8

         select case (trim(synth%runMode))

         case(RunModeGenerateExports)

            call FillState(internal,export,currTime,grid,synth,_RC) 

         case(runModecompareImports)
            call FillState(internal,export,currTime,grid,synth,_RC)
            call CompareState(import,export,0.001,_RC) 

         case(runModeFillImport) 
! Nothing to do, we are just letting ExtData run

         case(runModeFillExportFromImport)
            call CopyState(import,export,_RC)

         end select

         _RETURN(ESMF_SUCCESS)

      END SUBROUTINE Run_

   subroutine AddState(gc,cf,stateType,rc)
      type(ESMF_GridComp), intent(inout) :: gc
      type(ESMF_Config), intent(inout) :: cf
      character(len=*), intent(in) :: stateType
      integer, intent(out), optional :: rc

      integer :: status

      type(VarspecDescriptionVector) :: VarspecVec
      type(VarspecDescriptionVectorIterator) :: Iter
      type(VarspecDescription) :: VarspecDescr
      type(VarspecDescription), pointer :: VarspecPtr
      integer :: nrows,ncolumn,i

      if (trim(stateType) == 'IMPORT') then
         call ESMF_ConfigGetDim(cf,nrows,ncolumn,label="IMPORT_STATE::",rc=status)
         if (status==ESMF_SUCCESS) then
            call ESMF_ConfigFindLabel(cf,label="IMPORT_STATE::",_RC)
            do i=1,nrows
               call ESMF_ConfigNextLine(cf,_RC)
               VarspecDescr = VarspecDescription(CF,ncolumn,rc)
               call VarspecVec%push_back(VarspecDescr)
            enddo
         end if
      end if
      if (trim(stateType) == 'EXPORT') then
         call ESMF_ConfigGetDim(cf,nrows,ncolumn,label="EXPORT_STATE::",rc=status)
         if (status==ESMF_SUCCESS) then
         call ESMF_ConfigFindLabel(cf,label="EXPORT_STATE::",_RC)
            do i=1,nrows
               call ESMF_ConfigNextLine(cf,_RC)
               VarspecDescr = VarspecDescription(CF,ncolumn,rc)
               call VarspecVec%push_back(VarspecDescr)
            enddo
         endif
      end if
      iter = VarspecVec%begin()
      do while (iter /= VarspecVec%end())
         VarspecPtr => iter%get()
         call VarspecPtr%addNewSpec(gc,stateType,_RC)
         call iter%next()
      end do

   end subroutine AddState

   subroutine init_time(this,cf,currTime,rc)
      class(timeVar), intent(inout) :: this
      type(ESMF_Config), intent(inout) :: cf
      type(ESMF_Time), intent(inout) :: currTime
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: isPresent

      integer :: datetime(2), yy,mm,dd,mn,hh,ss,int_time

      call ESMF_ConfigFindLabel(cf,'REF_TIME:',isPresent=isPresent,_RC)
      if (isPresent) then
         call ESMF_ConfigGetAttribute(cf,datetime,label='REF_TIME:',_RC)
         YY =     datetime(1)/10000
         MM = mod(datetime(1),10000)/100
         DD = mod(datetime(1),100)
         HH =     datetime(2)/10000
         MN = mod(datetime(2),10000)/100
         SS = mod(datetime(2),100)
         call ESMF_TimeSet(this%refTime,yy=yy,mm=mm,dd=dd,h=hh,m=mn,s=ss,_RC)
      else
         this%refTime=currTime
      end if
      call ESMF_ConfigGetAttribute(cf,this%timeUnits,label='TIME_UNITS:',default='days',_RC)

      call ESMF_ConfigGetAttribute(cf,this%climYear,label='CLIM_YEAR:',default=-1,_RC)

      this%have_offset = .false.
      this%update_ref_time = -1
      call ESMF_ConfigFindLabel(cf,'UPDATE_OFFSET:',isPresent=isPresent,_RC)
      if (isPresent) then
         call ESMF_ConfigGetAttribute(cf,int_time,label='UPDATE_OFFSET:',_RC)
         HH = int_time/10000
         MN = mod(int_time,10000)/100
         SS = mod(int_time,100)
         call ESMF_TimeIntervalSet(this%update_offset,h=hh,m=mn,s=ss,_RC)
         this%have_offset = .true.
      end if
      call ESMF_ConfigFindLabel(cf,'UPDATE_REF_TIME:',isPresent=isPresent,_RC)
      if (isPresent) then
         call ESMF_ConfigGetAttribute(cf,this%update_ref_time,label='UPDATE_REF_TIME:',_RC)
      end if
      _RETURN(_SUCCESS)

   end subroutine init_time

   function evaluate_time(this,currTime,rc) result(dt)
      class(timeVar), intent(in) :: this
      type(ESMF_Time), intent(inout) :: currTime
      integer, optional, intent(out) :: rc
      real(kind=ESMF_KIND_R8) :: dt

      integer :: status

      type(ESMF_TimeInterval) :: timeInterval, yearInterval
      integer :: ycurr,yint
      type(ESMF_Time) :: periodic_time

      if (this%climYear > 0) then
         call ESMF_TimeGet(currTime,yy=ycurr,_RC)
         yint=this%climYear-ycurr
         call ESMF_TimeIntervalSet(yearInterval,yy=yint,_RC)
         currTime = currTime+yearInterval
      end if
      periodic_time = this%set_time_for_date(currTime,_RC) 
      if (this%have_offset) then
         timeInterval = periodic_time + this%update_offset - this%refTime 
      else
         timeInterval = periodic_time - this%refTime
      end if
      select case(trim(this%timeUnits))
      case ('days')
         call ESMF_TimeIntervalGet(timeInterval,d_r8=dt,_RC)
      case ('hours')
         call ESMF_TimeIntervalGet(timeInterval,h_r8=dt,_RC)
      case ('minutes')
         call ESMF_TimeIntervalGet(timeInterval,m_r8=dt,_RC)
      case ('seconds')
         call ESMF_TimeIntervalGet(timeInterval,s_r8=dt,_RC)
      case default
         _FAIL("Unsupported time units specify for interval")
      end select

   end function evaluate_time

   function set_time_for_date(this,input_time,rc) result(returned_time)
      type(ESMF_Time) :: returned_time

      class(timeVar), intent(in) :: this
      type(ESMF_Time), intent(inout) :: input_time
      integer, optional, intent(out) :: rc

      integer :: hour,minute,second,year,month,day,status
      type(ESMF_Time) :: new_time

      if (this%update_ref_time /= -1) then
         call ESMF_TimeGet(input_time,yy=year,mm=month,dd=day,_RC)
         call MAPL_UnpackTime(this%update_ref_time,hour,minute,second)
         call ESMF_TimeSet(new_time,yy=year,mm=month,dd=day,h=hour,m=minute,s=second,_RC)
         if (new_time == input_time) then
            returned_time = input_time
         else if (new_time < input_time) then
            returned_time = new_time
         else if (new_time > input_time) then        
            call ESMF_TimeSet(new_time,yy=year,mm=month,dd=day-1,h=hour,m=minute,s=second,_RC)
            returned_time = new_time
         end if
      else
         returned_time = input_time
      end if
      _RETURN(_SUCCESS)
   end function

   subroutine CopyState(inState,outState,rc)

      type(ESMF_State), intent(inout) :: inState
      type(ESMF_State), intent(inout) :: outState
      integer, optional, intent(out) :: rc

      integer :: status

      integer                             :: I
      real, pointer                       :: IMptr3(:,:,:) => null()
      real, pointer                       :: Exptr3(:,:,:) => null()
      real, pointer                       :: IMptr2(:,:) => null()
      real, pointer                       :: Exptr2(:,:) => null()
      integer :: itemcountIn,itemCountOut,rank
      character(len=ESMF_MAXSTR), allocatable :: inNameList(:)
      character(len=ESMF_MAXSTR), allocatable :: outNameList(:)
      type(ESMF_Field) :: expf,impf

      call ESMF_StateGet(inState,itemcount=itemCountIn,_RC)
      allocate(InNameList(itemCountIn),stat=status)
      _VERIFY(status)
      call ESMF_StateGet(inState,itemNameList=InNameList,_RC)

      call ESMF_StateGet(outState,itemcount=ItemCountOut,_RC)
      allocate(outNameList(ItemCountOut),stat=status)
      _VERIFY(status)
      call ESMF_StateGet(outState,itemNameList=outNameList,_RC)

      _ASSERT(itemCountIn == itemCountOut,'needs informative message')
      call ESMF_StateGet(inState,itemNameList=inNameList,_RC)
      do i=1,itemCountIn
         call ESMF_StateGet(inState,trim(inNameList(i)),impf,_RC)
         call ESMF_StateGet(outState,trim(outNameList(i)),expf,_RC)
         call ESMF_FieldGet(impf,rank=rank,_RC)
         if (rank==2) then
            call MAPL_GetPointer(inState,IMptr2,inNameList(i),_RC)
            call MAPL_GetPointer(outState,Exptr2,inNameList(i),alloc=.true.,_RC)
            EXptr2=IMptr2
         else if (rank==3) then
            call MAPL_GetPointer(inState,IMptr3,inNameList(i),_RC)
            call MAPL_GetPointer(outState,EXptr3,inNameList(i),alloc=.true.,_RC)
            EXptr3=IMptr3
         end if
      end do
      deallocate(inNameList,outNameList) 
      _RETURN(ESMF_SUCCESS)

   end subroutine CopyState

   subroutine FillState(inState,outState,time,grid,Synth,rc)

      type(ESMF_State), intent(inout) :: inState
      type(ESMF_State), intent(inout) :: outState
      type(ESMF_Time),  intent(Inout) :: time
      type(ESMF_Grid),  intent(inout) :: grid
      type(SyntheticFieldSupport) :: synth
      integer, optional, intent(out) :: rc

      integer :: status
      real, pointer                       :: Exptr2(:,:) => null()
      integer :: itemcount
      character(len=ESMF_MAXSTR), allocatable :: outNameList(:)
      type(ESMF_Field) :: expf,farray(6)
      type(ESMF_State) :: pstate
      character(len=:), pointer :: fexpr
      integer :: i1,in,j1,jn,ldims(3),i,j
      real(kind=ESMF_KIND_R8) :: doy,time_delta

      call MAPL_GridGet(grid,localcellcountperdim=ldims,_RC)
      call MAPL_Grid_Interior(grid,i1,in,j1,jn)
      call ESMF_StateGet(outState,itemcount=itemCount,_RC)
      allocate(outNameList(itemCount),stat=status)
      _VERIFY(status)
      call ESMF_StateGet(outState,itemNameList=outNameList,_RC)

      call MAPL_GetPointer(inState,exPtr2,'i_index',_RC)
      do j = 1,ldims(2)
         do i=1,ldims(1)
            exPtr2(i,j)=i1+i-1
         enddo
      enddo
      call MAPL_GetPointer(inState,exPtr2,'j_index',_RC)
      do i = 1,ldims(1)
         do j=1,ldims(2)
            exPtr2(i,j)=j1+j-1
         enddo
      enddo

      call ESMF_StateGet(inState,'time',farray(1),_RC)
      time_delta = synth%tFunc%evaluate_time(Time,_RC)
      call FieldSet(farray(1), time_delta,_RC)
      call ESMF_StateGet(inState,'lons',farray(2),_RC)
      call ESMF_StateGet(inState,'lats',farray(3),_RC)
      call ESMF_StateGet(inState,'i_index',farray(4),_RC)
      call ESMF_StateGet(inState,'j_index',farray(5),_RC)
      call ESMF_StateGet(inState,'doy',farray(6),_RC)
      doy = compute_doy(time,_RC)
      call FieldSet(farray(6), doy,_RC)
      pstate = ESMF_StateCreate(_RC)
      call ESMF_StateAdd(pstate,farray,_RC)

      do i=1,itemCount
         call ESMF_StateGet(outState,trim(outNameList(i)),expf,_RC)
         fexpr => synth%fillDefs%at(trim(outNameList(i)))
         call MAPL_StateEval(pstate,fexpr,expf,_RC)
      enddo

      _RETURN(ESMF_SUCCESS)

   end subroutine FillState

   subroutine CompareState(State1,State2,tol,rc)
      type(ESMF_State), intent(inout) :: State1
      type(ESMF_State), intent(inout) :: State2
      real, intent(in)                :: tol
      integer, optional, intent(out) :: rc

      integer :: status
      integer                             :: i
      real, pointer                       :: ptr3_1(:,:,:)
      real, pointer                       :: ptr3_2(:,:,:)
      real, pointer                       :: ptr2_1(:,:)
      real, pointer                       :: ptr2_2(:,:)
      integer :: itemcount,rank1,rank2
      character(len=ESMF_MAXSTR), allocatable :: NameList(:)
      logical, allocatable :: foundDiff(:)
      type(ESMF_Field) :: Field1,Field2
      logical :: all_undef1, all_undef2
    
      call ESMF_StateGet(State1,itemcount=itemCount,_RC)
         allocate(NameList(itemCount),stat=status)
         _VERIFY(status)
         allocate(foundDiff(itemCount),stat=status,source=.false.)
         _VERIFY(status)
         call ESMF_StateGet(State1,itemNameList=NameList,_RC)
         do i=1,itemCount
            call ESMF_StateGet(State1,trim(nameList(i)),field1,_RC)
            call ESMF_StateGet(State2,trim(nameList(i)),field2,_RC)
            call ESMF_FieldGet(field1,rank=rank1,_RC)
            call ESMF_FieldGet(field2,rank=rank2,_RC)
            all_undef1 = FieldIsConstant(field1,MAPL_UNDEF,_RC)
            all_undef2 = FieldIsConstant(field2,MAPL_UNDEF,_RC)
            if (all_undef1 .or. all_undef2) then
               exit
            end if
            _ASSERT(rank1==rank2,'needs informative message')
            foundDiff(i)=.false.
            if (rank1==2) then
               call MAPL_GetPointer(state1,ptr2_1,trim(nameList(i)),_RC)
               call MAPL_GetPointer(state2,ptr2_2,trim(nameList(i)),_RC)
               if (any((ptr2_1-ptr2_2) > tol)) then
                   foundDiff(i) = .true.
               end if
            else if (rank1==3) then
               call MAPL_GetPointer(state1,ptr3_1,trim(nameList(i)),_RC)
               call MAPL_GetPointer(state2,ptr3_2,trim(nameList(i)),_RC)
               if (any((ptr3_1-ptr3_2) > tol)) then
                   foundDiff(i) = .true.
               end if
            end if
            if (foundDiff(i)) then 
               _FAIL('found difference when compare state')
            end if
         enddo
         
         _RETURN(ESMF_SUCCESS)

      end subroutine CompareState

      subroutine ForceAllocation(state,rc)
         type(ESMF_State), intent(inout) :: state
         integer, optional, intent(out) :: rc
       
         integer :: status
  
         real, pointer :: ptr3d(:,:,:)
         real, pointer :: ptr2d(:,:)
         integer       :: ii
         integer :: itemcount,dims
         character(len=ESMF_MAXSTR), allocatable :: NameList(:)
         type (ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
         type(ESMF_Field) :: Field

         call ESMF_StateGet(State,itemcount=itemCount,_RC)
         allocate(NameList(itemCount),stat=status)
         _VERIFY(status)
         allocate(itemTypeList(itemCount),stat=status)
         _VERIFY(status)
         call ESMF_StateGet(State,itemNameList=NameList,itemTypeList=itemTypeList,_RC)
         if (itemCount == 0) then
            _RETURN(ESMF_SUCCESS)
         end if
         do ii=1,itemCount
            if (itemTypeList(ii)==ESMF_STATEITEM_FIELD) then
               call ESMF_StateGet(State,trim(nameList(ii)),field,_RC)
               call ESMF_AttributeGet(field,name='DIMS',value=dims,_RC)
               if (dims==MAPL_DimsHorzOnly) then
                  call MAPL_GetPointer(state,ptr2d,trim(nameList(ii)),alloc=.true.,_RC)
               else if (dims==MAPL_DimsHorzVert) then
                  call MAPL_GetPointer(state,ptr3d,trim(nameList(ii)),alloc=.true.,_RC)
               end if
            end if
         enddo
         _RETURN(ESMF_SUCCESS)

      end subroutine ForceAllocation

      function compute_doy(time,rc) result(doy)
         real(ESMF_KIND_R8) :: doy
         type(ESMF_Time), intent(in) :: time
         integer, optional, intent(out) :: rc

         type(ESMF_Time) :: start_0z, current_0z
         integer :: status
         type(ESMF_TimeInterval) :: tint

         integer :: year,month,day,hour,minute,second

         call ESMF_TimeGet(time,yy=year,mm=month,dd=day,h=hour,m=minute,s=second,_RC)
         call ESMF_TimeSet(start_0z,yy=year,mm=1,dd=1,h=0,m=0,s=0,_RC)
         call ESMF_TimeSet(current_0z,yy=year,mm=month,dd=day,h=hour,m=minute,s=second,_RC)
         tint = current_0z-start_0z
         call ESMF_TimeIntervalGet(tint,d_r8=doy,_RC)
         _RETURN(_SUCCESS)
      end function

end module ExtDataUtRoot_GridCompMod

