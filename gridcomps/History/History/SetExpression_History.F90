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
  SUBMODULE (MAPL_HistoryGridCompMod) SetExpression_smod
!
! !USES:
!
  implicit none

contains


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

! Local variables:

  integer:: i,j,m,k,status,largest_rank,iRepField,ivLoc
  logical :: ifound_vloc
  character(len=ESMF_MAXSTR) :: tmpList
  character(len=ESMF_MAXSTR) :: VarName
  integer                    :: idx
  character(len=ESMF_MAXSTR), allocatable :: VarNames(:)
  logical,                    allocatable :: VarNeeded(:)
  integer                                 :: iRealFields
  character(len=256)                      :: ExtVars
  integer                                 :: nExtraFields,nUniqueExtraFields
  character(len=ESMF_MAXSTR), allocatable :: NonUniqueVarNames(:,:)

  character(len=ESMF_MAXSTR), allocatable :: TotVarNames(:)
  character(len=ESMF_MAXSTR), allocatable :: TotCmpNames(:)
  character(len=ESMF_MAXSTR), allocatable :: TotAliasNames(:)
  integer,                    allocatable :: totRank(:)
  integer,                    allocatable :: totLoc(:)
  integer                                 :: totFields
  type(ESMF_State), pointer               :: exptmp (:)
  type(ESMF_State)                        :: state
  type(ESMF_Field)                        :: field
  integer                                 :: dims
  logical                                 :: hasField

! Set rewrite flag and tmpfields.
! To keep consistency, all the arithmetic parsing output fields must
! only be combinations of the alias output field variables (i.e., fields(3,:))
! rather than the actual output field variables (i.e., fields(1,:)).
! Also do check that there are no illegal operations
!-------------------------------------------------------------------
  allocate ( exptmp (1), _STAT )
  exptmp(1) = ExpState
  ! check which fields are actual exports or expressions
  nPExtraFields = 0
  iRealFields = 0
  do m=1,nfield

    call MAPL_ExportStateGet(exptmp,fields(2,m),state,_RC)
    call checkIfStateHasField(state, fields(1,m), hasField, _RC)
    if (hasField) then
       iRealFields = iRealFields + 1
       rewrite(m)= .FALSE.
       tmpfields(m)= trim(fields(1,m))
    else
       rewrite(m)= .TRUE.
       tmpfields(m)= trim(fields(1,m))
      end if
  enddo

  ! now that we know this allocated a place to store the names of the real fields
  allocate(VarNames(iRealFields),_STAT)
  allocate(VarNeeded(iRealFields),_STAT)
  k=0
  do m=1,nfield
     if ( (rewrite(m) .eqv. .False.)) then
        k=k+1
        VarNames(k)=fields(3,m)
     endif
  enddo

  ! now we can have extra fields that are not in collection if they are in the component
  ! we specify with the expression we get the number of these

  nExtraFields=0
  do m=1,nfield
     if (rewrite(m)) then

         ExtVars = ""
         call CheckSyntax(tmpfields(m),VarNames,VarNeeded,ExtVar=ExtVars,_RC)

         tmpList=ExtVars
         do i=1,len_trim(tmpList)
            idx=index(tmpList,',')
            if (idx /= 0) then
               varName = tmpList(1:idx-1)
               nExtraFields=nExtraFields+1
               tmpList = tmpList(idx+1:)
            else
               exit
            end if
         end do

      end if
   end do

  allocate(NonUniqueVarNames(nExtraFields,2))

  ! get the number of extra fields, after this we will have to check for duplicates
  nExtraFields=0
  do m=1,nfield
     if (rewrite(m)) then

         ExtVars = ""
         call CheckSyntax(tmpfields(m),VarNames,VarNeeded,ExtVar=ExtVars,_RC)

         tmpList=ExtVars
         do i=1,len_trim(tmpList)
            idx=index(tmpList,',')
            if (idx /= 0) then
               varName = tmpList(1:idx-1)
               nExtraFields=nExtraFields+1
               NonUniqueVarNames(nExtraFields,1) = trim(VarName)
               NonUniqueVarNames(nExtraFields,2) = fields(2,m)
               tmpList = tmpList(idx+1:)
            else
               exit
            end if
         end do

      end if
   end do

   deallocate(VarNames)
   deallocate(VarNeeded)

   ! blank out any duplicates
   do i=1,nExtraFields
      VarName = NonUniqueVarNames(i,1)
      do j=i+1,nExtraFields
         if (trim(VarName) == trim(NonUniqueVarNames(j,1))) then
            NonUniqueVarNames(j,1)="DUPLICATE"
         end if
      end do
   end do

   nUniqueExtraFields = 0
   do i=1,nExtraFields
      if (trim(NonUniqueVarNames(i,1)) /= "DUPLICATE") nUniqueExtraFields = nUniqueExtraFields + 1
   end do

  totFields = iRealFields + nUniqueExtraFields
  allocate(TotVarNames(totFields),_STAT)
  allocate(TotCmpNames(totFields),_STAT)
  allocate(TotAliasNames(totFields),_STAT)
  allocate(TotRank(totFields),_STAT)
  allocate(TotLoc(totFields),_STAT)

  iRealFields = 0
  do i=1,nfield
    if ( (.not.rewrite(i)) ) then
       iRealFields = iRealFields + 1
       TotVarNames(iRealFields) = trim(fields(1,i))
       TotCmpNames(iRealFields) = trim(fields(2,i))
       TotAliasNames(iRealFields) = trim(fields(3,i))

       call MAPL_ExportStateGet(exptmp,fields(2,i),state,_RC)
       call MAPL_StateGet(state,fields(1,i),field,_RC)
       call ESMF_AttributeGet(field,name='DIMS',value=dims,_RC)
       TotRank(iRealFields) = dims
       call ESMF_AttributeGet(field,name='VLOCATION',value=dims,_RC)
       TotLoc(iRealFields) = dims

    endif
  enddo
  nUniqueExtraFields = 0
  do i=1, nExtraFields
     if (trim(NonUniqueVarNames(i,1)) /= "DUPLICATE") then
        nUniqueExtraFields = nUniqueExtraFields + 1
        TotVarNames(iRealFields+nUniqueExtraFields) = NonUniqueVarNames(i,1)
        TotCmpNames(iRealFields+nUniqueExtraFields) = NonUniqueVarNames(i,2)
        TotAliasNames(iRealFields+nUniqueExtraFields) = NonUniqueVarNames(i,1)
        call MAPL_ExportStateGet ( exptmp,NonUniqueVarNames(i,2),state,_RC )
        call MAPL_StateGet(state, NonUniqueVarNames(i,1),field,_RC)

        call ESMF_AttributeGet(field,name='DIMS',value=dims,_RC)
        TotRank(iRealFields+nUniqueExtraFields) = dims
        call ESMF_AttributeGet(field,name='VLOCATION',value=dims,_RC)
        TotLoc(iRealFields+nUniqueExtraFields) = dims
     end if
  end do

  allocate(extraFields(nUniqueExtraFields),_STAT)
  allocate(extraGridComp(nUniqueExtraFields),_STAT)
  nPExtraFields = nUniqueExtraFields
  nUniqueExtraFields = 0
  do i=1,nExtraFields
     if (trim(NonUniqueVarNames(i,1)) /= "DUPLICATE") then
        nUniqueExtraFields = nUniqueExtraFields + 1
        extraFields(nUniqueExtraFields) = NonUniqueVarNames(i,1)
        extraGridComp(nUniqueExtraFields) = NonUniqueVarNames(i,2)
     end if
  end do

  deallocate(NonUniqueVarNames)
  deallocate(exptmp)
! Change the arithmetic parsing field containing mutiple variables
! to the dummy default field containing a single field variable.
! Since MAPL_HistoryGridCompMod does not understand arithmetic parsing field variable,
! we need to change the arithmetic parsing field variable to the dummy field to allocate memory.
! But the actual arithmetic parsing field already has been copied to the temporialy field.
! Also we will do some syntax checking here since this is a good place
!----------------------------------------------------------------------
 allocate(VarNeeded(TotFields),_STAT)

 do m=1,nfield
     if (Rewrite(m) .eqv. .TRUE.) then
         largest_rank =0
         ifound_vloc=.false.
         call CheckSyntax(tmpfields(m),TotAliasNames,VarNeeded,_RC)
         do i=1,TotFields
            if (VarNeeded(i)) then
               if (TotRank(i)> largest_rank) then
                  largest_rank=TotRank(i)
                  iRepField=i
               end if

               if (ifound_vloc) then
                  if (ivLoc /= Totloc(i) .and. totloc(i) /= MAPL_VLocationNone) then
                     _FAIL('arithmetic expression has two different vlocations')
                  end if
               else
                  if (totloc(i) /= MAPL_VLocationNone) then
                     ivloc = totloc(i)
                     ifound_vloc = .true.
                  endif
               end if
            end if
         end do
         fields(1,m)= TotVarNames(iRepField)
         fields(2,m)= TotCmpNames(iRepField)

     endif
 enddo

 deallocate(VarNeeded)
 deallocate(TotVarNames)
 deallocate(TotCmpNames)
 deallocate(TotAliasNames)
 deallocate(TotRank)
 deallocate(TotLoc)

 _RETURN(ESMF_SUCCESS)

 end subroutine MAPL_SetExpression

END SUBMODULE
