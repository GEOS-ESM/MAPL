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
  SUBMODULE (MAPL_HistoryGridCompMod) run_smod
!
! !USES:
!
  implicit none

contains

!=====================================================================
!>
! Run the `MAPL_HistoryGridComp` component.
!
 MODULE SUBROUTINE Run ( gc, import, export, clock, rc )

   intrinsic :: size

    type(ESMF_GridComp),    intent(inout) :: gc
    type(ESMF_State),       intent(inout) :: import
    type(ESMF_State),       intent(inout) :: export
    type(ESMF_Clock),       intent(inout) :: clock
    integer, optional,      intent(  out) :: rc

! Locals

    type(MAPL_MetaComp),  pointer  :: GENSTATE
    type(HistoryCollection),   pointer  :: list(:)
    type(HISTORY_STATE),  pointer  :: IntState
    type(HISTORY_wrap)             :: wrap
    integer                        :: nlist
    character(len=ESMF_MAXSTR)     :: fntmpl
    character(len=ESMF_MAXSTR),pointer     :: filename(:)
    integer                        :: n,m
    logical, allocatable           :: NewSeg(:)
    logical, allocatable           :: Writing(:)
    type(ESMF_State)               :: state_out, final_state
    type(ESMF_Field)               :: temp_field, state_field
    integer                        :: nymd, nhms
    character(len=ESMF_MAXSTR)     :: DateStamp
    type(ESMF_Time)                :: current_time
    type(ESMF_Time)                :: lastMonth
    type(ESMF_TimeInterval)        :: dur, oneMonth
    integer                        :: sec
    type (StringGridMap)           :: pt_output_grids
    character(len=ESMF_MAXSTR)     :: key_grid_label
    type (ESMF_Grid), pointer      :: pgrid

    integer :: collection_id
    integer :: create_mode
    type(StringStringMap) :: global_attributes
    type(timeData) :: timeinfo_uninit
    type(ESMF_Grid) :: new_grid
!   variables for "backwards" mode
    logical                        :: fwd
    logical, allocatable           :: Ignore(:)

!   ErrLog vars
    integer                        :: status
    logical                        :: file_exists
    type(GriddedIOitem) :: item

    type(Logger), pointer          :: lgr

!=============================================================================

! Begin...
    _UNUSED_DUMMY(import)
    _UNUSED_DUMMY(export)

! Retrieve the pointer to the state
!----------------------------------

    call ESMF_GridCompGetInternalState(gc, wrap, status)
    _VERIFY(status)
    IntState => wrap%ptr

! the collections
!----------------

    list => IntState%list
    nlist = size(list)

! Retrieve the pointer to the generic state
!------------------------------------------

    call MAPL_GetObjectFromGC ( gc, GENSTATE, _RC)

!   Get clocks' direction
    FWD = .not. ESMF_ClockIsReverse(clock)

   allocate(Ignore (nlist), _STAT)
   Ignore = .false.

  ! decide if clock direction and collections' backwards mode agree

   do n=1,nlist
      if (list(n)%backwards .eqv. FWD) Ignore(n) = .true.
   end do

!  Perform arithemetic parser operations
   do n=1,nlist
    if(Ignore(n)) cycle
    if ( Any(list(n)%ReWrite) ) then
     call MAPL_TimerOn(GENSTATE,"ParserRun")
     if( (.not.list(n)%disabled .and. IntState%average(n)) ) then
      call MAPL_RunExpression(IntState%CIM(n),list(n)%field_set%fields,list(n)%tmpfields, &
         list(n)%ReWrite,list(n)%field_set%nfields,_RC)
     end if
     if( (.not.list(n)%disabled) .and. (.not.IntState%average(n)) ) then
      call MAPL_RunExpression(IntState%GIM(n),list(n)%field_set%fields,list(n)%tmpfields, &
         list(n)%ReWrite,list(n)%field_set%nfields,_RC)
     end if
     call MAPL_TimerOff(GENSTATE,"ParserRun")
    endif
   end do

! We could make a copy for precision conversion here, if needed
! However, this is not very efficient. Copy is needed if it is
! time-averaged (i.e. couplers will be run), or if it is time to
! write instantaneous collection
!@   do n=1,nlist
!@      do m=1,list(n)%field_set%nfields
!@         if (list(n)%r8_to_r4(m)) then
!@            call MAPL_FieldCopy(from=list(n)%r8(m), to=list(n)%r4(m), _RC)
!@         end if
!@      end do
!@   end do

! Couplers are done here for now
!-------------------------------

    do n = 1, nlist
       call MAPL_TimerOn(GENSTATE,trim(list(n)%collection))
       call MAPL_TimerOn(GENSTATE,"Couplers")
       if(Ignore(n)) cycle
       if (.not.list(n)%disabled .and. IntState%average(n)) then
          ! R8 to R4 copy (if needed!)
          do m=1,list(n)%field_set%nfields
             if (list(n)%r8_to_r4(m)) then
                call MAPL_FieldCopy(from=list(n)%r8(m), &
                                    to=list(n)%r4(m), _RC)
             end if
          end do

          call ESMF_CplCompRun (INTSTATE%CCS(n), &
                                importState=INTSTATE%CIM(n), &
                                exportState=INTSTATE%GIM(n), &
                                clock=CLOCK,           &
                                userRC=STATUS)
          _VERIFY(STATUS)
       end if
       call MAPL_TimerOff(GENSTATE,"Couplers")
       call MAPL_TimerOff(GENSTATE,trim(list(n)%collection))
    end do

! Check for History Output
! ------------------------

   allocate(Writing (nlist), _STAT)
   allocate(filename(nlist), _STAT)
   allocate(NewSeg (nlist), _STAT)
   newSeg = .false.

  ! decide if we are writing based on alarms

   do n=1,nlist
      if (list(n)%disabled .or. ESMF_AlarmIsRinging(list(n)%end_alarm) ) then
         list(n)%disabled = .true.
         Writing(n) = .false.
      else if (list(n)%timeseries_output) then
         Writing(n) = ESMF_AlarmIsRinging ( list(n)%trajectory%alarm )
      else if (index(trim(list(n)%output_grid_label), 'SwathGrid') > 0) then
         Writing(n) = ESMF_AlarmIsRinging ( Hsampler%alarm )
      else
         Writing(n) = ESMF_AlarmIsRinging ( list(n)%his_alarm )
      endif

!      if(Writing(n)) then
!         call ESMF_AlarmRingerOff( list(n)%his_alarm,_RC )
!      end if

      if (Ignore(n)) then
         ! "Exersise" the alarms and then do nothing
         Writing(n) = .false.
!         if (ESMF_AlarmIsRinging ( list(n)%his_alarm )) then
!            call ESMF_AlarmRingerOff( list(n)%his_alarm,_RC )
!         end if
         if (ESMF_AlarmIsRinging ( list(n)%seg_alarm )) then
            call ESMF_AlarmRingerOff( list(n)%seg_alarm,_RC )
         end if
      end if

       if (writing(n) .and. .not.IntState%average(n)) then
          ! R8 to R4 copy (if needed!)
          do m=1,list(n)%field_set%nfields
             if (list(n)%r8_to_r4(m)) then
                call MAPL_FieldCopy(from=list(n)%r8(m), &
                                    to=list(n)%r4(m), _RC)
             end if
          end do
       end if

       ! Check for new segment
       !----------------------

       NewSeg(n) = ESMF_AlarmIsRinging ( list(n)%seg_alarm )

       if( NewSeg(n)) then
          call ESMF_AlarmRingerOff( list(n)%seg_alarm,_RC )
       endif

   end do


   if(any(Writing)) call WRITE_PARALLEL("")


  ! swath only
   epoch_swath_grid_case: do n=1,nlist
      call MAPL_TimerOn(GENSTATE,trim(list(n)%collection))
      call MAPL_TimerOn(GENSTATE,"SwathGen")
      if (index(trim(list(n)%output_grid_label), 'SwathGrid') > 0) then
         call Hsampler%regrid_accumulate(list(n)%xsampler,_RC)

         if( ESMF_AlarmIsRinging ( Hsampler%alarm ) ) then
            create_mode = PFIO_NOCLOBBER ! defaut no overwrite
            if (intState%allow_overwrite) create_mode = PFIO_CLOBBER
            ! add time to items
            ! true metadata comes here from mGriddedIO%metadata
            ! the mGriddedIO below only touches metadata, collection_id etc., it is safe.
            !
            if (.NOT. list(n)%xsampler%have_initalized) then
               list(n)%xsampler%have_initalized = .true.
               global_attributes = list(n)%global_atts%define_collection_attributes(_RC)
            endif
            item%itemType = ItemTypeScalar
            item%xname = 'time'
            call list(n)%items%push_back(item)
            call Hsampler%fill_time_in_bundle ('time', list(n)%xsampler%acc_bundle, list(n)%xsampler%output_grid, _RC)
            call list(n)%mGriddedIO%destroy(_RC)
            call list(n)%mGriddedIO%CreateFileMetaData(list(n)%items,list(n)%xsampler%acc_bundle,timeinfo_uninit,vdata=list(n)%vdata,global_attributes=global_attributes,_RC)
            call list(n)%items%pop_back()

            collection_id = o_Clients%add_hist_collection(list(n)%mGriddedIO%metadata, mode = create_mode)
            call list(n)%mGriddedIO%set_param(write_collection_id=collection_id)
         endif
      end if
      call MAPL_TimerOff(GENSTATE,"SwathGen")
      call MAPL_TimerOff(GENSTATE,trim(list(n)%collection))
   end do epoch_swath_grid_case

! Write Id and time
! -----------------

   if (any(writing)) call o_Clients%set_optimal_server(count(writing))

   OPENLOOP: do n=1,nlist
      call MAPL_TimerOn(GENSTATE,trim(list(n)%collection))
      call MAPL_TimerOn(GENSTATE,"IO Create")
      if( Writing(n) ) then

         call get_DateStamp ( clock, DateStamp=DateStamp,  &
              OFFSET = INTSTATE%STAMPOFFSET(n),            &
                                                 _RC )

         if (trim(INTSTATE%expid) == "") then
            fntmpl =          trim(list(n)%filename)
         else
            fntmpl = "%s." // trim(list(n)%filename)
         endif

         if (trim(list(n)%template) /= "") then
            fntmpl = trim(fntmpl) // "." //trim(list(n)%template)
         endif

         read(DateStamp( 1: 8),'(i8.8)') nymd
         read(DateStamp(10:15),'(i6.6)') nhms

         call fill_grads_template ( filename(n), fntmpl, &
              experiment_id=trim(INTSTATE%expid), &
              nymd=nymd, nhms=nhms, _RC ) ! here is where we get the actual filename of file we will write

         if(list(n)%monthly .and. list(n)%partial) then
            filename(n)=trim(filename(n)) // '-partial'
            list(n)%currentFile = filename(n)
         end if

         if( NewSeg(n)) then
            list(n)%partial = .false.
            if (list(n)%monthly) then
               ! get the number of seconds in this month
               ! it's tempting to use the variable "oneMonth" but it does not work
               ! instead we compute the differece between
               ! thisMonth and lastMonth and as a new timeInterval

               call ESMF_ClockGet(clock,currTime=current_time,_RC)
               call ESMF_TimeIntervalSet( oneMonth, MM=1, _RC)
               lastMonth = current_time - oneMonth
               dur = current_time - lastMonth
               call ESMF_TimeIntervalGet(dur, s=sec, _RC)
               call list(n)%mGriddedIO%modifyTimeIncrement(sec, _RC)
            end if
         endif

         lgr => logging%get_logger('HISTORY.sampler')
         if (list(n)%timeseries_output) then
            if( ESMF_AlarmIsRinging ( list(n)%trajectory%alarm ) ) then
               call list(n)%trajectory%create_file_handle(filename(n),_RC)
               list(n)%currentFile = filename(n)
               list(n)%unit = -1
            end if
         elseif (list(n)%sampler_spec == 'station') then
            if (list(n)%unit.eq.0) then
               call lgr%debug('%a %a',&
                    "Station_data output to new file:",trim(filename(n)))
               call list(n)%station_sampler%close_file_handle(_RC)
               call list(n)%station_sampler%create_file_handle(filename(n),_RC)
               list(n)%currentFile = filename(n)
               list(n)%unit = -1
            end if
         elseif (list(n)%sampler_spec == 'mask') then
            if (list(n)%unit.eq.0) then
               call lgr%debug('%a %a',&
                    "Mask_data output to new file:",trim(filename(n)))
               call list(n)%mask_sampler%close_file_handle(_RC)
               call list(n)%mask_sampler%create_file_handle(filename(n),_RC)
               list(n)%currentFile = filename(n)
               list(n)%unit = -1
            end if
         else
            if( list(n)%unit.eq.0 ) then
               if (list(n)%format == 'CFIO') then
                  if (.not.intState%allow_overwrite) then
                     inquire (file=trim(filename(n)),exist=file_exists)
                     _ASSERT(.not.file_exists,trim(filename(n))//" being created for History output already exists")
                  end if
                  if (index(trim(list(n)%output_grid_label), 'SwathGrid') == 0) then
                     call list(n)%mGriddedIO%modifyTime(oClients=o_Clients,_RC)
                  endif
                  list(n)%currentFile = filename(n)
                  list(n)%unit = -1
               else
                  list(n)%unit = GETFILE( trim(filename(n)),all_pes=.true.)
               end if
            end if
         end if

         if(  MAPL_AM_I_ROOT() ) then
              if (index(list(n)%format,'flat') == 0 .and. (.not.list(n)%timeseries_output)) &
              write(6,'(1X,"Writing: ",i6," Slices to File:  ",a)') &
                    list(n)%slices,trim(list(n)%currentFile)
         endif

      end if
!
      call MAPL_TimerOff(GENSTATE,"IO Create")
      call MAPL_TimerOff(GENSTATE,trim(list(n)%collection))
   enddo OPENLOOP


   POSTLOOP: do n=1,nlist
      call MAPL_TimerOn(GENSTATE,trim(list(n)%collection))
      call MAPL_TimerOn(GENSTATE,"IO Post")

      OUTTIME: if( Writing(n) ) then

         if (associated(IntState%Regrid(n)%PTR)) then
            state_out = INTSTATE%REGRID(n)%PTR%state_out

            if (.not. IntState%Regrid(n)%PTR%ontiles) then
               if (IntState%Regrid(n)%PTR%regridType == MAPL_T2G2G) then
                  call RegridTransformT2G2G(IntState%GIM(n), &
                       IntState%Regrid(n)%PTR%xform, &
                       IntState%Regrid(n)%PTR%xformNtv, &
                       state_out, &
                       IntState%Regrid(n)%PTR%LocIn, &
                       IntState%Regrid(n)%PTR%LocOut, &
                       IntState%Regrid(n)%PTR%LocNative, &
                       IntState%Regrid(n)%PTR%ntiles_in, &
                       IntState%Regrid(n)%PTR%ntiles_out,&
                       _RC)
               else
                  call RegridTransform(IntState%GIM(n), &
                       IntState%Regrid(n)%PTR%xform, &
                       state_out, &
                       IntState%Regrid(n)%PTR%LocIn, &
                       IntState%Regrid(n)%PTR%LocOut, &
                       IntState%Regrid(n)%PTR%ntiles_in, &
                       IntState%Regrid(n)%PTR%ntiles_out,&
                       _RC)
               end if
            else
               if (IntState%Regrid(n)%PTR%noxform) then
                  call RegridTransformT2G(STATE_IN=IntState%GIM(n), &
                       STATE_OUT=state_out, &
                       LS_OUT=IntState%Regrid(n)%PTR%LocOut, &
                       NTILES_OUT=IntState%Regrid(n)%PTR%ntiles_out, &
                       _RC)
               else
                  call RegridTransformT2G(STATE_IN=IntState%GIM(n), &
                       XFORM=IntState%Regrid(n)%PTR%xform, &
                       STATE_OUT=state_out, &
                       LS_OUT=IntState%Regrid(n)%PTR%LocOut, &
                       NTILES_OUT=IntState%Regrid(n)%PTR%ntiles_out, &
                       _RC)
               end if
            end if
         else
            state_out = INTSTATE%GIM(n)
         end if

         if (.not.list(n)%timeseries_output .AND. &
              list(n)%sampler_spec /= 'station' .AND. &
              list(n)%sampler_spec /= 'mask') then
            IOTYPE: if (list(n)%unit < 0) then    ! CFIO
               call list(n)%mGriddedIO%bundlepost(list(n)%currentFile,oClients=o_Clients,_RC)
            else

               if( INTSTATE%LCTL(n) ) then
                  call MAPL_GradsCtlWrite ( clock, state_out, list(n), &
                       filename(n), INTSTATE%expid, &
                       list(n)%global_atts%descr, intstate%output_grids,rc )
                  INTSTATE%LCTL(n) = .false.
               endif

               if (list(n)%nbits_to_keep < MAPL_NBITS_UPPER_LIMIT) then
                  final_state = ESMF_StateCreate(_RC)
                  do m=1,list(n)%field_set%nfields
                     call ESMF_StateGet(state_out,trim(list(n)%field_set%fields(3,m)),state_field,_RC)
                     temp_field = MAPL_FieldCreate(state_field,list(n)%field_set%fields(3,m),DoCopy=.true.,_RC)
                     call ESMF_StateAdd(final_state,[temp_field],_RC)
                  enddo
                  call ESMF_AttributeCopy(state_out,final_state,_RC)
                  call shavebits(final_state,list(n),_RC)
               end if

               do m=1,list(n)%field_set%nfields
                  if (list(n)%nbits_to_keep >=MAPL_NBITS_UPPER_LIMIT) then
                     call MAPL_VarWrite ( list(n)%unit, STATE=state_out, &
                        NAME=trim(list(n)%field_set%fields(3,m)), &
                        forceWriteNoRestart=.true., _RC )
                  else
                     call MAPL_VarWrite ( list(n)%unit, STATE=final_state, &
                        NAME=trim(list(n)%field_set%fields(3,m)), &
                        forceWriteNoRestart=.true., _RC )
                  endif
               enddo

               if (list(n)%nbits_to_keep < MAPL_NBITS_UPPER_LIMIT) then
                  do m=1,list(n)%field_set%nfields
                     call ESMF_StateGet(final_state,trim(list(n)%field_set%fields(3,m)),temp_field,_RC)
                     call ESMF_FieldDestroy(temp_field,noGarbage=.true.,_RC)
                  enddo
                  call ESMF_StateDestroy(final_state,noGarbage=.true.,_RC)
               end if
               call WRITE_PARALLEL("Wrote GrADS Output for File: "//trim(filename(n)))

            end if IOTYPE
         end if

         if (list(n)%sampler_spec == 'station') then
            call ESMF_ClockGet(clock,currTime=current_time,_RC)
            call list(n)%station_sampler%append_file(current_time,_RC)
         elseif (list(n)%sampler_spec == 'mask') then
            call ESMF_ClockGet(clock,currTime=current_time,_RC)
            call list(n)%mask_sampler%append_file(current_time,_RC)
         endif

      endif OUTTIME

      if( NewSeg(n) .and. list(n)%unit /= 0 .and. list(n)%duration /= 0 ) then
         if (list(n)%unit > 0 ) then
            call FREE_FILE( list(n)%unit )
         end if
         list(n)%unit = 0
       endif

      call MAPL_TimerOff(GENSTATE,"IO Post")
      call MAPL_TimerOff(GENSTATE,trim(list(n)%collection))
   enddo POSTLOOP


   call MAPL_TimerOn(GENSTATE,"Done Wait")
   if (any(writing)) then
      call o_Clients%done_collective_stage(_RC)
      call o_Clients%post_wait()
   endif
   call MAPL_TimerOff(GENSTATE,"Done Wait")

  ! destroy ogrid/RH/acc_bundle, regenerate them
  ! swath only
   epoch_swath_regen_grid: do n=1,nlist
      call MAPL_TimerOn(GENSTATE,trim(list(n)%collection))
      call MAPL_TimerOn(GENSTATE,"Swath regen")
      if (index(trim(list(n)%output_grid_label), 'SwathGrid') > 0) then
         if( ESMF_AlarmIsRinging ( Hsampler%alarm ) .and. .not. ESMF_AlarmIsRinging(list(n)%end_alarm) ) then

            key_grid_label = list(n)%output_grid_label
            call Hsampler%destroy_rh_regen_ogrid ( key_grid_label, IntState%output_grids, list(n)%xsampler, _RC )

            pgrid => IntState%output_grids%at(trim(list(n)%output_grid_label))
            call list(n)%xsampler%Create_bundle_RH(list(n)%items,list(n)%bundle,Hsampler%tunit, &
                 ogrid=pgrid,vdata=list(n)%vdata,_RC)
            if( MAPL_AM_I_ROOT() )  write(6,'(//)')
         endif
      end if
      call MAPL_TimerOff(GENSTATE,"Swath regen")
      call MAPL_TimerOff(GENSTATE,trim(list(n)%collection))
   end do epoch_swath_regen_grid


   WAITLOOP: do n=1,nlist

      if( Writing(n) .and. list(n)%unit < 0) then
         ! cleanup times
         if (allocated(list(n)%mGriddedIO%times)) deallocate(list(n)%mGriddedIO%times)
      end if

   enddo WAITLOOP

   WRITELOOP: do n=1,nlist

      call MAPL_TimerOn(GENSTATE,trim(list(n)%collection))
      call MAPL_TimerOn(GENSTATE,"Write Timeseries")
      if (list(n)%timeseries_output) then
         call list(n)%trajectory%regrid_accumulate(_RC)
         if( ESMF_AlarmIsRinging ( list(n)%trajectory%alarm ) ) then
            call list(n)%trajectory%append_file(current_time,_RC)
            call list(n)%trajectory%close_file_handle(_RC)
            if ( .not. ESMF_AlarmIsRinging(list(n)%end_alarm) ) then
               call list(n)%trajectory%destroy_rh_regen_LS (_RC)
            end if
         end if
      end if

      if( Writing(n) .and. list(n)%unit < 0) then

         list(n)%unit = -2

      end if

      call MAPL_TimerOff(GENSTATE,"Write Timeseries")
      call MAPL_TimerOff(GENSTATE,trim(list(n)%collection))
   enddo WRITELOOP

   if(any(Writing)) call WRITE_PARALLEL("")

   deallocate(NewSeg)
   deallocate(filename)
   deallocate(Writing)
   deallocate(Ignore)

   _RETURN(ESMF_SUCCESS)
 end subroutine Run

END SUBMODULE
