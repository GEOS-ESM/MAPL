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
  SUBMODULE (MAPL_HistoryGridCompMod) GradsCtlWrite
!
! !USES:
!
  implicit none

contains


  MODULE SUBROUTINE MAPL_GradsCtlWrite ( clock, state,list,fname,expid,expdsc,output_grids,rc )

   intrinsic :: size
   type(ESMF_Clock),  intent(inout) :: clock
   type(ESMF_State)                 :: state
   type(HistoryCollection)               :: list
   character(len=*)                 :: expid
   character(len=*)                 :: expdsc
   character(len=*)                 :: fname
   type(StringGridMap), intent(in)  :: output_grids
   integer, optional, intent(out)   :: rc

   type(ESMF_Array)               :: array
   type(ESMF_LocalArray)          :: larraylist(1)
   type(ESMF_Field)               :: field
   type(ESMF_Grid)                :: grid
   type(ESMF_Time)                :: CurrTime
   type(ESMF_Time)                :: StopTime
   type(ESMF_Time)                :: StartTime
   type(ESMF_Calendar)            :: cal
   type(ESMF_TimeInterval)        :: ti, Frequency
   integer                        :: nsteps
   integer, dimension(ESMF_MAXDIM):: lbounds, ubounds
   integer, allocatable           :: vdim(:)
   character(len=ESMF_MAXSTR)     :: TimeString
   character(len=ESMF_MAXSTR)     :: filename
   character(len=ESMF_MAXSTR)     :: options
   integer                        :: DIMS(3)
   integer                        :: IM,JM,LM

   character(len=3)               :: months(12)
   data months /'JAN','FEB','MAR','APR','MAY','JUN', &
                'JUL','AUG','SEP','OCT','NOV','DEC'/

   integer      :: unit,nfield
   integer      :: k,m,rank,status
   integer      :: year,month,day,hour,minute
   real(kind=REAL64)   LONBEG,DLON
   real(kind=REAL64)   LATBEG,DLAT
   integer  mass, freq,zero
   real(kind=REAL32),      pointer :: LATS(:,:), LONS(:,:)
   character(len=ESMF_MAXSTR):: gridname
   type(ESMF_Grid), pointer :: pgrid

! Mass-Weighted Diagnostics
! -------------------------
   integer     km
   parameter ( km = 4 )
   character(len=ESMF_MAXSTR) :: name(2,km)
   data name / 'THIM'     , 'PHYSICS'    , &
               'SIT'      , 'PHYSICS'    , &
               'DTDT'     , 'PHYSICS'    , &
               'DTDT'     , 'GWD'        /

   call ESMF_ClockGet ( clock, currTime=CurrTime,   _RC )
   call ESMF_ClockGet ( clock, StopTime=StopTime,   _RC )
   call ESMF_ClockGet ( clock, StartTime=StartTime, _RC )
   call ESMF_ClockGet ( clock, Calendar=cal,        _RC )

   call ESMF_TimeGet  ( CurrTime, timeString=TimeString, _RC )

   read(timestring( 1: 4),'(i4.4)') year
   read(timestring( 6: 7),'(i2.2)') month
   read(timestring( 9:10),'(i2.2)') day
   read(timestring(12:13),'(i2.2)') hour
   read(timestring(15:16),'(i2.2)') minute

   ti = StopTime-CurrTime
   freq = MAPL_nsecf( list%frequency )
   call ESMF_TimeIntervalSet( Frequency, S=freq, StartTime=StartTime, _RC )

   nsteps =  ti/Frequency + 1

   if( trim(expid) == "" ) then
       filename =                       trim(list%collection)
   else
       filename = trim(expid) // '.' // trim(list%collection)
   endif
           unit = GETFILE( trim(filename) // '.ctl', form="formatted" )

   if( list%template == "" .or. list%duration == 0 ) then
       options  = 'options sequential'
       filename = trim(fname)
   else
       options  = 'options sequential template'
       filename = trim(filename) // '.' // trim(list%template)
   endif

! Get Global Horizontal Dimensions
! --------------------------------
   call ESMF_StateGet ( state,trim(list%field_set%fields(3,1)),field,_RC )
   call ESMF_FieldGet ( field, grid=grid, _RC )

   call MAPL_GridGet(GRID, globalCellCountPerDim=DIMS, _RC)

   ZERO   =  0
   IM     =  DIMS(1)
   JM     =  DIMS(2)
   LM     =  DIMS(3)
   if (LM == 0) LM = 1 ! needed for tilegrids

   call ESMF_GridGet(grid, name=gridname, _RC)

   if (gridname(1:10) == 'tile_grid_') then
      DLON = 1.0
      DLAT = 1.0
      LATBEG = 0.0
      LONBEG = 0.0
   else
      if (IM /= 1) then
         DLON   =  360._REAL64/ IM
      else
         DLON = 1.0
      end if

      if (JM /= 1) then
         DLAT   =  180._REAL64/(JM-1)
      else
         DLAT   =  1.0
      end if

      call ESMFL_GridCoordGet(   GRID, LATS       , &
                                 Name     = "Latitude"              , &
                                 Location = ESMF_STAGGERLOC_CENTER  , &
                                 Units    = MAPL_UnitsRadians      , &
                                 _RC)

      call ESMFL_GridCoordGet(   GRID, LONS       , &
                                 Name     = "Longitude"             , &
                                 Location = ESMF_STAGGERLOC_CENTER  , &
                                 Units    = MAPL_UnitsRadians      , &
                                 _RC)

!ALT: Note: the LATS(1,1) and LONS(1,1) are correct ONLY on root
      if( MAPL_AM_I_ROOT() ) then
         LONBEG = LONS(1,1)*(180._REAL64/MAPL_PI_R8)
         if (size(LONS,1) > 1) then
            DLON = (LONS(2,1)-LONS(1,1))*(180._REAL64/MAPL_PI_R8)
         end if

         LATBEG = LATS(1,1)*(180._REAL64/MAPL_PI_R8)
         if (size(LATS,2) > 1) then
            DLAT = (LATS(1,2)-LATS(1,1))*(180._REAL64/MAPL_PI_R8)
         end if
      endif

!
! Check if changing resolution
! -------------------------------------------------------------------------
      block
         integer :: dims(3)
         pgrid => output_grids%at(trim(list%output_grid_label))
         if (associated(pgrid)) then
            call MAPL_GridGet(pgrid,globalCellCountPerDim=dims,_RC)
            IM = dims(1)
            JM = dims(2)
            DLON   =  360._REAL64/IM
            if (JM /= 1) then
               DLAT   =  180._REAL64/(JM-1)
            else
               DLAT   =  1._REAL64
            end if
            LONBEG = -180._REAL64
            LATBEG =  -90._REAL64
         endif
      end block
   end if

! Compute Vertical Dimension for each Field (Augment nfield for VDIMS > LM)
! -------------------------------------------------------------------------
   allocate( vdim(list%field_set%nfields), _STAT )
   vdim = 0
   nfield =   list%field_set%nfields
   do m = 1,list%field_set%nfields
      call ESMFL_StateGetFieldArray( state,trim(list%field_set%fields(3,m)),array,status )
      call ESMF_ArrayGet( array, localarrayList=larrayList, _RC )
      call ESMF_LocalArrayGet( larrayList(1), RANK=rank, totalLBound=lbounds, &
           totalUBound=ubounds, _RC )
      if( rank==3 ) then
         vdim(m) = ubounds(3)-lbounds(3)+1
         if( vdim(m).gt.LM ) nfield = nfield+1
      else if( rank==4 ) then
         vdim(m) = -(ubounds(3)-lbounds(3)+1)*(ubounds(4)-lbounds(4)+1)
      endif
   enddo

! Create Grads Control File
! -------------------------
   if( MAPL_AM_I_ROOT() ) then
      print *
      if ( freq < 3600 ) then
         write(unit,201) trim(filename),trim(expdsc),trim(options), &
              MAPL_UNDEF,IM,LONBEG,DLON, JM,LATBEG,DLAT, LM,  &
              nsteps, &
              hour,minute,day,months(month),year,&
              freq/60, nfield
      else if ( freq < 86400 ) then
         write(unit,202) trim(filename),trim(expdsc),trim(options), &
              MAPL_UNDEF,IM,LONBEG,DLON, JM,LATBEG,DLAT, LM,  &
              nsteps, &
              hour,minute,day,months(month),year,&
              freq/3600, nfield
      else if ( freq < 30*86400 ) then
         write(unit,203) trim(filename),trim(expdsc),trim(options), &
              MAPL_UNDEF,IM,LONBEG,DLON, JM,LATBEG,DLAT, LM,  &
              nsteps, &
              hour,minute,day,months(month),year,&
              freq/86400, nfield
      else
         write(unit,204) trim(filename),trim(expdsc),trim(options), &
              MAPL_UNDEF,IM,LONBEG,DLON, JM,LATBEG,DLAT, LM,  &
              nsteps, &
              hour,minute,day,months(month),year,&
              freq/(30*86400), nfield
      endif
      do m=1,list%field_set%nfields
         mass = 0
         do k=1,km
            if( trim(list%field_set%fields(1,m)).eq.trim(name(1,k))  .and. &
                 trim(list%field_set%fields(2,m)).eq.trim(name(2,k)) ) mass = 1  ! Check for Mass-Weighted Diagnostics
         enddo
         if( vdim(m).le.LM ) then
            write(unit,102) trim(list%field_set%fields(3,m)),abs(vdim(m)),mass,trim(list%field_set%fields(3,m))
         else
            write(unit,102) trim(list%field_set%fields(3,m)),LM     ,mass,trim(list%field_set%fields(3,m))
            if( trim(list%field_set%fields(1,m)).eq.'PLE' ) then
               write(unit,102) 'PS',zero,mass,'PS'
            else
               write(unit,102) trim(list%field_set%fields(3,m)) // 's',zero,mass,trim(list%field_set%fields(3,m)) // 's'
            endif
         endif
      enddo
      write(unit,103)
   endif
   call FREE_FILE( unit )
   deallocate( vdim )

201     format('dset ^',a,/, 'title ',a,/,a,/,             &
               'undef ',e15.6,/,                           &
               'xdef ',i8,' linear ',f8.3,2x,f14.9,/,      &
               'ydef ',i8,' linear ',f8.3,2x,f14.9,/,      &
               'zdef ',i3,' linear  1  1',/,               &
               'tdef ',i5,' linear  ',i2.2,':',i2.2,'z',i2.2,a3,i4.4,3x,i2.2,'mn',/, &
               'vars  ',i3)
202     format('dset ^',a,/, 'title ',a,/,a,/,             &
               'undef ',e15.6,/,                           &
               'xdef ',i8,' linear ',f8.3,2x,f14.9,/,      &
               'ydef ',i8,' linear ',f8.3,2x,f14.9,/,      &
               'zdef ',i3,' linear  1  1',/,               &
               'tdef ',i5,' linear  ',i2.2,':',i2.2,'z',i2.2,a3,i4.4,3x,i2.2,'hr',/, &
               'vars  ',i3)
203     format('dset ^',a,/, 'title ',a,/,a,/,             &
               'undef ',e15.6,/,                           &
               'xdef ',i8,' linear ',f8.3,2x,f14.9,/,      &
               'ydef ',i8,' linear ',f8.3,2x,f14.9,/,      &
               'zdef ',i3,' linear  1  1',/,               &
               'tdef ',i5,' linear  ',i2.2,':',i2.2,'z',i2.2,a3,i4.4,3x,i2.2,'dy',/, &
               'vars  ',i3)
204     format('dset ^',a,/, 'title ',a,/,a,/,             &
               'undef ',e15.6,/,                           &
               'xdef ',i8,' linear ',f8.3,2x,f14.9,/,      &
               'ydef ',i8,' linear ',f8.3,2x,f14.9,/,      &
               'zdef ',i3,' linear  1  1',/,               &
               'tdef ',i5,' linear  ',i2.2,':',i2.2,'z',i2.2,a3,i4.4,3x,i2.2,'mo',/, &
               'vars  ',i3)
102     format(a,i3,2x,i3,2x,"'",a,"'")
103     format('endvars')

   _RETURN(ESMF_SUCCESS)
 end subroutine MAPL_GradsCtlWrite

END SUBMODULE
