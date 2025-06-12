
URAMP = -abs(Ramp_)

if    (TX<=TMINTBL) then
   TI = TMINTBL
elseif (TX>=TMAXTBL-.001) then
   TI = TMAXTBL-.001
else
   TI = TX
end if

if ( ( URAMP==TMIX .OR. URAMP==0. ) .and. UTBL) then ! Use table

! Initialize table, if necessary

   if (.not.TableReady) call MAPL_EQsatSET

! Get saturation vapor pressure from table

   TT  = (TI - TMINTBL)*DEGSUBS+1
   IT  = int(TT)

   DDQ = TABLE(IT+1)  - TABLE(IT)
   EX  = (TT-IT)*DDQ  + TABLE(IT)
   if (URAMP==TMIX) then
      DDQ = TABLEX(IT+1) - TABLEX(IT)
      EX  = (TT-IT)*DDQ  + TABLEX(IT)
   endif

! Compute saturation specific humidity

   if (present(PL)) then
      EX  = 1.0
      DDQ = 0.0
      if (PP > EX) then
         DD  = 1.0/(PP - (1.0-ESFAC)*EX)
         EX  = ESFAC*EX*DD
         DDQ = ESFAC*DDQ*PP*(DD*DD)
      end if
   end if

else  ! Use exact formulation

   TI = TX - ZEROC

   if (TI <= URAMP) then
      if (present(PL)) then
         EX = FUNC(TX,PL=PP,DQ=DDQ,OverIce=.true.)
      else
         EX = FUNC(TX,DQ=DDQ,OverIce=.true.)
      endif
   else if (TI >= 0.0) then
      if (present(PL)) then
         EX = FUNC(TX,PL=PP,DQ=DDQ,OverIce=.false.)
      else
         EX = FUNC(TX,DQ=DDQ,OverIce=.false.)
      endif
   else
      if (present(PL)) then
         QQ = FUNC(TX,PL=PP,DQ=DQQ,OverIce=.false.)
         QI = FUNC(TX,PL=PP,DQ=DQI,OverIce=.true.)
         TI = TI/URAMP
         DDQ = TI*(DQI - DQQ) + DQQ
         EX = TI*( QI -  QQ) +  QQ
      else
         QQ = FUNC(TX,DQ=DQQ,OverIce=.false.)
         QI = FUNC(TX,DQ=DQI,OverIce=.true.)
         TI = TI/URAMP
         DDQ = TI*(DQI - DQQ) + DQQ
         EX = TI*( QI -  QQ) +  QQ
      endif
   endif

endif ! not table

if (present(DQ)) DX = DDQ*DEGSUBS
