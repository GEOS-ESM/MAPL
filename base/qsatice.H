#define TABLE   ESTBLE
#define FILE_    "esatice.H"
#define PHASE   ICE

if(TX>=TMAX(ICE)) then

#if KIND_==4
#define FUNC Qsat0
#else
#define FUNC Qsatd0
#endif
   
   if(present(PL)) then	
      if(present(DQ)) then	
         EX = FUNC(TX,PL=PX,DQ=DX,Pascals=.true.)
      else
         EX = FUNC(TX,PL=PX,Pascals=.true.)
      endif
   else
      if(present(DQ)) then	
         EX = FUNC(TX,DQ=DX)
      else
         EX = FUNC(TX)
      endif
   endif

#undef FUNC

else

#include "eqsat.H"

endif

#undef TABLE
#undef FILE_
#undef PHASE
