#include "MAPL_Generic.h"


module mapl3g_StateGetPointer
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use esmf
   implicit none(type,external)
   private

   public :: StateGetPointer

   interface StateGetPointer
      procedure state_get_fArrayPtr_r4_1d
      procedure state_get_fArrayPtr_r4_2d
      procedure state_get_fArrayPtr_r4_3d
      procedure state_get_fArrayPtr_r4_4d
      procedure state_get_fArrayPtr_r8_1d
      procedure state_get_fArrayPtr_r8_2d
      procedure state_get_fArrayPtr_r8_3d
      procedure state_get_fArrayPtr_r8_4d
   end interface StateGetPointer

contains

#ifdef NAME_
#  undef NAME_
#endif 

#define NAME_ state_get_fArrayPtr

#ifdef TYPEKIND_
#  undef TYPEKIND_
#endif 

#define TYPEKIND_ R4

   
! StateGetPointerToDataR4_1
#define RANK_ 1
#include "get_fArrayPtr_template.H"
#undef RANK_

! StateGetPointerToDataR4_2
#define RANK_ 2
#include "get_fArrayPtr_template.H"
#undef RANK_

! StateGetPointerToDataR4_3
#define RANK_ 3
#include "get_fArrayPtr_template.H"
#undef RANK_

! StateGetPointerToDataR4_4
#define RANK_ 4
#include "get_fArrayPtr_template.H"
#undef RANK_

#undef TYPEKIND_

#define TYPEKIND_ R8

! StateGetPointerToDataR8_1
#define RANK_ 1
#include "get_fArrayPtr_template.H"
#undef RANK_

! StateGetPointerToDataR8_2
#define RANK_ 2
#include "get_fArrayPtr_template.H"
#undef RANK_

! StateGetPointerToDataR8_3
#define RANK_ 3
#include "get_fArrayPtr_template.H"
#undef RANK_

! StateGetPointerToDataR8_4
#define RANK_ 4
#include "get_fArrayPtr_template.H"
#undef RANK_

#undef TYPEKIND_

#undef NAME_

end module mapl3g_StateGetPointer
