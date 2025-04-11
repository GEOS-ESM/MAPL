#include "MAPL_ErrLog.h"

module mapl3g_StateGetPointerToData

   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use esmf

   implicit none
   private

   public :: StateGetPointerToData

   interface StateGetPointerToData
      module procedure StateGetPointerToDataR4_1
      module procedure StateGetPointerToDataR4_2
      module procedure StateGetPointerToDataR4_3
      module procedure StateGetPointerToDataR4_4
      module procedure StateGetPointerToDataR8_1
      module procedure StateGetPointerToDataR8_2
      module procedure StateGetPointerToDataR8_3
      module procedure StateGetPointerToDataR8_4
   end interface StateGetPointerToData

contains

#define NAME_ StateGetPointerToData

#define TYPEKIND_ R4

#define RANK_ 1 ! <NAME_>R4_1
#include "StateGetPointerToDataTemplate.H"
#undef RANK_

#define RANK_ 2 ! <NAME_>R4_2
#include "StateGetPointerToDataTemplate.H"
#undef RANK_

#define RANK_ 3 ! <NAME_>R4_3
#include "StateGetPointerToDataTemplate.H"
#undef RANK_

#define RANK_ 4 ! <NAME_>R4_4
#include "StateGetPointerToDataTemplate.H"
#undef RANK_

#undef TYPEKIND_

#define TYPEKIND_ R8

#define RANK_ 1 ! <NAME_>R8_1
#include "StateGetPointerToDataTemplate.H"
#undef RANK_

#define RANK_ 2 ! <NAME_>R8_2
#include "StateGetPointerToDataTemplate.H"
#undef RANK_

#define RANK_ 3 ! <NAME_>R8_3
#include "StateGetPointerToDataTemplate.H"
#undef RANK_

#define RANK_ 4 ! <NAME_>R8_4
#include "StateGetPointerToDataTemplate.H"
#undef RANK_

#undef TYPEKIND_
#undef NAME_

end module mapl3g_StateGetPointerToData
