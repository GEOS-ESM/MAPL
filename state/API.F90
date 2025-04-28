module mapl3g_State_API
   use mapl3g_StateGet, only: MAPL_StateGet => StateGet
   use mapl3g_StateGetPointer, only: MAPL_StateGetPointer => StateGetPointer
   implicit none
   private

   ! Available to users
   public :: MAPL_StateGet
   public :: MAPL_StateGetPointer

end module mapl3g_State_API
