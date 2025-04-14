module mapl3g_State_API

   use mapl3g_StateGet, only: MAPL_StateGet => StateGet
   use mapl3g_StateGetPointerToData, only: MAPL_StateGetPointer => StateGetPointerToData

   implicit none

   private

   ! Available to users
   public :: MAPL_StateGet
   public :: MAPL_StateGetPointer

   ! Used internally by MAPL
   ! Users shouldn't need these

end module mapl3g_State_API
