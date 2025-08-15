module mapl3g_RestartModes

   implicit none(type, external)
   private

   public :: MAPL_RESTART_MODE
   public :: MAPL_RESTART_REQUIRED
   public :: MAPL_RESTART_SKIP

   enum, bind(C)
      enumerator :: MAPL_RESTART_MODE
      enumerator :: MAPL_RESTART_REQUIRED
      enumerator :: MAPL_RESTART_SKIP
   end enum

end module mapl3g_RestartModes
