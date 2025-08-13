module mapl3g_RestartModes

   implicit none(type, external)
   private

   public :: MAPL_RESTART_MODE
   public :: MAPL_RESTART_REQUIRED
   public :: MAPL_RESTART_SKIP
   public :: MAPL_RESTART_SKIP_INITIAL
   public :: MAPL_RESTART_BOOTSTRAP

   enum, bind(C)
      enumerator :: MAPL_RESTART_MODE
      enumerator :: MAPL_RESTART_REQUIRED
      enumerator :: MAPL_RESTART_SKIP
      enumerator :: MAPL_RESTART_SKIP_INITIAL
      enumerator :: MAPL_RESTART_BOOTSTRAP
   end enum

end module mapl3g_RestartModes
