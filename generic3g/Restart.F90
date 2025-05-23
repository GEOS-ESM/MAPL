module mapl3g_restart

   implicit none(type, external)

   private
   public :: MAPL_RESTART
   public :: MAPL_RESTART_OPTIONAL
   public :: MAPL_RESTART_SKIP
   public :: MAPL_RESTART_REQUIRED
   public :: MAPL_RESTART_BOOT
   public :: MAPL_RESTART_SKIP_INITIAL

   enum, bind(C)
      enumerator :: MAPL_RESTART
      enumerator :: MAPL_RESTART_OPTIONAL
      enumerator :: MAPL_RESTART_SKIP
      enumerator :: MAPL_RESTART_REQUIRED
      enumerator :: MAPL_RESTART_BOOT
      enumerator :: MAPL_RESTART_SKIP_INITIAL
   end enum

end module mapl3g_restart
