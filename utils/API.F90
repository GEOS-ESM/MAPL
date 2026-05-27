module mapl_Utils_API_mod
   ! Utils layer public API
   ! Part of MAPL3 API cleanup (#4999, part of #4975/#4969)
   
   ! String utilities - only export entities that are actually used
   use mapl_String_mod, only: String
   use mapl_StringUtilities_mod, only: split, lowercase, uppercase
   
   ! FileSystemUtilities - all functions unused in ported repos, don't export
   ! use mapl_FileSystemUtilities_mod - intentionally commented out
   
   ! DSO_Utilities - all functions unused in ported repos, don't export  
   ! use mapl_DSO_Utilities_mod - intentionally commented out
   
   ! DirPath - unused in ported repos, don't export
   ! use mapl_DirPath_mod - intentionally commented out
   
   ! OS utilities - all already have mapl_ prefix, export all
   use mapl_os_mod
   
   implicit none
   private
   
   ! Re-export selected entities
   public :: String
   public :: split
   public :: lowercase
   public :: uppercase
   
   ! Re-export all mapl_os_mod entities (already properly prefixed)
   public :: mapl_ChangeDirectory
   public :: mapl_ClearDirectoryStack
   public :: mapl_DirectoryExists
   public :: mapl_GetCurrentWorkingDirectory
   public :: mapl_MakeDirectory
   public :: mapl_MakeSymbolicLink
   public :: mapl_PathJoin
   public :: mapl_PopDirectory
   public :: mapl_PushDirectory
   public :: mapl_RemoveDirectory
   public :: mapl_RemoveFile

end module mapl_Utils_API_mod
