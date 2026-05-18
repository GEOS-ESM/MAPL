module mapl_DSO_Utilities
   use mapl_FileSystemUtilities
   implicit none

   public :: is_valid_dso_name
   public :: is_valid_dso_extension
   public :: is_supported_dso_name
   public :: is_supported_dso_extension
   public :: adjust_dso_name

   public :: SYSTEM_DSO_EXTENSION

   ! NOTE: SYSTEM_DSO_SUFFIX is a preprocessor macro set by CMake
   character(*), parameter :: SYSTEM_DSO_EXTENSION = SYSTEM_DSO_SUFFIX

contains

   pure logical function is_valid_dso_name(name)
      character(*), intent(in) :: name
      is_valid_dso_name = is_valid_dso_extension(get_dso_extension(name))
   end function is_valid_dso_name

   ! An empty extension is valid, as we can supply the system-specific one.
   pure logical function is_valid_dso_extension(extension)
      character(len=*), intent(in) :: extension
      is_valid_dso_extension = (extension == '' .or. extension == SYSTEM_DSO_EXTENSION)
   end function is_valid_dso_extension

   ! We allow users to specify a DSO extensions that is only valid on
   ! some other OS.  This allows things to work on say OSX if the user
   ! puts a Linux DSO in a resource file.
   !
   ! A name is supported if its DSO extension (if any) is a known DSO extension.
   ! Names with no DSO extension (including dotted names like
   ! libMAPL.componentDriverGridComp where the dot-segments are part of the
   ! library name, not a file extension) are treated as bare names and accepted.
   ! NOTE: names ending in non-DSO extensions like ".a" cannot be reliably
   ! distinguished from dotted library names, so they are also accepted here.
   ! Callers should not pass static-library names as DSO names.
   pure logical function is_supported_dso_name(name)
      character(len=*), intent(in) :: name
      is_supported_dso_name = is_supported_dso_extension(get_dso_extension(name))
   end function is_supported_dso_name

   ! We allow users to specify a DSO extensions that is only valid on
   ! some other OS.  This allows things to work on say OSX if the user
   ! puts a Linux DSO in a resource file.
   pure logical function is_supported_dso_extension(extension)
      character(len=*), intent(in) :: extension
      character(len=6), dimension(*), parameter :: SUPPORTED_DSO_EXTENSIONS = [character(len=6) :: '.so','.dylib','.dll', '']
      is_supported_dso_extension = any(extension == SUPPORTED_DSO_EXTENSIONS)
   end function is_supported_dso_extension

   ! We allow users to specify DSO file names with or without the
   ! suffix.  This function creates the full name appropriate to a
   ! given system.
   pure function adjust_dso_name(guess)
      character(:), allocatable :: adjust_dso_name
      character(*), intent(in) :: guess

      adjust_dso_name = get_dso_basename(guess) // SYSTEM_DSO_EXTENSION

  end function adjust_dso_name

   ! Returns the basename of a DSO name, stripping only a known DSO extension
   ! (.so, .dylib, .dll).  Unlike get_file_basename, this does NOT strip on
   ! any dot, so names like libMAPL.componentDriverGridComp are returned intact.
   pure function get_dso_basename(name) result(basename)
      character(len=*), intent(in) :: name
      character(len=:), allocatable :: basename
      character(len=:), allocatable :: ext

      ext = get_dso_extension(name)
      if (ext /= '') then
         basename = trim(name(1:len_trim(name)-len(ext)))
      else
         basename = trim(name)
      end if
   end function get_dso_basename

   ! Returns the DSO extension of a name (.so, .dylib, .dll), or '' if the
   ! last dot-segment is not a known DSO extension.  This prevents names like
   ! libMAPL.componentDriverGridComp from having their component treated as an
   ! extension.
   pure function get_dso_extension(name) result(extension)
      character(len=*), intent(in) :: name
      character(len=:), allocatable :: extension
      character(len=:), allocatable :: candidate

      candidate = get_file_extension(name)
      if (is_supported_dso_extension(candidate)) then
         extension = candidate
      else
         extension = ''
      end if
   end function get_dso_extension

end module mapl_DSO_Utilities
