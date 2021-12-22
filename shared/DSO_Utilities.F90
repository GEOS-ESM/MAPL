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
      is_valid_dso_name = is_valid_dso_extension(get_file_extension(name))
   end function is_valid_dso_name

   ! An empty extension is valid, as we can supply the system-specific one.
   pure logical function is_valid_dso_extension(extension)
      character(len=*), intent(in) :: extension
      is_valid_dso_extension = (extension == '' .or. extension == SYSTEM_DSO_EXTENSION)
   end function is_valid_dso_extension

   ! We allow users to specify a DSO extensions that is only valid on
   ! some other OS.  This allows things to work on say OSX if the user
   ! puts a Linux DSO in a resource file.
   pure logical function is_supported_dso_name(name)
      character(len=*), intent(in) :: name
      is_supported_dso_name = is_supported_dso_extension(get_file_extension(get_file_extension(name)))
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

      adjust_dso_name = get_file_basename(guess) // SYSTEM_DSO_EXTENSION

  end function adjust_dso_name

end module mapl_DSO_Utilities
