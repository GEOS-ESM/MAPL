module MAPL_StringUtilsMod

   implicit none
   private

   public :: get_file_extension
   public :: get_file_basename
   public :: get_system_dso_suffix
   public :: validate_dso_suffix

contains

   pure integer function find_extension_index(filename) result(dot_index)
      character(len=*), intent(in) :: filename
      dot_index = scan(trim(filename),'.', back=.true.)
   end function find_extension_index

   pure function get_file_extension(filename) result(extension)
      ! Function that returns the extension of a string filename
      ! where filename is "basename.extension"
      character(len=*), intent(in) :: filename
      character(len=:), allocatable :: extension
      integer :: dot_index

      dot_index = find_extension_index(filename)
      ! If the filename has no extension, return blank string
      if (dot_index > 0) then
         extension = filename(dot_index:)
      else
         extension = ''
      endif
   end function get_file_extension

   pure function get_file_basename(filename) result(basename)
      ! Function that returns the basename of a string filename
      ! where filename is "basename.extension"
      character(len=*), intent(in) :: filename
      character(len=:), allocatable :: basename
      integer :: dot_index

      dot_index = find_extension_index(filename)
      ! If the filename has no extension, return the filename
      if (dot_index > 0) then
         basename = filename(1:dot_index-1)
      else
         basename = filename
      end if
   end function get_file_basename

   pure function get_system_dso_suffix(remove_dot) result(suffix)
      ! Function that returns the system DSO suffix. Note that this
      ! suffix is set via preprocessor macro as detected by CMake
      !
      ! By default the suffix returned by CMake has the dot. This
      ! function has an optional argument to remove the dot. By
      ! default, this is set to false
      logical, intent(in), optional :: remove_dot
      character(len=:), allocatable :: suffix
      logical :: remove_dot_
      integer :: dot_index
      character(len=:), allocatable :: detected_dso_suffix

      if (present(remove_dot)) then
         remove_dot_ = remove_dot
      else
         remove_dot_ = .FALSE.
      end if
      ! NOTE: SYSTEM_DSO_SUFFIX is a preprocessor macro set by CMake
      detected_dso_suffix = SYSTEM_DSO_SUFFIX
      if (remove_dot_) then
         dot_index = find_extension_index(detected_dso_suffix)
         suffix = detected_dso_suffix(dot_index+1:)
      else
         suffix = detected_dso_suffix
      end if
   end function get_system_dso_suffix

   pure logical function validate_dso_suffix(suffix)
      ! We will only accept valid dso suffixes
      character(len=*), intent(in) :: suffix
      character(len=6), dimension(3), parameter :: allowed_dso = [character(len=6) :: '.so','.dylib','.dll']
      integer :: i
      logical :: valid_dso_suffix

      valid_dso_suffix = .false.
      do i = 1, size(allowed_dso)
         if ( trim(suffix) == trim(allowed_dso(i)) ) then
            valid_dso_suffix = .true.
            exit
         end if
      end do
      validate_dso_suffix = valid_dso_suffix
   end function validate_dso_suffix

end module MAPL_StringUtilsMod
