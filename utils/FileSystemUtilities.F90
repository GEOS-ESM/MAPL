module mapl_FileSystemUtilities
   implicit none
   private

   public :: get_file_extension
   public :: get_file_basename

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

      dot_index = find_extension_index(trim(filename))
      ! If the filename has no extension, return blank string
      if (dot_index > 0) then
         extension = trim(filename(dot_index:))
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

      dot_index = find_extension_index(trim(filename))
      ! If the filename has no extension, return the filename
      if (dot_index > 0) then
         basename = trim(filename(1:dot_index-1))
      else
         basename = trim(filename)
      end if
   end function get_file_basename

end module Mapl_FileSystemUtilities
