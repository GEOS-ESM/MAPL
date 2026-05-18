#include "MAPL.h"

! This module is a (poor) analog to the Python "os" package.

module mapl_os
   use gftl2_StringStack
   use mapl_ErrorHandling
   use, intrinsic :: iso_c_binding
   implicit none(type,external)
   private

   public :: mapl_GetCurrentWorkingDirectory
   public :: mapl_ChangeDirectory
   public :: mapl_MakeDirectory
   public :: mapl_DirectoryExists
   public :: mapl_RemoveDirectory
   public :: mapl_RemoveFile
   public :: mapl_PushDirectory
   public :: mapl_PopDirectory
   public :: mapl_ClearDirectoryStack
   public :: mapl_PathJoin
   public :: mapl_MakeSymbolicLink

   interface mapl_GetCurrentWorkingDirectory
      procedure :: get_current_working_directory
   end interface mapl_GetCurrentWorkingDirectory

   interface mapl_ChangeDirectory
      procedure :: change_directory
   end interface mapl_ChangeDirectory

   interface mapl_MakeDirectory
      procedure :: make_directory
   end interface mapl_MakeDirectory

   interface mapl_RemoveDirectory
      procedure :: remove_directory
   end interface mapl_RemoveDirectory

   interface mapl_RemoveFile
      procedure :: remove_file
   end interface mapl_RemoveFile

   interface mapl_DirectoryExists
      procedure directory_exists
   end interface mapl_DirectoryExists

   interface mapl_Pushdirectory
      procedure :: push_directory_default
      procedure :: push_directory
   end interface mapl_Pushdirectory

   interface mapl_PopDirectory
      procedure :: pop_directory
   end interface mapl_PopDirectory

   interface mapl_ClearDirectoryStack
      procedure :: clear_directory_stack
   end interface mapl_ClearDirectoryStack

   interface mapl_PathJoin
      module procedure :: path_join
   end interface mapl_PathJoin

   interface mapl_MakeSymbolicLink
      procedure :: make_symbolic_link
   end interface mapl_MakeSymbolicLink

   type(StringStack), protected :: directory_stack

contains


   function get_current_working_directory(rc) result(cwd)
      character(len=:), allocatable :: cwd
      integer, optional, intent(out) :: rc

      integer(kind=c_size_t), parameter :: MAX_BUFFER_SIZE = 65536
      character(kind=c_char), pointer :: c_string(:)
      type(c_ptr) :: c_ptr_result
      integer(c_size_t) :: buffer_size
      integer :: i, actual_length

      interface
         ! C interface for getcwd
         function c_getcwd(buf, size) bind(c, name="getcwd") result(ptr)
            use iso_c_binding
            type(c_ptr), value :: buf
            integer(c_size_t), value :: size
            type(c_ptr) :: ptr
         end function c_getcwd
      end interface

      allocate(character(len=0) :: cwd) ! just in case

      ! Start with a reasonable buffer size
      buffer_size = 128_c_size_t

      do
         ! Allocate C-compatible buffer
         allocate(character(kind=c_char) :: c_string(buffer_size))

         ! Call getcwd
         c_ptr_result = c_getcwd(c_loc(c_string(1)), buffer_size)

         if (c_associated(c_ptr_result)) exit

         ! Failed - buffer might be too small
         deallocate(c_string)
         buffer_size = buffer_size * 2

         ! Prevent infinite loop with reasonable maximum
         _ASSERT(buffer_size <= MAX_BUFFER_SIZE, 'Buffer size exceeded maximum limit.')
      end do

      ! Success - find actual string length
      actual_length = 0
      do i = 1, int(buffer_size)
         if (c_string(i) == c_null_char) exit
         actual_length = actual_length + 1
      end do

      ! Allocate result string with exact length needed
      deallocate(cwd)
      allocate(character(len=actual_length) :: cwd)

      ! Copy the string
      do i = 1, actual_length
         cwd(i:i) = c_string(i)
      end do

      deallocate(c_string)

      _RETURN(_SUCCESS)
   end function get_current_working_directory


   subroutine change_directory(path, rc)
      use iso_c_binding
      implicit none
      character(len=*), intent(in) :: path
      integer, optional, intent(out) :: rc

      integer :: status
      interface
         function c_chdir(path) bind(C, name="chdir") result(stat)
            use iso_c_binding
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int) :: stat
         end function c_chdir
      end interface

      status = c_chdir(trim(path) // c_null_char)
      _ASSERT(status == 0, 'Error changing directory to: ' // trim(path))

      _RETURN(_SUCCESS)
   end subroutine change_directory

   subroutine make_directory(path, force, rc)
      character(len=*), intent(in) :: path
      logical, optional, intent(in) :: force
      integer, optional, intent(out) :: rc

      ! Use execute_command_line (Fortran 2008)
      character(:), allocatable :: command
      integer :: status

      command = 'mkdir '
      if (present(force)) then
         if (force) command = command // '-p '
      end if
      command = command // trim(path)

      call execute_command_line(command, exitstat=status)

      _ASSERT(status==0, 'Error creating directory: ' // trim(path))
      _RETURN(_SUCCESS)
   end subroutine make_directory

   subroutine remove_directory(path, rc)
      character(len=*), intent(in) :: path
      integer, optional, intent(out) :: rc

      character(:), allocatable :: command
      integer :: status

      command = 'rmdir ' // trim(path)

      call execute_command_line(command, exitstat=status)

      _ASSERT(status==0, 'Error deleting directory: ' // trim(path))
      _RETURN(_SUCCESS)
   end subroutine remove_directory

   subroutine remove_file(path, force, rc)
      character(len=*), intent(in) :: path
      logical, optional, intent(in) :: force
      integer, optional, intent(out) :: rc

      character(:), allocatable :: command
      integer :: status

      command = 'rm '
      if (present(force)) then
         if (force) command = command // '-f '
      end if
      command = command // trim(path)

      call execute_command_line(command, exitstat=status)

      _ASSERT(status==0, 'Error deleting file: ' // trim(path))
      _RETURN(_SUCCESS)
   end subroutine remove_file

   logical function directory_exists(path, rc)
      character(len=*), intent(in) :: path
      integer, optional, intent(out) :: rc

#ifndef __INTEL_COMPILER
      inquire(file=path, exist=directory_exists)
#else
      inquire(directory=path, exist=directory_exists)
#endif

      _RETURN(_SUCCESS)
   end function directory_exists

   subroutine push_directory_default(rc)
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: path, cwd

      path = directory_stack%top()
      call directory_stack%pop()

      cwd = get_current_working_directory(_RC)
      call directory_stack%push(cwd)

      call change_directory(path, _RC)

      _RETURN(_SUCCESS)
   end subroutine push_directory_default

   subroutine push_directory(path, rc)
      character(*), intent(in) :: path
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: current_dir
      character(:), allocatable :: full_path

      current_dir = get_current_working_directory(_RC)
      call directory_stack%push(current_dir)
      if (directory_stack%size() == 0) then
         ! Initialize the stack with the current directory
      end if

      full_path = path
      if (path(1:1) /= '/') then
         full_path = path_join(current_dir, trim(path))
      end if

      call change_directory(full_path, _RC)
        
      _RETURN(_SUCCESS)
   end subroutine push_directory

   function pop_directory(rc) result(new_path)
      character(:), allocatable :: new_path
      integer, optional, intent(out) :: rc

      integer :: status

      new_path = '' ! need to always allocate something
      _ASSERT(directory_stack%size() > 0, 'No directory to pop')
      new_path = directory_stack%top()
      call directory_stack%pop()
      call change_directory(new_path, _RC)
      _RETURN(_SUCCESS)
   end function pop_directory

   subroutine clear_directory_stack()

      do while (directory_stack%size() > 0)
         call directory_stack%pop()
      end do
   end subroutine clear_directory_stack

   function path_join(path1, path2) result(joined_path)
      character(:), allocatable :: joined_path
      character(*), intent(in) :: path1, path2

      ! Join two paths with a single slash
      if (len_trim(path1) > 0 .and. len_trim(path2) > 0) then
         joined_path = trim(path1) // '/' // trim(path2)
      else if (len_trim(path1) > 0) then
         joined_path = trim(path1)
      else if (len_trim(path2) > 0) then
         joined_path = trim(path2)
      else
         joined_path = ''
      end if

   end function path_join


   subroutine make_symbolic_link(src_path, link_path, rc)
      character(*), intent(in) :: src_path
      character(*), intent(in) :: link_path
      integer, optional, intent(out) :: rc

      interface
         ! C interface for symlink system call
         function c_symlink(target, linkpath) bind(c, name="symlink") result(status)
            use iso_c_binding
            character(kind=c_char), intent(in) :: target(*)
            character(kind=c_char), intent(in) :: linkpath(*)
            integer(c_int) :: status
         end function c_symlink
      end interface

      integer :: status
      
      status = c_symlink(src_path // c_null_char, link_path // c_null_char)

      _ASSERT(status == 0, 'Error creating symbolic link from: ' // trim(src_path) // ' to: ' // trim(link_path))

      _RETURN(_SUCCESS)
   end subroutine make_symbolic_link

end module mapl_os
