module MAPL_DirPathMod
   use MAPL_KeywordEnforcerMod
   use MAPL_StringVectorMod
   private

   public :: DirPath
   public :: dirpaths
   public :: MAPL_SUCCESS
   public :: MAPL_FILE_NOT_FOUND

   type, extends(StringVector) :: DirPath
      private
   contains
      procedure :: find
      procedure :: append
   end type DirPath

   type(DirPath) :: dirpaths

   integer, parameter :: MAPL_SUCCESS = 0
   integer, parameter :: MAPL_FILE_NOT_FOUND = 1

contains

   function find(this, file, unusable, rc) result(full_name)
      character(len=:), allocatable :: full_name
      class (DirPath), intent(in) :: this
      character(len=*), intent(in) :: file
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type (StringVectorIterator) :: iter
      character(len=:), pointer :: dir
      logical :: exist

      iter = this%begin()
      do while (iter /= this%end())
         dir => iter%get()
         full_name = trim(dir) // '/' // file
         inquire(file=full_name, exist=exist)
         if (exist) then
            if (present(rc)) then
               rc = MAPL_SUCCESS
            end if
            return
         end if
         call iter%next()
      end do

      full_name = ''
      if (present(rc)) then
         rc = MAPL_FILE_NOT_FOUND
      end if
      
      
   end function find


   subroutine append(this, directory, unusable, rc)
      class (DirPath), intent(inout) :: this
      character(len=*), intent(in) :: directory
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      call this%push_back(directory)

      if (present(rc)) then
         rc = MAPL_SUCCESS
      end if
      
   end subroutine append

end module MAPL_DirPathMod
