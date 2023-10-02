#ifdef SET_RC
#   undef SET_RC
#endif
#define SET_RC(S,R) if(present(R)) R=S

program overwrite_config

   use ESMF
!   use ESMFL_Mod

   implicit none

!   interface overwrite_main
!      procedure :: file_main
!      procedure :: array_main
!   end interface overwrite_main
!
!   interface load_config
!      procedure :: load_config_array
!      procedure :: load_config_file
!   end interface load_config

   integer, parameter :: MAX_LENGTH = 80
   integer, parameter :: ILABEL = 1
   integer, parameter :: IVALUE = 2
   integer, parameter :: PAIR_SIZE = 2
   integer, parameter :: NCOL = 3
   integer, parameter :: SUCCESS = ESMF_SUCCESS
   integer, parameter :: FAILURE = SUCCESS - 1
   integer, parameter :: ROW = 1
   integer, parameter :: COLUMN = 2

   integer :: stat
!   character(len=MAX_LENGTH) :: config_filename
   character(len=MAX_LENGTH) :: attr(PAIR_SIZE, NCOL)

   attr(ILABEL, 1) = 'datetime'
   attr(IVALUE, 1) ='19991231T235959'

   attr(ILABEL, 2) = 'is_set'
   attr(IVALUE, 2) = '.T.'

   attr(ILABEL, 3) = 'size'
   attr(IVALUE, 3) = '3'

!   config_filename = 'overwrite.rc'

   call array_main(attr, rc=stat)
   if(stat /= SUCCESS) stop 'Failed'

contains

!   subroutine file_main(filename, rc)
!      character(len=*), intent(in) :: filename
!      integer, optional, intent(out) :: rc
!      type(ESMF_Config) :: config
!      integer :: stat
!
!      config = ESMF_ConfigCreate(rc=stat)
!
!      if(stat /= SUCCESS) then
!         SET_RC(stat, rc)
!         return
!      end if
!
!      call load_config(config, filename, rc=stat)
!
!      if(stat /= SUCCESS) then
!         SET_RC(stat, rc)
!         return
!      end if
!
!   end subroutine file_main

   subroutine array_main(attr_array, rc)
      character(len=*), intent(in) :: attr_array(:,:)
      integer, optional, intent(out) :: rc
      type(ESMF_Config) :: config
      integer :: stat

      if(size(attr_array, ROW) /= PAIR_SIZE) then
         SET_RC(FAILURE, rc)
         return
      end if

      config = ESMF_ConfigCreate(rc=stat)

      if(stat /= SUCCESS) then
         SET_RC(stat, rc)
         return
      end if

      call load_config_array(config, attr_array, rc=stat)
      if(stat /= SUCCESS) then
         SET_RC(stat, rc)
         return
      end if

   end subroutine array_main

   subroutine load_config_array(config, attr, rc)
      type(ESMF_Config), intent(inout) :: config
      character(len=*), intent(in) :: attr(:,:)
      integer, optional, intent(out) :: rc
      integer :: stat
      integer :: i

      if(.not. config_created(config, stat, rc)) then
         SET_RC(stat, rc)
         return
      end if

      do i = 1, size(attr)
         call ESMF_ConfigSetAttribute(config, value=attr(IVALUE, i), label=attr(ILABEL, i), rc=stat)
         if(stat /= SUCCESS) then
            SET_RC(stat, rc)
            return
         end if
      end do

   end subroutine load_config_array

!   subroutine load_config_file(config, filename, rc)
!      type(ESMF_Config), intent(inout) :: config
!      character(len=*), intent(in) :: filename
!      integer, optional, intent(out) :: rc
!      integer :: stat
!
!      if(.not. config_created(config, stat, rc=stat)) then
!         SET_RC(stat, rc)
!         return
!      end if
!
!      call ESMF_ConfigLoad(config, filename, rc=stat)
!      if(stat /= SUCCESS) then
!         SET_RC(stat, rc)
!         return
!      end if
!
!   end subroutine load_config_file

   logical function config_created(config, stat, rc) 
      type(ESMF_Config), intent(inout) :: config
      integer :: stat
      integer, optional, intent(out) :: rc
      logical :: config_is_created

      config_is_created = ESMF_ConfigIsCreated(config, rc=stat)
      config_is_created = .not. (config_is_created .and. (stat == SUCCESS))
      if(.not. config_is_created) then
         SET_RC(stat, rc)
      end if
      
      config_created = config_is_created

   end function config_created

end program overwrite_config

!module overwrite_config_mod
!
!   use ESMF
!
!   implicit none
!
!   public:: MAX_LENGTH
!   public:: ILABEL
!   public:: IVALUE
!   public:: PAIR_SIZE
!   public:: NCOL
!   public:: SUCCESS
!   public:: FAILURE
!   public:: ROW
!   public:: COLUMN
!
!   integer, parameter :: MAX_LENGTH = 80
!   integer, parameter :: ILABEL = 1
!   integer, parameter :: IVALUE = 2
!   integer, parameter :: PAIR_SIZE = 2
!   integer, parameter :: NCOL = 3
!   integer, parameter :: SUCCESS = ESMF_SUCCESS
!   integer, parameter :: FAILURE = SUCCESS - 1
!   integer, parameter :: ROW = 1
!   integer, parameter :: COLUMN = 2
!
!   private
!
!   public :: overwrite_main
!
!   interface overwrite_main
!      module procedure :: file_main
!      module procedure :: array_main
!   end interface overwrite_main
!
!   interface load_config
!      module procedure :: load_config_array
!      module procedure :: load_config_file
!   end interface load_config
!
!contains
!
!   subroutine file_main(filename, rc)
!      character(len=*), intent(in) :: filename
!      integer, optional, intent(out) :: rc
!      type(ESMF_Config) :: config
!      integer :: stat
!
!      config = ESMF_ConfigCreate(rc=stat)
!
!      if(stat /= SUCCESS) then
!         SET_RC(stat, rc)
!         return
!      end if
!
!      call load_config(config, filename, rc=stat)
!
!      if(stat /= SUCCESS) then
!         SET_RC(stat, rc)
!         return
!      end if
!
!   end subroutine file_main
!
!   subroutine array_main(attr_array, rc)
!      character(len=*), intent(in) :: attr_array(:,:)
!      integer, optional, intent(out) :: rc
!      type(ESMF_Config) :: config
!      integer :: stat
!
!      if(size(attr_array, ROW) /= PAIR_SIZE) then
!         SET_RC(FAILURE, rc)
!         return
!      end if
!
!      config = ESMF_ConfigCreate(rc=stat)
!
!      if(stat /= SUCCESS) then
!         SET_RC(stat, rc)
!         return
!      end if
!
!      call load_config(config, attr_array, rc=stat)
!      if(stat /= SUCCESS) then
!         SET_RC(stat, rc)
!         return
!      end if
!
!   end subroutine array_main
!
!   subroutine load_config_array(config, attr, rc)
!      type(ESMF_Config), intent(inout) :: config
!      character(len=*), intent(in) :: attr(:,:)
!      integer, optional, intent(out) :: rc
!      integer :: stat
!      integer :: i
!
!      if(.not. config_created(config, stat, rc)) then
!         SET_RC(stat, rc)
!         return
!      end if
!
!      do i = 1, size(attr)
!         call ESMF_ConfigSetAttribute(config, value=attr(IVALUE, i), label=attr(ILABEL, i), rc=stat)
!         if(stat /= SUCCESS) then
!            SET_RC(stat, rc)
!            return
!         end if
!      end do
!
!   end subroutine load_config_array
!
!   subroutine load_config_file(config, filename, rc)
!      type(ESMF_Config), intent(inout) :: config
!      character(len=*), intent(in) :: filename
!      integer, optional, intent(out) :: rc
!      integer :: stat
!
!      if(.not. config_created(config, stat, rc=stat)) then
!         SET_RC(stat, rc)
!         return
!      end if
!
!      call ESMF_ConfigLoad(config, filename, rc=stat)
!      if(stat /= SUCCESS) then
!         SET_RC(stat, rc)
!         return
!      end if
!
!   end subroutine load_config_file
!
!   logical function config_created(config, stat, rc) 
!      type(ESMF_Config), intent(inout) :: config
!      integer :: stat
!      integer, optional, intent(out) :: rc
!
!      config_created = ESMF_ConfigIsCreated(config, rc=stat)
!      config_created = .not. (config_created .and. (stat == SUCCESS))
!      if(.not. config_created) SET_RC(stat, rc)
!      
!   end function config_created
!
!end module overwrite_config_mod
