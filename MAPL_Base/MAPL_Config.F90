#include "MAPL_Generic.h"



! This module implements extensions that allow extending an
! ESMF_Config object.  Otherwise, ESMF only provides a constructor
! that loads the data from a text file.

module MAPL_ConfigMod
   use ESMF
   use MAPL_ErrorHandlingMod
   implicit none
   private

   public :: MAPL_ConfigCreate
   public :: MAPL_ConfigSetAttribute

   interface MAPL_ConfigSetAttribute
      module procedure :: MAPL_ConfigSetAttribute_int32
      module procedure :: MAPL_ConfigSetAttribute_string
   end interface

contains

   function MAPL_ConfigCreate(unusable, rc) result(config)
      use MAPL_KeywordEnforcerMod
      type (ESMF_Config) :: config
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      character, parameter :: EOB = achar(00)   ! end of buffer mark (null)
#ifdef ESMF_HAS_ACHAR_BUG
      character, parameter :: EOL = achar(12)   ! end of line mark (cr)
#else
      character, parameter :: EOL = achar(10)   ! end of line mark (newline)
#endif
      character, parameter :: NUL = achar(00)   ! what it says

      _UNUSED_DUMMY(unusable)
      config = ESMF_ConfigCreate(rc=rc)
      config%cptr%buffer(1:1) = EOL
      config%cptr%buffer(2:2) = EOB
      config%cptr%nbuf = 2
      config%cptr%next_line = 1
      config%cptr%value_begin = 1

   end function MAPL_ConfigCreate

! BOPI -------------------------------------------------------------------
!
! !IROUTINE: MAPL_ConfigSetAttribute - Set a 4-byte integer number

!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigSetAttribute()
      subroutine MAPL_ConfigSetAttribute_int32( config, value, label, rc )
         use, intrinsic :: iso_fortran_env, only: INT32
! !ARGUMENTS:
      type(ESMF_Config), intent(inout)             :: config     
      integer(kind=INT32), intent(in)            :: value
      character(len=*), intent(in), optional       :: label 
      integer, intent(out), optional               :: rc   

!
! !DESCRIPTION: 
!  Sets an integer {\tt value} in the {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [value]
!     Integer value to set. 
!   \item [{[label]}]
!     Identifying attribute label. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! EOPI -------------------------------------------------------------------

       !integer,   parameter :: LSZ = 256  ! Maximum line size
       integer,   parameter :: LSZ = max (1024,ESMF_MAXPATHLEN)  ! Maximum line size
       integer,   parameter :: MSZ = 256  ! Used to size buffer; this is
                                          ! usually *less* than the number
                                          ! of non-blank/comment lines
                                          ! (because most lines are shorter
                                          ! then LSZ)
 
       integer,   parameter :: NBUF_MAX = MSZ*LSZ ! max size of buffer
       integer,   parameter :: NATT_MAX = NBUF_MAX/64 ! max # attributes;  
                                                  ! assumes an average line
                                                  ! size of 16, the code
                                                  ! will do a bound check

       character, parameter :: BLK = achar(32)   ! blank (space)
       character, parameter :: TAB = achar(09)   ! TAB
#ifdef ESMF_HAS_ACHAR_BUG
       character, parameter :: EOL = achar(12)   ! end of line mark (cr)
#else
       character, parameter :: EOL = achar(10)   ! end of line mark (newline)
#endif
       character, parameter :: EOB = achar(00)   ! end of buffer mark (null)
       character, parameter :: NUL = achar(00)   ! what it says

!$$      character(len=ESMF_MAXSTR) :: Iam = 'MAPL_ConfigSetAttribute_int32'

      character(len=ESMF_MAXSTR) :: logmsg
      character(len=LSZ) :: curVal, newVal
      integer :: iret, i, j, k, m, nchar, ninsert, ndelete, lenThisLine

      ! Initialize return code; assume routine not implemented
      iret = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      !check variables
!ALT      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      ! Set config buffer at desired attribute
      if ( present (label) ) then
         call ESMF_ConfigGetAttribute( config, value=curVal, label=label, rc = iret )
      else
         call ESMF_ConfigGetAttribute( config, value=curVal, rc = iret )
      endif

      if ( iret .ne. ESMF_SUCCESS ) then
        if ( iret .eq. ESMF_RC_NOT_FOUND ) then
          ! set config buffer at end for appending
          i = config%cptr%nbuf
        else
          if ( present( rc ) ) then
            rc = iret
          endif
          return
        endif
      else ! attribute found
        ! set config buffer for overwriting/inserting
        i = config%cptr%value_begin
        curVal = BLK // trim(curVal) // BLK // EOL ! like config%cptr%this_line
      endif

      ! for appending, create new attribute string with label and value
      if ( i .eq. config%cptr%nbuf .and. present(label) ) then
        write(newVal, *) label, value
        newVal = trim(adjustl(newVal)) // EOL
        j = i + len_trim(newVal)

        ! check to ensure len of newVal doesn't exceed LSZ
        if ( (j-i) .gt. LSZ) then
           write(logmsg, *) ", attribute label, value & EOL are ", j-i, &
               " characters long, only ", LSZ, " characters allowed per line"
           _RETURN(ESMC_RC_LONG_STR)
        endif

        ! check if enough space left in config buffer
        if (j .ge. NBUF_MAX) then   ! room for EOB if necessary
           write(logmsg, *) ", attribute label & value require ", j-i+1, &
               " characters (including EOL & EOB), only ", NBUF_MAX-i, &
               " characters left in config buffer"
           _RETURN(ESMC_RC_LONG_STR)
        endif
      endif

      ! overwrite, with possible insertion or deletion of extra characters
      if (i .eq. config%cptr%value_begin) then
         write(newVal, *) value
         newVal = BLK // trim(adjustl(newVal)) // EOL
         j = i + len_trim(newVal) - 1

         !  check if we need more space to insert new characters;
         !  shift buffer down (linked-list redesign would be better!)
         nchar = j-i+1
         lenThisLine = len_trim(curVal) - 1
         if ( nchar .gt. lenThisLine) then

            ! check to ensure length of extended line doesn't exceed LSZ
            do m = i, 1, -1
              if (config%cptr%buffer(m:m) .eq. EOL) then
                exit
              endif
            enddo
            if (j-m+1 .gt. LSZ) then
               write(logmsg, *) ", attribute label, value & EOL are ", j-m+1, &
                  " characters long, only ", LSZ, " characters allowed per line"
               _RETURN(ESMC_RC_LONG_STR)
            endif

            ! check if enough space left in config buffer to extend line
            if (j+1 .ge. NBUF_MAX) then   ! room for EOB if necessary
               write(logmsg, *) ", attribute label & value require ", j-m+1, &
                   " characters (including EOL & EOB), only ", NBUF_MAX-i, &
                   " characters left in config buffer"
               _RETURN(ESMC_RC_LONG_STR)
            endif

            ninsert = nchar - lenThisLine
            do k = config%cptr%nbuf, j, -1
               config%cptr%buffer(k+ninsert:k+ninsert) = config%cptr%buffer(k:k)
            enddo
            config%cptr%nbuf = config%cptr%nbuf + ninsert

         ! or if we need less space and remove characters;
         ! shift buffer up
         elseif ( nchar .lt. lenThisLine ) then
           ndelete = lenThisLine - nchar
            do k = j+1, config%cptr%nbuf
               config%cptr%buffer(k-ndelete:k-ndelete) = config%cptr%buffer(k:k)
            enddo
            config%cptr%nbuf = config%cptr%nbuf - ndelete
         endif
      endif

      ! write new attribute value into config
      config%cptr%buffer(i:j) = newVal(1:len_trim(newVal))

      ! if appended, reset EOB marker and nbuf
      if (i .eq. config%cptr%nbuf) then
!@@        j = j + 1
!@@        config%cptr%buffer(j:j) = EOB
        config%cptr%nbuf = j
      endif

      if( present( rc )) then
        if ( iret .eq. ESMF_RC_NOT_FOUND ) iret = ESMF_SUCCESS
        rc = iret
      endif
      
      return
   end subroutine MAPL_ConfigSetAttribute_int32

! BOPI -------------------------------------------------------------------
!
! !IROUTINE: MAPL_ConfigSetAttribute - Set a 4-byte integer number

!
! !INTERFACE:
      ! Private name; call using MAPL_ConfigSetAttribute()
   subroutine MAPL_ConfigSetAttribute_string(config, value, label, rc)
! !ARGUMENTS:
      type(ESMF_Config), intent(inout)             :: config     
      character(len=*), intent(in)                 :: value
      character(len=*), intent(in), optional       :: label 
      integer, intent(out), optional               :: rc   

!
! !DESCRIPTION: 
!  Sets an integer {\tt value} in the {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [value]
!     Integer value to set. 
!   \item [{[label]}]
!     Identifying attribute label. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! EOPI -------------------------------------------------------------------

       !integer,   parameter :: LSZ = 256  ! Maximum line size
       integer,   parameter :: LSZ = max (1024,ESMF_MAXPATHLEN)  ! Maximum line size
       integer,   parameter :: MSZ = 256  ! Used to size buffer; this is
                                          ! usually *less* than the number
                                          ! of non-blank/comment lines
                                          ! (because most lines are shorter
                                          ! then LSZ)
 
       integer,   parameter :: NBUF_MAX = MSZ*LSZ ! max size of buffer
       integer,   parameter :: NATT_MAX = NBUF_MAX/64 ! max # attributes;  
                                                  ! assumes an average line
                                                  ! size of 16, the code
                                                  ! will do a bound check

       character, parameter :: BLK = achar(32)   ! blank (space)
       character, parameter :: TAB = achar(09)   ! TAB
#ifdef ESMF_HAS_ACHAR_BUG
       character, parameter :: EOL = achar(12)   ! end of line mark (cr)
#else
       character, parameter :: EOL = achar(10)   ! end of line mark (newline)
#endif
       character, parameter :: EOB = achar(00)   ! end of buffer mark (null)
       character, parameter :: NUL = achar(00)   ! what it says

!$$      character(len=ESMF_MAXSTR) :: Iam = 'MAPL_ConfigSetAttribute_string'

      character(len=ESMF_MAXSTR) :: logmsg
      character(len=LSZ) :: curVal
      character(len=:), allocatable :: newVal
      integer :: iret, i, j, k, m, nchar, ninsert, ndelete, lenThisLine

      ! Initialize return code; assume routine not implemented
      iret = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      !check variables
!ALT      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      ! Set config buffer at desired attribute
      if ( present (label) ) then
         call ESMF_ConfigGetAttribute( config, value=curVal, label=label, rc = iret )
      else
         call ESMF_ConfigGetAttribute( config, value=curVal, rc = iret )
      endif

      if ( iret .ne. ESMF_SUCCESS ) then
        if ( iret .eq. ESMF_RC_NOT_FOUND ) then
          ! set config buffer at end for appending
          i = config%cptr%nbuf
        else
          if ( present( rc ) ) then
            rc = iret
          endif
          return
        endif
      else ! attribute found
        ! set config buffer for overwriting/inserting
        i = config%cptr%value_begin
        curVal = BLK // trim(curVal) // BLK // EOL ! like config%cptr%this_line
      endif

      ! for appending, create new attribute string with label and value
      if ( i .eq. config%cptr%nbuf .and. present(label) ) then
         newVal = trim(adjustl(label)) // trim(value) // EOL
        j = i + len_trim(newVal)

        ! check to ensure len of newVal doesn't exceed LSZ
        if ( (j-i) .gt. LSZ) then
           write(logmsg, *) ", attribute label, value & EOL are ", j-i, &
               " characters long, only ", LSZ, " characters allowed per line"
           _RETURN(ESMC_RC_LONG_STR)
        endif

        ! check if enough space left in config buffer
        if (j .ge. NBUF_MAX) then   ! room for EOB if necessary
           write(logmsg, *) ", attribute label & value require ", j-i+1, &
               " characters (including EOL & EOB), only ", NBUF_MAX-i, &
               " characters left in config buffer"
           _RETURN(ESMC_RC_LONG_STR)
        endif
      endif

      ! overwrite, with possible insertion or deletion of extra characters
      if (i .eq. config%cptr%value_begin) then
         newval = BLK // trim(adjustl(value)) // EOL
         j = i + len_trim(newVal) - 1

         !  check if we need more space to insert new characters;
         !  shift buffer down (linked-list redesign would be better!)
         nchar = j-i+1
         lenThisLine = len_trim(curVal) - 1
         if ( nchar .gt. lenThisLine) then

            ! check to ensure length of extended line doesn't exceed LSZ
            do m = i, 1, -1
              if (config%cptr%buffer(m:m) .eq. EOL) then
                exit
              endif
            enddo
            if (j-m+1 .gt. LSZ) then
               write(logmsg, *) ", attribute label, value & EOL are ", j-m+1, &
                  " characters long, only ", LSZ, " characters allowed per line"
               _RETURN(ESMC_RC_LONG_STR)
            endif

            ! check if enough space left in config buffer to extend line
            if (j+1 .ge. NBUF_MAX) then   ! room for EOB if necessary
               write(logmsg, *) ", attribute label & value require ", j-m+1, &
                   " characters (including EOL & EOB), only ", NBUF_MAX-i, &
                   " characters left in config buffer"
               _RETURN(ESMC_RC_LONG_STR)
            endif

            ninsert = nchar - lenThisLine
            do k = config%cptr%nbuf, j, -1
               config%cptr%buffer(k+ninsert:k+ninsert) = config%cptr%buffer(k:k)
            enddo
            config%cptr%nbuf = config%cptr%nbuf + ninsert

         ! or if we need less space and remove characters;
         ! shift buffer up
         elseif ( nchar .lt. lenThisLine ) then
           ndelete = lenThisLine - nchar
            do k = j+1, config%cptr%nbuf
               config%cptr%buffer(k-ndelete:k-ndelete) = config%cptr%buffer(k:k)
            enddo
            config%cptr%nbuf = config%cptr%nbuf - ndelete
         endif
      endif

      ! write new attribute value into config
      config%cptr%buffer(i:j) = newVal

      ! if appended, reset EOB marker and nbuf
      if (i .eq. config%cptr%nbuf) then
!@@        j = j + 1
!@@        config%cptr%buffer(j:j) = EOB
        config%cptr%nbuf = j
      endif

      if( present( rc )) then
        if ( iret .eq. ESMF_RC_NOT_FOUND ) iret = ESMF_SUCCESS
        rc = iret
      endif
      
      _RETURN(_SUCCESS)

   end subroutine MAPL_ConfigSetAttribute_string

end module MAPL_ConfigMod
