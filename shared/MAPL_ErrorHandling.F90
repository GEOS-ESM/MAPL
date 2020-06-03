module MAPL_ErrorHandlingMod
   use MAPL_ThrowMod
   use MPI
   implicit none
   private

   public :: MAPL_Assert
   public :: MAPL_Verify
   public :: MAPL_Return
   public :: MAPL_RTRN
   public :: MAPL_Vrfy
   public :: MAPL_ASRT
   public :: MAPL_abort

interface MAPL_VRFY
   module procedure MAPL_VRFY
   module procedure MAPL_VRFYt
end interface

interface MAPL_ASRT
   module procedure MAPL_ASRT
   module procedure MAPL_ASRTt
end interface

interface MAPL_RTRN
   module procedure MAPL_RTRN
   module procedure MAPL_RTRNt
end interface

contains


   logical function MAPL_Assert(condition, message, filename, line, rc) result(fail)
      logical, intent(in) :: condition
      character(*), intent(in) :: message
      character(*), intent(in) :: filename
      integer, intent(in) :: line
      integer, optional, intent(out) :: rc ! Not present in MAIN

      fail = .not. condition

      if (fail) then
         call MAPL_throw_exception(filename, line, message=message)
         if (present(rc)) rc = 1
      end if

   end function MAPL_Assert


   logical function MAPL_Verify(status, filename, line, rc) result(fail)
      integer, intent(in) :: status
      character(*), intent(in) :: filename
      integer, intent(in) :: line
      integer, optional, intent(out) :: rc ! Not present in MAIN

      logical :: condition
      character(:), allocatable :: message
      character(16) :: status_string

      condition = (status == 0)
      fail = .not. condition

      if (fail) then
         write(status_string,'(i0)') status
         message = 'status=' // status_string
         call MAPL_throw_exception(filename, line, message=message)
         if (present(rc)) rc = status
      end if
      
   end function MAPL_Verify


   subroutine MAPL_Return(status, filename, line, rc) 
      integer, intent(in) :: status
      character(*), intent(in) :: filename
      integer, intent(in) :: line
      integer, intent(out), optional :: rc

      logical :: condition, fail
      character(:), allocatable :: message
      character(8) :: status_string

      condition = (status == 0)
      fail = .not. condition

      if (fail) then
         write(status_string,'(i0)') status
         message = 'status=' // status_string
         call MAPL_throw_exception(filename, line, message=message)
      end if
      ! Regardless of error:
      if (present(rc)) rc = status 
      
   end subroutine MAPL_Return

   logical function MAPL_RTRN(A,iam,line,rc)
      integer,           intent(IN ) :: A
      character*(*),     intent(IN ) :: iam
      integer,           intent(IN ) :: line
      integer, optional, intent(OUT) :: RC

        MAPL_RTRN = .true.
        if(A/=0) print '(A40,I10)',Iam,line
        if(present(RC)) RC=A
   end function MAPL_RTRN

   logical function MAPL_VRFY(A,iam,line,rc)
      integer,           intent(IN ) :: A
      character*(*),     intent(IN ) :: iam
      integer,           intent(IN ) :: line
      integer, optional, intent(OUT) :: RC
        MAPL_VRFY = A/=0
        if(MAPL_VRFY)then
          if(present(RC)) then
            print '(A40,I10)',Iam,line
            RC=A
          endif
        endif
   end function MAPL_VRFY

   logical function MAPL_ASRT(A,iam,line,rc)
      logical,           intent(IN ) :: A
      character*(*),     intent(IN ) :: iam
      integer,           intent(IN ) :: line
      integer, optional, intent(OUT) :: RC
        MAPL_ASRT = .not.A
        if(MAPL_ASRT)then
          if(present(RC))then
            print '(A40,I10)',Iam,LINE
            RC=1
          endif
        endif
   end function MAPL_ASRT   

   logical function MAPL_ASRTt(A,text,iam,line,rc)
      logical,           intent(IN ) :: A
      character*(*),     intent(IN ) :: iam,text
      integer,           intent(IN ) :: line
      integer, optional, intent(OUT) :: RC
        MAPL_ASRTt =   MAPL_ASRT(A,iam,line,rc)
        if(MAPL_ASRTt) print *, text
   end function MAPL_ASRTT

   logical function MAPL_RTRNt(A,text,iam,line,rc)
      integer,           intent(IN ) :: A
      character*(*),     intent(IN ) :: text,iam
      integer,           intent(IN ) :: line
      integer, optional, intent(OUT) :: RC

        MAPL_RTRNt = .true.
        if(A/=0)then
           print '(A40,I10)',Iam,line
           print *, text
        end if
        if(present(RC)) RC=A

   end function MAPL_RTRNT

   logical function MAPL_VRFYt(A,text,iam,line,rc)
      integer,           intent(IN ) :: A
      character*(*),     intent(IN ) :: iam,text
      integer,           intent(IN ) :: line
      integer, optional, intent(OUT) :: RC
        MAPL_VRFYt =  MAPL_VRFY(A,iam,line,rc)
        if(MAPL_VRFYt) print *, text
   end function MAPL_VRFYT

   subroutine MAPL_abort
      integer :: status
      integer :: error_code
      call MPI_Abort(MPI_COMM_WORLD,error_code,status)
  end subroutine MAPL_abort

end module MAPL_ErrorHandlingMod
