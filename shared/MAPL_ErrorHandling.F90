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


   public :: MAPL_UNKNOWN_ERROR
   public :: MAPL_SUCCESS

   public :: MAPL_NO_SUCH_PROPERTY
   public :: MAPL_NO_SUCH_VARIABLE
   public :: MAPL_TYPE_MISMATCH
   public :: MAPL_UNSUPPORTED_TYPE
   public :: MAPL_VALUE_NOT_SUPPORTED

   public :: MAPL_NO_DEFAULT_VALUE
   public :: MAPL_DUPLICATE_KEY
   public :: MAPL_STRING_TOO_SHORT

   enum, bind(c)
      enumerator :: MAPL_UNKNOWN_ERROR = -1
      enumerator :: MAPL_SUCCESS       = 0

      ! 001-005
      enumerator :: MAPL_NO_SUCH_PROPERTY
      enumerator :: MAPL_NO_SUCH_VARIABLE
      enumerator :: MAPL_TYPE_MISMATCH
      enumerator :: MAPL_UNSUPPORTED_TYPE
      enumerator :: MAPL_VALUE_NOT_SUPPORTED

      ! 006-010
      enumerator :: MAPL_NO_DEFAULT_VALUE
      enumerator :: MAPL_DUPLICATE_KEY
      enumerator :: MAPL_STRING_TOO_SHORT
   end enum


   interface MAPL_Assert
      module procedure MAPL_Assert_condition
      module procedure MAPL_Assert_return_code
   end interface MAPL_Assert

   interface MAPL_VRFY
      module procedure MAPL_VRFY
      module procedure MAPL_VRFYt
   end interface MAPL_VRFY
   
   interface MAPL_ASRT
      module procedure MAPL_ASRT
      module procedure MAPL_ASRTt
   end interface MAPL_ASRT
   
   interface MAPL_RTRN
      module procedure MAPL_RTRN
      module procedure MAPL_RTRNt
   end interface MAPL_RTRN
   
contains


   logical function MAPL_Assert_condition(condition, message, filename, line, rc) result(fail)
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

   end function MAPL_Assert_Condition


   logical function MAPL_Assert_return_code(condition, return_code, filename, line, rc) result(fail)
      logical, intent(in) :: condition
      integer, intent(in) :: return_code
      character(*), intent(in) :: filename
      integer, intent(in) :: line
      integer, optional, intent(out) :: rc ! Not present in MAIN
      character(:), allocatable :: message
      
      fail = .not. condition

      if (fail) then
         message = get_error_message(return_code)
         call MAPL_throw_exception(filename, line, message=message)
         if (present(rc)) rc = 1
      end if

   end function MAPL_Assert_Return_Code


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

      condition = (status == 0)
      fail = .not. condition

      if (fail) then
         message = get_error_message(status)
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

  function get_error_message(error_code) result(description)
     use gFTL_IntegerStringMap
     character(:), allocatable :: description
     integer, intent(in) :: error_code

     type(IntegerStringMap), save :: error_messages
     logical, save :: initialized = .false.


     call initialize_err()

     if (error_messages%count(error_code) > 0) then
        description = error_messages%at(error_code)
     else
        description = error_messages%at(MAPL_UNKNOWN_ERROR)
     end if

  contains

     subroutine initialize_err()

        if (.not. initialized) then
           initialized = .true.
           call error_messages%insert(MAPL_UNKNOWN_ERROR, 'unknown error')
           call error_messages%insert(MAPL_SUCCESS, 'success')

           call error_messages%insert(MAPL_NO_SUCH_PROPERTY, 'no such property')
           call error_messages%insert(MAPL_NO_SUCH_VARIABLE, 'no such variable')
           call error_messages%insert(MAPL_TYPE_MISMATCH,    'passed argument does not match expected type')
           call error_messages%insert(MAPL_UNSUPPORTED_TYPE, 'provided data type is not supported by this subclass')
           call error_messages%insert(MAPL_VALUE_NOT_SUPPORTED, 'provided value is not supported by this subclass')

           call error_messages%insert(MAPL_NO_DEFAULT_VALUE, 'no default value has been provided for this property')
           call error_messages%insert(MAPL_DUPLICATE_KEY, 'map container already has the specified key')
           call error_messages%insert(MAPL_STRING_TOO_SHORT, 'fixed length string is not long enough to contain requested data')
        end if

     end subroutine initialize_err

  end function get_error_message

end module MAPL_ErrorHandlingMod
