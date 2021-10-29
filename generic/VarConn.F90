#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module mapl_VarConn
   use ESMF
   use MAPL_Constants, only: MAPL_ConnUnknown, MAPL_Self
   use MAPL_VarSpecPtrMod
   use MAPL_VarSpecMod
   use MAPL_VarConnPoint
   use MAPL_VarConnType
   use MAPL_VarConnVector
   use MAPL_ErrorHandlingMod
   use pFlogger
   implicit none
   private

   public :: VarConn ! wraps VarConnVector (vector of VarConnType)

   ! A trivial wrapper to encapsulate management of
   ! vector under the hood.  
   type VarConn
!!$      private
      type(VarConnVector) :: conn_v
   contains
      procedure :: append
      procedure :: checkReq
      procedure :: checkUnused
      procedure :: varIsConnected_IE
      procedure :: varIsConnected_name
      generic :: varIsConnected => varIsConnected_IE
      generic :: varIsConnected => varIsConnected_name
      procedure :: varIsListed
   end type VarConn

contains


   subroutine append(CONN, SHORT_NAME, TO_NAME, &
        FROM_EXPORT, TO_IMPORT, RC  )

      class (VarConn ), intent(inout) :: CONN
      character (len=*)             , intent(IN   ) :: SHORT_NAME
      character (len=*),    optional, intent(IN   ) :: TO_NAME
      integer,              optional, intent(IN   ) :: FROM_EXPORT
      integer,              optional, intent(IN   ) :: TO_IMPORT
      integer,              optional, intent(  OUT) :: RC     ! Error code:


      integer                    :: usableFROM_EXPORT
      integer                    :: usableTO_IMPORT
      type(VarConnType), pointer :: new_conn
      character(len=ESMF_MAXSTR) :: usableTONAME

      usableFROM_EXPORT=MAPL_ConnUnknown
      usableTO_IMPORT=MAPL_ConnUnknown

      if(present(TO_NAME)) then
         usableTONAME = TO_NAME
      else
         usableTONAME = SHORT_NAME
      endif

      if(present(FROM_EXPORT)) then
         usableFROM_EXPORT=FROM_EXPORT
      end if
      if(present(TO_IMPORT)) then
         usableTO_IMPORT=TO_IMPORT
      end if

      ! Push and then construct.
      call conn%conn_v%resize(conn%conn_v%size()+1)
      new_conn => CONN%conn_v%back()
      new_conn%From = VarConnPoint(SHORT_NAME, gc_id=usableFROM_EXPORT)
      new_conn%To   = VarConnPoint(usableTONAME, gc_id=usableTO_IMPORT)

      _RETURN(ESMF_SUCCESS)

   end subroutine append

   subroutine checkReq(this, ImSpecPtr, ExSpecPtr, RC)
      class (VarConn), target, intent(inout) :: this
      type (MAPL_VarSpecPtr),     pointer   :: ImSpecPtr(:)
      type (MAPL_VarSpecPtr),     pointer   :: ExSpecPtr(:)
      integer, optional,        intent(OUT) :: RC


      type(VarConnType), pointer :: conn
      integer                               :: I, J
      integer                               :: IMP
      integer                               :: FI
      integer                               :: TI
      integer                               :: FE
      integer                               :: TE
      character(len=ESMF_MAXSTR)            :: NAME

      associate (conn_v => this%conn_v)
        do I = 1, conn_v%size()
           conn => conn_v%of(i)

           FI = MAPL_ConnUnknown
           TI = conn%to%get_gc_id()

           IMP = MAPL_ConnUnknown
           if(FI /= MAPL_ConnUnknown) then
              IMP = FI
              NAME = conn%FROM%get_short_name()
           else if (TI /= MAPL_ConnUnknown) then
              IMP = TI
              NAME = conn%TO%get_short_name()
           end if

           if (IMP /= MAPL_ConnUnknown .and. IMP /= MAPL_Self) then
              ! check if the component has an import spec
              if(.not. associated(ImSpecPtr(IMP)%Spec)) then
                 conn%notRequired = .true.
                 cycle
              end if
              if(MAPL_VarSpecGetIndex(ImSpecPtr(IMP)%Spec, NAME)==-1) then

                 FE = conn%from%get_gc_id()
                 TE = MAPL_ConnUnknown

                 J = MAPL_ConnUnknown
                 if(FE /= MAPL_ConnUnknown) then
                    J = FE
                    NAME = conn%FROM%get_short_name()
                 else if (TE /= MAPL_ConnUnknown) then
                    J = TE
                    NAME = conn%TO%get_short_name()
                 end if

                 if(MAPL_VarSpecGetIndex(ExSpecPtr(J)%Spec, NAME)/=-1) then
                    !            Export does exist while import does not - we relax the requirement 
                    conn%notRequired = .true.
                 end if
              endif
           end if

        end do
      end associate
      _RETURN(ESMF_SUCCESS)

   end subroutine CheckReq

   logical function checkUnused(this)
      class(VarConn), target, intent(inout) :: this

      integer                               :: I
      class(Logger), pointer :: lgr
      type(VarConnType), pointer :: conn

      associate (conn_v => this%conn_v)
        checkUnused = .true.

        do I = 1, conn_v%size()
           conn => conn_v%of(i)
           if (conn%notRequired) cycle
           if (.not. conn%USED) then
              checkUnused = .false.
              lgr => logging%get_logger('MAPL.GENERIC')
              call lgr%error( &
                   'SRC_NAME: <%a~>  DST_NAME: <%a~> is not satisfied', &
                   trim(conn%FROM%get_short_name()),trim(conn%TO%get_short_name()))
           end if
        end do
      end associate
      return

   end function CheckUnused

   logical function varIsConnected_IE(this, IMPORT_NAME, EXPORT_NAME, &
        import, EXPORT, RC)
      class(VarConn), target, intent(inout) :: this
      character (len=*),           intent(IN   ) :: IMPORT_NAME
      character (len=*), optional, intent(  OUT) :: EXPORT_NAME
      integer,                     intent(IN   ) :: IMPORT
      integer,                     intent(IN   ) :: EXPORT
      integer,           optional, intent(  OUT) :: RC     ! Error code:



      integer                               :: I
      integer                               :: TI, FE
      type(VarConnType), pointer :: conn

      varIsConnected_IE = .false.

      ! try to find a match with "TO"
      associate (conn_v => this%conn_v)
        do I = 1, conn_v%size()
           conn => conn_v%of(i)
           if (conn%TO%get_short_name() /= IMPORT_NAME) then
              cycle
           end if
           TI = conn%to%get_gc_id()
           FE = conn%from%get_gc_id()
           
           if (TI /= import) then
              cycle
           end if
           
           if (FE /= EXPORT) then
              cycle
           end if
           
           varIsConnected_IE = .true.
           conn%used = .true.
           if (present(EXPORT_NAME)) then
              EXPORT_NAME = conn%FROM%get_short_name()
           end if
           _RETURN(ESMF_SUCCESS)
        end do
      end associate
      varIsConnected_IE = .false.

      _RETURN(ESMF_SUCCESS)
   end function VarIsConnected_IE

   logical function varIsConnected_name(this, IMPORT_NAME, import, RC)
      class(VarConn), target, intent(inout):: this
      character (len=*),           intent(IN   ) :: IMPORT_NAME
      integer,                     intent(in   ) :: import
      integer,           optional, intent(  OUT) :: RC     ! Error code:


      integer                               :: I
      integer                               :: TI
      type(VarConnType), pointer :: conn

      varIsConnected_name = .false.

      associate (conn_v => this%conn_v)
        ! try to find a match with "TO"
        do I = 1, conn_v%size()
           conn => conn_v%of(i)
           if (conn%to%get_short_name() /= IMPORT_NAME) then
              cycle
           end if
           
           TI = conn%to%get_gc_id()
           if (TI /= import) then
              cycle
           end if
           
           varIsConnected_name = .true.
           _RETURN(ESMF_SUCCESS)
        end do
      end associate

      varIsConnected_name = .false.

      _RETURN(ESMF_SUCCESS)
   end function VarIsConnected_Name

   logical function varIsListed(this, SHORT_NAME, import, RC)
      class(VarConn ), target, intent(inout):: this
      character (len=*),               intent(IN) :: SHORT_NAME
      integer,                         intent(IN) :: IMPORT
      integer,              optional, intent(OUT) :: RC     ! Error code:

      type(VarConnType), pointer :: conn
      integer                               :: I
      integer                               :: TI

      associate (conn_v => this%conn_v)
        do I = 1, conn_v%size()
           conn => conn_v%of(i)
           if (conn%FROM%get_short_name() /= SHORT_NAME) then
              cycle
           end if
           TI = conn%to%get_gc_id()
           ! check for a match
           if(TI == import)  then
              varIsListed = .true.
              conn%used = .true.
              _RETURN(ESMF_SUCCESS)
           end if
        end do
      end associate

      varIsListed = .false.
      _RETURN(ESMF_SUCCESS)
   end function VarIsListed


end module mapl_VarConn
