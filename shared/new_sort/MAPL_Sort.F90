
#define ASSERT_(A) if(.not.(A))call exit(1)


!  $Id: MAPL_Sort.F90,v 1.12 2014-12-11 21:08:00 atrayano Exp $

!=============================================================================
!BOP

! !MODULE: MAPL_Sort   -- A utility to sort integers

! !INTERFACE:

module MAPL_SortMod

  implicit none
  private

! !PUBLIC ROUTINES:

  public MAPL_Sort

! !DESCRIPTION:
! 
!   {\tt MAPL\_Sort} is a utility to do a quicksort on integers. The general
!   interface is:
!\bv       
!       subroutine MAPL_Sort(A[,Bi,Bl,Br,Bd,Ci,Cl,Cr,Cd,Di,Dl,Dr,Dd,DIM])
!         integer(kind=[4,8]),           intent(INOUT) :: A(:)
!         integer(kind=4)    , optional, intent(INOUT) :: Bi(:,:)
!         integer(kind=8)    , optional, intent(INOUT) :: Bl(:,:)
!         real   (kind=4)    , optional, intent(INOUT) :: Br(:,:)
!         real   (kind=8)    , optional, intent(INOUT) :: Bd(:,:)
!         integer(kind=4)    , optional, intent(INOUT) :: Ci(:,:)
!         integer(kind=8)    , optional, intent(INOUT) :: Cl(:,:)
!         real   (kind=4)    , optional, intent(INOUT) :: Cr(:,:)
!         real   (kind=8)    , optional, intent(INOUT) :: Cd(:,:)
!         integer(kind=4)    , optional, intent(INOUT) :: Di(:,:)
!         integer(kind=8)    , optional, intent(INOUT) :: Dl(:,:)
!         real   (kind=4)    , optional, intent(INOUT) :: Dr(:,:)
!         real   (kind=8)    , optional, intent(INOUT) :: Dd(:,:)
!         integer(kind=4),     optional, intent(IN   ) :: DIM
!
!       subroutine MAPL_Sort(A[Ci,Cl,Cr,Cd,DIM])
!         integer(kind=[4,8]),           intent(INOUT) :: A (:,:)
!         integer(kind=4)    , optional, intent(INOUT) :: Ci(:,:)
!         integer(kind=8)    , optional, intent(INOUT) :: Cl(:,:)
!         real   (kind=4)    , optional, intent(INOUT) :: Cr(:,:)
!         real   (kind=8)    , optional, intent(INOUT) :: Cd(:,:)
!         integer(kind=4),     optional, intent(IN   ) :: DIM
!
!\ev
!   {\tt MAPL\_Sort} sorts the key (contained in a row or column of A)
!   in ascending order and reorders the data in B or in non-key rows or columns of A
!   in the same order; i.e., it does the same exchanges as were done 
!   to the key in sorting it.  If, for example, on input B(:) contains the ordered integers
!   from 1 to size(A), on output it will contain the positions of the elements of
!   the sorted A in the unsorted A. In the last two signatures, DIM is the dimension
!   of A or B being reordered. In the last signature, for example, DIM=1 corresponds
!   to a B ordered as B(size(A),:), whereas DIM=2 corresponds to B(:,size(A)).
!   The default is DIM=2. The quicksort is coded in C and does not appear here.

!EOP
!=============================================================================

interface MAPL_Sort
   module procedure SORTS
   module procedure SORTSL
   module procedure SORTSR
   module procedure SORTSD
   module procedure SORTL
   module procedure SORTLL
   module procedure SORTLR
   module procedure SORTLD
   module procedure SORT2AS
   module procedure SORT2AL

   module procedure SORT2LS
   module procedure SORT2LL
   module procedure SORT2LR
   module procedure SORT2LD
   module procedure SORT2SS
   module procedure SORT2SR
   module procedure SORT2SL
   module procedure SORT2SD
end interface

contains

subroutine SORTL(A,Bi,Bl,Br,Bd,Ci,Cl,Cr,Cd,Di,Dl,Dr,Dd,DIM)
  integer(kind=8),           intent(INOUT) :: A(:)
  integer(kind=4), optional, intent(INOUT) :: Bi(:)
  integer(kind=8), optional, intent(INOUT) :: Bl(:)
  real   (kind=4), optional, intent(INOUT) :: Br(:)
  real   (kind=8), optional, intent(INOUT) :: Bd(:)
  integer(kind=4), optional, intent(INOUT) :: Ci(:,:)
  integer(kind=8), optional, intent(INOUT) :: Cl(:,:)
  real   (kind=4), optional, intent(INOUT) :: Cr(:,:)
  real   (kind=8), optional, intent(INOUT) :: Cd(:,:)
  integer(kind=4), optional, intent(INOUT) :: Di(:,:)
  integer(kind=8), optional, intent(INOUT) :: Dl(:,:)
  real   (kind=4), optional, intent(INOUT) :: Dr(:,:)
  real   (kind=8), optional, intent(INOUT) :: Dd(:,:)
  integer,         optional, intent(IN   ) :: DIM

  integer :: uDIM, sg, id, len
  integer :: Dm(0,0)

  len = size(A)

  if    (present(Bi)) then
     call QSORTL44(A,Bi,Dm,len,1,0)
  elseif(present(Bl))then
     call QSORTL84(A,Bl,Dm,len,1,0)
  elseif(present(Br))then
     call QSORTL44(A,Br,Dm,len,1,0)
  elseif(present(Bd)) then
     call QSORTL84(A,Bd,Dm,len,1,0)
  else
     
     if(present(DIM)) then
        ASSERT_(DIM>0 .and. DIM<3)
        uDIM = DIM
     else
        uDIM = 2
     end if

     if(uDIM==1) then
        sg = -1
        id = 2
     else
        sg = 1
        id = 1
     end if

     if    (present(Ci)) then
        if    (present(Di)) then
           call QSORTL44(A,Ci,Di,len,sg*size(Ci,id),sg*size(Di,id))
        elseif(present(Dl))then                                
           call QSORTL48(A,Ci,Dl,len,sg*size(Ci,id),sg*size(Dl,id))
        elseif(present(Dr))then                                
           call QSORTL44(A,Ci,Dr,len,sg*size(Ci,id),sg*size(Dr,id))
        elseif(present(Dd)) then                               
           call QSORTL48(A,Ci,Dd,len,sg*size(Ci,id),sg*size(Dd,id))
        else
           call QSORTL44(A,Ci,Dm,len,sg*size(Ci,id),0)
        endif
     elseif(present(Cl))then
        if    (present(Di)) then
           call QSORTL84(A,Cl,Di,len,sg*size(Cl,id),sg*size(Di,id))
        elseif(present(Dl))then                               
           call QSORTL88(A,Cl,Dl,len,sg*size(Cl,id),sg*size(Dl,id))
        elseif(present(Dr))then                               
           call QSORTL84(A,Cl,Dr,len,sg*size(Cl,id),sg*size(Dr,id))
        elseif(present(Dd)) then                              
           call QSORTL88(A,Cl,Dd,len,sg*size(Cl,id),sg*size(Dd,id))
        else
           call QSORTL84(A,Cl,Dm,len,sg*size(Cl,id),0)
        endif
     elseif(present(Cr))then
        if    (present(Di)) then
           call QSORTL44(A,Cr,Di,len,sg*size(Cr,id),sg*size(Di,id))
        elseif(present(Dl))then                               
           call QSORTL48(A,Cr,Dl,len,sg*size(Cr,id),sg*size(Dl,id))
        elseif(present(Dr))then                               
           call QSORTL44(A,Cr,Dr,len,sg*size(Cr,id),sg*size(Dr,id))
        elseif(present(Dd)) then                              
           call QSORTL48(A,Cr,Dd,len,sg*size(Cr,id),sg*size(Dd,id))
        else
           call QSORTL44(A,Cr,Dm,len,sg*size(Cr,id),0)
        endif
     elseif(present(Cd)) then
        if    (present(Di)) then
           call QSORTL84(A,Cd,Di,len,sg*size(Cd,id),sg*size(Di,id))
        elseif(present(Dl))then                                
           call QSORTL88(A,Cd,Dl,len,sg*size(Cd,id),sg*size(Dl,id))
        elseif(present(Dr))then                                
           call QSORTL84(A,Cd,Dr,len,sg*size(Cd,id),sg*size(Dr,id))
        elseif(present(Dd)) then                               
           call QSORTL88(A,Cd,Dd,len,sg*size(Cd,id),sg*size(Dd,id))
        else
           call QSORTL84(A,Cd,Dm,len,sg*size(Cd,id),0)
        endif
     end if
  end if

end subroutine SORTL



subroutine  SORTS(A,Bi,Bl,Br,Bd,Ci,Cl,Cr,Cd,Di,Dl,Dr,Dd,DIM)
  integer(kind=4),           intent(INOUT) :: A(:)
  integer(kind=4), optional, intent(INOUT) :: Bi(:)
  integer(kind=8), optional, intent(INOUT) :: Bl(:)
  real   (kind=4), optional, intent(INOUT) :: Br(:)
  real   (kind=8), optional, intent(INOUT) :: Bd(:)
  integer(kind=4), optional, intent(INOUT) :: Ci(:,:)
  integer(kind=8), optional, intent(INOUT) :: Cl(:,:)
  real   (kind=4), optional, intent(INOUT) :: Cr(:,:)
  real   (kind=8), optional, intent(INOUT) :: Cd(:,:)
  integer(kind=4), optional, intent(INOUT) :: Di(:,:)
  integer(kind=8), optional, intent(INOUT) :: Dl(:,:)
  real   (kind=4), optional, intent(INOUT) :: Dr(:,:)
  real   (kind=8), optional, intent(INOUT) :: Dd(:,:)
  integer,         optional, intent(IN   ) :: DIM

  integer :: uDIM, sg, id, len
  integer :: Dm(0,0)

  len = size(A)

  if    (present(Bi)) then
     call QSORTS44(A,Bi,Dm,len,1,0)
  elseif(present(Bl))then
     call QSORTS84(A,Bl,Dm,len,1,0)
  elseif(present(Br))then
     call QSORTS44(A,Br,Dm,len,1,0)
  elseif(present(Bd)) then
     call QSORTS84(A,Bd,Dm,len,1,0)
  else
     
     if(present(DIM)) then
        ASSERT_(DIM>0 .and. DIM<3)
        uDIM = DIM
     else
        uDIM = 2
     end if

     if(uDIM==1) then
        sg = -1
        id = 2
     else
        sg = 1
        id = 1
     end if

     if    (present(Ci)) then
        if    (present(Di)) then
           call QSORTS44(A,Ci,Di,len,sg*size(Ci,id),sg*size(Di,id))
        elseif(present(Dl))then                                
           call QSORTS48(A,Ci,Dl,len,sg*size(Ci,id),sg*size(Dl,id))
        elseif(present(Dr))then                                
           call QSORTS44(A,Ci,Dr,len,sg*size(Ci,id),sg*size(Dr,id))
        elseif(present(Dd)) then
           call QSORTS48(A,Ci,Dd,len,sg*size(Ci,id),sg*size(Dd,id))
        else
           call QSORTS44(A,Ci,Dm,len,sg*size(Ci,id),0)
        endif
     elseif(present(Cl))then
        if    (present(Di)) then
           call QSORTS84(A,Cl,Di,len,sg*size(Cl,id),sg*size(Di,id))
        elseif(present(Dl))then                               
           call QSORTS88(A,Cl,Dl,len,sg*size(Cl,id),sg*size(Dl,id))
        elseif(present(Dr))then                               
           call QSORTS84(A,Cl,Dr,len,sg*size(Cl,id),sg*size(Dr,id))
        elseif(present(Dd)) then                              
           call QSORTS88(A,Cl,Dd,len,sg*size(Cl,id),sg*size(Dd,id))
        else
           call QSORTS84(A,Cl,Dm,len,sg*size(Cl,id),0)
        endif
     elseif(present(Cr))then
        if    (present(Di)) then
           call QSORTS44(A,Cr,Di,len,sg*size(Cr,id),sg*size(Di,id))
        elseif(present(Dl))then                               
           call QSORTS48(A,Cr,Dl,len,sg*size(Cr,id),sg*size(Dl,id))
        elseif(present(Dr))then                               
           call QSORTS44(A,Cr,Dr,len,sg*size(Cr,id),sg*size(Dr,id))
        elseif(present(Dd)) then                              
           call QSORTS48(A,Cr,Dd,len,sg*size(Cr,id),sg*size(Dd,id))
        else
           call QSORTS44(A,Cr,Dm,len,sg*size(Cr,id),0)
        endif
     elseif(present(Cd)) then
        if    (present(Di)) then
           call QSORTS84(A,Cd,Di,len,sg*size(Cd,id),sg*size(Di,id))
        elseif(present(Dl))then                                
           call QSORTS88(A,Cd,Dl,len,sg*size(Cd,id),sg*size(Dl,id))
        elseif(present(Dr))then                                
           call QSORTS84(A,Cd,Dr,len,sg*size(Cd,id),sg*size(Dr,id))
        elseif(present(Dd)) then                               
           call QSORTS88(A,Cd,Dd,len,sg*size(Cd,id),sg*size(Dd,id))
        else
           call QSORTS84(A,Cd,Dm,len,sg*size(Cd,id),0)
        endif
     end if
  end if

end subroutine SORTS



subroutine SORTSL(A,B)
  integer(kind=4),           intent(INOUT) :: A(:)
  integer(kind=8),           intent(INOUT) :: B(:)

  call SORTS(A,Bl=B)

end subroutine SORTSL


subroutine SORTSR(A,B)
  integer(kind=4),           intent(INOUT) :: A(:)
  real   (kind=4),           intent(INOUT) :: B(:)

  call SORTS(A,Br=B)

end subroutine SORTSR

subroutine SORTSD(A,B)
  integer(kind=4),           intent(INOUT) :: A(:)
  real   (kind=8),           intent(INOUT) :: B(:)

  call SORTS(A,Bd=B)

end subroutine SORTSD

subroutine SORTLL(A,B)
  integer(kind=8),           intent(INOUT) :: A(:)
  integer(kind=8),           intent(INOUT) :: B(:)

  call SORTL(A,Bl=B)

end subroutine SORTLL


subroutine SORTLR(A,B)
  integer(kind=8),           intent(INOUT) :: A(:)
  real   (kind=4),           intent(INOUT) :: B(:)

  call SORTL(A,Br=B)

end subroutine SORTLR

subroutine SORTLD(A,B)
  integer(kind=8),           intent(INOUT) :: A(:)
  real   (kind=8),           intent(INOUT) :: B(:)

  call SORTL(A,Bd=B)

end subroutine SORTLD

subroutine SORT2SS(A,C,DIM)
  integer(kind=4),           intent(INOUT) :: A(:)
  integer(kind=4),           intent(INOUT) :: C(:,:)
  integer,         optional, intent(IN   ) :: DIM

  call SORTS(A,Ci=C,DIM=DIM)

end subroutine SORT2SS


subroutine SORT2SL(A,C,DIM)
  integer(kind=4),           intent(INOUT) :: A(:)
  integer(kind=8),           intent(INOUT) :: C(:,:)
  integer,         optional, intent(IN   ) :: DIM

  call SORTS(A,Cl=C,DIM=DIM)

end subroutine SORT2SL


subroutine SORT2SR(A,C,DIM)
  integer(kind=4),           intent(INOUT) :: A(:)
  real   (kind=4),           intent(INOUT) :: C(:,:)
  integer,         optional, intent(IN   ) :: DIM

  call SORTS(A,Cr=C,DIM=DIM)

end subroutine SORT2SR

subroutine SORT2SD(A,C,DIM)
  integer(kind=4),           intent(INOUT) :: A(:)
  real   (kind=8),           intent(INOUT) :: C(:,:)
  integer,         optional, intent(IN   ) :: DIM

  call SORTS(A,Cd=C,DIM=DIM)

end subroutine SORT2SD

subroutine SORT2LS(A,C,DIM)
  integer(kind=8),           intent(INOUT) :: A(:)
  integer(kind=4),           intent(INOUT) :: C(:,:)
  integer,         optional, intent(IN   ) :: DIM

  call SORTL(A,Ci=C,DIM=DIM)

end subroutine SORT2LS


subroutine SORT2LL(A,C,DIM)
  integer(kind=8),           intent(INOUT) :: A(:)
  integer(kind=8),           intent(INOUT) :: C(:,:)
  integer,         optional, intent(IN   ) :: DIM

  call SORTL(A,Cl=C,DIM=DIM)

end subroutine SORT2LL


subroutine SORT2LR(A,C,DIM)
  integer(kind=8),           intent(INOUT) :: A(:)
  real   (kind=4),           intent(INOUT) :: C(:,:)
  integer,         optional, intent(IN   ) :: DIM

  call SORTL(A,Cr=C,DIM=DIM)

end subroutine SORT2LR

subroutine SORT2LD(A,C,DIM)
  integer(kind=8),           intent(INOUT) :: A(:)
  real   (kind=8),           intent(INOUT) :: C(:,:)
  integer,         optional, intent(IN   ) :: DIM

  call SORTL(A,Cd=C,DIM=DIM)

end subroutine SORT2LD

subroutine SORT2AS(A,Ci,Cl,Cr,Cd,DIM)
  integer(kind=4),           intent(INOUT) :: A(:,:)
  integer(kind=4), optional, intent(INOUT) :: Ci(:,:)
  integer(kind=8), optional, intent(INOUT) :: Cl(:,:)
  real   (kind=4), optional, intent(INOUT) :: Cr(:,:)
  real   (kind=8), optional, intent(INOUT) :: Cd(:,:)
  integer,         optional, intent(IN   ) :: DIM

  integer :: uDIM

  if(present(DIM)) then
     uDIM = DIM
  else
     uDIM = 2
  end if

  ASSERT_(uDIM>0 .and. uDIM<3)

  if(uDIM==1) then
     call SORTS(A(:,1),Ci=A(:,2:),Di=Ci,Dl=Cl,Dr=Cr,Dd=Cd,DIM=DIM)
  else                                        
     call SORTS(A(1,:),Ci=A(2:,:),Di=Ci,Dl=Cl,Dr=Cr,Dd=Cd,DIM=DIM)
  end if
end subroutine SORT2AS

subroutine SORT2AL(A,Ci,Cl,Cr,Cd,DIM)
  integer(kind=8),           intent(INOUT) :: A(:,:)
  integer(kind=4), optional, intent(INOUT) :: Ci(:,:)
  integer(kind=8), optional, intent(INOUT) :: Cl(:,:)
  real   (kind=4), optional, intent(INOUT) :: Cr(:,:)
  real   (kind=8), optional, intent(INOUT) :: Cd(:,:)
  integer,         optional, intent(IN   ) :: DIM

  integer :: uDIM

  if(present(DIM)) then
     uDIM = DIM
  else
     uDIM = 2
  end if

  ASSERT_(uDIM>0 .and. uDIM<3)

  if(uDIM==1) then
     call SORTL(A(:,1),Cl=A(:,2:),Di=Ci,Dl=Cl,Dr=Cr,Dd=Cd,DIM=DIM)
  else
     call SORTL(A(1,:),Cl=A(2:,:),Di=Ci,Dl=Cl,Dr=Cr,Dd=Cd,DIM=DIM)
  end if
end subroutine SORT2AL

end module MAPL_SortMod
