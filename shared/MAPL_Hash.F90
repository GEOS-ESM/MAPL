!------------------------------------------------------------------------------
!               Global Modeling and Assimilation Office (GMAO)                !
!                    Goddard Earth Observing System (GEOS)                    !
!                                 MAPL Component                              !
!------------------------------------------------------------------------------
!
#define INT_MAX 2147483647

#include "MAPL_ErrLog.h"
!
!>
!### MODULE: `MAPL_HashMod`
!
! Author: GMAO SI-Team
!
! `MAPL_HashMod`  -- A utility to manage hash tables.
! 
! `MAPL_HashMod` is a FORTRAN binding to a simple C has facility.
!
! The API is:
!```fortran
!
!   ! Create a hash table with Nbuckets
!
!       integer function MAPL_HashCreate(Nbuckets)
!         integer, intent(IN) :: Nbuckets
! 
!   ! Update table Hash with integer[s] i[,j]
!   ! The return value is the order of occurence of the integer[s].
!   ! If i is new, the return value is the new hash size.
!
!       integer function MAPL_HashIncrement(Hash,i,j)
!         integer,           intent(IN) :: Hash
!         integer,           intent(IN) :: i
!         integer, optional, intent(IN) :: j
!
!   ! Dump the list of integers or integer pairs in the hash.
!   !  The list is in no particular order.
!   ! If the arrays are not long enough, nothing is dumped and -1
!   !  is returned; otherwise it returns the current hash size 
!   !  (the length of the list).
!
!       integer function MAPL_HashDump(Hash,i,j)
!         integer,           intent(IN)  :: Hash
!         integer,           intent(OUT) :: i(:)
!         integer, optional, intent(OUT) :: j(:)
!
!   ! Get the size of a hash
!
!       integer function MAPL_HashSize(Hash)
!         integer, intent(IN) :: Hash
!
!   ! Destroy a hash table
!
!       subroutine MAPL_HashDestroy(Hash)
!         integer, intent(IN) :: Hash
!```
!
! The following is a sample usage that makes a list of
! unique integers in the large array II. It can similarly
! be used to find ordered pairs of integers. The asserts
! are put in to clarify the usage.
!
!```fortran       
!       integer :: Hash, k, II(100000), FoundOrder(10000)
!
!       Hash = MAPL_HashCreate(1000)
!
!       latest = 0
!       do i=1,100000
!         k = MAPL_HashIncrement(Hash,ii(i))
!         if(k>latest) then
!           latest   = k
!           isnew    = .true.
!           FoundOrder(k) = ii(i)
!           _ASSERT(k==MAPL_HashSize(Hash),'needs informative message')
!         else
!           isnew = .false.
!           _ASSERT(FoundOrder(k)==ii(i),'needs informative message')
!         endif
!       enddo
!```
!
module MAPL_HashMod

  use MAPL_ExceptionHandling

  implicit none
  private

! !PUBLIC ROUTINES:

  public MAPL_HashCreate
  public MAPL_HashIncrement
  public MAPL_HashDestroy
  public MAPL_HashSize
  public MAPL_HashDump

!=============================================================================

contains

integer function  MAPL_HashCreate(Nbuckets)
  integer,           intent(IN) :: Nbuckets

  integer CREATEHASH
  MAPL_HashCreate = CREATEHASH(Nbuckets)

end function MAPL_HashCreate

!----------------------------------------------

integer function MAPL_HashIncrement(Hash,i,j,k)
  integer,           intent(IN) :: Hash
  integer,           intent(IN) :: i
  integer, optional, intent(IN) :: j
  integer, optional, intent(IN) :: k

  integer :: INCREMENTHASH, rc

  if    (present(k)) then
     _ASSERT(present(j),'needs informative message')
     MAPL_HashIncrement = INCREMENTHASH(HASH,I,J,K)
  elseif(present(j)) then
     MAPL_HashIncrement = INCREMENTHASH(HASH,I,J,INT_MAX)
  else
     MAPL_HashIncrement = INCREMENTHASH(HASH,I,INT_MAX,INT_MAX)
  endif

end function MAPL_HashIncrement

!----------------------------------------------

subroutine MAPL_HashDestroy(Hash)
  integer, intent(IN) :: Hash

  call DESTROYHASH(HASH)

end subroutine MAPL_HashDestroy

!----------------------------------------------

integer function MAPL_HashDump(Hash,i,j)
  integer, intent(IN ) :: Hash
  integer, intent(OUT) :: i(:)
  integer, optional, intent(OUT) :: j(:)

  integer, allocatable :: jj(:)

  integer :: rc

  MAPL_HashDump = MAPL_HashSize(HASH)

  if(size(i) < MAPL_HashSize(HASH)) then
     MAPL_HashDump = -1
     return
  end if

  if(present(j)) then
     _ASSERT(size(i) == size(j),'needs informative message')
     call DUMPHASH(HASH,I,J)
  else
     allocate(jj(size(i)))
     call DUMPHASH(HASH,I,JJ)
     deallocate(JJ)
  end if

end function MAPL_HashDump

!----------------------------------------------

integer function  MAPL_HashSize(Hash)
  integer, intent(IN) :: Hash

  integer HASHSIZE
  MAPL_HashSize = HASHSIZE(Hash)

end function MAPL_HashSize

!----------------------------------------------

end module MAPL_HashMod
