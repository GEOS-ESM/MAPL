subroutine foo
USE mapl_sortMod

integer*8 :: a(5), c(5)
integer*4 :: a4(5), c4(5)
integer*4 ::  b1(5,3), b2(3,5)
integer*4 ::  d1(5,3), d2(3,5)


a = (/5,3,8,6,-7/)
do j=1,3
b1(:,j) = a
b2(j,:) = a
enddo
a4 = a


!C$c=a
!C$call qsort(c,d2,5,0)
!C$print *, c
!C$
!C$c=a
!C$d2=b2
!C$call qsort(c,d2,5,3)
!C$print *, c
!C$
!C$do j=1,3
!C$print *, j, '===', d2(j,:)
!C$end do
!C$
!C$
!C$c  = a
!C$d1 = b1
!C$call qsort(c,d1,5,-3)
!C$print *, c
!C$do j=1,3
!C$print *, j, '===', d1(:,j)
!C$end do


C=A
d1=b1
call MAPL_SORT(C,d1,dim=1)
PRINT *, C
do j=1,3
print *, j, '===', d1(:,j)
end do

end subroutine

subroutine mapl_initialize()
end subroutine mapl_initialize
