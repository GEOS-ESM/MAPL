!-----------------------------------------------------
! Yonggang G. Yu 
! University of Minnesota
! yuxxx135@umn.edu
!-----------------------------------------------------





!-----------------------------------------
!  solve special A X = b  problem
!-----------------------------------------
!
subroutine AX_b_square_linear (A, n, np, b, x)
!
!       solve  A   x  =   b    square matrix
!
!
  implicit none
  integer, intent (in)  :: n, np
  real*8,  intent (in)  :: A(np, np), b(np)
  real*8,  intent (out) :: x(np)

  real*8  :: A_aux(np, np), d
  real*8  :: b_aux(n)     ! b_aux has dim=n not np. 
  integer :: indx(n)

  A_aux(1:np,1:np)=A(1:np,1:np)
  b_aux(1:n)=b(1:n)
  call ludcmp (A_aux, n, np, indx, d)       ! A= L*U
  call lubksb (A_aux, n, np, indx, b_aux)   ! A*x=b,  x--> b
  x(1:np)=0.d0
  x(1:n)=b_aux(1:n)
  return
end subroutine AX_b_square_linear


subroutine AX_b_overdeterm (A, y, m, n, mp, np, xce)
!
!       solve  A   Xce  =   Y
!
!
  implicit none
  integer, intent (in)  :: m, n, mp, np
  real*8,  intent (in)  :: A(mp, np), y(mp)
  real*8,  intent (out) :: xce(np)
  ! local var
  real*8  :: wmax, wmin
  integer :: m1,   n1
  real*8, allocatable ::  uu(:,:), vv(:,:), w(:)
  real*8  :: AX(mp), diff(mp), res          !  residual


  allocate (uu(mp, np), w(np), vv(np, np))
  uu(:,:)=a(:,:)      ! copy to avoid overwrite in svd
  !
  write(6,*) ' A matrix'
  do m1=1, m
     write(6,'(20f16.5)') uu(m1,1:n)
  enddo
  !
  call dsvdcmp(uu,m,n,mp,np,w,vv) ! svd matrix a
  !
  write(6,*) ' u matrix'
  do m1=1, m
     write(6,'(20f16.5)') uu(m1,1:n)
  enddo
  write(6,*) ' vv matrix'
  do m1=1, n
     write(6,'(20f16.5)') vv(m1,1:n)
  enddo
  write(6,*) ' w matrix ---- singular values'
  write(6,'(20f16.5)') w(1:n)

  wmax=0.d0
  do n1=1, n
     if(w(n1).gt.wmax) wmax=w(n1)
  enddo
  wmin=wmax*1.d-6    ! threshold for singular values
  do n1=1, n
     if(w(n1).lt.wmin) w(n1)=0.d0
  enddo
  !
  write(6,*) ' wmax, wmin'
  write(6,'(20f16.5)') wmax, wmin
  write(6,*) ' refinded w matrix ---- singular values'
  write(6,'(20f16.5)') w(1:n)



  call dsvbksb(uu,w,vv,m,n,mp,np,y,xce)
  uu(:,:)=a(:,:)
  call AX_mult (uu, xce, AX, m, n, mp, np)
  diff(1:mp)=AX(1:mp)-Y(1:mp)
  call X_2norm (diff, res, m)

  write(6, '(a)'      ) 'svd fit ce(1:np,j) ='
  write(6, '(10f12.4)') xce(1:n)
  write(6, '(a)'      ) 'AX'
  write(6, '(10f12.4)') AX(1:m)
  write(6, '(a)'      ) 'Y'
  write(6, '(10f12.4)') Y(1:m)
  write(6, '(a)'      ) 'diff= AX - Y'
  write(6, '(10f12.4)') diff(1:m)
  write(6, '(a)'      ) 'residual =  || AX - Y ||^2'
  write(6, '(f12.4,/)') res
  deallocate (uu, w, vv)
  return
end subroutine AX_b_overdeterm





subroutine vandermonde (x, y, m, n, mp, np, xce)
!
!       solve F= c0 + c1*f + c2*f^2 + c3*f^3
!
!
!	 |  1   f_1   f_1^2   f_1^3  |   ce_1  =  |  y_1  |       
!	 |  1   f_2   f_2^2   f_2^3  |   ce_2  =  |  y_2  |
!
!	 ...
!
!	 |  1   f_m   f_m^2   f_m^3  |   ce_4  =  |  y_m  |
!
!
  integer, intent (in)  :: m, n, mp, np
  real*8,  intent (in)  :: x(mp), y(mp)
  real*8,  intent (out) :: xce(np)
  ! local var
  real*8  :: wmax, wmin
  integer :: m1,   n1
  real*8, allocatable :: a(:,:), uu(:,:), vv(:,:), w(:)
  allocate (a(mp, np), uu(mp, np), w(np), vv(np, np))

  !  foreach j fit dU(j)= c0 + c1*f + c2*f^2 + c3*f^3
  a(:,:)=1.d0
  do m1=1,mp
     do n1=2, np
        a(m1,n1)=x(m1)**dble(n1-1)
     enddo
  enddo
  uu(:,:)=a(:,:)      ! copy to avoid overwrite in svd
  !
  write(6,*) ' A matrix'
  do m1=1, mp
     write(6,'(20f16.5)') uu(m1,:)
  enddo
  !
  call dsvdcmp(uu,m,n,mp,np,w,vv) ! svd matrix a
  !
  write(6,*) ' u matrix'
  do m1=1, mp
     write(6,'(20f16.5)') uu(m1,:)
  enddo
  write(6,*) ' vv matrix'
  do m1=1, np
     write(6,'(20f16.5)') vv(m1,:)
  enddo
  write(6,*) ' w matrix ---- singular values'
  write(6,'(20f16.5)') w(1:np)
	
  wmax=0.d0
  do n1=1, np
     if(w(n1).gt.wmax) wmax=w(n1)
  enddo
  wmin=wmax*1.d-6    ! threshold for singular values
  do n1=1, np
     if(w(n1).lt.wmin) w(n1)=0.d0
  enddo
  call dsvbksb(uu,w,vv,m,n,mp,np,y,xce)
  write(6, '(a)') ' svd fit ce(1:np,j) ='
  write(6, '(10f12.2)') xce(:)  
  deallocate (a, uu, w, vv)
  return
end subroutine vandermonde




subroutine dsvdcmp(a,m,n,mp,np,w,v)
      INTEGER m,mp,n,np,NMAX
      DOUBLE PRECISION a(mp,np),v(np,np),w(np)
      PARAMETER (NMAX=500)
!CU    USES dpythag
      INTEGER i,its,j,jj,k,l,nm
      DOUBLE PRECISION anorm,c,f,g,h,s,scale,x,y,z,rv1(NMAX),dpythag
      g=0.0d0
      scale=0.0d0
      anorm=0.0d0
      do 25 i=1,n
        l=i+1
        rv1(i)=scale*g
        g=0.0d0
        s=0.0d0
        scale=0.0d0
        if(i.le.m)then
          do 11 k=i,m
            scale=scale+abs(a(k,i))
11        continue
          if(scale.ne.0.0d0)then
            do 12 k=i,m
              a(k,i)=a(k,i)/scale
              s=s+a(k,i)*a(k,i)
12          continue
            f=a(i,i)
            g=-sign(sqrt(s),f)
            h=f*g-s
            a(i,i)=f-g
            do 15 j=l,n
              s=0.0d0
              do 13 k=i,m
                s=s+a(k,i)*a(k,j)
13            continue
              f=s/h
              do 14 k=i,m
                a(k,j)=a(k,j)+f*a(k,i)
14            continue
15          continue
            do 16 k=i,m
              a(k,i)=scale*a(k,i)
16          continue
          endif
        endif
        w(i)=scale *g
        g=0.0d0
        s=0.0d0
        scale=0.0d0
        if((i.le.m).and.(i.ne.n))then
          do 17 k=l,n
            scale=scale+abs(a(i,k))
17        continue
          if(scale.ne.0.0d0)then
            do 18 k=l,n
              a(i,k)=a(i,k)/scale
              s=s+a(i,k)*a(i,k)
18          continue
            f=a(i,l)
            g=-sign(sqrt(s),f)
            h=f*g-s
            a(i,l)=f-g
            do 19 k=l,n
              rv1(k)=a(i,k)/h
19          continue
            do 23 j=l,m
              s=0.0d0
              do 21 k=l,n
                s=s+a(j,k)*a(i,k)
21            continue
              do 22 k=l,n
                a(j,k)=a(j,k)+s*rv1(k)
22            continue
23          continue
            do 24 k=l,n
              a(i,k)=scale*a(i,k)
24          continue
          endif
        endif
        anorm=max(anorm,(abs(w(i))+abs(rv1(i))))
25    continue
      do 32 i=n,1,-1
        if(i.lt.n)then
          if(g.ne.0.0d0)then
            do 26 j=l,n
              v(j,i)=(a(i,j)/a(i,l))/g
26          continue
            do 29 j=l,n
              s=0.0d0
              do 27 k=l,n
                s=s+a(i,k)*v(k,j)
27            continue
              do 28 k=l,n
                v(k,j)=v(k,j)+s*v(k,i)
28            continue
29          continue
          endif
          do 31 j=l,n
            v(i,j)=0.0d0
            v(j,i)=0.0d0
31        continue
        endif
        v(i,i)=1.0d0
        g=rv1(i)
        l=i
32    continue
      do 39 i=min(m,n),1,-1
        l=i+1
        g=w(i)
        do 33 j=l,n
          a(i,j)=0.0d0
33      continue
        if(g.ne.0.0d0)then
          g=1.0d0/g
          do 36 j=l,n
            s=0.0d0
            do 34 k=l,m
              s=s+a(k,i)*a(k,j)
34          continue
            f=(s/a(i,i))*g
            do 35 k=i,m
              a(k,j)=a(k,j)+f*a(k,i)
35          continue
36        continue
          do 37 j=i,m
            a(j,i)=a(j,i)*g
37        continue
        else
          do 38 j= i,m
            a(j,i)=0.0d0
38        continue
        endif
        a(i,i)=a(i,i)+1.0d0
39    continue
      do 49 k=n,1,-1
        do 48 its=1,30
          do 41 l=k,1,-1
            nm=l-1
            if((abs(rv1(l))+anorm).eq.anorm)  goto 2
            if((abs(w(nm))+anorm).eq.anorm)  goto 1
41        continue
1         c=0.0d0
          s=1.0d0
          do 43 i=l,k
            f=s*rv1(i)
            rv1(i)=c*rv1(i)
            if((abs(f)+anorm).eq.anorm) goto 2
            g=w(i)
            h=dpythag(f,g)
            w(i)=h
            h=1.0d0/h
            c= (g*h)
            s=-(f*h)
            do 42 j=1,m
              y=a(j,nm)
              z=a(j,i)
              a(j,nm)=(y*c)+(z*s)
              a(j,i)=-(y*s)+(z*c)
42          continue
43        continue
2         z=w(k)
          if(l.eq.k)then
            if(z.lt.0.0d0)then
              w(k)=-z
              do 44 j=1,n
                v(j,k)=-v(j,k)
44            continue
            endif
            goto 3
          endif
          if(its.eq.30) stop 'no convergence in svdcmp'
          x=w(l)
          nm=k-1
          y=w(nm)
          g=rv1(nm)
          h=rv1(k)
          f=((y-z)*(y+z)+(g-h)*(g+h))/(2.0d0*h*y)
          g=dpythag(f,1.0d0)
          f=((x-z)*(x+z)+h*((y/(f+sign(g,f)))-h))/x
          c=1.0d0
          s=1.0d0
          do 47 j=l,nm
            i=j+1
            g=rv1(i)
            y=w(i)
            h=s*g
            g=c*g
            z=dpythag(f,h)
            rv1(j)=z
            c=f/z
            s=h/z
            f= (x*c)+(g*s)
            g=-(x*s)+(g*c)
            h=y*s
            y=y*c
            do 45 jj=1,n
              x=v(jj,j)
              z=v(jj,i)
              v(jj,j)= (x*c)+(z*s)
              v(jj,i)=-(x*s)+(z*c)
45          continue
            z=dpythag(f,h)
            w(j)=z
            if(z.ne.0.0d0)then
              z=1.0d0/z
              c=f*z
              s=h*z
            endif
            f= (c*g)+(s*y)
            x=-(s*g)+(c*y)
            do 46 jj=1,m
              y=a(jj,j)
              z=a(jj,i)
              a(jj,j)= (y*c)+(z*s)
              a(jj,i)=-(y*s)+(z*c)
46          continue
47        continue
          rv1(l)=0.0d0
          rv1(k)=f
          w(k)=x
48      continue
3       continue
49    continue
      return
      END
!C  (C) Copr. 1986-92 Numerical Recipes Software '>9m_L3.


      FUNCTION dpythag(a,b)
      DOUBLE PRECISION a,b,dpythag
      DOUBLE PRECISION absa,absb
      absa=abs(a)
      absb=abs(b)
      if(absa.gt.absb)then
        dpythag=absa*sqrt(1.0d0+(absb/absa)**2)
      else
        if(absb.eq.0.0d0)then
          dpythag=0.0d0
        else
          dpythag=absb*sqrt(1.0d0+(absa/absb)**2)
        endif
      endif
      return
      END
!C  (C) Copr. 1986-92 Numerical Recipes Software '>9m_L3.



subroutine dsvbksb(u,w,v,m,n,mp,np,b,x)
      INTEGER m,mp,n,np,NMAX
      DOUBLE PRECISION b(mp),u(mp,np),v(np,np),w(np),x(np)
      PARAMETER (NMAX=500)
      INTEGER i,j,jj
      DOUBLE PRECISION s,tmp(NMAX)
      do 12 j=1,n
        s=0.0d0
        if(w(j).ne.0.0d0)then
          do 11 i=1,m
            s=s+u(i,j)*b(i)
11        continue
          s=s/w(j)
        endif
        tmp(j)=s
12    continue
      do 14 j=1,n
        s=0.0d0
        do 13 jj=1,n
          s=s+v(j,jj)*tmp(jj)
13      continue
        x(j)=s
14    continue
      return
      END
!C  (C) Copr. 1986-92 Numerical Recipes Software '>9m_L3.
!------



SUBROUTINE ludcmp(a,n,np,indx,d)
      INTEGER n,np,indx(n),NMAX
      REAL*8 d,a(np,np),TINY
      PARAMETER (NMAX=500,TINY=1.0e-20)
      INTEGER i,imax,j,k
      REAL*8 aamax,dum,sum,vv(NMAX)
      d=1.
      do 12 i=1,n
        aamax=0.
        do 11 j=1,n
          if (abs(a(i,j)).gt.aamax) aamax=abs(a(i,j))
11      continue
        if (aamax.eq.0.) stop 'singular matrix in ludcmp'
        vv(i)=1./aamax
12    continue
      do 19 j=1,n
        do 14 i=1,j-1
          sum=a(i,j)
          do 13 k=1,i-1
            sum=sum-a(i,k)*a(k,j)
13        continue
          a(i,j)=sum
14      continue
        aamax=0.
        do 16 i=j,n
          sum=a(i,j)
          do 15 k=1,j-1
            sum=sum-a(i,k)*a(k,j)
15        continue
          a(i,j)=sum
          dum=vv(i)*abs(sum)
          if (dum.ge.aamax) then
            imax=i
            aamax=dum
          endif
16      continue
        if (j.ne.imax)then
          do 17 k=1,n
            dum=a(imax,k)
            a(imax,k)=a(j,k)
            a(j,k)=dum
17        continue
          d=-d
          vv(imax)=vv(j)
        endif
        indx(j)=imax
        if(a(j,j).eq.0.)a(j,j)=TINY
        if(j.ne.n)then
          dum=1./a(j,j)
          do 18 i=j+1,n
            a(i,j)=a(i,j)*dum
18        continue
        endif
19    continue
      return
END SUBROUTINE ludcmp
!  (C) Copr. 1986-92 Numerical Recipes Software '>9m_L3.



SUBROUTINE lubksb(a,n,np,indx,b)
      INTEGER n,np,indx(n)
      REAL*8 a(np,np),b(n)
      INTEGER i,ii,j,ll
      REAL*8 sum
      ii=0
      do 12 i=1,n
        ll=indx(i)
        sum=b(ll)
        b(ll)=b(i)
        if (ii.ne.0)then
          do 11 j=ii,i-1
            sum=sum-a(i,j)*b(j)
11        continue
        else if (sum.ne.0.) then
          ii=i
        endif
        b(i)=sum
12    continue
      do 14 i=n,1,-1
        sum=b(i)
        do 13 j=i+1,n
          sum=sum-a(i,j)*b(j)
13      continue
        b(i)=sum/a(i,i)
14    continue
      return
END SUBROUTINE lubksb
!  (C) Copr. 1986-92 Numerical Recipes Software '>9m_L3.



      SUBROUTINE gaussj(a,n,np,b,m,mp)
      INTEGER m,mp,n,np,NMAX
      REAL*8  a(np,np),b(np,mp)
      PARAMETER (NMAX=50)
      INTEGER i,icol,irow,j,k,l,ll,indxc(NMAX),indxr(NMAX),ipiv(NMAX)
      REAL*8  big,dum,pivinv
      do 11 j=1,n
        ipiv(j)=0
11    continue
      do 22 i=1,n
        big=0.d0
        do 13 j=1,n
          if(ipiv(j).ne.1)then
            do 12 k=1,n
              if (ipiv(k).eq.0) then
                if (abs(a(j,k)).ge.big)then
                  big=abs(a(j,k))
                  irow=j
                  icol=k
                endif
              else if (ipiv(k).gt.1) then
                pause 'singular matrix in gaussj'
              endif
12          continue
          endif
13      continue
        ipiv(icol)=ipiv(icol)+1
        if (irow.ne.icol) then
          do 14 l=1,n
            dum=a(irow,l)
            a(irow,l)=a(icol,l)
            a(icol,l)=dum
14        continue
          do 15 l=1,m
            dum=b(irow,l)
            b(irow,l)=b(icol,l)
            b(icol,l)=dum
15        continue
        endif
        indxr(i)=irow
        indxc(i)=icol
        if (a(icol,icol).eq.0.) pause 'singular matrix in gaussj'
        pivinv=1./a(icol,icol)
        a(icol,icol)=1.
        do 16 l=1,n
          a(icol,l)=a(icol,l)*pivinv
16      continue
        do 17 l=1,m
          b(icol,l)=b(icol,l)*pivinv
17      continue
        do 21 ll=1,n
          if(ll.ne.icol)then
            dum=a(ll,icol)
            a(ll,icol)=0.
            do 18 l=1,n
              a(ll,l)=a(ll,l)-a(icol,l)*dum
18          continue
            do 19 l=1,m
              b(ll,l)=b(ll,l)-b(icol,l)*dum
19          continue
          endif
21      continue
22    continue
      do 24 l=n,1,-1
        if(indxr(l).ne.indxc(l))then
          do 23 k=1,n
            dum=a(k,indxr(l))
            a(k,indxr(l))=a(k,indxc(l))
            a(k,indxc(l))=dum
23        continue
        endif
24    continue
      return
      END subroutine
!  (C) Copr. 1986-92 Numerical Recipes Software '>9m_L3.




!================================== 
!           M A T R I X   
!================================== 
subroutine A_X_b( A, X, b, Y, n)
!    input : A, X, b, n
!   output : Y
    real*8 :: A(n, n), X(n), b(n), Y(n)
!   Y = AX + b
!    b(1:3)=0.d0
    if(n .ne. 3) stop '  error:  A_X_b, n .ne. 3'

    Y(1:3) =0.d0
    do i = 1, 3
       do j = 1, 3
       Y(i) = Y(i) + A(i,j)*X(j) 
       enddo
    enddo
    Y(1:3)= Y(1:3) + b(1:3)
    return
end subroutine A_X_b


subroutine AB_mult( A, B, C, m, n, p )
  implicit none
  integer :: m, n, p
  integer :: i, j, k
  real*8 ::  a(m, n), b(n, p), c(m, p), tmp1
  do i=1, m
     do j=1, p
        c(i,j) = 0.d0
        do k=1, n
           c(i,j) = c(i,j) + a(i,k)*b(k,j)
        enddo
     enddo
  enddo
  return
end subroutine


subroutine AX_mult(A, X, Y, m, n, mp, np)
  implicit none
  integer, intent(in)  :: m, n, mp, np
  real*8,  intent(in)  :: A(mp, np), X(np)
  real*8,  intent(out) :: Y(mp)
  real*8   :: tmp
  integer  :: i, j

  Y(1:mp)= 0.d0
  do i=1, m
     tmp=0.d0
     do j=1, n
        tmp=tmp+A(i,j)*X(j)
     enddo
     Y(i)=tmp
  enddo
  return
end subroutine AX_mult



subroutine matrix_inv( A, A_inv, n, np )
  implicit none
  integer :: n, np, indx(np)
  integer :: i, j
  real*8 :: a(np, np), a_1(np, np), y(np, np), a_inv(np, np), d

  a_1(:,:)=a(:,:)                ! copy
  y(:,:) = 0.d0                  ! identity matrix
  do i=1, np
     y(i,i) = 1.d0
  enddo
  call ludcmp( a, n, np, indx, d )
  do j=1,n
     call lubksb( a, n, np, indx, y(:,j) )
  enddo
  A_inv(:,:) = y(:,:)
  a(:,:)=a_1(:,:)                ! copy back
  return
end subroutine matrix_inv


subroutine matrix_det( A, A_det, n, np )
  implicit none
  integer :: n, np, indx(np)
  integer :: j
  real*8  :: a(np, np), a_1(np, np), A_det, d

  a_1(:,:)=a(:,:)                ! copy
  call ludcmp( a, n, np, indx, d )
  do j=1,n
     d = d * a(j,j)
  enddo
  A_det = d
  a(:,:)=a_1(:,:)                ! copy back
  return
end subroutine matrix_det





!=======================================
!         V E C T O R  
!=======================================
subroutine X_2norm( x, xnm, n )
! xnm = sqrt( x dot x )
  real*8 :: X(n), tmp, xnm
  integer :: n, i
  tmp=0.d0
  do i = 1, n
     tmp= tmp+x(i)*x(i)
  enddo
  xnm = dsqrt( tmp )
  return
end subroutine

subroutine angle( v1, v2, beta )
    real*8, parameter ::  pi = 3.14159265357159d0
    real*8 :: v1(3), v2(3), beta, temp1(3), tmp1
    temp1(1:3)= 0.d0
    do i=1,3
       temp1(1) = temp1(1) + v1(i)*v2(i)
    enddo
    do i=1,3
       temp1(2) = temp1(2) + v1(i)*v1(i)
    enddo
    do i=1,3
       temp1(3) = temp1(3) + v2(i)*v2(i)
    enddo
    tmp1 = temp1(1)/ dsqrt( temp1(2)*temp1(3) )
    beta = dacos( tmp1 ) / pi * 180
    return
end subroutine 


subroutine  XY_cross( X, Y, Z, n)
  ! X cross Y = Z
  integer, intent(in)  :: n
  real*8,  intent(in)  :: X(n), Y(n)
  real*8,  intent(out) :: Z(n)

  if ( n.NE.3 )  then
     stop 'dim >3 wrong no cross defined'
  endif
  Z(1) = X(2)*Y(3) - X(3)*Y(2)
  Z(2) = X(3)*Y(1) - X(1)*Y(3)
  Z(3) = X(1)*Y(2) - X(2)*Y(1)
  return
end subroutine XY_cross



subroutine  XY_dot (X, Y, n, np, Z)
! Z = X dot Y
  integer, intent(in)  :: n, np
  real*8,  intent(in)  :: X(np), Y(np)
  real*8,  intent(out) :: Z
  !
  integer :: i
  Z=0.d0
  do i=1, n
     Z=Z+X(i)*Y(i)
  enddo
  return
end subroutine XY_dot



subroutine  XY_dot_ir (IX, Y, n, np, Z)
! Z = X dot Y
  integer, intent(in)  :: n, np
  integer, intent(in)  :: IX(np)
  real*8,  intent(in)  :: Y(np)
  real*8,  intent(out) :: Z
  !
  integer :: i
  Z=0.d0
  do i=1, n
     Z=Z+dble(IX(i))*Y(i)
  enddo
  return
end subroutine XY_dot_ir




!=======================================
!         S O R T 
!=======================================
subroutine sort_reduce (n, nuniq, x, y, bg, ndeg, iovwt)
  integer, intent (in)  :: n
  logical, intent (in)  :: iovwt
  integer, intent (out) :: nuniq
  real*8,  intent (inout) :: x(n)
  real*8,  intent (out) :: y(n)
  integer, intent (out) :: bg(n)
  integer, intent (out) :: ndeg(n)

  integer :: i, j
  integer :: iwksp(n)
  real*8  :: wksp(n)
  real*8  :: tol

!----
!          0-0-0-0
!          |
!      #-#-#
!      |
!  *-*-*
!----

  if (n.le.1) then
     STOP 'sort_reduce, n<= 1, stop'
  endif
  tol=1.d-8
  call indexx (n, x, iwksp)
  do i=1, n
     wksp(i)=x(iwksp(i))
  enddo
  if (iovwt) then
     x(1:n)=wksp(1:n)
  endif
  bg(1)=1
  y(1)=wksp(1)
  k=1
  do i=2, n
     if (wksp(i)-wksp(i-1).gt.tol) then
        k=k+1
        bg(k)=i
        y(k)=wksp(i)
     endif
  enddo
  nuniq=k
  do i=1, nuniq-1
     ndeg(i)=bg(i+1)-bg(i)
  enddo
  ndeg(nuniq)=n-bg(nuniq)+1

  return
end subroutine sort_reduce




SUBROUTINE sort3(n,ra,rb,rc,wksp,iwksp)
      INTEGER n,iwksp(n)
      REAL*8 ra(n),rb(n),rc(n),wksp(n)
!CU    USES indexx
      INTEGER j
      call indexx(n,ra,iwksp)
      do 11 j=1,n
        wksp(j)=ra(j)
11    continue
      do 12 j=1,n
        ra(j)=wksp(iwksp(j))
12    continue
      do 13 j=1,n
        wksp(j)=rb(j)
13    continue
      do 14 j=1,n
        rb(j)=wksp(iwksp(j))
14    continue
      do 15 j=1,n
        wksp(j)=rc(j)
15    continue
      do 16 j=1,n
        rc(j)=wksp(iwksp(j))
16    continue
      return
      END
!C  (C) Copr. 1986-92 Numerical Recipes Software '>9m_L3.


SUBROUTINE sort4r(n,ra,rb,rc,rd,wksp,iwksp)
      INTEGER n,iwksp(n)
      REAL*8 ra(n),rb(n),rc(n),rd(n),wksp(n)
!CU    USES indexx
      INTEGER j
      call indexx(n,ra,iwksp)
      do 11 j=1,n
        wksp(j)=ra(j)
11    continue
      do 12 j=1,n
        ra(j)=wksp(iwksp(j))
12    continue
      do 13 j=1,n
        wksp(j)=rb(j)
13    continue
      do 14 j=1,n
        rb(j)=wksp(iwksp(j))
14    continue
      do 15 j=1,n
        wksp(j)=rc(j)
15    continue
      do 16 j=1,n
        rc(j)=wksp(iwksp(j))
16    continue
      !
      wksp(1:n)=rd(1:n)
      do 17 j=1,n
        rd(j)=wksp(iwksp(j))
17    continue
      return
      END
!C  (C) Copr. 1986-92 Numerical Recipes Software '>9m_L3.



SUBROUTINE indexx(n,arr,indx)
      INTEGER n,indx(n),M,NSTACK
      REAL*8 arr(n)
      PARAMETER (M=7,NSTACK=50)
      INTEGER i,indxt,ir,itemp,j,jstack,k,l,istack(NSTACK)
      REAL*8 a
      do 11 j=1,n
        indx(j)=j
11    continue
      jstack=0
      l=1
      ir=n
1     if(ir-l.lt.M)then
        do 13 j=l+1,ir
          indxt=indx(j)
          a=arr(indxt)
          do 12 i=j-1,1,-1
            if(arr(indx(i)).le.a)goto 2
            indx(i+1)=indx(i)
12        continue
          i=0
2         indx(i+1)=indxt
13      continue
        if(jstack.eq.0)return
        ir=istack(jstack)
        l=istack(jstack-1)
        jstack=jstack-2
      else
        k=(l+ir)/2
        itemp=indx(k)
        indx(k)=indx(l+1)
        indx(l+1)=itemp
        if(arr(indx(l+1)).gt.arr(indx(ir)))then
          itemp=indx(l+1)
          indx(l+1)=indx(ir)
          indx(ir)=itemp
        endif
        if(arr(indx(l)).gt.arr(indx(ir)))then
          itemp=indx(l)
          indx(l)=indx(ir)
          indx(ir)=itemp
        endif
        if(arr(indx(l+1)).gt.arr(indx(l)))then
          itemp=indx(l+1)
          indx(l+1)=indx(l)
          indx(l)=itemp
        endif
        i=l+1
        j=ir
        indxt=indx(l)
        a=arr(indxt)
3       continue
          i=i+1
        if(arr(indx(i)).lt.a)goto 3
4       continue
          j=j-1
        if(arr(indx(j)).gt.a)goto 4
        if(j.lt.i)goto 5
        itemp=indx(i)
        indx(i)=indx(j)
        indx(j)=itemp
        goto 3
5       indx(l)=indx(j)
        indx(j)=indxt
        jstack=jstack+2
        if(jstack.gt.NSTACK) stop 'NSTACK too small in indexx'
        if(ir-i+1.ge.j-l)then
          istack(jstack)=ir
          istack(jstack-1)=i
          ir=j-1
        else
          istack(jstack)=j-1
          istack(jstack-1)=l
          l=i
        endif
      endif
      goto 1
      END
!  (C) Copr. 1986-92 Numerical Recipes Software '>9m_L3.



subroutine shufflen4(n, iwksp, nx1, nx2, nx3, nx4)
  integer, intent(in) :: n, iwksp(n)
  integer :: nx1(n), nx2(n), nx3(n), nx4(n)
  integer :: wksp(n), i
  wksp=nx1
  do i=1, n
     nx1(i)=wksp(iwksp(i))
  enddo
  wksp=nx2
  do i=1, n
     nx2(i)=wksp(iwksp(i))
  enddo
  wksp=nx3
  do i=1, n
     nx3(i)=wksp(iwksp(i))
  enddo
  wksp=nx4
  do i=1, n
     nx4(i)=wksp(iwksp(i))
  enddo
end subroutine shufflen4



!****************************************
!    C o m b i n a t i o n   N u m b e r
!****************************************

SUBROUTINE piksrt_ijk(n,arr)
      INTEGER :: n
      integer :: arr(n)
      INTEGER :: i,j
      REAL*8  :: a
      do 12 j=2,n
        a=arr(j)
        do 11 i=j-1,1,-1
          if(arr(i).le.a)goto 10
          arr(i+1)=arr(i)
11      continue
        i=0
10      arr(i+1)=a
12    continue
      return
end subroutine




subroutine allnr(n, r, j, ifault, kount, Cnr_f, mxcmb, rmx )   
       ! kount, matrix Cnr_f(1:r,kcount)
!org   subroutine allnr(n, r, j, ifault)
!
!         Algorithm AS 88  Appl. Statist. (1975) Vol.24, No. 3
!
!         When called once, generates all possible combinations
!         from a group of N items.  Each combination (represented in j as
!         r ordered integers between 1 and n) is processed within allnr.
!
!         Parameters:-
!        
!         n        integer             input:  The size of the group from which
!                                              the combinations are selected.
!
!         r        integer             input:  The size of each comination.
!
!         j        integer array(r)  workspace: Used by allnr to store
!                                               combinations.
!
!         ifault   integer            output:  Fault indicator, equal to:
!                                              0 if 1 le R le N;
!                                              1 otherwise.
!         output:
!
!         kount   integer 
!         
!         Cnr_f    int  arrray(r, kcount) output: Store all combination groups.
!
       implicit none
       integer, intent (in) :: n, r, rmx
       integer, intent (in) :: mxcmb

       integer, intent (out):: ifault
       integer, intent (out):: kount
       integer, intent (out):: Cnr_f(rmx, mxcmb)
       ! work space
       integer :: j(rmx)

       !local
       integer :: nmr, i, ip1, l
!       integer, n, r, rmx, j(rmx), ifault, kount, mxcmb, Cnr_f(rmx,mxcmb)
!
       ifault = 1
       if (r .lt. 1 .OR. r .gt. n) then
          if (r.eq.0 .OR. r.eq.n) then
             kount=1
          endif
          return
       endif
       ifault = 0
       kount = 0
       nmr = n - r
!
!         Initialize J(1) to lower limit separately, since lower limit for
!         each index depends on lower limit for previous index
!
       i = 1
       j(1) = 1
!
!         Initialize indices for loops i=1,...,r to lower limits
!
    1  if (i .eq. r) goto 3
       ip1 = i + 1
       do 2 l = ip1, r
    2  j(l) = j(l - 1) + 1
!
!         Update the count (kount) of combinations and process the current
!         combination.  The call to Subroutine job may be replaced by
!         statements to process the current combination.
    3  kount = kount + 1
       call job(n, r, j, kount)
       Cnr_f(1:r, kount) = j(1:r)
!
!         Increment the first possible index (of loop i) among indices of
!         loops R, R-1,...,1
!
       i = r
    4  if (j(i) .lt. nmr + i) goto 5
       i = i - 1
!
!         Return after all indices have achieved their upper limits
!
       if (i .le. 0) return
       goto 4
    5  j(i) = j(i) + 1
       goto 1
end subroutine


subroutine job (n, r, j, kount)
       integer :: r, j(r)
       integer :: n, kount
!       write(6, '(a, i4, a, 20i4 )' ) '# ', kount, ' -- ', j(1:r)
       return
end subroutine job


function combine(n, m)
   integer :: n, m
   integer :: combine    ! combine=C_n^m
   ! local
   integer :: i, kn, km, knm

   if (m.lt.0 .OR. m.gt.n) then
      stop 'wrong m<0 or m>n, in functional combine'
   elseif (m.eq.0 .OR. m.eq.n) then
      combine=1
   else
      knm=1
      do i=n, n-m+1, -1
         knm=knm*i
      enddo
      km=1
      do i=m, 1, -1
         km = km * i
      enddo
      combine= knm/km
   endif
   return
end function combine




subroutine mtch_2_real_vec(nB,ar1,ar2,lsame)
  implicit none
  real*8, parameter   :: tol=1.d-5
  integer, intent(in) :: nB
  real*8, intent(in)  :: ar1(nB), ar2(nB)
  logical, intent(out):: lsame

  integer :: j, iwksp1(nB), iwksp2(nB)
  real*8  :: x, y, z, res


  call indexx(nB,ar1,iwksp1)
  call indexx(nB,ar2,iwksp2)
  res=0.d0
  do j=1, nB
     x=ar1(iwksp1(j))
     y=ar2(iwksp2(j))
     z=x-y
     res=res+z*z
  enddo
  res=dsqrt(res)


  if (res.lt.tol) then
     lsame=.true.
  else
     lsame=.false.
  endif

  return
end subroutine mtch_2_real_vec



subroutine mtch_2_int_vec(nB,ar1,ar2,lsame)
  implicit none
  real*8, parameter   :: tol=1.d-5
  integer, intent(in) :: nB
  integer, intent(in) :: ar1(nB), ar2(nB)
  logical, intent(out):: lsame

  integer :: j, iwksp1(nB), iwksp2(nB)
  real*8  :: br1(nB), br2(nB)
  real*8  :: x, y, z, res


  do j=1, nB
     br1(j)=dble(ar1(j))
     br2(j)=dble(ar2(j))
  enddo

  call indexx(nB,br1,iwksp1)
  call indexx(nB,br2,iwksp2)
  res=0.d0
  do j=1, nB
     x=br1(iwksp1(j))
     y=br2(iwksp2(j))
     z=x-y
     res=res+z*z
  enddo
  res=dsqrt(res)


  if (res.lt.tol) then
     lsame=.true.
  else
     lsame=.false.
  endif

  return
end subroutine mtch_2_int_vec





!---------------------------------------
!     Random number generator  
!     Knuth subtractive method, NR p273
!
!---------------------------------------
      FUNCTION ran3(idum)
      INTEGER idum
      INTEGER MBIG,MSEED,MZ
!     REAL MBIG,MSEED,MZ
      REAL*8 :: ran3,FAC
      PARAMETER (MBIG=1000000000,MSEED=161803398,MZ=0,FAC=1.d0/MBIG)
!     PARAMETER (MBIG=4000000.,MSEED=1618033.,MZ=0.,FAC=1./MBIG)
      INTEGER i,iff,ii,inext,inextp,k
      INTEGER mj,mk,ma(55)
!     REAL mj,mk,ma(55)
      SAVE iff,inext,inextp,ma
      DATA iff /0/
      if(idum.lt.0.or.iff.eq.0)then
        iff=1
        mj=MSEED-iabs(idum)
        mj=mod(mj,MBIG)
        ma(55)=mj
        mk=1
        do 11 i=1,54
          ii=mod(21*i,55)
          ma(ii)=mk
          mk=mj-mk
          if(mk.lt.MZ)mk=mk+MBIG
          mj=ma(ii)
11      continue
        do 13 k=1,4
          do 12 i=1,55
            ma(i)=ma(i)-ma(1+mod(i+30,55))
            if(ma(i).lt.MZ)ma(i)=ma(i)+MBIG
12        continue
13      continue
        inext=0
        inextp=31
        idum=1
      endif
      inext=inext+1
      if(inext.eq.56)inext=1
      inextp=inextp+1
      if(inextp.eq.56)inextp=1
      mj=ma(inext)-ma(inextp)
      if(mj.lt.MZ)mj=mj+MBIG
      ma(inext)=mj
      ran3=mj*FAC
      return
      END
!  (C) Copr. 1986-92 Numerical Recipes Software '>9m_L3.




subroutine lagrange_P3( xi, yi, n, np, x, y )
  !  three points, 
  integer, intent (in) :: n, np
  real*8,  intent (in) :: xi(np), yi(np), x
  real*8,  intent (out):: y
  if (n.ne.3) then
     stop 'lagrange_P3  wrong, n.NE.3'
  endif
  y  = (x - xi(2))*(x - xi(3))/(xi(1) - xi(2))/ &
       (xi(1)- xi(3)) * yi(1) +                 &
       (x - xi(1))*(x - xi(3))/(xi(2) - xi(1))/ &
       (xi(2)- xi(3)) * yi(2) +                 &
       (x - xi(1))*(x - xi(2))/(xi(3) - xi(1))/ &
       (xi(3)- xi(2)) * yi(3)
  return
end subroutine lagrange_P3




subroutine seq_rand(n,s,idum)
  integer, intent(in) :: n
  integer, intent(out) :: s(n)
  integer, intent(inout) :: idum 
  integer :: h(n)
  integer :: j, k
  real*8 ::  ran, ran3

     h(1:n)=1
     k=0
100  ran=ran3(idum)
     j=int(ran*dble(n))+1
     !write(6,*) 'j=', j
     if (j.gt.n) then
        stop 'ran number > n, error in sub seq_rand!' 
     endif
     if (h(j).eq.1) then
        k=k+1
        s(k)=j
        h(j)=0
        if (k.eq.n) goto 200
     endif
     goto 100
200  continue
  return
end subroutine seq_rand



subroutine gen_rand_arrayR(n, s,idum)
  integer, intent(in) :: n
  real, intent(inout) :: s(n)
  integer, intent(inout) :: idum 

  real*8 ::  ran, ran3
  integer :: i

!!  write(6,*) 'ran3(idum)'
  
!!  write(6,*) 'n=', n
  do i=1, n
     s(i) = real( ran3(idum) )
!!     write(6,*) s(i)
  enddo
end subroutine gen_rand_arrayR



recursive subroutine value2digit (res, p, rk, m, mx, dig)
  integer, intent (in) :: p, m, mx
  integer, intent (out):: dig(mx)
  integer, intent (inout) :: res, rk
  integer :: k, q
  !
  ! rank    1  2  3  ...  m-1  m
  ! q       m-rk               0
  ! p^q     p**m-1             p**0
  !
  ! algorithm
  !
  !         k = mod ( res,  p** (m-rk) ) 
  !         dig(rk)=  (res - k)/ p** (m-rk)
  !         res    =  k
  !
  if (res .ge. p**m)  then
     STOP 'obtaindigit  wrong,  res >=  p**mx'
  endif
  !
  !
  if (rk.lt.m) then
     q=m-rk
     k=mod(res, p**q)
     dig(rk)= (res-k)/p**q
     res=k
     rk=rk+1
     call value2digit (res, p, rk, m, mx, dig)
  elseif (rk.eq.m) then
     dig(rk)=res
     return
  endif
end subroutine value2digit




subroutine digit2value (p, m, mx, dig, res)
  integer, intent (in) :: p, m, mx, dig(mx)
  integer, intent (out):: res
  integer :: i, k
  !
  ! remember  dig(1:m) --> 0 ... p-1
  !
  do i=1, m
     if (dig(i).lt.0 .OR. dig(i).gt.p-1) then
        STOP 'dig beyond [0,p-1]'
     endif
  enddo
  !
  res=0
  do k=1, m
     res=res+dig(k)*p**(m-k)
  enddo
  res=res+1
  return
end subroutine digit2value




subroutine gauss_legendre_quad (func, a, b, t, w, nptgs, ss)
  implicit none
  !
  interface
     function func (y)
       real*8  :: func
       real*8, intent (in) :: y
     end function func
  end  interface
  !
  integer,intent(in) :: nptgs
  real*8, intent(in) :: a, b, t(nptgs), w(nptgs)
  real*8, intent(out):: ss
  ! loc var
  real*8  :: xm, xr, x
  integer :: i

  ss=0.d0
  xm=0.5d0*(a+b)
  xr=0.5d0*(b-a)
  do i=1, nptgs
     x= xr*t(i)+xm
     ss= ss + w(i)*func(x)
     ! ck
     ! write(6,*) 't,x', t(i), x
     ! write(6,*) 'func(x)', func(x)
  enddo
  ss= xr*ss
  return
end subroutine gauss_legendre_quad




subroutine  simpson_3pt_int (xpt, ypt, npt, np_lda, sum)
  implicit none
  integer, intent (in)  :: npt, np_lda           ! npt is actual dim, np is LDA
  real*8 , intent (in)  :: xpt(np_lda), ypt(np_lda)
  real*8 , intent (out) :: sum
  !
  !  local
  real*8  :: h, s1, s2, xt, yt, xi(3), yi(3)
  integer :: i, N, ndim


  !
  ! if npt is odd,  2*N+1=npt
  !           even, 2*N+1=npt-1
  !
  if (mod(npt,2).eq.1) then
     N=(npt-1)/2
     h=(xpt(npt)-xpt(1))/dble(2*N)     ! odd 2N+1=npt
  else
     N=(npt-2)/2
     h=(xpt(npt-1)-xpt(1))/dble(2*N)   ! even 2N+1=npt-1
  endif


  s1=0.d0
  s2=0.d0
  do i=1, N
     s1=s1+ypt(2*i)
     s2=s2+ypt(2*i+1)
  enddo
  sum=4.d0*s1+2.d0*s2-ypt(2*N+1)+ypt(1)
  sum=h/3.d0*sum


  !
  !  add a parabolic mid-point to even number of points
  if (mod(npt,2).eq.0) then
     h=h/2.d0
     ndim=3
     xt=(xpt(npt-1)+xpt(npt))/2.d0   ! mid point
     xi(1)=xpt(npt-2)
     yi(1)=ypt(npt-2)
     xi(2)=xpt(npt-1)
     yi(2)=ypt(npt-1)
     xi(3)=xpt(npt)
     yi(3)=ypt(npt)
     call lagrange_P3 (xi, yi, 3, ndim, xt, yt)
     sum=sum+h/3.d0*(yi(2)+4.d0*yt+yi(3))
     ! write(6,*) 'xi(1), yi(1)', xi(1), yi(1)
     ! write(6,*) 'xi(2), yi(2)', xi(2), yi(2)
     ! write(6,*) 'xi(3), yi(3)', xi(3), yi(3)
     ! write(6,*) 'xt, yt',      xt, yt
     ! write(6,*) 'last, h, sum', h, h/3.d0*(yi(2)+4.d0*yt+yi(3))
  endif


! worse
!  if (mod(npt,2).eq.0) then
!     sum=sum+h*ypt(npt-1)
!  endif

  return
end subroutine simpson_3pt_int



!------------------------------------------------------------
!
!  F r o m    NR
!    (C) Copr. 1986-92 Numerical Recipes Software '>9m_L3
!  caution:  change  REAL  to REAL*8 
!
!------------------------------------------------------------


SUBROUTINE locate(xx,n,x,j)
  INTEGER, intent(in) :: n
  REAL*8,  intent(in) :: x,xx(n)
  INTEGER, intent(out):: j
  INTEGER  jl,jm,ju
  jl=0
  ju=n+1
10 if(ju-jl.gt.1)then
     jm=(ju+jl)/2
     if((xx(n).gt.xx(1)).eqv.(x.gt.xx(jm)))then
        jl=jm
     else
        ju=jm
     endif
     goto 10
  endif
  j=jl
  return
END SUBROUTINE locate



!------------------------------------------------
!
!  Lagrange polynormial fit to find root
!
!------------------------------------------------
!
!  ygyu change JMAX=40 --> 80

subroutine bisectply3 ( vect1, vect2, rtbis)
  real*8 :: vect1(4), vect2(4)
  INTEGER JMAX
  REAL*8:: rtbis,x1,x2,xacc
  PARAMETER (JMAX=80)
  INTEGER j
  REAL*8 :: dx,f1,f2,f,fmid,xmid
  real*8:: ply3

  xacc=1.d-10
  x1 = vect1(1)   !  two bounds
  x2 = vect1(4)   ! 
  f1  = ply3(vect1, vect2, x1)
  f2 =  ply3(vect1, vect2, x2)
  if ( f1*f2 .gt. 0.d0 )  then
     write(6,*)  'error ply3 !'
!     stop
  endif
  if( f1.gt.0.d0) then
     rtbis = x2
     x2 = x1
     x1 = rtbis
  endif
  dx = x2 - x1
  do j=1,jmax
     dx=dx/2.d0
     xmid = x1 + dx
     fmid = ply3(vect1, vect2, xmid)
     if ( fmid .le. 0.d0 ) then
        x1 = xmid
     else
        x2=xmid
     endif
     if(abs(dx).lt.xacc .or. fmid.eq.0.d0) then      
        goto 11
     endif
  enddo
11 rtbis = x1
  return
end subroutine bisectply3


function ply3( xi, fi, x )
  real*8 ::    ply3, xi(4), fi(4), x
  call lagrange_P4( xi, fi, x, ply3 )
  return
end function ply3


subroutine lagrange_P4( xi, yi, x, y )
  real*8 ::  xi(1:4), yi(1:4), x, y
  y  = (x - xi(2))*(x - xi(3))*(x - xi(4))/(xi(1) - xi(2))/    &
       (xi(1)- xi(3))/(xi(1) - xi(4)) * yi(1) +  &
       (x - xi(1))*(x - xi(3))*(x - xi(4))/(xi(2) - xi(1))/ &
       (xi(2)- xi(3))/(xi(2) - xi(4)) * yi(2) +             &
       (x - xi(1))*(x - xi(2))*(x - xi(4))/(xi(3) - xi(1))/ &
       (xi(3)- xi(2))/(xi(3) - xi(4)) * yi(3) +             &
       (x - xi(1))*(x - xi(2))*(x - xi(3))/(xi(4) - xi(1))/ &
       (xi(4)- xi(2))/(xi(4) - xi(3)) * yi(4)
  return
end subroutine lagrange_P4


subroutine firstdrv(x, y, dydx, n)
  integer, intent (in)  :: n
  real*8 , intent (in)  :: x(n), y(n)
  real*8 , intent (out):: dydx(n)
  ! local
  integer :: i 
  dydx(1)= ( y(2)-y(1) )/ ( x(2)-x(1) )
  dydx(n)= ( y(n)-y(n-1) )/ ( x(n)-x(n-1) )
  do i=2,n-1
     dydx(i)=( y(i+1)-y(i-1) )/( x(i+1)-x(i-1) )
  enddo
  return
end subroutine firstdrv



subroutine interp_linear (x1_in, y1_in, n1, x2, y2, n2)
!
! replace
!subroutine P2xgrid (F1x, F1y, n1, F2x, F2y, n2)
!
!              hi
!             /
!            /
!           /
!-------------------------------
!         /
!        /
!       /
!      lo


! input:  x1_in, y1_in  
!         x2  can be in any order
! output: y2
!
!

  integer, intent (in) :: n1, n2
  real*8 , intent (in) :: x1_in(n1)
  real*8 , intent (in) :: y1_in(n1)
  real*8 , intent (in) :: x2(n2)
  real*8 , intent (out):: y2(n2)
  real*8               :: x1(n1), y1(n1)  ! to be used 

  ! local
  integer :: i
  real*8  :: xt, gt
  real*8  :: t(4)
  real*8  :: f(4)
  real*8  :: tol
  integer :: klo, khi, k


  ! if x1 is not in increasing order, reverse x1_in, y1_in
  if (x1_in(n1).lt.x1_in(1)) then
     do i=1, n1
        x1(i)=x1_in(n1+1-i)
        y1(i)=y1_in(n1+1-i)
     enddo
     !write(6,*) 'x1 in assending order'
     !write(6, 110) x1(:)
     !write(6, 110) y1(:)
  else
     x1(:)=x1_in(:)
     y1(:)=y1_in(:)
  endif


  do i=1, n2
     xt=x2(i)
     klo=1
     khi=n1
     if (xt.lt.x1(1)) then
        t(1)=x1(1)
        t(2)=x1(2)
        t(3)=x1(3)
        t(4)=x1(4)
        f(1)=y1(1)
        f(2)=y1(2)
        f(3)=y1(3)
        f(4)=y1(4)
        khi=1
     elseif (xt.gt.x1(n1)) then
        t(1)=x1(n1)
        klo=n1
        t(2)=x1(n1-1)
        t(3)=x1(n1-2)
        t(4)=x1(n1-3)
        f(1)=y1(n1)
        f(2)=y1(n1-1)
        f(3)=y1(n1-2)
        f(4)=y1(n1-3)
     else
1       if (khi-klo.gt.1) then
           k=(klo+khi)/2
           if (xt.gt.x1(k)) then
              klo=k
           else
              khi=k
           endif
           goto 1
        endif
        if (klo.eq.1) then
           klo=klo+1
           khi=khi+1
        elseif (khi.eq.n1) then
           klo=klo-1
           khi=khi-1
        endif
        t(1)=x1(klo-1)
        t(2)=x1(klo)
        t(3)=x1(khi)
        t(4)=x1(khi+1)
        f(1)=y1(klo-1)
        f(2)=y1(klo)
        f(3)=y1(khi)
        f(4)=y1(khi+1)
     endif
     write(6, *)  'klo, khi =', klo, khi
     write(6, 111)  'x2(i), x1(klo), x1(khi): ', xt, x1(klo), x1(khi)

     call lagrange_P4 (t, f, xt, gt)
     y2(i)=gt
  enddo
  return

110 format (20f15.8)
111 format (2x,a,10f12.5)
end subroutine interp_linear

!------
!************************
!------ 
!************************
!
!c ygyu mod
!c      subroutine spline(x, y, n, yp1, ypn, y2, work)
      subroutine sple_ab(x, y, n, yp1, ypn, y2, work)

! Cubic spline routine based on p. 88 of Numerical Recipes.
! Given arrays x and y of length n containing y=f(x) with x s in 
! ascending order and given yp1 and ypn for first derivative of interpolating
! function at the endpoints, returns array y2 of length n which contains
! the second derivatives of the interpolating function at the tabulated
! points x.  If yp1 and/or ypn are 1.e30 or larger, routine sets corresponding
! boundary condition for a natural spline, with zero second derivative on
! that boundary.
! The cubic spline fit to the function is then given by
!
!  y = A y  + B y    + C y'' + D y''
!	  j	 j+1	  j	  j+1
!
! with A=(x(j+1)-x)/(x(j+1)-x(j)), B=1-A=(x-x(j))/(x(j+1)-x(j)),
! C=(A^3-A)(x(j+1)-x(j))^2/6, and D=(B^3-B)(x(j+1)-x(j))^2/6.
!
! The first derivative is therefore (with dx = x(j+1)-x(j))
!
!  y prime = (y(j+1)-y(j))/dx + (3A^2-1)dx y''(j)/6 + (3B^2-1)dx y''(j+1)/6
!
! and the second derivative is
!
!  y'' = A y''(j) + B y''(j+1)
!
! Input:
!  x(n)=x values in ascending order.
!  y(n)=y values at x points.
!  n=number of incoming data points.
!  yp1=y prime at x(1) or else > 1e30 (latter uses natural spline).
!  ypn=y prime at x(n) or else > 1e30 (as above).
!  Note that use of a natural spline has little to recommend it.
!  work(n)=work space.
! Output:
!  y2(n)=spline fit array of y'' values.

      implicit none
      integer n
      double precision x(n),y(n),y2(n),work(n),yp1,ypn
      integer i,k
      double precision sig,p,qn,workn

      if (yp1 .gt. 1.0d+30) then
!c       lower boundary condition is either natural ...
        y2(1) = 0.0d0
        work(1) = 0.0d0
      else
!c       or else to have a specified first derivative.
        y2(1) =  - 0.50d0
        work(1) = (3.d0/(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
      end if
!c     Decomposition loop of tridiagonal algorithm:
      do 100 i=2,n-1
        sig = (x(i)-x(i-1))/(x(i+1)-x(i-1))
        p = sig*y2(i-1) + 2.0d0
        y2(i) = (sig-1.0d0)/p
        work(i)=(6.0d0*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1)) &
     &   /(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*work(i-1))/p
  100 continue
      if (ypn.gt.1.0d+30) then
!c       Set upper boundary condition to be natural ...
        qn = 0.0d0
        workn = 0.0d0
      else
!c       Or else to have a specified first derivative
        qn = 0.50d0
        workn=(3.d0/(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
      end if
      y2(n) = (workn-qn*work(n-1))/(qn*y2(n-1)+1.0d0)
!c     Backsubstitution loop of tridiagonal algorithm:
      do 200 k=n-1,1,-1
        y2(k)=y2(k)*y2(k+1)+work(k)
  200 continue
      return
      end subroutine sple_ab




!------ 
!************************
!      ygyu change
!
!      subroutine splint (nspline,xspline,yspline,ysplin2,
!     *                   nfit,xfit,yfit)
!
      subroutine splt_ab (nspline,xspline,yspline,ysplin2,nfit,xfit,yfit)
!                   calculate spline interpolation
!  ON INPUT:
!  nspline: number of grid points of input mesh
!  xspline(nspline): input mesh
!  yspline(nspline): function on input mesh
!  ysplin2(nspline): second derivative of yspline on input mesh
!  nfit: number of points of output mesh
!  xfit(nfit): output mesh
!  ON OUTPUT:
!  yfit(nfit): function on output mesh

      implicit none
      integer klo,i,nfit,nspline,k,khi
      double precision xspline(nspline), yspline(nspline),  &
     &       ysplin2(nspline),xfit(nfit), yfit(nfit),h,a,b
      klo = 1
      do i=1, nfit
        do k=klo+1, nspline
            if(xspline(k).ge.xfit(i)) then
               if(xspline(k-1).le.xfit(i)) then
                  khi = k
                  klo = k-1
               else
                  if (k-1.eq.1 .and. i.eq.1) then
                     ! test 
                     write(6,*) '  SPLINT: xfit(1) < xspline(1)'
                     write(6,*) 'k=',k
                     stop '  SPLINT: xfit(1) < xspline(1)'
                  else
                     ! test
                     write(6,*) '  SPLINT: xfit not properly ordered'
                     write(6,*) 'k=',k
                     stop '  SPLINT: xfit not properly ordered'
                  end if
               end if
               h= xspline(khi) - xspline(klo)
               a= (xspline(khi)-xfit(i))/h
               b= (xfit(i)-xspline(klo))/h

               yfit(i) = a*yspline(klo) + b*yspline(khi) &
                    +( (a*a*a-a)*ysplin2(klo) +          &
                    (b*b*b-b)*ysplin2(khi) ) *h*h/6.d0
               go to 10
            end if
         end do
!        stop '  SPLINT: out of bounds'
! This is for the unlikely event that rmax exceed r(mesh)
        yfit(i)=0.d0
 10     continue
      end do

      return
    end subroutine splt_ab


!------
!************************
       SUBROUTINE SPLIFT_SD (X,Y,YP,YPP,N,W,IERR,ISX,A1,B1,AN,BN)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (FOUR=4.D0)
!RAY      PARAMETER (FOUR=4.0)
!  
!  NJTJ
!  ###  CRAY CONVERSIONS  
!  ###    1)Comment out the implicit double precision.
!  ###    2)Switch double precision parameter 
!  ###      to single precision parameter
!  ###  CRAY CONVERSIONS  
!  NJTJ
!
!     SANDIA MATHEMATICAL PROGRAM LIBRARY
!     APPLIED MATHEMATICS DIVISION 2613
!     SANDIA LABORATORIES
!     ALBUQUERQUE, NEW MEXICO  87185
!     CONTROL DATA 6600/7600  VERSION 7.2  MAY 1978
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!                    ISSUED BY SANDIA LABORATORIES
!  *                   A PRIME CONTRACTOR TO THE
!  *                UNITED STATES DEPARTMENT OF ENERGY
!  * * * * * * * * * * * * * * * NOTICE  * * * * * * * * * * * * * * *
!  * THIS REPORT WAS PREPARED AS AN ACCOUNT OF WORK SPONSORED BY THE
!  * UNITED STATES GOVERNMENT.  NEITHER THE UNITED STATES NOR THE
!  * UNITED STATES DEPARTMENT OF ENERGY NOR ANY OF THEIR EMPLOYEES,
!  * NOR ANY OF THEIR CONTRACTORS, SUBCONTRACTORS, OR THEIR EMPLOYEES
!  * MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL
!  * LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS OR
!  * USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT OR PROCESS
!  * DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE
!  * OWNED RIGHTS.
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!  * THE PRIMARY DOCUMENT FOR THE LIBRARY OF WHICH THIS ROUTINE IS
!  * PART IS SAND77-1441.
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
!     WRITTEN BY RONDALL E. JONES
!
!     ABSTRACT
!         SPLIFT FITS AN INTERPOLATING CUBIC SPLINE TO THE N DATA POINT
!         GIVEN IN X AND Y AND RETURNS THE FIRST AND SECOND DERIVATIVES
!         IN YP AND YPP.  THE RESULTING SPLINE (DEFINED BY X, Y, AND
!         YPP) AND ITS FIRST AND SECOND DERIVATIVES MAY THEN BE
!         EVALUATED USING SPLINT.  THE SPLINE MAY BE INTEGRATED USING
!         SPLIQ.  FOR A SMOOTHING SPLINE FIT SEE SUBROUTINE SMOO.
!
!     DESCRIPTION OF ARGUMENTS
!         THE USER MUST DIMENSION ALL ARRAYS APPEARING IN THE CALL LIST
!         E.G.   X(N), Y(N), YP(N), YPP(N), W(3N)
!
!       --INPUT--
!
!         X    - ARRAY OF ABSCISSAS OF DATA (IN INCREASING ORDER)
!         Y    - ARRAY OF ORDINATES OF DATA
!         N    - THE NUMBER OF DATA POINTS.  THE ARRAYS X, Y, YP, AND
!                YPP MUST BE DIMENSIONED AT LEAST N.  (N .GE. 4)
!         ISX  - MUST BE ZERO ON THE INITIAL CALL TO SPLIFT.
!                IF A SPLINE IS TO BE FITTED TO A SECOND SET OF DATA
!                THAT HAS THE SAME SET OF ABSCISSAS AS A PREVIOUS SET,
!                AND IF THE CONTENTS OF W HAVE NOT BEEN CHANGED SINCE
!                THAT PREVIOUS FIT WAS COMPUTED, THEN ISX MAY BE
!                SET TO ONE FOR FASTER EXECUTION.
!         A1,B1,AN,BN - SPECIFY THE END CONDITIONS FOR THE SPLINE WHICH
!                ARE EXPRESSED AS CONSTRAINTS ON THE SECOND DERIVATIVE
!                OF THE SPLINE AT THE END POINTS (SEE YPP).
!                THE END CONDITION CONSTRAINTS ARE
!                        YPP(1) = A1*YPP(2) + B1
!                AND
!                        YPP(N) = AN*YPP(N-1) + BN
!                WHERE
!                        ABS(A1).LT. 1.0  AND  ABS(AN).LT. 1.0.
!
!                THE SMOOTHEST SPLINE (I.E., LEAST INTEGRAL OF SQUARE
!                OF SECOND DERIVATIVE) IS OBTAINED BY A1=B1=AN=BN=0.
!                IN THIS CASE THERE IS AN INFLECTION AT X(1) AND X(N).
!                IF THE DATA IS TO BE EXTRAPOLATED (SAY, BY USING SPLIN
!                TO EVALUATE THE SPLINE OUTSIDE THE RANGE X(1) TO X(N))
!                THEN TAKING A1=AN=0.5 AND B1=BN=0 MAY YIELD BETTER
!                RESULTS.  IN THIS CASE THERE IS AN INFLECTION
!                AT X(1) - (X(2)-X(1)) AND AT X(N) + (X(N)-X(N-1)).
!                IN THE MORE GENERAL CASE OF A1=AN=A  AND B1=BN=0,
!                THERE IS AN INFLECTION AT X(1) - (X(2)-X(1))*A/(1.0-A)
!                AND AT X(N) + (X(N)-X(N-1))*A/(1.0-A).
!
!                A SPLINE THAT HAS A GIVEN FIRST DERIVATIVE YP1 AT X(1)
!                AND YPN AT Y(N) MAY BE DEFINED BY USING THE
!                FOLLOWING CONDITIONS.
!
!                A1=-0.5
!
!                B1= 3.0*((Y(2)-Y(1))/(X(2)-X(1))-YP1)/(X(2)-X(1))
!
!                AN=-0.5
!
!                BN=-3.0*((Y(N)-Y(N-1))/(X(N)-X(N-1))-YPN)/(X(N)-X(N-1)
!
!       --OUTPUT--
!
!         YP   - ARRAY OF FIRST DERIVATIVES OF SPLINE (AT THE X(I))
!         YPP  - ARRAY OF SECOND DERIVATIVES OF SPLINE (AT THE X(I))
!         IERR - A STATUS CODE
!              --NORMAL CODE
!                 1 MEANS THAT THE REQUESTED SPLINE WAS COMPUTED.
!              --ABNORMAL CODES
!                 2 MEANS THAT N, THE NUMBER OF POINTS, WAS .LT. 4.
!                 3 MEANS THE ABSCISSAS WERE NOT STRICTLY INCREASING.
!
!       --WORK--
!
!         W    - ARRAY OF WORKING STORAGE DIMENSIONED AT LEAST 3N.
       DIMENSION X(N),Y(N),YP(N),YPP(N),W(N,3)
!
       IF (N.LT.4) THEN
         IERR = 2
         RETURN
       ENDIF
       NM1  = N-1
       NM2  = N-2
       IF (ISX.GT.0) GO TO 40
       DO 5 I=2,N
         IF (X(I)-X(I-1) .LE. 0) THEN
           IERR = 3
           RETURN
         ENDIF
 5     CONTINUE
!
!     DEFINE THE TRIDIAGONAL MATRIX
!
       W(1,3) = X(2)-X(1)
       DO 10 I=2,NM1
         W(I,2) = W(I-1,3)
         W(I,3) = X(I+1)-X(I)
 10      W(I,1) = 2.D0*(W(I,2)+W(I,3))
       W(1,1) = FOUR
       W(1,3) =-4.D0*A1
       W(N,1) = FOUR
       W(N,2) =-4.D0*AN
!
!     L U DECOMPOSITION
!
       DO 30 I=2,N
         W(I-1,3) = W(I-1,3)/W(I-1,1)
 30    W(I,1) = W(I,1) - W(I,2)*W(I-1,3)
!
!     DEFINE *CONSTANT* VECTOR
!
 40   YPP(1) = 4.D0*B1
      DOLD = (Y(2)-Y(1))/W(2,2)
      DO 50 I=2,NM2
        DNEW   = (Y(I+1) - Y(I))/W(I+1,2)
        YPP(I) = 6.D0*(DNEW - DOLD)
        YP(I)  = DOLD
 50   DOLD = DNEW
      DNEW = (Y(N)-Y(N-1))/(X(N)-X(N-1))
      YPP(NM1) = 6.D0*(DNEW - DOLD)
      YPP(N) = 4.D0*BN
      YP(NM1)= DOLD
      YP(N) = DNEW
!
!     FORWARD SUBSTITUTION
!
      YPP(1) = YPP(1)/W(1,1)
      DO 60 I=2,N
 60   YPP(I) = (YPP(I) - W(I,2)*YPP(I-1))/W(I,1)
!
!     BACKWARD SUBSTITUTION
!
       DO 70 J=1,NM1
         I = N-J
   70 YPP(I) = YPP(I) - W(I,3)*YPP(I+1)
!
!     COMPUTE FIRST DERIVATIVES
!
      YP(1) = (Y(2)-Y(1))/(X(2)-X(1)) - (X(2)-X(1))*(2.D0*YPP(1) &
           + YPP(2))/6.D0
      DO 80 I=2,NM1
 80   YP(I) = YP(I) + W(I,2)*(YPP(I-1) + 2.D0*YPP(I))/6.D0
      YP(N) = YP(N) + (X(N)-X(NM1))*(YPP(NM1) + 2.D0*YPP(N))/6.D0
!
      IERR = 1
      RETURN
      END


!------
!************************
       SUBROUTINE SPLINT_SD (X,Y,YPP,N,XI,YI,YPI,YPPI,NI,KERR)
       implicit double precision (a-h,o-z)
!
!     SANDIA MATHEMATICAL PROGRAM LIBRARY
!     APPLIED MATHEMATICS DIVISION 2613
!     SANDIA LABORATORIES
!     ALBUQUERQUE, NEW MEXICO  87185
!     CONTROL DATA 6600/7600  VERSION 7.2  MAY 1978
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!                    ISSUED BY SANDIA LABORATORIES
!  *                   A PRIME CONTRACTOR TO THE
!  *                UNITED STATES DEPARTMENT OF ENERGY
!  * * * * * * * * * * * * * * * NOTICE  * * * * * * * * * * * * * * *
!  * THIS REPORT WAS PREPARED AS AN ACCOUNT OF WORK SPONSORED BY THE
!  * UNITED STATES GOVERNMENT.  NEITHER THE UNITED STATES NOR THE
!  * UNITED STATES DEPARTMENT OF ENERGY NOR ANY OF THEIR EMPLOYEES,
!  * NOR ANY OF THEIR CONTRACTORS, SUBCONTRACTORS, OR THEIR EMPLOYEES
!  * MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL
!  * LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS OR
!  * USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT OR PROCESS
!  * DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE
!  * OWNED RIGHTS.
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!  * THE PRIMARY DOCUMENT FOR THE LIBRARY OF WHICH THIS ROUTINE IS
!  * PART IS SAND77-1441.
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
!     WRITTEN BY RONDALL E. JONES
!
!     ABSTRACT
!
!         SPLINT EVALUATES A CUBIC SPLINE AND ITS FIRST AND SECOND
!         DERIVATIVES AT THE ABSCISSAS IN XI.  THE SPLINE (WHICH
!         IS DEFINED BY X, Y, AND YPP) MAY HAVE BEEN DETERMINED BY
!         SPLIFT OR SMOO OR ANY OTHER SPLINE FITTING ROUTINE THAT
!         PROVIDES SECOND DERIVATIVES.
!
!     DESCRIPTION OF ARGUMENTS
!         THE USER MUST DIMENSION ALL ARRAYS APPEARING IN THE CALL LIST
!         E.G.  X(N), Y(N), YPP(N), XI(NI), YI(NI), YPI(NI), YPPI(NI)
!
!       --INPUT--
!
!         X   - ARRAY OF ABSCISSAS (IN INCREASING ORDER) THAT DEFINE TH
!               SPLINE.  USUALLY X IS THE SAME AS X IN SPLIFT OR SMOO.
!         Y   - ARRAY OF ORDINATES THAT DEFINE THE SPLINE.  USUALLY Y I
!               THE SAME AS Y IN SPLIFT OR AS R IN SMOO.
!         YPP - ARRAY OF SECOND DERIVATIVES THAT DEFINE THE SPLINE.
!               USUALLY YPP IS THE SAME AS YPP IN SPLIFT OR R2 IN SMOO.
!         N   - THE NUMBER OF DATA POINTS THAT DEFINE THE SPLINE.
!               THE ARRAYS X, Y, AND YPP MUST BE DIMENSIONED AT LEAST N
!               N MUST BE GREATER THAN OR EQUAL TO 2.
!         XI  - THE ABSCISSA OR ARRAY OF ABSCISSAS (IN ARBITRARY ORDER)
!               AT WHICH THE SPLINE IS TO BE EVALUATED.
!               EACH XI(K) THAT LIES BETWEEN X(1) AND X(N) IS A CASE OF
!               INTERPOLATION.  EACH XI(K) THAT DOES NOT LIE BETWEEN
!               X(1) AND X(N) IS A CASE OF EXTRAPOLATION.  BOTH CASES
!               ARE ALLOWED.  SEE DESCRIPTION OF KERR.
!         NI  - THE NUMBER OF ABSCISSAS AT WHICH THE SPLINE IS TO BE
!               EVALUATED.  IF NI IS GREATER THAN 1, THEN XI, YI, YPI,
!               AND YPPI MUST BE ARRAYS DIMENSIONED AT LEAST NI.
!               NI MUST BE GREATER THAN OR EQUAL TO 1.
!
!       --OUTPUT--
!
!         YI  - ARRAY OF VALUES OF THE SPLINE (ORDINATES) AT XI.
!         YPI - ARRAY OF VALUES OF THE FIRST DERIVATIVE OF SPLINE AT XI
!         YPPI- ARRAY OF VALUES OF SECOND DERIVATIVES OF SPLINE AT XI.
!         KERR- A STATUS CODE
!             --NORMAL CODES
!                1 MEANS THAT THE SPLINE WAS EVALUATED AT EACH ABSCISSA
!                  IN XI USING ONLY INTERPOLATION.
!                2 MEANS THAT THE SPLINE WAS EVALUATED AT EACH ABSCISSA
!                  IN XI, BUT AT LEAST ONE EXTRAPOLATION WAS PERFORMED.
!             -- ABNORMAL CODE
!                3 MEANS THAT THE REQUESTED NUMBER OF EVALUATIONS, NI,
!                  WAS NOT POSITIVE.
!
       DIMENSION X(N),Y(N),YPP(N),XI(NI),YI(NI),YPI(NI),YPPI(NI)
!
!     CHECK INPUT
!
      IF (NI) 1,1,2
 1    CONTINUE
!    1 CALL ERRCHK(67,67HIN SPLINT,  THE REQUESTED NUMBER OF INTERPOLATI
!     1NS WAS NOT POSITIVE)
      KERR = 3
      RETURN
    2 KERR = 1
      NM1= N-1
!
!     K IS INDEX ON VALUE OF XI BEING WORKED ON.  XX IS THAT VALUE.
!     I IS CURRENT INDEX INTO X ARRAY.
!
       K  = 1
       XX = XI(1)
       IF (XX.LT.X(1)) GO TO 90
       IF (XX.GT.X(N)) GO TO 80
       IL = 1
       IR = N
!
!     BISECTION SEARCH
!
   10 I  = (IL+IR)/2
       IF (I.EQ.IL) GO TO 100
       IF (XX-X(I)) 20,100,30
   20 IR = I
       GO TO 10
   30 IL = I
       GO TO 10
!
!     LINEAR FORWARD SEARCH
!
   50 IF (XX-X(I+1)) 100,100,60
   60 IF (I.GE.NM1) GO TO 80
       I  = I+1
       GO TO 50
!
!     EXTRAPOLATION
!
   80 KERR = 2
      I  = NM1
      GO TO 100
   90 KERR = 2
      I  = 1
!
!     INTERPOLATION
!
  100 H  = X(I+1) - X(I)
       H2 = H*H
       XR = (X(I+1)-XX)/H
       XR2= XR*XR
       XR3= XR*XR2
       XL = (XX-X(I))/H
       XL2= XL*XL
       XL3= XL*XL2
       YI(K) = Y(I)*XR + Y(I+1)*XL  &
          -H2*(YPP(I)*(XR-XR3) + YPP(I+1)*(XL-XL3))/6.0D0
       YPI(K) = (Y(I+1)-Y(I))/H  &
          +H*(YPP(I)*(1.0D0-3.0D0*XR2)-YPP(I+1)*(1.0D0-3.0D0*XL2))/6.0D0
       YPPI(K) = YPP(I)*XR + YPP(I+1)*XL
!
!     NEXT POINT
!
       IF (K.GE.NI) RETURN
       K = K+1
       XX = XI(K)
       IF (XX.LT.X(1)) GO TO 90
       IF (XX.GT.X(N)) GO TO 80
       IF (XX-XI(K-1)) 110,100,50
  110 IL = 1
       IR = I+1
       GO TO 10
!
       END


!------
!************************
       SUBROUTINE SPLIQ_SD(X,Y,YP,YPP,N,XLO,XUP,NUP,ANS,IERR)
! 
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!  
!  NJTJ
!  ###  CRAY CONVERSIONS  
!  ###    1)Comment out implicit double precision.
!  ###  CRAY CONVERSIONS  
!  NJTJ
!
       DIMENSION X(N),Y(N),YP(N),YPP(N),XUP(NUP),ANS(NUP)
!
!     SANDIA MATHEMATICAL PROGRAM LIBRARY
!     APPLIED MATHEMATICS DIVISION 2613
!     SANDIA LABORATORIES
!     ALBUQUERQUE, NEW MEXICO  87185
!     CONTROL DATA 6600/7600  VERSION 7.2  MAY 1978
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!                    ISSUED BY SANDIA LABORATORIES
!  *                   A PRIME CONTRACTOR TO THE
!  *                UNITED STATES DEPARTMENT OF ENERGY
!  * * * * * * * * * * * * * * * NOTICE  * * * * * * * * * * * * * * *
!  * THIS REPORT WAS PREPARED AS AN ACCOUNT OF WORK SPONSORED BY THE
!  * UNITED STATES GOVERNMENT.  NEITHER THE UNITED STATES NOR THE
!  * UNITED STATES DEPARTMENT OF ENERGY NOR ANY OF THEIR EMPLOYEES,
!  * NOR ANY OF THEIR CONTRACTORS, SUBCONTRACTORS, OR THEIR EMPLOYEES
!  * MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL
!  * LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS OR
!  * USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT OR PROCESS
!  * DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE
!  * OWNED RIGHTS.
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!  * THE PRIMARY DOCUMENT FOR THE LIBRARY OF WHICH THIS ROUTINE IS
!  * PART IS SAND77-1441.
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
!     THIS ROUTINE WAS WRITTEN BY M. K. GORDON
!
!     ABSTRACT
!
!     SUBROUTINE SPLIQ INTEGRATES A CUBIC SPLINE (GENERATED BY
!     SPLIFT, SMOO, ETC.) ON THE INTERVALS (XLO,XUP(I)), WHERE XUP
!     IS A SEQUENCE OF UPPER LIMITS ON THE INTERVALS OF INTEGRATION.
!     THE ONLY RESTRICTIONS ON XLO AND XUP(*) ARE
!                XLO .LT. XUP(1),
!                XUP(I) .LE. XUP(I+1)   FOR EACH I .
!     ENDPOINTS BEYOND THE SPAN OF ABSCISSAS ARE ALLOWED.
!     THE SPLINE OVER THE INTERVAL (X(I),X(I+1)) IS REGARDED
!     AS A CUBIC POLYNOMIAL EXPANDED ABOUT X(I) AND IS INTEGRATED
!     ANALYTICALLY.
!
!     DESCRIPTION OF ARGUMENTS
!         THE USER MUST DIMENSION ALL ARRAYS APPEARING IN THE CALL LIST
!         E.G.  X(N), Y(N), YP(N), YPP(N), XUP(NUP), ANS(NUP)
!
!      --INPUT--
!
!        X    - ARRAY OF ABSCISSAS (IN INCREASING ORDER) THAT DEFINE TH
!               SPLINE.  USUALLY X IS THE SAME AS X IN SPLIFT OR SMOO.
!        Y    - ARRAY OF ORDINATES THAT DEFINE THE SPLINE.  USUALLY Y I
!               THE SAME AS Y IN SPLIFT OR AS R IN SMOO.
!        YP   - ARRAY OF FIRST DERIVATIVES OF THE SPLINE AT ABSCISSAS.
!               USUALLY YP IS THE SAME AS YP IN SPLIFT OR R1 IN SMOO.
!        YPP  - ARRAY OF SECOND DERIVATIVES THAT DEFINE THE SPLINE.
!               USUALLY YPP IS THE SAME AS YPP IN SPLIFT OR R2 IN SMOO.
!        N    - THE NUMBER OF DATA POINTS THAT DEFINE THE SPLINE.
!        XLO  - LEFT ENDPOINT OF INTEGRATION INTERVALS.
!        XUP  - RIGHT ENDPOINT OR ARRAY OF RIGHT ENDPOINTS OF
!               INTEGRATION INTERVALS IN ASCENDING ORDER.
!        NUP  - THE NUMBER OF RIGHT ENDPOINTS.  IF NUP IS GREATER THAN
!               1, THEN XUP AND ANS MUST BE DIMENSIONED AT LEAST NUP.
!
!      --OUTPUT--
!
!        ANS -- ARRAY OF INTEGRAL VALUES, THAT IS,
!               ANS(I) = INTEGRAL FROM XLO TO XUP(I)
!        IERR -- ERROR STATUS
!                = 1 INTEGRATION SUCCESSFUL
!                = 2 IMPROPER INPUT - N.LT.4 OR NUP.LT.1
!                = 3 IMPROPER INPUT - ABSCISSAS NOT IN
!                        STRICTLY ASCENDING ORDER
!                = 4 IMPROPER INPUT - RIGHT ENDPOINTS XUP NOT
!                        IN ASCENDING ORDER
!                = 5 IMPROPER INPUT - XLO.GT.XUP(1)
!                = 6 INTEGRATION SUCCESSFUL BUT AT LEAST ONE ENDPOINT
!                        NOT WITHIN SPAN OF ABSCISSAS
!              ** NOTE.  ERRCHK PROCESSES DIAGNOSTICS FOR CODES 2,3,4,5
!
!   CHECK FOR IMPROPER INPUT
!
       IERR = 2
       IF(N .LT. 4  .OR.  NUP .LT. 1) THEN 
         RETURN
       ENDIF
       NM1 = N-1
       NM2 = N-2
       IERR = 3
       DO 2 I = 1,NM1
         IF(X(I) .GE. X(I+1)) THEN
           RETURN
         ENDIF
 2     CONTINUE
       IF(NUP .NE. 1) THEN
         IERR = 4
         DO 3 I = 2,NUP
           IF(XUP(I-1) .GT. XUP(I)) THEN
             RETURN
           ENDIF
 3       CONTINUE
       ENDIF
       IERR = 5
       IF(XLO .GT. XUP(1)) THEN
         RETURN
       ENDIF
       IERR = 1
       IF(XLO .LT. X(1)  .OR.  XUP(NUP) .GT. X(N)) IERR = 6
!
!   LOCATE XLO IN INTERVAL (X(I),X(I+1))
!
       DO 10 I = 1,NM2
         IF(XLO .LT. X(I+1)) GO TO 20
 10      CONTINUE
       I = NM1
 20    HLO = XLO-X(I)
       HLO2 = HLO*HLO
       HI = X(I+1)-X(I)
       HI2 = HI*HI
       DO 30 J = 1,NUP
         IF(XUP(J) .GT. X(I+1)  .AND.  XLO .LT. X(NM1)) GO TO 40
!
!   COMPUTE SPECIAL CASES OF XUP IN INTERVAL WITH XLO
!
         HUP = XUP(J)-X(I)
         HSUM = HUP+HLO
         HDIFF = HUP-HLO
         HUP2 = HUP*HUP
         SUM = (YPP(I+1)-YPP(I))*HSUM*HDIFF*(HUP2+HLO2)/(24.D0*HI)
         SUM = SUM + YPP(I)*HDIFF*(HUP2+HLO*HUP+HLO2)/6.D0
         SUM = SUM + YP(I)*HDIFF*HSUM/2.D0
         SUM = SUM + Y(I)*HDIFF
 30    ANS(J) = SUM
       RETURN
!
!   COMPUTE INTEGRAL BETWEEN XLO AND X(I+1) AS FOUR TERMS IN TAYLOR
!   POLYNOMIAL AND ADVANCE I TO I+1
!
 40    HDIFF = HI-HLO
       HSUM = HI+HLO
       SUM0 = Y(I)*HDIFF
       SUM1 = YP(I)*HDIFF*HSUM
       SUM2 = YPP(I)*HDIFF*(HI2+HI*HLO+HLO2)
       SUM3 = (YPP(I+1)-YPP(I))*HDIFF*HSUM*(HI2+HLO2)/HI
       I = I+1
!
!   LOCATE EACH XUP(M) IN INTERVAL (X(I),X(I+1))
!
       DO 80 M = J,NUP
 50      IF(XUP(M) .LT. X(I+1)  .OR.  I .EQ. NM1) GO TO 60
!
!   AUGMENT INTEGRAL BETWEEN ABSCISSAS TO INCLUDE INTERVAL
!   (X(I),X(I+1)) AND ADVANCE I TO I+1
!
         HI = X(I+1)-X(I)
         HI2 = HI*HI
         HI3 = HI2*HI
         SUM0 = SUM0 + Y(I)*HI
         SUM1 = SUM1 + YP(I)*HI2
         SUM2 = SUM2 + YPP(I)*HI3
         SUM3 = SUM3 + (YPP(I+1)-YPP(I))*HI3
         I = I+1
         GO TO 50
!
!   INTEGRAL BETWEEN X(I) AND XUP(M) IS ZERO
!
 60      IF(XUP(M) .NE. X(I)) THEN
!
!   COMPUTE INTEGRAL BETWEEN X(I) AND XUP(M) AND EVALUATE
!   TAYLOR POLYNOMIAL IN REVERSE ORDER
!
           HUP = XUP(M)-X(I)
           HUP2 = HUP*HUP
           HUP3 = HUP2*HUP
           HUP4 = HUP3*HUP
           HI = X(I+1)-X(I)
           PSUM0 = Y(I)*HUP
           PSUM1 = YP(I)*HUP2
           PSUM2 = YPP(I)*HUP3
           PSUM3 = (YPP(I+1)-YPP(I))*HUP4/HI
           SUM = (SUM3+PSUM3)/24.D0 + (SUM2+PSUM2)/6.D0
           SUM = SUM + (SUM1+PSUM1)/2.D0
           SUM = SUM + (SUM0+PSUM0)
         ELSE
           SUM = ((SUM3/24.D0 + SUM2/6.D0) + SUM1/2.D0) + SUM0
         ENDIF
 80    ANS(M) = SUM
       RETURN
       END
!------
!************************


      FUNCTION rtbis(func,x1,x2,xacc)
      !
      interface
         function func (y)
           real*8  :: func
           real*8, intent (in) :: y
         end function func
      end  interface
      !
      integer,  PARAMETER :: JMAX=80
      REAL*8    rtbis,x1,x2,xacc
      INTEGER   j
      REAL*8    dx,f,fmid,xmid

      fmid=func(x2)
      f=func(x1)
      if(f*fmid.ge.0.d0) STOP 'root must be bracketed in rtbis'
      if(f.lt.0.d0)then
        rtbis=x1
        dx=x2-x1
      else
        rtbis=x2
        dx=x1-x2
      endif
      do 11 j=1,JMAX
        dx=dx*0.5d0
        xmid=rtbis+dx
        fmid=func(xmid)
        if(fmid.le.0.d0)rtbis=xmid
        if(dabs(dx).lt.xacc .or. fmid.eq.0.d0) return
!--ygyu ck
!        write(6,'(2x,a,2f12.8,E12.4)') &
!             'rtbis, xmid, fmid', rtbis, xmid, fmid
!--
11    continue
      pause 'too many bisections in rtbis'
      END FUNCTION rtbis
!  (C) Copr. 1986-92 Numerical Recipes Software '>9m_L3.
!------
!************************
  

!!     4-May-2021
!! --  kind(rt) does not work, mixing NOAA work with Frankfurt, I removed this
!!
!!!-- input P1:  (lam, theta, r)
!!subroutine arc_len (P1, P2, s, ndim)
!!  use kinds, only: rt
!!  implicit none
!!  integer ,intent(in) :: ndim
!!  real(rt),intent(in ):: P1(ndim), P2(ndim) ! pt1_llr(3), pt2_llr(3), lam, theta, r
!!  real(rt),intent(out):: s     !  arc_len
!!  real(rt)  :: lon1, lon2, lat1, lat2
!!  !
!!  lon1=P1(1); lat1=P1(2)
!!  lon2=P2(1); lat2=P2(2)
!!  s= acos( sin(lat1)*sin(lat2)+ cos(lat1)*cos(lat2)*cos(lon1-lon2) )
!!  return
!!end subroutine arc_len
!!
!!
!!subroutine basis_from_sph_2_car (P_sph, basis, ndim)
!!  use kinds, only: rt
!!  use module_constants, only : pi
!!  implicit none
!!  integer ,intent(in) :: ndim
!!  real(rt),intent(in ):: P_sph(ndim)     ! pt1_llr(3), pt2_llr(3), lam, theta, r
!!  real(rt),intent(out):: basis(3,3)
!!  real(rt)  :: lon, lat, r
!!  !
!!  ! (\vec lambda,  theta,  r) = (e1, e2, e3)  basis
!!  !   x = r cos \theta  cos \lambda
!!  !   y = r cos \theta  sin \lambda
!!  !   z = r sin \theta
!!  !
!!  lon=P_sph(1); lat=P_sph(2); r=P_sph(3)
!!  if ( pi/2.d0 - abs(lat).GT. 1.d-8) then
!!     basis(1,1)= - sin(lon)               ! \vec lambda \dot x  =  - x/rho,  rho=sqrt(x^2+y^2)
!!     basis(2,1)=   cos(lon)               !             \dot y  =    y/rho
!!     basis(3,1)=   0.d0                   !             \dot z  =    0
!!     basis(1,2)=  -sin(lat)*cos(lon)      ! \vec theta  \dot x  =   -z/r * x/rho
!!     basis(2,2)=  -sin(lat)*sin(lon)      !                  y  =   -z/r * y/rho
!!     basis(3,2)=   cos(lat)               !                  z  =    rho/r
!!     basis(1,3)=   cos(lat)*cos(lon)      ! \vec r      \dot x  =    x/r
!!     basis(2,3)=   cos(lat)*sin(lon)      !                  y  =    y/r
!!     basis(3,3)=   sin(lat)               !                  z  =    z/r
!!  else
!!     ! singularity  define \lambda= \vec y,  \theta= -\vec x
!!     !    vi = \polar \dot x_xi
!!     basis(1,1)= 0.d0
!!     basis(2,1)= 1.d0
!!     basis(3,1)= 0.d0
!!     basis(1,2)=-1.d0
!!     basis(2,2)= 0.d0
!!     basis(3,2)= 0.d0
!!     basis(1,3)= 0.d0
!!     basis(2,3)= 0.d0
!!     basis(3,3)= 1.d0
!!  endif
!!  return
!!end subroutine basis_from_sph_2_car
!!
!!
!!subroutine basis_from_car_2_sph (P_car, basis, ndim)
!!  use kinds, only: rt
!!  implicit none
!!  integer ,intent(in) :: ndim
!!  real(rt),intent(in ):: P_car(ndim)     ! pt1_llr(3), pt2_llr(3), lam, theta, r
!!  real(rt),intent(out):: basis(3,3)
!!  real(rt)  :: x, y, z, rho, r
!!  !
!!  ! (\vec lambda,  theta,  r) = (e1, e2, e3)  basis
!!  !   x = r cos \theta  cos \lambda
!!  !   y = r cos \theta  sin \lambda
!!  !   z = r sin \theta
!!  !
!!  x=P_car(1); y=P_car(2); z=P_car(3)
!!  rho=sqrt(x*x+y*y); r=sqrt(x*x+y*y+z*z)
!!  if (rho/r .GT. 1.d-8) then
!!     ! regular
!!     basis(1,1)= -y/rho       ! -sin(lon)         ! \vec lambda \dot x =  - y/rho,  rho=sqrt(x^2+y^2)
!!     basis(2,1)= -z*x/r/rho   ! -sin(lat)*cos(lon)! \vec theta  \dot x =   -z/r * x/rho
!!     basis(3,1)=  x/r         !  cos(lat)*cos(lon)! \vec r      \dot x =    x/r
!!     !
!!     basis(1,2)=  x/rho       !  cos(lon)           ! \lambda \dot y  =    x/rho
!!     basis(2,2)= -z*y/r/rho   ! -sin(lat)*sin(lon)  ! \theta       y  =   -z/r * y/rho
!!     basis(3,2)=  y/r         !  cos(lat)*sin(lon)  ! \r           y  =    y/r
!!     !
!!     basis(1,3)=  0.d0        !  0.d0     ! \lambda  \dot z  =    0
!!     basis(2,3)=  rho/r       !  cos(lat) ! \theta        z  =    rho/r
!!     basis(3,3)=  z/r         !  sin(lat) ! \r            z  =    z/r
!!     !
!!  else
!!     ! polar singularity
!!     ! singularity  define \lambda= \vec y,  \theta= -\vec x
!!     !    vi = \xi \dot polar
!!     basis(1,1)= 0.d0
!!     basis(2,1)=-1.d0
!!     basis(3,1)= 0.d0
!!     basis(1,2)= 1.d0
!!     basis(2,2)= 0.d0
!!     basis(3,2)= 0.d0
!!     basis(1,3)= 0.d0
!!     basis(2,3)= 0.d0
!!     basis(3,3)= 1.d0
!!  endif
!!  return
!!end subroutine basis_from_car_2_sph
!!
!!
!!
!!!subroutine basis_from_car_2_sph (P_car, basis, ndim)
!!subroutine vect_from_sph_2_car(P_sph, P_car, ndim)
!!  use kinds, only: rt
!!  implicit none
!!  integer ,intent(in) :: ndim
!!  real(rt),intent(in ):: P_sph(ndim)     ! pt1_llr(3), pt2_llr(3), lam, theta, r
!!  real(rt),intent(out):: P_car(ndim)
!!  real(rt)  :: lon, lat, r, x, y, z
!!  !
!!  ! (\vec lambda,  theta,  r) = (e1, e2, e3)  basis
!!  !   x = r cos \theta  cos \lambda
!!  !   y = r cos \theta  sin \lambda
!!  !   z = r sin \theta
!!  !
!!  lon=P_sph(1); lat=P_sph(2); r=P_sph(3)
!!  x= r*cos(lat)*cos(lon)
!!  y= r*cos(lat)*sin(lon)
!!  z= r*sin(lat)
!!  P_car(1)=x; P_car(2)=y; P_car(3)=z
!!  return
!!end subroutine vect_from_sph_2_car
!!
!!
!!subroutine vect_from_car_2_sph (P_car, P_sph, ndim)
!!  use kinds, only: rt
!!  use module_constants, only : pi, lon_pol
!!  implicit none
!!  integer ,intent(in) :: ndim
!!  real(rt),intent(in ):: P_car(ndim)     ! pt1_llr(3), pt2_llr(3), lam, theta, r
!!  real(rt),intent(out):: P_sph(ndim)
!!  real(rt)  :: x, y, z, rho, r, lat, lon
!!  !
!!  ! (\vec lambda,  theta,  r) = (e1, e2, e3)  basis
!!  !   r= sqrt(x*x+y*y+z*z)
!!  !   \lambda =  
!!  !   \theta  = asin( z/r ) 
!!  !
!!  x=P_car(1); y=P_car(2); z=P_car(3)
!!  rho= sqrt(x*x+y*y)
!!  r  = sqrt(x*x+y*y+z*z)
!!  lat= asin(z/r)
!!  !
!!  if ( (r-abs(z))/r  .LE. 1.d-9) then
!!     lon = lon_pol      ! N/S polar singularity, give a constant longitude
!!  else
!!     if ( y.GE.0.d0) then
!!        lon = acos( x/rho )
!!     else
!!        lon = 2*pi - acos( x/rho )        
!!     endif
!!  endif
!!  P_sph(1)=lon
!!  P_sph(2)=lat
!!  P_sph(3)=r
!!  return
!!end subroutine vect_from_car_2_sph
!!
!!
!!
!!subroutine arc_mid_ntvec (P1_sph, P2_sph, Pmid_car, Pmid_sph, Vn_car, Vt_car, ndim)
!!  use kinds, only: rt
!!  implicit none
!!  integer ,intent(in) :: ndim
!!  real(rt),intent(in ):: P1_sph(ndim), P2_sph(ndim) ! pt1_llr(3)  lam, theta, r
!!  real(rt),intent(out):: Pmid_car(3), Pmid_sph(3)
!!  real(rt),intent(out):: Vn_car(3), Vt_car(3)   ! N_vec,  T_vec
!!  real(rt)  :: lon1, lon2, lat1, lat2, s
!!  real(rt)  :: cross(3)
!!  real(rt)  :: P1_car(3), P2_car(3), rvec_car(3)
!!  integer   :: i
!!  !
!!  lon1=P1_sph(1); lat1=P1_sph(2)
!!  lon2=P2_sph(1); lat2=P2_sph(2)
!!  P1_car(1)= cos(lat1)*cos(lon1)   ! x
!!  P1_car(2)= cos(lat1)*sin(lon1)   ! y
!!  P1_car(3)= sin(lat1)             ! z
!!  P2_car(1)= cos(lat2)*cos(lon2)   ! x
!!  P2_car(2)= cos(lat2)*sin(lon2)   ! y
!!  P2_car(3)= sin(lat2)             ! z
!!  do i=1, 3
!!     Pmid_car(i)= 0.5d0*(P2_car(i)+P1_car(i))
!!  enddo
!!  call X_2norm (Pmid_car, s, 3)    ! s cannot be zero
!!  do i=1,3
!!     Pmid_car(i)=Pmid_car(i)/s
!!     rvec_car(i)=Pmid_car(i)
!!  enddo
!!  !
!!  call XY_cross (P2_car, P1_car, cross, 3)
!!  call X_2norm (cross, s, 3)    ! s cannot be zero
!!  do i=1, 3
!!     Vn_car(i)= cross(i)/s
!!  enddo
!!  call XY_cross (rvec_car, Vn_car, Vt_car, 3)
!!  call vect_from_car_2_sph (Pmid_car, Pmid_sph, ndim)
!!  !
!!  return
!!end subroutine arc_mid_ntvec
!!
!!
!!subroutine area_sph_triangle (P1_sph, P2_sph, P3_sph, area, ndim)
!!  use kinds, only: rt
!!  implicit none
!!  integer ,intent(in) :: ndim
!!  real(rt),intent(in ):: P1_sph(ndim), P2_sph(ndim), P3_sph(ndim)
!!  real(rt),intent(out):: area
!!  real(rt)  :: V1(3), V2(3), V3(3)      ! cartesian of P1_sph, P2_sph, etc.
!!  real(rt)  :: R, a, b, c, s, E
!!  real(rt)  :: s1, s2, s3               ! dot product
!!  integer   :: i
!!  !
!!  !  Area = R^2 * Exess,  Excess = (A+B+C)-pi
!!  !  tan(E/4) = sqrt [ tan s/2 * tan( (s-a)/2 ) * tan( (s-b)/2 ) * tan( (s-c)/2 ) ]
!!  !  s= (a+b+c)/2
!!  !  \vec a dot b = R^2 cos(c/R)
!!  !
!!  !     P1         P2 
!!  !       \       /
!!  !        \     /
!!  !         \   /
!!  !          \ /______  P3_
!!  !
!!  R= P1_sph(3)
!!  if (abs(P2_sph(3)-R).GT.1.d-8 .OR. abs(P3_sph(3)-R).GT.1.d-8 ) then
!!     STOP 'P1,P2,P3 not on sphere'
!!  endif
!!  call vect_from_sph_2_car (P1_sph, V1, ndim)
!!  call vect_from_sph_2_car (P2_sph, V2, ndim)
!!  call vect_from_sph_2_car (P3_sph, V3, ndim)
!!  call XY_dot (V2, V3, ndim, ndim, s1)
!!  call XY_dot (V3, V1, ndim, ndim, s2)
!!  call XY_dot (V1, V2, ndim, ndim, s3)
!!  a= R* acos(s1/R/R)
!!  b= R* acos(s2/R/R)
!!  c= R* acos(s3/R/R)
!!  s= 0.5d0*(a+b+c)
!!  E= 4.d0 * atan(sqrt(tan(0.5d0*s)*tan(0.5d0*(s-a))*tan(0.5d0*(s-b))*tan(0.5d0*(s-c))))
!!  area= R*R*E
!!  return
!!end subroutine area_sph_triangle
!!
!!
!!
!!subroutine center_wt_sph_triangle (P1_sph, P2_sph, P3_sph, center_sph, center_car, wt, ndim, npt)
!!  use kinds, only: rt
!!  implicit none
!!  integer ,intent(in) :: ndim, npt
!!  real(rt),intent(in ):: P1_sph(ndim), P2_sph(ndim), P3_sph(ndim)
!!  real(rt),intent(out):: center_sph(ndim), center_car(ndim)
!!  real(rt),intent(out):: wt(npt)
!!  !
!!  real(rt)  :: V1(3), V2(3), V3(3), T1(3), T2(3)    ! cartesian of P1_sph, P2_sph, etc.
!!  real(rt)  :: T1(3), T2(3), Rvec(3)   ! cartesian of in-plane vector
!!  real(rt)  :: R, a, b, c, s, E
!!  real(rt)  :: s1, s2, s3               ! dot product
!!  integer   :: i
!!  !
!!  !  Area = R^2 * Exess,  Excess = (A+B+C)-pi
!!  !  tan(E/4) = sqrt [ tan s/2 * tan( (s-a)/2 ) * tan( (s-b)/2 ) * tan( (s-c)/2 ) ]
!!  !  s= (a+b+c)/2
!!  !  \vec a dot b = R^2 cos(c/R)
!!  !
!!  !     P1 ------  P2 
!!  !       \  .    /  \
!!  !        \   . /    \
!!  !         \   / .    \
!!  !          \ /_____. P3
!!  !          O
!!  ! 
!!  R= P1_sph(3)
!!  if (abs(P2_sph(3)-R).GT.1.d-8 .OR. abs(P3_sph(3)-R).GT.1.d-8 ) then
!!     STOP 'P1,P2,P3 not on sphere'
!!  endif
!!  call vect_from_sph_2_car (P1_sph, V1, ndim)
!!  call vect_from_sph_2_car (P2_sph, V2, ndim)
!!  call vect_from_sph_2_car (P3_sph, V3, ndim)
!!  do i=1, 3
!!     T1(i)= V2(i)-V1(i)
!!     T2(i)= V3(i)-V1(i)
!!  enddo
!!  call XY_cross (T1, T2, Rvec, 3)
!!  call X_2norm (Rvec, s, 3)    ! s cannot be zero
!!  do i=1, 3
!!     center_car(i)= Rvec(i)/s
!!  enddo
!!  call XY_cross (rvec_car, Vn_car, Vt_car, 3)
!!  call vect_from_car_2_sph (Pmid_car, Pmid_sph, ndim)
!!
!!
!!
!!
!!  call XY_cross (rvec_car, Vn_car, Vt_car, 3)
!!  call vect_from_car_2_sph (Pmid_car, Pmid_sph, ndim)
!!
!!
!!
!!  call XY_dot (V2, V3, ndim, ndim, s1)
!!  call XY_dot (V3, V1, ndim, ndim, s2)
!!  call XY_dot (V1, V2, ndim, ndim, s3)
!!  a= R* acos(s1/R/R)
!!  b= R* acos(s2/R/R)
!!  c= R* acos(s3/R/R)
!!  s= 0.5d0*(a+b+c)
!!  E= 4.d0 * atan(sqrt(tan(0.5d0*s)*tan(0.5d0*(s-a))*tan(0.5d0*(s-b))*tan(0.5d0*(s-c))))
!!  area= R*R*E
!!  return
!!end subroutine area_sph_triangle
!!
!!
