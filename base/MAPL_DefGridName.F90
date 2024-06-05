subroutine MAPL_DefGridName (im,jm,gridname,iamroot)
implicit none
integer,intent(in)::im,jm
logical,intent(in)::iamroot
character(len=*),intent(out)::gridname
character(len=2) poletype
character(len=3) llcb
character(len=30) myfmt
character(len=2) ii,jj
poletype='PC'
if(mod(jm,2)==0) poletype='PE'

llcb='-DC' ! lat-lon
if(6*im==jm) llcb='-CF' ! cubed

! better than it was before, but still dummy
myfmt='null'
if(im>    10.and.im<100)   ii='i2'
if(im>   100.and.im<1000)  ii='i3'
if(im>  1000.and.im<10000) ii='i4'
if(im> 10000.and.im<100000)ii='i5'
!
if(jm>    10.and.jm<100)   jj='i2'
if(jm>   100.and.jm<1000)  jj='i3'
if(jm>  1000.and.jm<10000) jj='i4'
if(jm> 10000.and.jm<100000)jj='i5'

myfmt = '(a,'//ii//',a,'//jj//',a)'
write(gridname,fmt=trim(myfmt)) trim(poletype),im,'x',jm,trim(llcb)
end subroutine MAPL_DefGridName
