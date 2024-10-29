subroutine MAPL_DefGridName (im,jm,gridname,iamroot)
implicit none
integer,intent(in)::im,jm
logical,intent(in)::iamroot
character(len=*),intent(out)::gridname
character(len=2) poletype
character(len=3) llcb
character(len=30) myfmt
poletype='PC'
if(mod(jm,2)==0) poletype='PE'

llcb='-DC' ! lat-lon
if(6*im==jm) llcb='-CF' ! cubed

! there has to be a smarter way to do this format 
if(im>10.and.im<100.and.&
   jm>10.and.jm<100) then
   myfmt='(a,i2,a,i2,a)'
endif
if(im>100.and.im<1000.and.&
   jm>10.and.jm<100) then
   myfmt='(a,i3,a,i2,a)'
endif
if(im>100.and.im<1000.and.&
   jm>100.and.jm<1000) then
   myfmt='(a,i3,a,i3,a)'
endif
if(im>1000.and.im<10000.and.&
   jm>100 .and.jm<1000) then
   myfmt='(a,i4,a,i3,a)'
endif
if(im>100 .and.im<1000.and.&
   jm>1000.and.jm<100) then
   myfmt='(a,i3,a,i4,a)'
endif
if(im>1000.and.im<10000.and.&
   jm>1000.and.jm<10000) then
   myfmt='(a,i4,a,i4,a)'
endif
write(gridname,fmt=trim(myfmt)) trim(poletype),im,'x',jm,trim(llcb)
if(iamroot)print*,'MAPL_DefGridName: ',trim(gridname)
end subroutine MAPL_DefGridName
