subroutine MAPL_DefGridName (im,jm,gridname,iamroot)
implicit none
integer,intent(in)::im,jm
logical,intent(in)::iamroot
character(len=*),intent(out)::gridname
character(len=2) poletype
character(len=3) llcb
character(len=30) imstr,jmstr
poletype='PC'
if(mod(jm,2)==0) poletype='PE'

llcb='-DC' ! lat-lon
if(6*im==jm) llcb='-CF' ! cubed

write(imstr,*) im
write(jmstr,*) jm

gridname=trim(poletype)//trim(adjustl(imstr))//'x'//&
                         trim(adjustl(jmstr))//trim(llcb)

end subroutine MAPL_DefGridName
