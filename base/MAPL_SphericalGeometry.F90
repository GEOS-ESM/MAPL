#include "MAPL_ErrLog.h"
module MAPL_SphericalGeometry
   use MAPL_KeywordEnforcerMod
   use mapl_ErrorHandlingMod
   use ESMF
   use MAPL_ConstantsMod
   use, intrinsic :: iso_fortran_env, only: REAL64,REAL32

implicit none
private
public get_points_in_spherical_domain
contains

 subroutine get_points_in_spherical_domain(center_lons,center_lats,corner_lons,corner_lats,lons,lats,ii,jj,rc)
    real(real64), intent(in) :: center_lats(:,:),center_lons(:,:)
    real(real64), intent(in) :: corner_lats(:,:),corner_lons(:,:)
    real(real64), intent(in) :: lons(:),lats(:)
    integer, intent(out) :: ii(:),jj(:)
    integer, intent(out), optional :: rc

    integer :: npts,i,n,niter,im,jm,ilb,jlb,iub,jub,ifound,jfound
    integer :: lold,uold,lnew,unew
    logical :: in_region,in_sub_region
    integer :: status

    npts = size(lats)

    _ASSERT(size(lats)==size(lons),"lats and lons do not match")
    _ASSERT(npts==size(ii),"size of ii does not match")
    _ASSERT(npts==size(ii),"size of jj does not match")

    im=size(corner_lons,1)-1
    jm=size(corner_lons,2)-1
    niter = max(im,jm)

    do i=1,npts
       ifound=-1
       jfound=-1
       ilb=1
       iub=im
       jlb=1
       jub=jm
       in_region = point_in_polygon([lons(i),lats(i)],[center_lons(ilb,jlb),center_lats(ilb,jlb)],  &
           [corner_lons(ilb,jlb),corner_lats(ilb,jlb)], &
           [corner_lons(iub+1,jlb),corner_lats(iub+1,jlb)], &
           [corner_lons(iub+1,jub+1),corner_lats(iub+1,jub+1)], &
           [corner_lons(ilb,jub+1),corner_lats(ilb,jub+1)])
       if (in_region) then
          ! bisect first dimension
          lnew=ilb
          unew=iub 
          do n = 1,niter
             lold=lnew
             uold=unew
             unew=lold+(uold-lold)/2
             in_sub_region = point_in_polygon([lons(i),lats(i)], [center_lons(lnew,jlb),center_lats(lnew,jlb)], &
                 [corner_lons(lnew,jlb),corner_lats(lnew,jlb)], &
                 [corner_lons(unew+1,jlb),corner_lats(unew+1,jlb)], &
                 [corner_lons(unew+1,jub+1),corner_lats(unew+1,jub+1)], &
                 [corner_lons(lnew,jub+1),corner_lats(lnew,jub+1)])
             if (in_sub_region) then
               lnew=lold
               unew=unew 
             else
               lnew=unew+1
               unew=uold
             end if
             if (unew==lnew) then
                ifound=unew
                exit
             end if
          enddo
          ! bisect 2nd dimension
          lnew=jlb
          unew=jub
          do n = 1,niter
             lold=lnew
             uold=unew
             unew=lold+(uold-lold)/2
             in_sub_region = point_in_polygon([lons(i),lats(i)], [center_lons(ifound,lnew),center_lats(ifound,lnew)] , &
                 [corner_lons(ifound,lnew),corner_lats(ifound,lnew)], &
                 [corner_lons(ifound+1,lnew),corner_lats(ifound+1,lnew)], &
                 [corner_lons(ifound+1,unew+1),corner_lats(ifound+1,unew+1)], &
                 [corner_lons(ifound,unew+1),corner_lats(ifound,unew+1)])
             if (in_sub_region) then
               lnew=lold
               unew=unew 
             else
               lnew=unew+1
               unew=uold
             end if
             if (unew==lnew) then
                jfound=unew
                exit
             end if
          enddo
       end if
       ii(i)=ifound
       jj(i)=jfound
    enddo
    _RETURN(_SUCCESS)
         
 end subroutine get_points_in_spherical_domain 

 function point_in_polygon(p0,pinside,a1,a2,a3,a4) result(in_poly)
    real(real64), intent(in) :: p0(2),pinside(2),a1(2),a2(2),a3(2),a4(2)
    logical :: in_poly
 
    real(real64) :: p1c(3),p2c(3),a1c(3),a2c(3),a3c(3),a4c(3)
    logical :: intersect(4)
    p1c=convert_to_cart(p0)
    p2c=convert_to_cart(pinside)
    a1c=convert_to_cart(a1)
    a2c=convert_to_cart(a2)
    a3c=convert_to_cart(a3)
    a4c=convert_to_cart(a4)

    intersect(1) = lines_intersect(p1c,p2c,a1c,a2c)
    intersect(2) = lines_intersect(p1c,p2c,a2c,a3c)
    intersect(3) = lines_intersect(p1c,p2c,a3c,a4c)
    intersect(4) = lines_intersect(p1c,p2c,a4c,a1c)
    if (mod(count(intersect),2)==0) then
       in_poly=.true.
    else
       in_poly=.false.
    end if


 end function point_in_polygon

! it is claimed this should work but doesn't
 !function point_in_polygon_crosprod(p1,a1,a2,a3,a4) result(in_poly)
    !real(real64), intent(in) :: p1(2),a1(2),a2(2),a3(2),a4(2)
    !logical :: in_poly
 
    !real(real64) :: p1c(3),a1c(3),a2c(3),a3c(3),a4c(3)
    !real(real64) :: crs12(3),crs23(3),crs34(3),crs41(3)
    !real(real64) :: d12,d23,d34,d41
    !logical :: signs(4)
    !! a1 -> a2 -> a3 -> a4 so a4 connects to a1

    !p1c=convert_to_cart(p1)
    !a1c=convert_to_cart(a1)
    !a2c=convert_to_cart(a2)
    !a3c=convert_to_cart(a3)
    !a4c=convert_to_cart(a4)

    !crs12 = cross_prod(a1c,a2c)
    !crs23 = cross_prod(a2c,a3c)
    !crs34 = cross_prod(a3c,a4c)
    !crs41 = cross_prod(a4c,a1c)
    !d12=dot_product(p1c,crs12)
    !d23=dot_product(p1c,crs23)
    !d34=dot_product(p1c,crs34)
    !d41=dot_product(p1c,crs41)
    !signs(1)= (d12<0.0)
    !signs(2)= (d23<0.0)
    !signs(3)= (d34<0.0)
    !signs(4)= (d41<0.0)
    !in_poly=( (count(signs)==0) .or. (count(signs)==4) )

 !end function point_in_polygon_crossprod

 function lines_intersect(b0,b1,a0,a1)  result(intersect)
    real(real64), intent(in) :: b0(3),b1(3),a0(3),a1(3)
    logical :: intersect
    real(real64) :: p(3),q(3),t(3)
    real(real64) :: s1,s2,s3,s4
    logical :: signs(4)

    intersect=.false.
    q=cross_prod(b0,b1)
    p=cross_prod(a0,a1)
    t=normal_vect(cross_prod(p,q))

    s1=dot_product(cross_prod(a0,p),t)
    s2=dot_product(cross_prod(a1,p),t)
    s3=dot_product(cross_prod(b0,q),t)
    s4=dot_product(cross_prod(b1,q),t)

    signs(1) = -s1 <0.d0
    signs(2) = s2 <0.d0
    signs(3) = -s3 < 0.d0
    signs(4) = s4 < 0.d0

    intersect = ((count(signs)==0) .or. (count(signs)==4))

 end function lines_intersect

 function normal_vect(vin) result(vout)
    real(real64), intent(in) :: vin(3)
    real(real64) :: vout(3)
    vout=vin/sqrt(vin(1)*vin(1)+vin(2)*vin(2)+vin(3)*vin(3))

 end function normal_vect

 function cross_prod(v1,v2) result(vout)
    real(real64), intent(in) :: v1(3),v2(3)
    real(real64) :: vout(3)
    vout(1)=v1(2)*v2(3)-v1(3)*v2(2)
    vout(2)=v1(3)*v2(1)-v1(1)*v2(3)
    vout(3)=v1(1)*v2(2)-v1(2)*v2(1)
 end function cross_prod

 function convert_to_cart(v) result(xyz)
    real(real64), intent(in) :: v(2)
    real(real64) :: xyz(3)

    xyz(1)=cos(v(2))*cos(v(1))
    xyz(2)=cos(v(2))*sin(v(1))
    xyz(3)=sin(v(2))

 end function convert_to_cart

function vect_mag(v) result(mag)
   real(real64), intent(in) :: v(3)
   real :: mag
   mag = sqrt(v(1)*v(1)+v(2)*v(2)+v(3)*v(3))
end function vect_mag

end module MAPL_SphericalGeometry
