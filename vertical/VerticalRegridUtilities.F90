#include "MAPL_Exceptions.h"
module VerticalRegridUtilitiesMod
   use PFIO
   use MAPL_ExceptionHandling
   use MAPL_CommsMod
   use MAPL_ConstantsMod, only: MAPL_GRAV

   implicit none
   private
   public check_conservation

contains

   subroutine check_conservation(src_pressure, src_values, dst_pressure, dst_values, constituent_type)
      real, intent(in) :: src_pressure(:)
      real, intent(in) :: src_values(:)
      real, intent(in) :: dst_pressure(:)
      real, intent(in) :: dst_values(:)
      integer, intent(in) :: constituent_type
      real :: src_mass, dst_mass, delp
      integer :: lb_src, lb_dst, lm_src, lm_dst, i
      lm_src = size(src_values)
      lm_dst = size(dst_values)
      lb_src = lbound(src_pressure,1)
      lb_dst = lbound(dst_pressure,1)
      src_mass=0.0
      dst_mass=0.0
      do i=1,lm_src
         delp = src_pressure(lb_src+i)-src_pressure(lb_src+i-1)
         src_mass = src_mass + src_values(i)*delp/MAPL_GRAV
      enddo
      do i=1,lm_dst
         delp = dst_pressure(lb_dst+i)-dst_pressure(lb_dst+i-1)
         dst_mass = dst_mass + dst_values(i)*delp/MAPL_GRAV
      enddo
      if (src_mass .ne. 0.0) then
      _HERE, src_mass,dst_mass
      _HERE,(dst_mass-src_mass)/src_mass
      end if
   end subroutine check_conservation

end module VerticalRegridUtilitiesMod
