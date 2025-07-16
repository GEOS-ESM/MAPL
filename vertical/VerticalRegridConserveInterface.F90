#include "MAPL_Exceptions.h"
module VerticalRegridConserveInterfaceMod
   use PFIO
   use MAPL_ExceptionHandling
   use MAPL_CommsMod
   use MAPL_Eta2EtaMod
   use MAPL_ConstantsMod, only: MAPL_GRAV,MAPL_AIRMW,MAPL_H2OMW
   use udunits2f, UDUNITS_are_convertible => are_convertible, &
      initialize_udunits => initialize, finalize_udunits => finalize

   implicit none
   private
   public vremap_conserve_mass_mixing
   public vremap_conserve_emission
   public vremap_conserve_vol_mixing
   public mass_mixing
   public volume_mixing
   public emission

   enum, bind(c)
      enumerator :: mass_mixing
      enumerator :: volume_mixing
      enumerator :: emission
   end enum

contains

   subroutine vremap_conserve_mass_mixing(src_pressure, src_values, dst_pressure, dst_values)
      real, intent(in) :: src_pressure(:,:,:)
      real, intent(in) :: src_values(:,:,:)
      real, intent(in) :: dst_pressure(:,:,:)
      real, intent(inout) :: dst_values(:,:,:)
      real, allocatable :: temp_pressures_src(:,:,:), temp_values_src(:,:,:)
      real, allocatable :: temp_pressures_dst(:,:,:), temp_values_dst(:,:,:)
      real :: src_max_p
      integer :: lb_src, lb_dst, lm_src, lm_dst, ub_src, ub_dst, im, jm

      lm_src = size(src_values,3)
      lm_dst = size(dst_values,3)
      lb_src = lbound(src_pressure,3)
      lb_dst = lbound(dst_pressure,3)
      ub_src = ubound(src_pressure,3)
      ub_dst = ubound(dst_pressure,3)
      im = size(src_values,1)
      jm = size(src_values,2)

      ! src gets extra level that is zero becasue gmap persists src value in dst below surface
      src_max_p = maxval(src_pressure(:,:,ub_src))
      allocate(temp_pressures_src(im,jm,lb_src:ub_src+1))  
      allocate(temp_values_src(im,jm,lm_src+1))
      temp_pressures_src(:,:,lb_src:ub_src) = src_pressure
      temp_values_src(:,:,1:lm_src) = src_values
      temp_pressures_src(:,:,ub_src+1) = src_pressure(:,:,ub_src)+10.0
      temp_values_src(:,:,lm_src+1) = 0.0

      ! add an extra level on dst because if src is below destination we will need the extra stuff
      ! we need to make sure "extra" stuf from src gets included
      allocate(temp_pressures_dst(im,jm,lb_dst:ub_dst+1))  
      allocate(temp_values_dst(im,jm,lm_dst+1))
      temp_pressures_dst(:,:,lb_dst:ub_dst) = dst_pressure
      temp_pressures_dst(:,:,ub_dst+1) = src_max_p + 10.0

      call gmap(im, jm, lm_src+1, temp_pressures_src, temp_values_src, lm_dst+1, temp_pressures_dst, temp_values_dst)

      ! add back the "extra" level, have to convert to emission to do it
      temp_values_dst(:,:,lm_dst) = temp_values_dst(:,:,lm_dst)*(temp_pressures_dst(:,:,lm_dst+1)-temp_pressures_dst(:,:,lm_dst))/MAPL_GRAV &
         + temp_values_dst(:,:,lm_dst+1)*(temp_pressures_dst(:,:,lm_dst+2)-temp_pressures_dst(:,:,lm_dst+1))/MAPL_GRAV
      ! add convert back to mass mixing
      temp_values_dst(:,:,lm_dst) = temp_values_dst(:,:,lm_dst)*MAPL_GRAV/(temp_pressures_dst(:,:,lm_dst+1)-temp_pressures_dst(:,:,lm_dst))

      dst_values = temp_values_dst(:,:,1:lm_dst)

   end subroutine vremap_conserve_mass_mixing

   subroutine vremap_conserve_emission(src_pressure, src_values, dst_pressure, dst_values)
      real, intent(in) :: src_pressure(:,:,:)
      real, intent(in) :: src_values(:,:,:)
      real, intent(in) :: dst_pressure(:,:,:)
      real, intent(inout) :: dst_values(:,:,:)
      real, allocatable :: temp_pressures_src(:,:,:), temp_values_src(:,:,:)
      real, allocatable :: temp_pressures_dst(:,:,:), temp_values_dst(:,:,:)
      real :: src_max_p
      integer :: lb_src, lb_dst, lm_src, lm_dst, ub_src, ub_dst, i , im, jm

      lm_src = size(src_values,3)
      lm_dst = size(dst_values,3)
      lb_src = lbound(src_pressure,3)
      lb_dst = lbound(dst_pressure,3)
      ub_src = ubound(src_pressure,3)
      ub_dst = ubound(dst_pressure,3)
      im = size(src_values,1)
      jm = size(src_values,2)

      ! src gets extra level that is zero becasue gmap persists src value in dst below surface
      src_max_p = maxval(src_pressure(:,:,ub_src))
      allocate(temp_pressures_src(im,jm,lb_src:ub_src+1))  
      allocate(temp_values_src(im,jm,lm_src+1))
      temp_pressures_src(:,:,lb_src:ub_src) = src_pressure
      temp_values_src(:,:,1:lm_src) = src_values
      temp_pressures_src(:,:,ub_src+1) = src_pressure(:,:,ub_src)+10.0
      temp_values_src(:,:,lm_src+1) = 0.0

      ! add an extra level on dst because if src is below destination we will need the extra stuff
      ! we need to make sure "extra" stuf from src gets included
      allocate(temp_pressures_dst(im,jm,lb_dst:ub_dst+1))  
      allocate(temp_values_dst(im,jm,lm_dst+1))
      temp_pressures_dst(:,:,lb_dst:ub_dst) = dst_pressure
      temp_pressures_dst(:,:,ub_dst+1) = src_max_p + 10.0

      ! gmap wants mass mixing
      do i=1,lm_src
         temp_values_src(:,:,i) = temp_values_src(:,:,i)*MAPL_GRAV/(temp_pressures_src(:,:,i+1)-temp_pressures_src(:,:,i))
      enddo

      call gmap(im, jm, lm_src+1, temp_pressures_src, temp_values_src, lm_dst+1, temp_pressures_dst, temp_values_dst)

      ! add back the "extra" level
      temp_values_dst(:,:,lm_dst) = temp_values_dst(:,:,lm_dst) + temp_values_dst(:,:,lm_dst+1)

      ! if we were emission need to convert back from mass mixing
      do i=1,lm_dst
         temp_values_dst(:,:,i) = temp_values_dst(:,:,i)*(temp_pressures_dst(:,:,i+1)-temp_pressures_dst(:,:,i))/MAPL_GRAV
      enddo
      dst_values = temp_values_dst(:,:,1:lm_dst)

   end subroutine vremap_conserve_emission

   subroutine vremap_conserve_vol_mixing(src_pressure, src_q, mol_weight, src_values, dst_pressure, dst_q, dst_values, rc)
      real, intent(in) :: src_pressure(:,:,:)
      real, intent(in) :: src_q(:,:,:)
      real, intent(in) :: src_values(:,:,:)
      real, intent(in) :: mol_weight
      real, intent(in) :: dst_pressure(:,:,:)
      real, intent(in) :: dst_q(:,:,:)
      real, intent(inout) :: dst_values(:,:,:)
      integer, intent(out), optional :: rc

      integer :: status
      real, allocatable :: temp_pressures_src(:,:,:), temp_values_src(:,:,:)
      real, allocatable :: temp_pressures_dst(:,:,:), temp_values_dst(:,:,:)
      real :: src_max_p
      integer :: lb_src, lb_dst, lm_src, lm_dst, ub_src, ub_dst, im, jm

      lm_src = size(src_values,3)
      lm_dst = size(dst_values,3)
      lb_src = lbound(src_pressure,3)
      lb_dst = lbound(dst_pressure,3)
      ub_src = ubound(src_pressure,3)
      ub_dst = ubound(dst_pressure,3)
      im = size(src_values,1)
      jm = size(src_values,2)

      ! src gets extra level that is zero becasue gmap persists src value in dst below surface
      src_max_p = maxval(src_pressure(:,:,ub_src))
      allocate(temp_pressures_src(im,jm,lb_src:ub_src+1))  
      allocate(temp_values_src(im,jm,lm_src+1))
      temp_pressures_src(:,:,lb_src:ub_src) = src_pressure
      temp_values_src(:,:,1:lm_src) = src_values
      temp_pressures_src(:,:,ub_src+1) = src_pressure(:,:,ub_src)+10.0
      temp_values_src(:,:,lm_src+1) = 0.0
      temp_values_src(:,:,1:lm_src) = temp_values_src(:,:,1:lm_src)*(((1.0-src_q)*(mol_weight/MAPL_AIRMW))+(src_q*mol_weight/MAPL_H2OMW))

      ! add an extra level on dst because if src is below destination we will need the extra stuff
      ! we need to make sure "extra" stuf from src gets included
      allocate(temp_pressures_dst(im,jm,lb_dst:ub_dst+1))  
      allocate(temp_values_dst(im,jm,lm_dst+1))
      temp_pressures_dst(:,:,lb_dst:ub_dst) = dst_pressure
      temp_pressures_dst(:,:,ub_dst+1) = src_max_p + 10.0

      call gmap(im, jm, lm_src+1, temp_pressures_src, temp_values_src, lm_dst+1, temp_pressures_dst, temp_values_dst)

      ! add back the "extra" level, have to convert to emission to do it
      temp_values_dst(:,:,lm_dst) = temp_values_dst(:,:,lm_dst)*(temp_pressures_dst(:,:,lm_dst+1)-temp_pressures_dst(:,:,lm_dst))/MAPL_GRAV &
         + temp_values_dst(:,:,lm_dst+1)*(temp_pressures_dst(:,:,lm_dst+2)-temp_pressures_dst(:,:,lm_dst+1))/MAPL_GRAV
      ! add convert back to mass mixing
      temp_values_dst(:,:,lm_dst) = temp_values_dst(:,:,lm_dst)*MAPL_GRAV/(temp_pressures_dst(:,:,lm_dst+1)-temp_pressures_dst(:,:,lm_dst))
      ! convert to volume mixing

      dst_values = temp_values_dst(:,:,1:lm_dst)
      dst_values = dst_values/(((1.0-dst_q)*(mol_weight/MAPL_AIRMW))+(dst_q*mol_weight/MAPL_H2OMW))

      _RETURN(_SUCCESS)
   end subroutine vremap_conserve_vol_mixing

end module VerticalRegridConserveInterfaceMod
