#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonGeomSpec) make_decomposition_smod
   use mapl3g_CoordinateAxis
   use mapl3g_GeomSpec
   use pfio
   use mapl_ErrorHandling
   use esmf
   implicit none (type, external)
   
contains

   module function make_decomposition(hconfig, dims, rc) result(decomp)
      type(LatLonDecomposition) :: decomp
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, intent(in) :: dims(2)
      integer, optional, intent(out) :: rc
      integer, allocatable :: ims(:), jms(:)
      integer :: nx, ny

      integer :: status
      logical :: has_ims, has_jms, has_nx, has_ny

      has_ims = ESMF_HConfigIsDefined(hconfig, keystring='ims', _RC)
      has_jms = ESMF_HConfigIsDefined(hconfig, keystring='jms', _RC)
      _ASSERT(has_ims .eqv. has_jms, 'ims and jms must be both defined or both undefined')

      if (has_ims) then
         ims = ESMF_HConfigAsI4Seq(hconfig, keyString='ims', _RC)
         jms = ESMF_HConfigAsI4Seq(hconfig, keyString='jms', _RC)
         decomp = LatLonDecomposition(ims, jms)
         _RETURN(_SUCCESS)
      end if

      has_nx = ESMF_HConfigIsDefined(hconfig, keystring='nx', _RC)
      has_ny = ESMF_HConfigIsDefined(hconfig, keystring='ny', _RC)
      _ASSERT(has_nx .eqv. has_ny, 'nx and ny must be both defined or both undefined')

      if (has_nx) then
         nx = ESMF_HConfigAsI4(hconfig, keyString='nx', _RC)
         ny = ESMF_HConfigAsI4(hconfig, keyString='ny', _RC)
         decomp = LatLonDecomposition(dims, topology=[nx, ny])
         _RETURN(_SUCCESS)
      end if

      ! Invent a decomposition
      decomp = make_LatLonDecomposition(dims, _RC)
      
      _RETURN(_SUCCESS)
   end function make_decomposition

end submodule make_decomposition_smod
