#include "MAPL.h"

submodule (mapl3g_XYGeomSpec) make_XYGeomSpec_from_hconfig_smod
   use mapl_ErrorHandlingMod
   use mapl_InternalConstants
   use mapl3g_Comms, only: am_i_root, ROOT_PROCESS_ID
   use NetCDF
   use esmf
   implicit none (type, external)

contains

   module function make_XYGeomSpec_from_hconfig(hconfig, rc) result(spec)
      type(XYGeomSpec) :: spec
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: arr(2)
      type(ESMF_VM) :: vm
      character(len=:), allocatable :: grid_file_name
      character(len=:), allocatable :: coord_class
      integer :: thin_factor, n1, n2, ncid, dimid
      logical :: has_lm, found
      character(len=:), allocatable :: index_name_x, index_name_y
      character(len=:), allocatable :: var_name_x, var_name_y
      character(len=:), allocatable :: var_name_proj, att_name_proj
      integer :: coord_mode

      ! Required: grid_file_name
      grid_file_name = ESMF_HConfigAsString(hconfig, keyString='grid_file_name', _RC)
      spec%grid_file_name = grid_file_name

      ! Optional vertical extent
      has_lm = ESMF_HConfigIsDefined(hconfig, keystring='LM', _RC)
      if (has_lm) then
         spec%lm = ESMF_HConfigAsI4(hconfig, keyString='LM', _RC)
      end if

      ! ABI/GOES scan-angle metadata (default to standard XY)
      coord_class = 'standard'
      found = ESMF_HConfigIsDefined(hconfig, keyString='coord_class', _RC)
      if (found) coord_class = ESMF_HConfigAsString(hconfig, keyString='coord_class', _RC)
      if (coord_class == 'ABI') then
          coord_mode = XY_COORD_ABI
      else
          coord_mode = XY_COORD_STANDARD
      end if
      spec%coord_mode = coord_mode

      if (coord_mode == XY_COORD_ABI) then
         index_name_x  = 'x' ;  index_name_y  = 'y'
         var_name_x    = 'x' ;  var_name_y    = 'y'
         var_name_proj = ''  ;  att_name_proj = ''
         thin_factor   = 1
         found = ESMF_HConfigIsDefined(hconfig, keyString='index_name_x', _RC)
         if (found) index_name_x = ESMF_HConfigAsString(hconfig, keyString='index_name_x', _RC)
         found = ESMF_HConfigIsDefined(hconfig, keyString='index_name_y', _RC)
         if (found) index_name_y = ESMF_HConfigAsString(hconfig, keyString='index_name_y', _RC)
         found = ESMF_HConfigIsDefined(hconfig, keyString='var_name_x', _RC)
         if (found) var_name_x = ESMF_HConfigAsString(hconfig, keyString='var_name_x', _RC)
         found = ESMF_HConfigIsDefined(hconfig, keyString='var_name_y', _RC)
         if (found) var_name_y = ESMF_HConfigAsString(hconfig, keyString='var_name_y', _RC)
         found = ESMF_HConfigIsDefined(hconfig, keyString='var_name_proj', _RC)
         if (found) var_name_proj = ESMF_HConfigAsString(hconfig, keyString='var_name_proj', _RC)
         found = ESMF_HConfigIsDefined(hconfig, keyString='att_name_proj', _RC)
         if (found) att_name_proj = ESMF_HConfigAsString(hconfig, keyString='att_name_proj', _RC)
         found = ESMF_HConfigIsDefined(hconfig, keyString='thin_factor', _RC)
         if (found) thin_factor = ESMF_HConfigAsI4(hconfig, keyString='thin_factor', _RC)
         spec%index_name_x  = index_name_x
         spec%index_name_y  = index_name_y
         spec%var_name_x    = var_name_x
         spec%var_name_y    = var_name_y
         spec%var_name_proj = var_name_proj
         spec%att_name_proj = att_name_proj
         spec%thin_factor   = thin_factor
      else
         index_name_x = 'Xdim' ;  index_name_y = 'Ydim'
         found = ESMF_HConfigIsDefined(hconfig, keyString='index_name_x', _RC)
         if (found) index_name_x = ESMF_HConfigAsString(hconfig, keyString='index_name_x', _RC)
         found = ESMF_HConfigIsDefined(hconfig, keyString='index_name_y', _RC)
         if (found) index_name_y = ESMF_HConfigAsString(hconfig, keyString='index_name_y', _RC)
         spec%index_name_x = index_name_x
         spec%index_name_y = index_name_y
         thin_factor = 1
      end if

      ! Read grid dimensions from file on root, then broadcast
      call ESMF_VMGetCurrent(vm, _RC)
      if (am_i_root()) then
         status = nf90_open(grid_file_name, NF90_NOWRITE, ncid)
         _VERIFY(status)
         status = nf90_inq_dimid(ncid, index_name_x, dimid)
         _VERIFY(status)
         status = nf90_inquire_dimension(ncid, dimid, len=n1)
         _VERIFY(status)
         status = nf90_inq_dimid(ncid, index_name_y, dimid)
         _VERIFY(status)
         status = nf90_inquire_dimension(ncid, dimid, len=n2)
         _VERIFY(status)
         status = nf90_close(ncid)
         _VERIFY(status)
         arr(1) = n1
         arr(2) = n2
      end if
      call ESMF_VMBroadcast(vm, arr, 2, ROOT_PROCESS_ID, _RC)
      n1 = arr(1)
      n2 = arr(2)

      spec%xdim_true = n1
      spec%ydim_true = n2
      spec%im_world  = n1 / thin_factor
      spec%jm_world  = n2 / thin_factor

      ! Check for corner coordinates (standard mode only)
      if (coord_mode == XY_COORD_STANDARD) then
         call check_has_corners(grid_file_name, spec%has_corners, vm, _RC)
      end if

      ! Compute decomposition
      call make_decomposition(spec, _RC)

      _RETURN(_SUCCESS)

   end function make_XYGeomSpec_from_hconfig

   ! -----------------------------------------------------------------------

   subroutine check_has_corners(filename, has_corners, vm, rc)
      character(len=*), intent(in)    :: filename
      logical,          intent(out)   :: has_corners
      type(ESMF_VM),    intent(in)    :: vm
      integer, optional, intent(out)  :: rc

      integer :: status, ncid, varid
      integer :: log_array(1)
      intrinsic :: merge
      intrinsic :: merge

      if (am_i_root()) then
         status = nf90_open(filename, NF90_NOWRITE, ncid)
         _VERIFY(status)
         status = NF90_inq_varid(ncid, 'corner_lons', varid)
         log_array(1) = merge(1, 0, status == NF90_NOERR)
         status = nf90_close(ncid)
         _VERIFY(status)
      end if
      call ESMF_VMBroadcast(vm, log_array, 1, ROOT_PROCESS_ID, _RC)
      has_corners = (log_array(1) == 1)

      _RETURN(_SUCCESS)
   end subroutine check_has_corners

   subroutine make_decomposition(spec, rc)
      type(XYGeomSpec), intent(inout) :: spec
      integer, optional, intent(out)  :: rc

      integer :: status
      integer :: petCount, nx, ny
      integer :: im, jm
      type(ESMF_VM) :: vm
      integer, allocatable :: ims(:), jms(:)

      im = spec%im_world
      jm = spec%jm_world

      call ESMF_VMGetCurrent(vm, _RC)
      call ESMF_VMGet(vm, petCount=petCount, _RC)

      ! Square-ish decomposition: factor petCount into nx * ny
      call compute_layout(petCount, im, jm, nx, ny)

      allocate(ims(nx), jms(ny))
      call distribute_dim(im, nx, ims)
      call distribute_dim(jm, ny, jms)

      spec%ims = ims
      spec%jms = jms

      _RETURN(_SUCCESS)
   end subroutine make_decomposition

   ! Distribute n elements across k DEs as evenly as possible,
   ! with minimum extent of 2 (required by ESMF_FieldRegrid).
   subroutine distribute_dim(n, k, counts)
      integer, intent(in)  :: n, k
      integer, intent(out) :: counts(k)
      integer :: base, remainder, i
      intrinsic :: merge
      intrinsic :: merge
      base      = n / k
      remainder = mod(n, k)
      do i = 1, k
         counts(i) = base + merge(1, 0, i <= remainder)
      end do
   end subroutine distribute_dim

   ! Factor petCount into nx * ny such that domains are as square as possible.
   subroutine compute_layout(petCount, im, jm, nx, ny)
      integer, intent(in)  :: petCount, im, jm
      integer, intent(out) :: nx, ny
      real    :: ratio, best, candidate
      integer :: i
      best  = huge(1.0)
      nx    = 1
      ny    = petCount
      ratio = real(im) / real(jm)
      do i = 1, petCount
         if (mod(petCount, i) == 0) then
            candidate = abs(real(i) / real(petCount/i) - ratio)
            if (candidate < best) then
               best = candidate
               nx   = i
               ny   = petCount / i
            end if
         end if
      end do
   end subroutine compute_layout

end submodule make_XYGeomSpec_from_hconfig_smod
