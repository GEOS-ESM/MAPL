#include "MAPL.h"

submodule (mapl3g_XYGeomSpec) make_XYGeomSpec_from_metadata_smod
   use mapl_ErrorHandlingMod
   use mapl_InternalConstants
   use mapl3g_Comms, only: am_i_root, ROOT_PROCESS_ID
   use NetCDF
   use pfio, only: FileMetadata
   use esmf
   implicit none

contains

   ! Build an XYGeomSpec from a pFIO FileMetadata object.
   ! The metadata must have 'Xdim' and 'Ydim' dimensions (as written
   ! by the MAPL2 XYGridFactory append_metadata), and a 'grid_type'
   ! attribute equal to 'XY'.
   module function make_XYGeomSpec_from_metadata(file_metadata, rc) result(spec)
      type(XYGeomSpec) :: spec
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status

      spec%im_world = file_metadata%get_dimension('Xdim', _RC)
      spec%jm_world = file_metadata%get_dimension('Ydim', _RC)

      if (file_metadata%has_dimension('lev')) then
         spec%lm = file_metadata%get_dimension('lev', _RC)
      end if

      spec%grid_file_name = file_metadata%get_source_file()
      spec%coord_mode     = XY_COORD_STANDARD

      ! Check for corner coordinates
      spec%has_corners = file_metadata%has_dimension('XCdim')

      call make_decomposition(spec, _RC)

      _RETURN(_SUCCESS)

   end function make_XYGeomSpec_from_metadata

   subroutine make_decomposition(spec, rc)
      type(XYGeomSpec), intent(inout) :: spec
      integer, optional, intent(out)  :: rc

      integer :: status
      integer :: petCount, nx, ny
      type(ESMF_VM) :: vm
      integer, allocatable :: ims(:), jms(:)

      call ESMF_VMGetCurrent(vm, _RC)
      call ESMF_VMGet(vm, petCount=petCount, _RC)

      call compute_layout(petCount, spec%im_world, spec%jm_world, nx, ny)
      allocate(ims(nx), jms(ny))
      call distribute_dim(spec%im_world, nx, ims)
      call distribute_dim(spec%jm_world, ny, jms)
      spec%ims = ims
      spec%jms = jms

      _RETURN(_SUCCESS)
   end subroutine make_decomposition

   subroutine distribute_dim(n, k, counts)
      integer, intent(in)  :: n, k
      integer, intent(out) :: counts(k)
      integer :: base, remainder, i
      base      = n / k
      remainder = mod(n, k)
      do i = 1, k
         counts(i) = base + merge(1, 0, i <= remainder)
      end do
   end subroutine distribute_dim

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

end submodule make_XYGeomSpec_from_metadata_smod
