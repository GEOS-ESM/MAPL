#include "MAPL.h"

submodule (mapl3g_EASEGeomSpec) make_EASEGeomSpec_from_hconfig_smod
   use mapl3g_GeomSpec
   use mapl3g_EASEConversion
   use mapl3g_EASEDecomposition
   use mapl_ErrorHandling
   use esmf
   implicit none (type, external)

contains

   module function make_EASEGeomSpec_from_hconfig(hconfig, rc) result(spec)
      type(EASEGeomSpec) :: spec
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status, cols, rows
      character(:), allocatable :: grid_name
      type(EASEDecomposition) :: decomposition

      grid_name = ESMF_HConfigAsString(hconfig, keyString='grid_name', _RC)
      call ease_extent(grid_name, cols, rows, _RC)
      decomposition = make_decomposition_from_hconfig_(hconfig, [cols, rows], _RC)
      spec = EASEGeomSpec(grid_name, decomposition, _RC)

      _RETURN(_SUCCESS)
   end function make_EASEGeomSpec_from_hconfig

   ! ---------------------------------------------------------------
   ! Private helper: build an EASEDecomposition from hconfig.
   !
   ! Priority order (first match wins):
   !   1. ims_file / jms_file  — paths to YAML files each containing
   !                             a top-level sequence key 'ims' or 'jms'
   !   2. ims / jms            — inline integer sequences
   !   3. nx / ny              — explicit topology (tile counts)
   !   4. (none)               — auto-partition from current ESMF VM
   ! ---------------------------------------------------------------
   function make_decomposition_from_hconfig_(hconfig, dims, rc) result(decomp)
      type(EASEDecomposition) :: decomp
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, intent(in) :: dims(2)
      integer, optional, intent(out) :: rc

      integer :: status, nx, ny
      integer, allocatable :: ims(:), jms(:)
      logical :: has_ims_file, has_jms_file, has_ims, has_jms, has_nx, has_ny

      ! --- 1. File-based ims/jms ---
      has_ims_file = ESMF_HConfigIsDefined(hconfig, keyString='ims_file', _RC)
      has_jms_file = ESMF_HConfigIsDefined(hconfig, keyString='jms_file', _RC)
      _ASSERT(has_ims_file .eqv. has_jms_file, 'ims_file and jms_file must both be defined or both be absent')

      if (has_ims_file) then
         ims = read_distribution_file_('ims_file', hconfig, _RC)
         jms = read_distribution_file_('jms_file', hconfig, _RC)
         decomp = EASEDecomposition(ims, jms)
         _RETURN(_SUCCESS)
      end if

      ! --- 2. Inline ims/jms sequences ---
      has_ims = ESMF_HConfigIsDefined(hconfig, keyString='ims', _RC)
      has_jms = ESMF_HConfigIsDefined(hconfig, keyString='jms', _RC)
      _ASSERT(has_ims .eqv. has_jms, 'ims and jms must both be defined or both be absent')

      if (has_ims) then
         ims = ESMF_HConfigAsI4Seq(hconfig, keyString='ims', _RC)
         jms = ESMF_HConfigAsI4Seq(hconfig, keyString='jms', _RC)
         decomp = EASEDecomposition(ims, jms)
         _RETURN(_SUCCESS)
      end if

      ! --- 3. Explicit tile-count topology nx/ny ---
      has_nx = ESMF_HConfigIsDefined(hconfig, keyString='nx', _RC)
      has_ny = ESMF_HConfigIsDefined(hconfig, keyString='ny', _RC)
      _ASSERT(has_nx .eqv. has_ny, 'nx and ny must both be defined or both be absent')

      if (has_nx) then
         nx = ESMF_HConfigAsI4(hconfig, keyString='nx', _RC)
         ny = ESMF_HConfigAsI4(hconfig, keyString='ny', _RC)
         decomp = EASEDecomposition(dims, topology=[nx, ny])
         _RETURN(_SUCCESS)
      end if

      ! --- 4. Auto-partition from current ESMF VM ---
      decomp = make_EASEDecomposition(dims, _RC)

      _RETURN(_SUCCESS)
   end function make_decomposition_from_hconfig_

   ! ---------------------------------------------------------------
   ! Read a per-DE distribution array from a YAML file whose path is
   ! stored in hconfig under the given key.  The file must contain a
   ! top-level mapping with a single sequence key matching the
   ! distribution name, e.g.:
   !
   !   ims: [1, 5, 3, ...]
   !
   ! The key inside the file is the same as the key_name but without
   ! the trailing '_file' suffix (i.e. 'ims_file' -> read key 'ims',
   ! 'jms_file' -> read key 'jms').
   ! ---------------------------------------------------------------
   function read_distribution_file_(key_name, hconfig, rc) result(dist)
      integer, allocatable :: dist(:)
      character(len=*), intent(in) :: key_name
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: filepath, inner_key
      type(ESMF_HConfig) :: file_hconfig

      filepath = ESMF_HConfigAsString(hconfig, keyString=key_name, _RC)

      file_hconfig = ESMF_HConfigCreate(filename=filepath, _RC)

      ! Strip the trailing '_file' to get the inner key ('ims' or 'jms')
      inner_key = key_name(1 : len(key_name) - len('_file'))

      dist = ESMF_HConfigAsI4Seq(file_hconfig, keyString=inner_key, _RC)

      call ESMF_HConfigDestroy(file_hconfig, _RC)

      _RETURN(_SUCCESS)
   end function read_distribution_file_

end submodule make_EASEGeomSpec_from_hconfig_smod
