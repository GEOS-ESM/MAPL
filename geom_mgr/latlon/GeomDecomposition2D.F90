#include "MAPL_ErrLog.h"

module mapl3g_GeomDecomposition2D
   use MaplShared
   use mapl3g_HConfigUtils
   use esmf
   implicit none
   private

   public :: GeomDecomposition2D


   type :: GeomDecomposition2D
      integer :: nx = MAPL_UNDEFINED_INTEGER
      integer :: ny = MAPL_UNDEFINED_INTEGER
      integer, allocatable :: ims(:)
      integer, allocatable :: jms(:)
   end type GeomDecomposition2D

   interface GeomDecomposition2D
      procedure new_GeomDecomposition_from_hconfig
   end interface GeomDecomposition2D

contains


   function new_GeomDecomposition_from_hconfig(hconfig, rc) result(decomposition)
      type(GeomDecomposition2D) :: decomposition
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status

      associate (nx => decomposition%nx, ny => decomposition%ny)
        call MAPL_GetResource(nx, hconfig, 'nx', default=MAPL_UNDEFINED_INTEGER, _RC)
        decomposition%ims = get_1d_layout(hconfig, 'ims', nx, _RC)

        call MAPL_GetResource(ny, hconfig, 'ny', default=MAPL_UNDEFINED_INTEGER, _RC)
        decomposition%jms = get_1d_layout(hconfig, 'jms', ny, _RC)
      end associate

      _RETURN(_SUCCESS)
   end function new_GeomDecomposition_from_hconfig


   function get_1d_layout(hconfig, key, n, rc) result(ms)
      integer, allocatable :: ms(:)
      type(ESMF_HConfig), intent(in) :: hconfig
      character(*), intent(in) :: key
      integer, intent(in) :: n
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: decomp_from_file
      character(:), allocatable :: filename

      decomp_from_file = ESMF_HConfigIsDefined(hconfig, keystring=key//'_file', _RC)
      if ( decomp_from_file ) then
         filename = ESMF_HConfigAsString(hconfig, keystring=key//'_file', _RC)
         ms = get_ms_from_file(filename, n, _RC)
      else
         call MAPL_GetResource(ms, hconfig, key, _RC)
      end if

      _RETURN(_SUCCESS)
   end function get_1d_layout

   function get_ms_from_file(filename, n, rc) result(values)
      integer, allocatable :: values(:)
      character(len=*), intent(in) :: filename
      integer, intent(in) :: n
      integer, optional, intent(out) :: rc

      type(ESMF_VM) :: vm
      logical :: file_exists
      integer :: i, total, unit
      integer :: localPet
      integer :: status


      allocate(values(n), _STAT) ! ensure result is always allocated
      call ESMF_VMGetCurrent(vm, _RC)
      call ESMF_VMGet(vm, localPet=localPet, _RC)

      ! To be efficient and robust on distributed filesystems, we only
      ! reed on root process and then broadcast to all others.
      if (localPet == 0) then
         inquire(FILE = trim(filename), exist=file_exists)
         _ASSERT(file_exists, 'File does not exist: '//filename)

         open(newunit=unit, file=filename, form='formatted', iostat=status)
         _ASSERT(status == 0, 'Error opening file: '//filename)
         read(unit,*, iostat=status) total; _VERIFY(status)
         _ASSERT(total == n, 'File '//filename//' has incorrect number of bins')

         do i = 1, n
            read(unit,*,iostat=status) values(i); _VERIFY(status)
         enddo

         close(unit, _IOSTAT)
      endif

      call ESMF_VMBroadcast(vm, values, count=n, rootPet=0, _RC)
      _RETURN(_SUCCESS)
   end function get_ms_from_file


end module mapl3g_GeomDecomposition2D

