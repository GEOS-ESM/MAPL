#include "MAPL_Generic.h"
#define _DEALOCS(A) if(associated(A)) then;if(MAPL_ShmInitialized) then; call MAPL_DeAllocNodeArray(A,rc=status);else; deallocate(A);endif;NULLIFY(A);endif

module MAPL_TilingRegridderMod
   use MAPL_AbstractRegridderMod
   use MAPL_KeywordEnforcerMod
   use mapl_ErrorHandlingMod
   use MAPL_RegridderSpec
   use MAPL_RegridMethods
   use MAPL_DirPathMod
   use MAPL_BaseMod, only: MAPL_UNDEF, MAPL_TileNameLength
   use MAPL_ShmemMod
   use Regrid_Functions_Mod, only: readTileFileNC_file 
   use ESMF
   use gFTL_IntegerVector
   use, intrinsic :: ISO_C_BINDING
   use, intrinsic :: iso_fortran_env, only: REAL32,INT32
   implicit none
   private

   public :: TilingRegridder


   enum, bind(C)
     enumerator :: TEMPEST
     enumerator :: GEOS_BINARY
   end enum

   ! Data for tiling of a single grid
   type GridTiling
      integer :: im
      integer :: jm
      integer, allocatable :: i_indices(:)
      integer, allocatable :: j_indices(:)
      real, allocatable :: weights(:)
      character(len=:), allocatable :: grid_name
   end type GridTiling

   ! A tile file contains a single tiling of two
   ! different grids.
   type TileFile
      integer :: n_tiles
      type (GridTiling) :: grid_tiles(2)
   end type TileFile

   ! An XTile contains only the information neeeded
   ! to regrid at a single tile.
   type, bind(c) :: XTile
      integer(INT32) :: idx_i_in
      integer(INT32) :: idx_j_in
      integer(INT32) :: idx_i_out
      integer(INT32) :: idx_j_out
      real(REAL32) :: weight
   end type XTile


   type, abstract, extends(AbstractRegridder) :: TilingRegridder
      private
      integer :: out_shape(2)
      type (XTile), pointer     :: global_x_tiles(:)
      type (XTile), allocatable :: local_x_tiles(:)
      integer :: file_type = GEOS_BINARY
   contains
      procedure :: clone
      procedure :: initialize_subclass
      procedure :: regrid_scalar_2d_real32
      procedure :: find_tile_file
      procedure :: copy_global_to_local
      procedure, nopass :: init_regrid
      procedure :: loop_over_tiles
      procedure (add_contribution), nopass, deferred :: add_contribution
      procedure, nopass :: final_regrid
   end type TilingRegridder

   abstract interface

      subroutine add_contribution(x_in, weight, x_out, fraction)
         use, intrinsic :: iso_fortran_env, only: REAL32
         real (kind=REAL32), intent(in) :: x_in
         real (kind=REAL32), intent(in) :: weight
         real (kind=REAL32), intent(inout) :: x_out
         real (kind=REAL32), intent(inout) :: fraction
      end subroutine add_contribution

   end interface

   interface MAPL_AllocNodeArray
      module procedure MAPL_AllocNodeArray_Tiling
   end interface

   interface MAPL_DeAllocNodeArray
      module procedure MAPL_DeAllocNodeArray_Tiling
   end interface

contains

   !--------------------------------------------------------------------------------
   ! Find and read a tile file corresponding to the requested grids (from spec).
   ! Then copy the TileFile object  data into a more custom structure for
   ! regridding purposes.
   !--------------------------------------------------------------------------------
   subroutine initialize_subclass(this, unusable, rc)
      use MAPL_KeywordEnforcerMod
      use MAPL_BaseMod, only: MAPL_GridGet
      class (TilingRegridder), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      
      integer :: status      
      character(len=*), parameter :: Iam = 'initialize_subclass'
      
      type (TileFile) :: tile_file
      character(len=:), allocatable :: file_name ! tile file
      logical :: swap
      integer :: idx_tiling_in
      integer :: idx_tiling_out
      type (RegridderSpec) :: spec

      _UNUSED_DUMMY(unusable)

      call this%find_tile_file(file_name, swap, rc=status)
      _VERIFY(status)

      select case (this%file_type)
      case (GEOS_BINARY)
         tile_file = read_geos_binary(file_name, rc=status)
         _VERIFY(status)
      case (TEMPEST)
         tile_file = read_tempest(file_name, rc=status)
         _VERIFY(status)
      end select

      if (swap) then
         idx_tiling_in = 2
         idx_tiling_out = 1
      else
         idx_tiling_in = 1
         idx_tiling_out = 2
      end if

      ! Copy tile_file into global_x_tiles
      if(MAPL_ShmInitialized) then
         call MAPL_AllocNodeArray( this%global_x_tiles,(/tile_file%n_tiles/),rc=status)
         _VERIFY(STATUS)
      else
         allocate(this%global_x_tiles(tile_file%n_tiles),stat=status)
         _VERIFY(STATUS)
      end if
      if (.not. MAPL_ShmInitialized  .or. MAPL_AmNodeRoot) then
         associate (tiles => this%global_x_tiles)
           tiles(:)%idx_i_in = tile_file%grid_tiles(idx_tiling_in)%i_indices(:)
           tiles(:)%idx_j_in = tile_file%grid_tiles(idx_tiling_in)%j_indices(:)
           tiles(:)%idx_i_out = tile_file%grid_tiles(idx_tiling_out)%i_indices(:)
           tiles(:)%idx_j_out = tile_file%grid_tiles(idx_tiling_out)%j_indices(:)
           tiles(:)%weight = tile_file%grid_tiles(idx_tiling_out)%weights(:)
         end associate
      end if
      call MAPL_SyncSharedMemory(rc=status)
      _VERIFY(status)

      ! Copy local subset of global_x_tiles into local_x_tiles
      call this%copy_global_to_local()

      associate (tile => tile_file%grid_tiles(idx_tiling_out))
        this%out_shape = [tile%im, tile%jm]
      end associate


      spec = this%get_spec()
      if (iand(spec%hints, REGRID_HINT_LOCAL) /= 0) then
         _DEALOCS(this%global_x_tiles)
      end if

      _RETURN(_SUCCESS)

   end subroutine initialize_subclass

   !--------------------------------------------------------------------------------
   ! Read the records of the named tile file and store them in the components
   ! of a TileFile object.
   !--------------------------------------------------------------------------------
    function read_geos_binary(file_name, unusable, rc) result(tile_file)
      use MAPL_CommsMod
      type (TileFile) :: tile_file
      character(len=*), intent(in) :: file_name
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: n_tiles
      integer :: n_grids
      integer :: idx_grid
      integer :: unit
      integer :: status
      character(len=*), parameter :: Iam = 'read_geos_binary'

      integer :: npes, deId
      logical :: am_i_root
      type (ESMF_VM) :: vm

      _UNUSED_DUMMY(unusable)

      call ESMF_VMGetCurrent(vm, rc=status)
      _VERIFY(status)
      call ESMF_VmGet(VM, localPet=deId, petCount=npes, rc=status)
      _VERIFY(status)

      am_i_root = (deId == 0)

      if (am_i_root) then
         open(file=file_name, newunit=unit, form='unformatted', iostat=status)
         _VERIFY(status)
      end if

      call read_integer(n_tiles)
      _VERIFY(status)
      tile_file%n_tiles = n_tiles

      call read_integer(n_grids)
      _VERIFY(status)
      _ASSERT(n_grids == 2, 'illegal value for n_grids (must be 2)')

      do idx_grid = 1, n_grids
         call read_tiling_metadata(tile_file%grid_tiles(idx_grid)) ! in
         _VERIFY(status)
      end do

      ! Skip records that are not used here
      if (am_i_root) then
         read(unit) ! skip type
         read(unit) ! skip X
         read(unit) ! skip Y
      end if

      do idx_grid = 1, n_grids ! always 2
         call read_tiling_data(tile_file%n_tiles, tile_file%grid_tiles(idx_grid))
         _VERIFY(status)
      end do

      if (am_i_root) then
         close(unit)
      end if

      _RETURN(_SUCCESS)

   contains

      subroutine read_integer(item)
         integer, intent(out) :: item

         if (am_i_root) then
            read(unit) item
         end if
         call MAPL_CommsBcast(vm, data=item, N=1, root=0, rc=status)
         
      end subroutine read_integer


      subroutine read_tiling_metadata(tiling, unusable, rc)
         type (GridTiling), intent(out) :: tiling
         class (KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc

         character(len=MAPL_TileNameLength) :: buffer
         integer :: length
         character(len=*), parameter :: Iam = 'read_tiling_metadata'
         integer :: status

         if (am_i_root) then
            read(unit) buffer
            read(unit) tiling%im
            read(unit) tiling%jm
            length = len(tiling%grid_name)
         end if
         
         call MAPL_CommsBcast(vm, data=length, N=1, ROOT=0, RC=status)
         _VERIFY(status)
         call MAPL_CommsBcast(vm, data=buffer, N=length, ROOT=0, RC=status)
         _VERIFY(status)
         tiling%grid_name = buffer(1:length)

         call MAPL_CommsBcast(vm, data=tiling%im, N=1, ROOT=0, RC=status)
         _VERIFY(status)
         call MAPL_CommsBcast(vm, data=tiling%jm, N=1, ROOT=0, RC=status)
         _VERIFY(status)

         _RETURN(_SUCCESS)
      end subroutine read_tiling_metadata

      
      subroutine read_tiling_data(n_tiles, tiling, unusable, rc)
         use MAPL_ShmemMod, only: MAPL_AllocateShared, MAPL_DeallocNodeArray
         use MAPL_CommsMod, only: MAPL_BcastShared
         integer, intent(in) :: n_tiles
         type (GridTiling), intent(inout) :: tiling
         class (KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc

         real(kind=REAL32), pointer :: buffer(:)
         integer :: length
         character(len=*), parameter :: Iam = 'read_tiling_data'
         integer :: status

         logical :: transroot
         logical :: rootonly_ = .false.


!!$         if (RootOnly_) then
!!$            TransRoot = amIRoot
!!$         else
            TransRoot = .true.
!!$         end if

         call MAPL_AllocateShared(buffer, [n_tiles], TransRoot=TransRoot, rc=status)
         _VERIFY(status)

         if (am_i_root) read(unit) buffer
         call MAPL_BcastShared(vm, data=buffer, N=n_tiles, ROOT=0, RootOnly=RootOnly_, rc=status)
         _VERIFY(status)
         allocate(tiling%i_indices(n_tiles))
         tiling%i_indices = nint(buffer)


         if (am_i_root) read(unit) buffer
         call MAPL_BcastShared(vm, data=buffer, N=n_tiles, ROOT=0, RootOnly=RootOnly_, rc=status)
         _VERIFY(status)
         allocate(tiling%j_indices(n_tiles))
         tiling%j_indices = nint(buffer)

         if (am_i_root) read(unit) buffer
         call MAPL_BcastShared(vm, data=buffer, N=n_tiles, ROOT=0, RootOnly=RootOnly_, rc=status)
         _VERIFY(status)
         allocate(tiling%weights(n_tiles))
         tiling%weights = buffer

         call MAPL_SyncSharedMemory(rc=status)
         _VERIFY(status)
         if (associated(buffer)) then
            _DEALOCS(buffer)
         end if

         _RETURN(_SUCCESS)

      end subroutine read_tiling_data

    end function read_geos_binary

    function read_tempest(file_name, unusable, rc) result(tile_file)
      use Regrid_Functions_Mod
      type (TileFile) :: tile_file
      character(len=*), intent(in) :: file_name
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = 'read_tempest'

      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(rc)

      call readTileFileNC_file(file_name)

      tile_file%grid_tiles(1)%i_indices = II_in
      tile_file%grid_tiles(1)%j_indices = JJ_in
      tile_file%grid_tiles(1)%i_indices = II_out
      tile_file%grid_tiles(1)%j_indices = JJ_out
      tile_file%grid_tiles(1)%weights = W

    end function read_tempest

   !--------------------------------------------------------------------------------
   ! A single tile file supports regridding in both directions, and is
   ! given a canonical name that involves the names of both in and out
   ! grids.  Unfortunately, this leaves two possibilities, as there is
   ! no convention about which grid is listed first in the name.
   !
   ! Here we try both and return the file name that is found to
   ! exist. (Or throw an exception.)  We also return a logical "swap"
   ! to indicate whether or not the in grid corresponds to the 1st
   ! grid in the file.
   !--------------------------------------------------------------------------------
   subroutine find_tile_file(this, file_name, swap, unusable, rc)
      class (TilingRegridder), intent(inout) :: this
      character(len=:), allocatable :: file_name
      logical, intent(out) :: swap
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status      
      character(len=*), parameter :: Iam = 'find_tile_file'

      type (RegridderSpec) :: spec
      character(len=:), allocatable :: grid_name_in
      character(len=:), allocatable :: grid_name_out
      character(len=:), allocatable :: trial_name
      logical :: exists

      _UNUSED_DUMMY(unusable)

      spec = this%get_spec()

      block
        use MAPL_AbstractGridFactoryMod
        use MAPL_GridManagerMod
        class (AbstractGridFactory), pointer :: factory

        factory => get_factory(spec%grid_in)
        grid_name_in = factory%generate_grid_name()
        factory => get_factory(spec%grid_out)
        grid_name_out = factory%generate_grid_name()
      end block

      ! First we search for a ".bin" file
      trial_name = make_tile_file_name(grid_name_in, grid_name_out, '.bin')
      
      inquire(file=trial_name, exist=exists)
      if (exists) then
         swap = .false.
      else ! swap
         swap = .true.
         trial_name = make_tile_file_name(grid_name_out, grid_name_in, '.bin')
         inquire(file=trial_name, exist=exists)
      end if

      if (exists) then
         file_name = trial_name
         this%file_type = GEOS_BINARY
         _RETURN(_SUCCESS)
      end if
         
      ! Next we search for a tempest ".nc4" file
      trial_name = make_tile_file_name(grid_name_in, grid_name_out,'.nc4')
      inquire(file=trial_name, exist=exists)
      if (exists) then
         swap = .false.
      else ! swap
         swap = .true.
         trial_name = make_tile_file_name(grid_name_out, grid_name_in, '.nc4')
         inquire(file=trial_name, exist=exists)
         ! This was the last chance - fail if we still have not found the file.
         _ASSERT(exists, 'could not find tempest file')
      end if

      if (exists) then
         file_name = trial_name
         this%file_type = TEMPEST
         _RETURN(_SUCCESS)
      end if

      _RETURN(_FAILURE)


   contains


      function get_grid_name(grid, unusable, rc) result(name)
         character(len=:), allocatable :: name
         type (ESMF_Grid), intent(in) :: grid
         class (KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
         
         character(len=MAPL_TileNameLength) :: buffer
         
         call ESMF_GridGet(grid, name=buffer, rc=status)
         _VERIFY(status)
         name = trim(buffer)
         
      end function get_grid_name

   end subroutine find_tile_file


   ! Encapsulate the construction of the canonical file name for a tile file.
   function make_tile_file_name(grid_name_1, grid_name_2, suffix, unusable, rc) result(name)
      character(len=:), allocatable :: name
      character(len=*), intent(in) :: grid_name_1
      character(len=*), intent(in) :: grid_name_2
      character(len=*), intent(in) :: suffix
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(rc)

      name = grid_name_1 // '_' // grid_name_2 // suffix

   end function make_tile_file_name
   

   ! ------------------------------------------------------------------------
   ! Some use cases have the regrid data distributed (on one side or
   ! the other) rather than collected globally.  To support this we
   ! allocate a smaller structure "local_x_tiles" which is a subset of
   ! the tiles that correspond to the local cells on the grid.
   ! ------------------------------------------------------------------------
   subroutine copy_global_to_local(this)
      use MAPL_BaseMod, only: MAPL_Grid_interior
      use pFIO
      class (TilingRegridder), intent(inout) :: this

      type (IntegerVector) :: local_indices
      type (IntegerVectorIterator) :: iter
      integer :: i_tile
      logical :: cell_is_local
      integer :: i1, in, j1, jn ! index bounds for grid_out
      integer :: i, j
      type (RegridderSpec) :: spec

      integer :: n
      integer :: n_tiles
      
      spec = this%get_spec()

      call MAPL_Grid_interior(spec%grid_out, i1, in, j1, jn)

      n_tiles = size(this%global_x_tiles)
      do i_tile = 1, n_tiles
         i = this%global_x_tiles(i_tile)%idx_i_out
         j = this%global_x_tiles(i_tile)%idx_j_out
         cell_is_local = (i1<=i .and. in>=i .and.  j1<=j .and. jn>=j)
         if (cell_is_local) then
            call local_indices%push_back(i_tile)
         end if
      end do

      allocate(this%local_x_tiles(local_indices%size()))

      associate (tiles => this%local_x_tiles)
        iter = local_indices%begin()
        n = 0
        do while (iter /= local_indices%end())
           n = n + 1
           this%local_x_tiles(n) = this%global_x_tiles(iter%get())
           call iter%next
        end do
        ! fix offsets
        tiles(:)%idx_i_out = tiles(:)%idx_i_out - i1 + 1
        tiles(:)%idx_j_out = tiles(:)%idx_j_out - j1 + 1

      end associate
      
   end subroutine copy_global_to_local

   subroutine regrid_scalar_2d_real32(this, q_in, q_out, rc)
      class (TilingRegridder), intent(in) :: this
      real(kind=REAL32), intent(in) :: q_in(:,:)
      real(kind=REAL32), intent(out) :: q_out(:,:)
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = 'regrid_2d_real32'

      real (kind=REAL32), allocatable :: fraction(:,:)
      type (RegridderSpec) :: spec

      _ASSERT(.not. this%has_undef_value(), 'undefined value')

      call this%init_regrid(q_out)
      allocate(fraction, source = q_out)
      fraction = 0

      spec = this%get_spec()

      if (all(shape(q_out) == this%out_shape)) then
         call this%loop_over_tiles(this%global_x_tiles, q_in, q_out, fraction)
      else
         _ASSERT(iand(spec%hints, REGRID_HINT_LOCAL) /= 0, 'bad hint')
         call this%loop_over_tiles(this%local_x_tiles, q_in, q_out, fraction)
      end if

      call this%final_regrid(q_out, fraction)

      _RETURN(_SUCCESS)

   end subroutine regrid_scalar_2d_real32


   subroutine init_regrid(x_out)
      use, intrinsic :: iso_fortran_env, only: REAL32
      real (kind=REAL32), intent(out) :: x_out(:,:)
      x_out = 0
   end subroutine init_regrid
      

   subroutine loop_over_tiles(this, tiles, q_in, q_out, fraction)
      class(TilingRegridder), intent(in) :: this
      type (XTile), intent(in) :: tiles(:)
      real(kind=REAL32), intent(in) :: q_in(:,:)
      real(kind=REAL32), intent(inout) :: q_out(:,:)
      real (kind=REAL32), intent(inout) :: fraction(:,:)

      integer :: i_tile
      
      do i_tile = 1, size(tiles)
         
         associate (tile => tiles(i_tile))
           associate ( &
                & x_in => q_in(tile%idx_i_in, tile%idx_j_in), & 
                & x_out => q_out(tile%idx_i_out, tile%idx_j_out), &
                & weight => tile%weight, &
                & f => fraction(tile%idx_i_out, tile%idx_j_out) )
             
             if (x_in /= MAPL_UNDEF) call this%add_contribution(x_in, weight, x_out, f)
           end associate
           
         end associate
      end do
   end subroutine loop_over_tiles

   subroutine final_regrid(x_out, fraction)
      use, intrinsic :: iso_fortran_env, only: REAL32
      real (kind=REAL32), intent(inout) :: x_out(:,:)
      real (kind=REAL32), intent(in) :: fraction(:,:)

      where (fraction /= 0)
         x_out = x_out / fraction
      elsewhere
         x_out = MAPL_UNDEF
      end where

   end subroutine final_regrid
      

   function clone(this)
      class (AbstractRegridder), allocatable :: clone
      class (TilingRegridder), intent(in) :: this
      
      allocate(clone, source=this)
      
   end function clone

   subroutine MAPL_AllocNodeArray_Tiling(Ptr,shp,rc)
      type(XTile), pointer, intent(inout) :: Ptr(:)
      integer,              intent(in)    :: shp(1)
      integer, optional,    intent(  out) :: rc

      integer                             :: status
      character(len=*), parameter :: Iam = 'MAPL_AllocNodeArray_Tiling'
      
      type(c_ptr) :: Caddr
      integer     :: len,memsize,intsize
      type(XTile) :: xdummy

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif
      ! Xtile type has 4 32-bit integers, and 1 real 4
      memsize=storage_size(xdummy)
      intsize=storage_size(1)
      len=shp(1)*(memsize/intsize)
      call GetSharedMemory(Caddr, len, rc=STATUS)
      _VERIFY(STATUS)

      call c_f_pointer(Caddr, Ptr, Shp) ! C ptr to Fortran ptr
      _RETURN(0)

   end subroutine MAPL_AllocNodeArray_Tiling
   

    subroutine MAPL_DeAllocNodeArray_Tiling(Ptr,rc)
      type(XTile),  pointer              :: Ptr(:)
      integer, optional, intent(OUT) :: rc

      type(c_ptr) :: Caddr
      integer     :: status
      character(len=*), parameter :: Iam = 'MAPL_DeAllocNodeArray_Tiling'

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1)))

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      _VERIFY(status)

      _RETURN(_SUCCESS)
    end subroutine MAPL_DeAllocNodeArray_Tiling
   
end module MAPL_TilingRegridderMod
