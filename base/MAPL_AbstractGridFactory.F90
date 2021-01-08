#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_AbstractGridFactoryMod
   use ESMF
   use pFIO
   use MAPL_ExceptionHandling
   use MAPL_BaseMod, only: MAPL_UNDEF
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   use MAPL_KeywordEnforcerMod
   implicit none
   private

   public :: AbstractGridFactory

   type, abstract :: AbstractGridFactory
      private
      type (ESMF_Grid), allocatable :: grid
      real(kind=REAL64), allocatable :: NS_basis(:,:,:,:)
      real(kind=REAL64), allocatable :: grid_basis(:,:,:,:)
      real(kind=REAL64), allocatable :: xyz_basis(:,:,:,:)

   contains

      procedure, nopass :: make_arbitrary_decomposition
      procedure :: make_grid
      procedure :: get_grid
      procedure (make_new_grid), deferred :: make_new_grid
!!$      procedure (make_field), deferred :: make_esmf_field

      procedure :: clone
      procedure (equals), deferred:: equals
      generic :: operator(==) => equals

      procedure :: initialize_from_config
      procedure (initialize_from_file_metadata), deferred :: initialize_from_file_metadata
      procedure (initialize_from_config_with_prefix), deferred :: initialize_from_config_with_prefix
      procedure (initialize_from_esmf_distGrid), deferred :: initialize_from_esmf_distGrid

      generic :: initialize => initialize_from_config
      generic :: initialize => initialize_from_file_metadata
      generic :: initialize => initialize_from_config_with_prefix
      generic :: initialize => initialize_from_esmf_distGrid

      ! from MAPL_stubs
      procedure(halo), deferred :: halo

      procedure (generate_grid_name), deferred :: generate_grid_name
      procedure :: to_string

      procedure :: get_basis

      generic :: spherical_to_cartesian => spherical_to_cartesian_2d_real32
      generic :: spherical_to_cartesian => spherical_to_cartesian_2d_real64
      generic :: spherical_to_cartesian => spherical_to_cartesian_3d_real32
      generic :: spherical_to_cartesian => spherical_to_cartesian_3d_real64
      
      generic :: cartesian_to_spherical => cartesian_to_spherical_2d_real32
      generic :: cartesian_to_spherical => cartesian_to_spherical_2d_real64
      generic :: cartesian_to_spherical => cartesian_to_spherical_3d_real32
      generic :: cartesian_to_spherical => cartesian_to_spherical_3d_real64
      

      procedure :: spherical_to_cartesian_2d_real32
      procedure :: spherical_to_cartesian_2d_real64
      procedure :: spherical_to_cartesian_3d_real32
      procedure :: spherical_to_cartesian_3d_real64
      
      procedure :: cartesian_to_spherical_2d_real32
      procedure :: cartesian_to_spherical_2d_real64
      procedure :: cartesian_to_spherical_3d_real32
      procedure :: cartesian_to_spherical_3d_real64

      procedure(append_metadata), deferred :: append_metadata
      procedure(get_grid_vars), deferred :: get_grid_vars
      procedure(append_variable_metadata), deferred :: append_variable_metadata
      procedure(generate_file_bounds), deferred :: generate_file_bounds
      procedure(generate_file_corner_bounds), deferred :: generate_file_corner_bounds
      procedure(generate_file_reference2D), deferred :: generate_file_reference2D
      procedure(generate_file_reference3D), deferred :: generate_file_reference3D
   end type AbstractGridFactory

   abstract interface

      logical function equals(a, b)
         import AbstractGridFactory
         class (AbstractGridFactory), intent(in) :: a
         class (AbstractGridFactory), intent(in) :: b
      end function equals

      function make_new_grid(this, unusable, rc) result(grid)
         use esmf
         use MAPL_KeywordEnforcerMod
         import AbstractGridFactory
         type (ESMF_Grid) :: grid
         class (AbstractGridFactory), intent(in) :: this
         class (KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end function make_new_grid

      subroutine initialize_from_config_with_prefix(this, config, prefix, unusable, rc) 
         use esmf
         use MAPL_KeywordEnforcerMod
         import AbstractGridFactory
         class (AbstractGridFactory), intent(inout)  :: this
         type (ESMF_Config), intent(inout) :: config
         character(len=*), intent(in) :: prefix
         class (KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end subroutine initialize_from_config_with_prefix


      subroutine initialize_from_file_metadata(this, file_metadata, unusable, rc)
        use MAPL_KeywordEnforcerMod
        use pFIO
        import AbstractGridFactory
        class (AbstractGridFactory), intent(inout)  :: this
        type (FileMetadata), target, intent(in) :: file_metadata
        class (KeywordEnforcer), optional, intent(in) :: unusable
        integer, optional, intent(out) :: rc
      end subroutine initialize_from_file_metadata


      subroutine initialize_from_esmf_distGrid(this, dist_grid, lon_array, lat_array, unusable, rc) 
         use esmf
         use MAPL_KeywordEnforcerMod
         import AbstractGridFactory
         class (AbstractGridFactory), intent(inout)  :: this
         type (ESMF_DistGrid), intent(in) :: dist_grid
         type (ESMF_LocalArray), intent(in) :: lon_array
         type (ESMF_LocalArray), intent(in) :: lat_array
         class (KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end subroutine initialize_from_esmf_distGrid


      subroutine halo(this, array, unusable, halo_width, rc)
         use, intrinsic :: iso_fortran_env, only: REAL32
         use MAPL_KeywordEnforcerMod
         import AbstractGridFactory
         class (AbstractGridFactory), intent(inout) :: this
         real(kind=REAL32), intent(inout) :: array(:,:)
         class (KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(in) :: halo_width
         integer, optional, intent(out) :: rc
      end subroutine halo

      function generate_grid_name(this) result(name)
         import AbstractGridFactory
         implicit none
         character(len=:), allocatable :: name
         class (AbstractGridFactory), intent(in) :: this
      end function generate_grid_name

      subroutine append_metadata(this, metadata)
         use pFIO
         import AbstractGridFactory
         class (AbstractGridFactory), intent(inout) :: this
         type (FileMetadata), intent(inout) :: metadata
      end subroutine append_metadata

      function get_grid_vars(this) result(vars)
         import AbstractGridFactory
         class (AbstractGridFactory), intent(inout) :: this
         character(len=:), allocatable :: vars
      end function get_grid_vars

      subroutine append_variable_metadata(this,var)
         use pFIO
         import AbstractGridFactory
         class (AbstractGridFactory), intent(inout) :: this
         type(Variable), intent(inout) :: var
      end subroutine append_variable_metadata

      subroutine generate_file_bounds(this,grid,local_start,global_start,global_count,rc)
         use esmf
         import AbstractGridFactory
         class (AbstractGridFactory), intent(inout) :: this
         type(ESMF_Grid), intent(inout)      :: grid
         integer, allocatable, intent(out) :: local_start(:)
         integer, allocatable, intent(out) :: global_start(:)
         integer, allocatable, intent(out) :: global_count(:)
         integer, optional, intent(out) :: rc
         
      end subroutine generate_file_bounds

      subroutine generate_file_corner_bounds(this,grid,local_start,global_start,global_count,rc)
         use esmf
         import AbstractGridFactory
         class (AbstractGridFactory), intent(inout) :: this
         type(ESMF_Grid), intent(inout)      :: grid
         integer, allocatable, intent(out) :: local_start(:)
         integer, allocatable, intent(out) :: global_start(:)
         integer, allocatable, intent(out) :: global_count(:)
         integer, optional, intent(out) :: rc
         
      end subroutine generate_file_corner_bounds

      function generate_file_reference2D(this,fpointer) result(ref)
         use pFIO
         import AbstractGridFactory
         type(ArrayReference) :: ref
         class (AbstractGridFactory), intent(inout) :: this
         real, pointer, intent(in) :: fpointer(:,:)
      end function generate_file_reference2D

      function generate_file_reference3D(this,fpointer) result(ref)
         use pFIO
         import AbstractGridFactory
         type(ArrayReference) :: ref
         class (AbstractGridFactory), intent(inout) :: this
         real, pointer, intent(in) :: fpointer(:,:,:)
      end function generate_file_reference3D

   end interface

   character(len=*), parameter :: MOD_NAME = 'MAPL_AbstractGridFactory::'


contains


   subroutine initialize_from_config_file(this, config_file, unusable, prefix, rc)
      use MAPL_KeywordEnforcerMod
      class (AbstractGridFactory), intent(inout)  :: this
      character(len=*), intent(in) :: config_file
      class (KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: prefix
      integer, optional, intent(out) :: rc

      type (ESMF_Config) :: config
      character(len=*), parameter :: Iam= MOD_NAME // 'initialize_from_file'
      integer :: status

      _UNUSED_DUMMY(unusable)

      config = ESMF_ConfigCreate(rc=status)
      _VERIFY(status)
      call ESMF_ConfigLoadFile   (config, config_file, rc=STATUS )
      _VERIFY(status)

      call this%initialize(config, prefix=prefix, rc=status)
      _VERIFY(status)

      call ESMF_ConfigDestroy(config, rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
      
   end subroutine initialize_from_config_file


   subroutine initialize_from_config(this, config, unusable, rc)
      use MAPL_KeywordEnforcerMod
      class (AbstractGridFactory), intent(inout)  :: this
      type (ESMF_Config), intent(inout) :: config
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam= MOD_NAME // 'initialize_from_config'

      _UNUSED_DUMMY(unusable)
      call this%initialize(config, prefix='', rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
      
   end subroutine initialize_from_config


   ! stub implementation - should override, but don't force it
   function to_string(this) result(string)
      character(len=:), allocatable :: string
      class (AbstractGridFactory), intent(in) :: this

      _UNUSED_DUMMY(this)
      string = ''

   end function to_string


   ! Subclasses are generally just collections of primitive data.
   ! But care should be given to ensure that this procedure
   ! works in each case.
   function clone(this)
      class (AbstractGridFactory), allocatable :: clone
      class (AbstractGridFactory), intent(in) :: this

      allocate(clone, source=this)

   end function clone

   function make_grid(this, unusable, rc) result(grid)
      use esmf
      use MAPL_KeywordEnforcerMod
      type (ESMF_Grid) :: grid
      class (AbstractGridFactory), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      
      integer :: status
      character(len=*), parameter :: Iam= MOD_NAME // 'make_grid'

      _UNUSED_DUMMY(unusable)

      if (allocated(this%grid)) then
         grid = this%grid
      else
         this%grid = this%make_new_grid(rc=status)
         _VERIFY(status)
         grid = this%grid
      end if

      _RETURN(_SUCCESS)

   end function make_grid

   ! --------------------------------------------------------------------
   ! This subroutine uses the current ESMF VM to determine an arbitary
   ! domain decomposition.  The decomposition used for model variables
   ! is generally not relevant (CS vs LatLon) in those situations, but
   ! ESMF regridding still requires some decomposition no the same
   ! number processors.  The algorithm here tries to find a decomp
   ! that is as close as possible to sqrt(npes)*sqrt(npes) with the
   ! leading dimension using fewer processes
   ! --------------------------------------------------------------------
   subroutine make_arbitrary_decomposition(nx, ny, unusable, reduceFactor, rc)
      use ESMF
      use MAPL_KeywordEnforcerMod
      integer, intent(out) :: nx
      integer, intent(out) :: ny
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: reduceFactor
      integer, optional, intent(out) :: rc

      type (ESMF_VM) :: vm
      integer :: pet_count

      character(len=*), parameter :: Iam= MOD_NAME // 'make_arbitrary_decomposition()'
      integer :: status

      _UNUSED_DUMMY(unusable)

      call ESMF_VMGetCurrent(vm, rc=status)
      _VERIFY(status)
      call ESMF_VMGet(vm, petCount=pet_count, rc=status)
      _VERIFY(status)
      if (present(reduceFactor)) pet_count=pet_count/reduceFactor

      ! count down from sqrt(n)
      ! Note: inal iteration (nx=1) is guaranteed to succeed.
      do nx = floor(sqrt(real(2*pet_count))), 1, -1
         if (mod(pet_count, nx) == 0) then ! found a decomposition
            ny = pet_count / nx
            exit
         end if
      end do

      _RETURN(_SUCCESS)

   end subroutine make_arbitrary_decomposition

   subroutine spherical_to_cartesian_2d_real32(this,u,v,xyz,basis,unusable,rc)
      use esmf
      use MAPL_KeywordEnforcerMod
      real(REAL32), intent(IN) :: u(:,:)
      real(REAL32), intent(IN) :: v(:,:)
      real(REAL32), intent(OUT) :: xyz(:,:,:,:)
      character(len=*), intent(in) :: basis
      class (AbstractGridFactory), target, intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc 

      real(REAL64), pointer :: basis_vectors(:,:,:,:)
      real(REAL32) :: uv(2)
      integer :: i, j, im, jm
      character(len=*), parameter :: Iam= MOD_NAME // 'spherical_to_cartesian_2d'
      integer :: status
     
      _UNUSED_DUMMY(unusable)

      im = size(u,1)
      jm = size(u,2)
      _ASSERT(im == size(v,1), 'u-v shape mismatch for IM')
      _ASSERT(jm == size(v,2), 'u-v shape mismatch for JM')
      _ASSERT(3 == size(xyz,1), 'u-v shape mismatch (LM=1)')
      _ASSERT(1 == size(xyz,2), 'u-xyz shape mismatch (LM=1)')
      _ASSERT(im == size(xyz,3), 'u-xyz shape mismatch for IM')
      _ASSERT(jm == size(xyz,4), 'u-xyz shape mismatch for JM')
      basis_vectors => this%get_basis(basis,rc=status)
      _VERIFY(status)
      do j=1,jm
         do i=1,im
            if (u(i,j) == MAPL_UNDEF .or. v(i,j) == MAPL_UNDEF) then
               xyz(:,1,i,j) = MAPL_UNDEF
            else
               uv = [u(i,j),v(i,j)]
               xyz(:,1,i,j) = matmul(basis_vectors(:,:,i,j), uv)
            end if
         enddo
      enddo

      _RETURN(_SUCCESS)

   end subroutine spherical_to_cartesian_2d_real32

   subroutine spherical_to_cartesian_2d_real64(this,u,v,xyz,basis,unusable,rc)
      use esmf
      use MAPL_KeywordEnforcerMod
      real(REAL64), intent(IN) :: u(:,:)
      real(REAL64), intent(IN) :: v(:,:)
      real(REAL64), intent(OUT) :: xyz(:,:,:,:)
      character(len=*), intent(in) :: basis
      class (AbstractGridFactory), target, intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc 

      real(REAL64), pointer :: basis_vectors(:,:,:,:)
      real(REAL64) :: uv(2)
      integer :: i, j, im, jm
      character(len=*), parameter :: Iam= MOD_NAME // 'spherical_to_cartesian_2d'
      integer :: status
     
      _UNUSED_DUMMY(unusable)

      im = size(u,1)
      jm = size(u,2)
      _ASSERT(im == size(v,1), 'u-v shape mismatch for IM')
      _ASSERT(jm == size(v,2), 'u-v shape mismatch for JM')
      _ASSERT(3 == size(xyz,1), 'u-v shape mismatch (LM=1)')
      _ASSERT(1 == size(xyz,2), 'u-xyz shape mismatch (LM=1)')
      _ASSERT(im == size(xyz,3), 'u-xyz shape mismatch for IM')
      _ASSERT(jm == size(xyz,4), 'u-xyz shape mismatch for JM')
      basis_vectors => this%get_basis(basis,rc=status)
      _VERIFY(status)
      do j=1,jm
         do i=1,im
            if (u(i,j) == MAPL_UNDEF .or. v(i,j) == MAPL_UNDEF) then
               xyz(:,1,i,j) = MAPL_UNDEF
            else
               uv = [u(i,j),v(i,j)]
               xyz(:,1,i,j) = matmul(basis_vectors(:,:,i,j), uv)
            end if
         enddo
      enddo

      _RETURN(_SUCCESS)

    end subroutine spherical_to_cartesian_2d_real64
   
   subroutine spherical_to_cartesian_3d_real32(this,u,v,xyz,basis,unusable,rc)
      use esmf
      use MAPL_KeywordEnforcerMod
      real(REAL32), intent(IN) :: u(:,:,:)
      real(REAL32), intent(IN) :: v(:,:,:)
      real(REAL32), intent(OUT) :: xyz(:,:,:,:)
      character(len=*), intent(in) :: basis
      class (AbstractGridFactory), target, intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc 

      real(REAL64), pointer :: basis_vectors(:,:,:,:)
      real(REAL32) :: uv(2)
      integer :: i, j, k, im, jm, km
      character(len=*), parameter :: Iam= MOD_NAME // 'spherical_to_cartesian_3d'
      integer :: status
    
      _UNUSED_DUMMY(unusable)

      im = size(u,1)
      jm = size(u,2)
      km = size(u,3)
      _ASSERT(im == size(v,1), 'u-v shape mismatch for IM')
      _ASSERT(jm == size(v,2), 'u-v shape mismatch for JM')
      _ASSERT(km == size(v,3), 'u-v shape mismatch for LM')
      _ASSERT(3 == size(xyz,1), 'u-xyz shape mismatch for')
      _ASSERT(km == size(xyz,2), 'u-xyz shape mismatch for LM')
      _ASSERT(im == size(xyz,3), 'u-xyz shape mismatch for IM')
      _ASSERT(jm == size(xyz,4), 'u-xyz shape mismatch for JM')
      basis_vectors => this%get_basis(basis,rc=status)
      _VERIFY(status)
      do j=1,jm
         do i=1,im
            do k=1,km
               if (u(i,j,k) == MAPL_UNDEF .or. v(i,j,k) == MAPL_UNDEF) then
                  xyz(:,k,i,j)=MAPL_UNDEF
               else
                  uv = [u(i,j,k),v(i,j,k)]
                  xyz(:,k,i,j) = matmul(basis_vectors(:,:,i,j), uv)
               end if
            enddo 
         enddo
      enddo

      _RETURN(_SUCCESS)
   end subroutine spherical_to_cartesian_3d_real32


   subroutine spherical_to_cartesian_3d_real64(this,u,v,xyz,basis,unusable,rc)
     use esmf
     use MAPL_KeywordEnforcerMod
     real(REAL64), intent(IN) :: u(:,:,:)
     real(REAL64), intent(IN) :: v(:,:,:)
     real(REAL64), intent(OUT) :: xyz(:,:,:,:)
     character(len=*), intent(in) :: basis
     class (AbstractGridFactory), target, intent(inout) :: this
     class (KeywordEnforcer), optional, intent(in) :: unusable
     integer, optional, intent(out) :: rc 

     real(REAL64), pointer :: basis_vectors(:,:,:,:)
     real(REAL64) :: uv(2)
     integer :: i, j, k, im, jm, km
     character(len=*), parameter :: Iam= MOD_NAME // 'spherical_to_cartesian_3d'
     integer :: status

     _UNUSED_DUMMY(unusable)

     im = size(u,1)
     jm = size(u,2)
     km = size(u,3)
     _ASSERT(im == size(v,1), 'u-v shape mismatch for IM')
     _ASSERT(jm == size(v,2), 'u-v shape mismatch for JM')
     _ASSERT(km == size(v,3), 'u-v shape mismatch for LM')
     _ASSERT(3 == size(xyz,1), 'u-xyz shape mismatch for')
     _ASSERT(km == size(xyz,2), 'u-xyz shape mismatch for LM')
     _ASSERT(im == size(xyz,3), 'u-xyz shape mismatch for IM')
     _ASSERT(jm == size(xyz,4), 'u-xyz shape mismatch for JM')
     basis_vectors => this%get_basis(basis,rc=status)
     _VERIFY(status)
     do j=1,jm
        do i=1,im
           do k=1,km
              if (u(i,j,k) == MAPL_UNDEF .or. v(i,j,k) == MAPL_UNDEF) then
                 xyz(:,k,i,j)=MAPL_UNDEF
              else
                 uv = [u(i,j,k),v(i,j,k)]
                 xyz(:,k,i,j) = matmul(basis_vectors(:,:,i,j), uv)
              end if
           enddo
        enddo
     enddo

     _RETURN(_SUCCESS)
   end subroutine spherical_to_cartesian_3d_real64
   
   
   subroutine cartesian_to_spherical_2d_real32(this,xyz,u,v,basis,unusable,rc)
      use esmf
      use MAPL_KeywordEnforcerMod
      real(REAL32), intent(IN) :: xyz(:,:,:,:)
      character(len=*), intent(IN) :: basis
      real(REAL32), intent(OUT) :: u(:,:)
      real(REAL32), intent(OUT) :: v(:,:)
      class (AbstractGridFactory), target, intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc 

      real(REAL64), pointer :: basis_vectors(:,:,:,:)
      real(REAL32) :: uv(2)
      integer :: i, j, im, jm
      character(len=*), parameter :: Iam= MOD_NAME // 'cartesian_to_spherical_2d'
      integer :: status

      _UNUSED_DUMMY(unusable)

      im = size(u,1)
      jm = size(u,2)
      _ASSERT(im == size(v,1), 'u-v shape mismatch for IM')
      _ASSERT(jm == size(v,2), 'u-v shape mismatch for JM')
      _ASSERT(3 == size(xyz,1), 'u-v shape mismatch')
      _ASSERT(1 == size(xyz,2), 'u-v shape mismatch')
      _ASSERT(im == size(xyz,3), 'u-v shape mismatch for IM')
      _ASSERT(jm == size(xyz,4), 'u-v shape mismatch for JM')

      basis_vectors => this%get_basis(basis,rc=status)
      _VERIFY(status)
      do j=1,jm
         do i=1,im
            if (xyz(1,1,i,j) == MAPL_UNDEF .or. xyz(2,1,i,j) == MAPL_UNDEF .or. &
                xyz(3,1,i,j) == MAPL_UNDEF) then
               u(i,j)=MAPL_UNDEF
               v(i,j)=MAPL_UNDEF
            else
               uv = matmul(transpose(basis_vectors(:,:,i,j)),xyz(:,1,i,j))
               u(i,j)=uv(1)
               v(i,j)=uv(2)
            end if
         enddo
      enddo
      _RETURN(_SUCCESS)

   end subroutine cartesian_to_spherical_2d_real32

   
   subroutine cartesian_to_spherical_2d_real64(this,xyz,u,v,basis,unusable,rc)
     use esmf
     use MAPL_KeywordEnforcerMod
     real(REAL64), intent(IN) :: xyz(:,:,:,:)
     character(len=*), intent(IN) :: basis
     real(REAL64), intent(OUT) :: u(:,:)
     real(REAL64), intent(OUT) :: v(:,:)
     class (AbstractGridFactory), target, intent(inout) :: this
     class (KeywordEnforcer), optional, intent(in) :: unusable
     integer, optional, intent(out) :: rc 

     real(REAL64), pointer :: basis_vectors(:,:,:,:)
     real(REAL64) :: uv(2)
     integer :: i, j, im, jm
     character(len=*), parameter :: Iam= MOD_NAME // 'cartesian_to_spherical_2d'
     integer :: status

     _UNUSED_DUMMY(unusable)

     im = size(u,1)
     jm = size(u,2)
     _ASSERT(im == size(v,1), 'u-v shape mismatch for IM')
     _ASSERT(jm == size(v,2), 'u-v shape mismatch for JM')
     _ASSERT(3 == size(xyz,1), 'u-v shape mismatch')
     _ASSERT(1 == size(xyz,2), 'u-v shape mismatch')
     _ASSERT(im == size(xyz,3), 'u-v shape mismatch for IM')
     _ASSERT(jm == size(xyz,4), 'u-v shape mismatch for JM')

     basis_vectors => this%get_basis(basis,rc=status)
     _VERIFY(status)
     do j=1,jm
        do i=1,im
           if (xyz(1,1,i,j) == MAPL_UNDEF .or. xyz(2,1,i,j) == MAPL_UNDEF .or. &
                xyz(3,1,i,j) == MAPL_UNDEF) then
              u(i,j)=MAPL_UNDEF
              v(i,j)=MAPL_UNDEF
           else
              uv = matmul(transpose(basis_vectors(:,:,i,j)),xyz(:,1,i,j))
              u(i,j)=uv(1)
              v(i,j)=uv(2)
           end if
        enddo
     enddo
     _RETURN(_SUCCESS)

   end subroutine cartesian_to_spherical_2d_real64
   
   
   subroutine cartesian_to_spherical_3d_real32(this,xyz,u,v,basis,unusable,rc)
      use esmf
      use MAPL_KeywordEnforcerMod
      real(REAL32), intent(IN) :: xyz(:,:,:,:)
      character(len=*), intent(IN) :: basis
      real(REAL32), intent(OUT) :: u(:,:,:)
      real(REAL32), intent(OUT) :: v(:,:,:)
      class (AbstractGridFactory), target, intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc 

      real(REAL64), pointer :: basis_vectors(:,:,:,:)
      real(REAL32) :: uv(2)
      integer :: i, j, k, im, jm, km
      character(len=*), parameter :: Iam= MOD_NAME // 'cartesian_to_spherical_3d'
      integer :: status
     
      _UNUSED_DUMMY(unusable)

      im = size(u,1)
      jm = size(u,2)
      km = size(u,3)
      _ASSERT(im == size(v,1), 'u-v shape mismatch for IM')
      _ASSERT(jm == size(v,2), 'u-v shape mismatch for JM')
      _ASSERT(3 == size(xyz,1), 'u-v shape mismatch (LM=1)')
      _ASSERT(km == size(xyz,2), 'u-xyz shape mismatch (LM=1)')
      _ASSERT(im == size(xyz,3), 'u-xyz shape mismatch for IM')
      _ASSERT(jm == size(xyz,4), 'u-xyz shape mismatch for JM')
      basis_vectors => this%get_basis(basis,rc=status)
      _VERIFY(status)
      do j=1,jm
         do i=1,im
            do k = 1, km
               if (xyz(1,k,i,j) == MAPL_UNDEF .or. xyz(2,k,i,j) == MAPL_UNDEF .or. &
                   xyz(3,k,i,j) == MAPL_UNDEF) then
                  u(i,j,k)=MAPL_UNDEF
                  v(i,j,k)=MAPL_UNDEF
               else
                  uv = matmul(transpose(basis_vectors(:,:,i,j)),xyz(:,k,i,j))
                  u(i,j,k)=uv(1)
                  v(i,j,k)=uv(2)
               end if
            enddo 
         enddo
      enddo
      _RETURN(_SUCCESS)

   end subroutine cartesian_to_spherical_3d_real32


   subroutine cartesian_to_spherical_3d_real64(this,xyz,u,v,basis,unusable,rc)
     use esmf
     use MAPL_KeywordEnforcerMod
     real(REAL64), intent(IN) :: xyz(:,:,:,:)
     character(len=*), intent(IN) :: basis
     real(REAL64), intent(OUT) :: u(:,:,:)
     real(REAL64), intent(OUT) :: v(:,:,:)
     class (AbstractGridFactory), target, intent(inout) :: this
     class (KeywordEnforcer), optional, intent(in) :: unusable
     integer, optional, intent(out) :: rc 

     real(REAL64), pointer :: basis_vectors(:,:,:,:)
     real(REAL64) :: uv(2)
     integer :: i, j, k, im, jm, km
     character(len=*), parameter :: Iam= MOD_NAME // 'cartesian_to_spherical_3d'
     integer :: status

     _UNUSED_DUMMY(unusable)

     im = size(u,1)
     jm = size(u,2)
     km = size(u,3)
     _ASSERT(im == size(v,1), 'u-v shape mismatch for IM')
     _ASSERT(jm == size(v,2), 'u-v shape mismatch for JM')
     _ASSERT(3 == size(xyz,1), 'u-v shape mismatch (LM=1)')
     _ASSERT(km == size(xyz,2), 'u-xyz shape mismatch (LM=1)')
     _ASSERT(im == size(xyz,3), 'u-xyz shape mismatch for IM')
     _ASSERT(jm == size(xyz,4), 'u-xyz shape mismatch for JM')
     basis_vectors => this%get_basis(basis,rc=status)
     _VERIFY(status)
     do j=1,jm
        do i=1,im
           do k = 1, km
              if (xyz(1,k,i,j) == MAPL_UNDEF .or. xyz(2,k,i,j) == MAPL_UNDEF .or. &
                   xyz(3,k,i,j) == MAPL_UNDEF) then
                 u(i,j,k)=MAPL_UNDEF
                 v(i,j,k)=MAPL_UNDEF
              else
                 uv = matmul(transpose(basis_vectors(:,:,i,j)),xyz(:,k,i,j))
                 u(i,j,k)=uv(1)
                 v(i,j,k)=uv(2)
              end if
           enddo
        enddo
     enddo
     _RETURN(_SUCCESS)

   end subroutine cartesian_to_spherical_3d_real64
   
   function get_basis(this,basis,unusable,rc) result(basis_vectors)
      use esmf
      use MAPL_KeywordEnforcerMod
      use MAPL_ConstantsMod, only : PI => MAPL_PI_R8
      real(REAL64), pointer :: basis_vectors(:,:,:,:)
      character(len=*), intent(in) :: basis
      class (AbstractGridFactory), target, intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc 

      character(len=*), parameter :: Iam= MOD_NAME // 'get_basis'
      integer :: status
      real(REAL64), pointer :: temp_vect(:,:,:,:)
      real(REAL64), pointer :: Xcoord(:,:) => null()
      real(REAL64), pointer :: Ycoord(:,:) => null()

      _UNUSED_DUMMY(unusable)

      _ASSERT(allocated(this%grid), 'grid not allocated')
      select case (basis)
      case ('north-south')
         if (.not.allocated(this%NS_basis)) then
            call ESMF_GridGetCoord(this%grid,1, &
                 staggerLoc=ESMF_STAGGERLOC_CENTER, &
                 farrayPtr=Xcoord, rc=status) 
            _VERIFY(status)
            call ESMF_GridGetCoord(this%grid,2, &
                 staggerLoc=ESMF_STAGGERLOC_CENTER, &
                 farrayPtr=Ycoord, rc=status)
            _VERIFY(status)
            allocate(this%NS_Basis(3,2,size(Xcoord,1),size(Xcoord,2)),stat=status)
            _VERIFY(status)
            basis_vectors => this%NS_Basis
            basis_vectors(1,1,:,:)= -sin(Xcoord)
            basis_vectors(2,1,:,:)=  cos(Xcoord)
            basis_vectors(3,1,:,:)=  0.0
            basis_vectors(1,2,:,:)= -sin(Ycoord)*cos(Xcoord)
            basis_vectors(2,2,:,:)= -sin(Ycoord)*sin(Xcoord)
            basis_vectors(3,2,:,:)=  cos(Ycoord)
         else
            basis_vectors => this%NS_basis
         end if
      case ('grid')
         if (.not.allocated(this%grid_basis)) then
            this%grid_basis = ComputeGridBasis(this%grid,rc=status)
            _VERIFY(status)
            basis_vectors => this%grid_Basis

         else
            basis_vectors => this%grid_basis
         end if

      case('xyz')
             
         if (.not.allocated(this%XYZ_basis)) then
            if (.not.allocated(this%grid_basis)) then
               this%grid_basis = ComputeGridBasis(this%grid,rc=status)
               _VERIFY(status)
               temp_vect => this%grid_basis
            else
               temp_vect => this%grid_basis
            end if
            this%XYZ_basis = ComputeXYZbasis(temp_vect,rc=status)
            _VERIFY(status)
            basis_vectors => this%XYZ_basis
         else
            basis_vectors => this%XYZ_basis
         end if

      end select

      _RETURN(_SUCCESS)

   end function get_basis

   function ComputeGridBasis(grid,unusable,rc) result(basis) 
      use esmf
      use MAPL_KeywordEnforcerMod
      use MAPL_BaseMod, only : MAPL_GridGetCorners,MAPL_GridGet

      type(ESMF_Grid), intent(inout) :: grid
      real(REAL64), allocatable :: basis(:,:,:,:)  
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc 

      character(len=*), parameter :: Iam= MOD_NAME // 'computeGridBasis'
      integer :: status

      real(REAL64), allocatable :: corners(:,:,:)
      real(REAL64), pointer :: Xcenters(:,:),Ycenters(:,:)
      real(REAL64) :: p1(2),p2(2),p3(2),p4(2),c1(2)
      integer :: i, j, im, jm, counts(3)

      _UNUSED_DUMMY(unusable)

      call MAPL_GridGet(grid,localCellCountPerDim=counts,rc=status)
      _VERIFY(status)
      im=counts(1)
      jm=counts(2)
 
      allocate(basis(3,2,im,jm),stat=status)
      _VERIFY(status)

      allocate(corners(im+1,jm+1,2),stat=status)
      _VERIFY(status)

      call MAPL_GridGetCorners(grid,corners(:,:,1),corners(:,:,2),rc=status)
      _VERIFY(status)

      call ESMF_GridGetCoord(grid,localDE=0,coordDim=1,staggerloc=ESMF_STAGGERLOC_CENTER, &
           farrayPtr=Xcenters, rc=status)
      _VERIFY(status)
      call ESMF_GridGetCoord(grid,localDE=0,coordDim=2,staggerloc=ESMF_STAGGERLOC_CENTER, &
           farrayPtr=Ycenters, rc=status)
      _VERIFY(status)
      do j=1,jm
         do i=1,im 
            p1 = mid_pt_sphere(corners(i,j,:),corners(i,j+1,:))
            p2 = mid_pt_sphere(corners(i,j,:),corners(i+1,j,:))
            p3 = mid_pt_sphere(corners(i+1,j,:),corners(i+1,j+1,:))
            p4 = mid_pt_sphere(corners(i,j+1,:),corners(i+1,j+1,:))

            c1(1) = Xcenters(i,j)
            c1(2) = Ycenters(i,j)
            basis(:,1,i,j) = get_unit_vector(p3, c1, p1)
            basis(:,2,i,j) = get_unit_vector(p4, c1, p2)
         enddo
      enddo
      deallocate(corners)
      _RETURN(_SUCCESS)

   end function ComputeGridBasis

   function ComputeXYZBasis(grid_basis,unusable,rc) result(basis) 
      use esmf
      use MAPL_KeywordEnforcerMod
      real(REAL64), intent(in) :: grid_basis(:,:,:,:)  
      real(REAL64), allocatable :: basis(:,:,:,:)  
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc 

      character(len=*), parameter :: Iam= MOD_NAME // 'computeXYZBasis'
      integer :: status
      integer :: im, jm, i, j
      real(real64) :: dp,fac

      _UNUSED_DUMMY(unusable)

      im = size(grid_basis,3)
      jm = size(grid_basis,4)
      allocate(basis(3,2,im,jm),stat=status)
      _VERIFY(status)
      do j=1,jm
         do i=1,im
            dp = dot_product(grid_basis(:,1,i,j),grid_basis(:,2,i,j))
            fac = 1.0d0/(dp**2-1.0d0)
            basis(:,1,i,j)=fac*(grid_basis(:,2,i,j)*dp-grid_basis(:,1,i,j))
            basis(:,2,i,j)=fac*(grid_basis(:,1,i,j)*dp-grid_basis(:,2,i,j))
         enddo
      enddo
      _RETURN(_SUCCESS)

   end function ComputeXYZBasis

   function mid_pt_sphere(p1, p2) result(pm)
      real(REAL64) , intent(IN)  :: p1(2), p2(2)
      real(REAL64) :: pm(2)
      real(REAL64) :: e1(3), e2(3), e3(3),dd

      e1 = latlon2xyz(p1,right_Hand=.true.)
      e2 = latlon2xyz(p2,right_Hand=.true.)
      e3 = e1+e2
      dd = sqrt(dot_product(e3,e3))
      e3 = e3 / dd
      pm = xyz2latlon(e3)

   end function mid_pt_sphere

   function latlon2xyz(sph_coord,right_hand) result(xyz_coord)
      real(REAL64), intent(in), dimension(2) :: sph_coord
      logical, intent(in), optional :: right_hand
      real(REAL64), dimension(3) :: xyz_coord

      logical :: rh_
      if (present(right_hand)) then
         rh_=right_hand
      else
         rh_=.true.
      end if
      xyz_coord(1) = cos(sph_coord(2)) * cos(sph_coord(1))
      xyz_coord(2) = cos(sph_coord(2)) * sin(sph_coord(1))
      if (rh_) then
         xyz_coord(3) = sin(sph_coord(2))
      else
         xyz_coord(3) = -sin(sph_coord(2))
      end if
 
   end function

   function xyz2latlon(xyz_coord) result(sph_coord)
      use MAPL_ConstantsMod, only: PI => MAPL_PI_R8
      real(REAL64), intent(inout):: xyz_coord(3)
      real(REAL64) :: sph_coord(2)
      real(REAL64), parameter:: esl=1.e-10
      real(REAL64):: p(3)
      real(REAL64):: dist, lat, lon
      integer k

      p = xyz_coord
      dist =sqrt( dot_product(p,p))
      do k=1,3
         p(k) = p(k) / dist
      enddo

      if ( (abs(p(1))+abs(p(2)))  < esl ) then
           lon = 0.
      else
           lon = atan2( p(2), p(1) )   ! range [-pi,pi]
      endif

      if ( lon < 0.) lon = 2.*pi + lon
      lat = asin(p(3))

      sph_coord(1) = lon
      sph_coord(2) = lat

   end function xyz2latlon

   function get_unit_vector( p1, p2, p3 ) result(uvect)
      real(REAL64), intent(in):: p1(2), p2(2), p3(2) 
      real(REAL64) :: uvect(3) 
      real(REAL64) :: xyz1(3), xyz2(3), xyz3(3)
      real(REAL64) :: ap 

      xyz1 = latlon2xyz(p1,right_hand=.true.)
      xyz2 = latlon2xyz(p2,right_hand=.true.)
      xyz3 = latlon2xyz(p3,right_hand=.true.)
      uvect = xyz3-xyz1

      ap = dot_product(uvect,xyz2) 
      uvect = uvect - ap*xyz2
      ap = dot_product(uvect,uvect)
      uvect=uvect/sqrt(ap)

   end function get_unit_vector

   function get_grid(this, unusable, rc) result(grid)
      type (ESMF_Grid), pointer :: grid
      class (AbstractGridFactory), target :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam= MOD_NAME // 'get_grid'

      _UNUSED_DUMMY(unusable)

      if (allocated(this%grid)) then
         grid => this%grid
         _RETURN(_SUCCESS)
      else
         grid => null()
         _RETURN(_FAILURE)
      end if

   end function get_grid

      
end module MAPL_AbstractGridFactoryMod
