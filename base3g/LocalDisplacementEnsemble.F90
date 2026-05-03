#include "MAPL.h"

module mapl_LocalDisplacementEnsemble
   use mpi
   use mapl_ErrorHandling, only: MAPL_Assert, MAPL_Verify, MAPL_Return
   use esmf
   use gftl2_integer64Set
   implicit none !(type,external)
   private

   public :: LocalDisplacementEnsemble

   integer, parameter :: UnInit=-9999
   
   type :: LocalDisplacementEnsemble
      private
      integer :: hw
      integer :: num_members
      integer, allocatable :: mapping(:,:,:,:)
      type(ESMF_RouteHandle) :: routehandle
      type(esmf_Field) :: hfield ! 2d, R4
   contains
      procedure :: initialize
      procedure :: get
      procedure :: fill
      procedure :: destroy
   end type LocalDisplacementEnsemble


contains

   subroutine initialize(this, grid, halo_width, rc)
      class(LocalDisplacementEnsemble), intent(inout) :: this
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: halo_width
      integer, optional, intent(out) :: rc
      
      integer :: status
      type(esmf_vm) :: vm
      type(esmf_Field) :: field
      type(esmf_RouteHandle) :: rh
      type(esmf_distgrid) :: distgrid
!      integer(ESMF_KIND_I8), pointer :: label(1-halo_width:,1-halowidth:)
      integer(ESMF_KIND_I8), pointer :: label(:,:)
      type(Integer64Set) :: set
!      type(IntegerSet) :: set
      integer, allocatable :: minIndex(:,:), maxIndex(:,:)
      integer, allocatable :: tileMinIndex(:,:), tileMaxIndex(:,:)
      integer :: tileCount
      integer(kind=ESMF_KIND_I8) :: NI_GLOB, NJ_GLOB, global_index
      integer :: NI_LOC, NJ_LOC
      integer :: i, j, comm, mpierr, count
      integer :: di, dj
      integer :: num_unique, maxMembers
      integer :: petCount, localPet, localMaxMembers, lp

      integer :: hw
      
      hw = halo_width ! shorthand
      this%hw = hw

      field = esmf_FieldCreate(grid, typekind=ESMF_TYPEKIND_I8, &
           totalLWidth=[hw, hw], &
           totalUWidth=[hw, hw], _RC)

      call ESMF_FieldHaloStore(field, rh, &
           haloLDepth=[hw,hw], &
           haloUDepth=[hw,hw], _RC)

      call esmf_FieldGet(field, fArrayPtr=label, _RC)
      
      call esmf_VMGetCurrent(vm, _RC)
      call esmf_VMGet(vm, petCount=petCount, localPet=localPet, &
           mpicommunicator=comm, _RC)
      lp = localPet+1 ! 1-based
      
      allocate(minIndex(2,petCount))
      allocate(maxIndex(2,petCount))

      call esmf_GridGet(grid, distGrid=distgrid, _RC)
      call ESMF_DistGridGet(distgrid, tileCount=tileCount, _RC)
      allocate(tileMinIndex(2, tileCount))
      allocate(tileMaxIndex(2, tileCount))
      call ESMF_DistGridGet(distgrid, &
           minIndexPDe=minIndex, maxIndexPDe=maxIndex, &
           minIndexPTile=tileMinIndex, maxIndexPTile=tileMaxIndex, _RC)

      NI_GLOB = maxval(tileMaxIndex(1,:) - tileMinIndex(1,:)) + 1
      NJ_GLOB = maxval(tileMaxIndex(2,:) - tileMinIndex(2,:)) + 1
      NI_LOC = maxIndex(1,lp) - minIndex(1,lp) + 1
      NJ_LOC = maxIndex(2,lp) - minIndex(2,lp) + 1

      ! ASSERT that halo width is smaller than local domain size
      ! the assert should check the size of the neighbors we are bringing,
      ! but the statements below is a good first approximation

      _ASSERT(NI_LOC > hw, 'Local domain (X) is smaller than HW')
      _ASSERT(NJ_LOC > hw, 'Local domain (Y) is smaller than HW')

      ! Set values on exclusive domain (interior!!!)
      
      do j = 1, nj_loc
         do i = 1, ni_loc
            label(i,j) = (NI_GLOB*NJ_GLOB) * localPet + (i + NI_LOC * j)
         end do
      end do

      call esmf_FieldHalo(field, rh, _RC) 
      call ESMF_RouteHandleDestroy(rh, noGarbage=.true., _RC)

      maxMembers = huge(1)
      do j = 1, nj_loc
         do i = 1, ni_loc

            num_unique = 0
            do dj = -hw, hw
               do di = -hw, hw

                  global_index = label(i+di, j+dj)
                  call set%insert(global_index)
               end do
            end do
            num_unique = set%size()
            call set%clear()
            maxMembers = min(maxMembers, num_unique)
         end do
      end do
      localMaxMembers = maxMembers
      call mpi_allReduce(localMaxMembers, maxMembers, 1, MPI_INTEGER, MPI_MIN, comm, mpierr)
      allocate(this%mapping(2, ni_loc, nj_loc, maxMembers))
      this%mapping = UnInit ! initialize, we might not need it
      do j = 1, nj_loc
         do i = 1, ni_loc

            count = 0
            outer_dj: do dj = -hw, hw
               inner_di: do di = -hw, hw

                  ! Potential directional bias 
                  global_index = label(i+di, j+dj)
                  if (set%count(global_index) == 0) then ! new index
                     count = count + 1
                     this%mapping(1:,i,j,count) = [di, dj]
                     call set%insert(global_index)
                  end if
                  if (set%size() == maxMembers) exit outer_dj
               end do inner_di
            end do outer_dj
            call set%clear()

         end do
      end do

      call esmf_FieldDestroy(field, noGarbage=.true., _RC)

      ! Create reusable field and routehandle for fill() procedure
      this%hfield = esmf_FieldCreate(grid, typekind=ESMF_TYPEKIND_R4, &
           totalLWidth=[hw, hw], &
           totalUWidth=[hw, hw], _RC)

      call esmf_FieldHaloStore(this%hfield, this%routehandle, &
           haloLDepth=[hw,hw], &
           haloUDepth=[hw,hw], _RC)

      _RETURN(_SUCCESS)
   end subroutine initialize


   subroutine get(this, num_members, max_members, rc)
      class(LocalDisplacementEnsemble), intent(in) :: this
      integer, allocatable, optional :: num_members(:,:)
      integer, optional :: max_members
      integer, optional, intent(out) :: rc

      if (present(num_members)) then
         _FAIL('getting NUM_MEMBERS is not supported yet')
         num_members = this%num_members !???
      end if

      if (present(max_members)) then
         max_members = size(this%mapping,4)
      end if
      
      _RETURN(ESMF_SUCCESS)
   end subroutine get

   subroutine fill(this, array_in, array_out, rc)
     class(LocalDisplacementEnsemble), intent(inout) :: this

     real, intent(in) :: array_in(:,:)      ! 30x30   (1:30,1:30)
     real, intent(inout), allocatable :: array_out(:,:,:)  ! 30x30xmax_members
     integer, optional, intent(out) :: rc

     real, pointer :: hdata(:,:)
     integer :: status
     integer :: i,j,n,di,dj,nmax
     integer :: i1, i2, j1, j2

     call esmf_FieldGet(this%hfield, fArrayPtr=hdata, _RC)

     i1 = lbound(array_in, 1); i2 = ubound(array_in, 1)
     j1 = lbound(array_in, 2); j2 = ubound(array_in, 2)
     hdata(i1:i2,j1:j2) = array_in

     call esmf_FieldHalo(this%hfield, this%routehandle, _RC)

     call this%get(max_members=nmax)

     allocate(array_out(i1:i2,j1:j2,nmax))
     do j = 1, size(array_out,2)
        do i = 1, size(array_out,1)
           do n = 1, nmax
              di = this%mapping(1,i,j,n)
              dj = this%mapping(2,i,j,n)
              !               print *,'DEBUG:lde:i,j,n',i,j,n,di,dj
              array_out(i,j,n) = hdata(i+di, j+dj)
           end do
        end do
     end do
     _RETURN(_SUCCESS)
   end subroutine fill

    subroutine destroy(this, rc)
      class(LocalDisplacementEnsemble), intent(inout) :: this
      integer, optional, intent(out) :: rc
      
      integer :: status
      
      call esmf_FieldDestroy(this%hfield, noGarbage=.true., _RC)
      call ESMF_RouteHandleDestroy(this%routehandle, noGarbage=.true., _RC)


      _RETURN(_SUCCESS)
    end subroutine destroy
end module mapl_LocalDisplacementEnsemble


   
