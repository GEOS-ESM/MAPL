#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"

module mapl_MaplGrid
   use ESMF
   use pFlogger, only: logging, Logger, WrapArray
   use MAPL_ErrorHandlingMod
   use MAPL_KeywordEnforcerMod
   implicit none
   private

   public :: MaplGrid
   public :: MAPL_GridGet
   public :: MAPL_DistGridGet
   public :: MAPL_GetImsJms
   public :: MAPL_GridHasDE

   type :: MaplGrid
      type(ESMF_Grid)                          :: ESMFGRID
      type(ESMF_DELayout)                      :: LAYOUT
      real, pointer                        :: LATS(:,:)
      real, pointer                        :: LONS(:,:)
      integer                                  :: VERTDIM
      integer                                  :: IM_WORLD, JM_WORLD ! Global counts
      integer                                  :: IM, JM, LM ! Local counts
      integer                                  :: MYID, NX, NY, NX0, NY0
      integer                                  :: comm
      integer                                  :: Xcomm, Ycomm
      integer                                  :: readers_comm, IOscattercomm
      integer                                  :: writers_comm, IOgathercomm
      integer                                  :: num_readers, num_writers
      logical                                  :: write_restart_by_face = .false. ! only apply to cubed-sphere grid
      logical                                  :: write_restart_by_oserver = .false. 
      logical                                  :: read_restart_by_face = .false. ! only apply to cubed-sphere grid
      integer, allocatable                      :: i1(:), in(:), j1(:), jn(:)

   contains
      procedure :: set
   end type MaplGrid


contains

  subroutine set(this, grid, unusable, rc)
    class(MaplGrid), intent(inout) :: this
    type(ESMF_Grid), intent(in) :: grid
    class(KeywordEnforcer), optional, intent(in) :: unusable
    integer, optional, intent(out) :: rc

    integer                          :: ndes
    integer                          :: dimcount
    integer                          :: counts(3)
    integer, allocatable             :: minindex(:,:)
    integer, allocatable             :: maxindex(:,:)
    integer, pointer                 :: ims(:) => null()
    integer, pointer                 :: jms(:) => null()
    type(ESMF_DistGrid)              :: distgrid
    type(ESMF_VM)                    :: vm

    integer              :: status
    class(Logger), pointer :: lgr => null()

    _UNUSED_DUMMY(unusable)
    ! At this point, this component must have a valid grid!
    !------------------------------------------------------
    call ESMF_GridValidate(grid, RC=STATUS)
    _VERIFY(STATUS)

    this%ESMFGRID = GRID


! We keep these in the component's grid  for convenience
!-------------------------------------------------------

    call ESMF_GridGet(this%ESMFGRID, DistGrid=distgrid, dimCount=dimCount, RC=STATUS)
    _VERIFY(STATUS)
    call ESMF_DistGridGet(distGRID, deLayout=this%LAYOUT, RC=STATUS)
    _VERIFY(STATUS)

    call ESMF_VmGetCurrent(VM, rc=status)
    _VERIFY(STATUS)
    call ESMF_VmGet(VM, localPet=this%MYID, petCount=ndes, rc=status)
    _VERIFY(STATUS)

! Vertical coordinate must exist and be THE THIRD DIMENSION
! ---------------------------------------------------------

    this%VERTDIM = 3

    call MAPL_GridGet(this%ESMFGRID, localCellCountPerDim=COUNTS, RC=STATUS)
    _VERIFY(STATUS)

#ifdef DEBUG
    print *,'dbg:myId=',this%MYID,trim(Iam)
    print *,'dbg:local gridcounts=',counts
#endif

! Local sizes of three dimensions
!--------------------------------

    this%IM = COUNTS(1)
    this%JM = COUNTS(2)
    this%LM = COUNTS(3)

    call MAPL_GridGet(this%ESMFGRID, globalCellCountPerDim=COUNTS, RC=STATUS)
    _VERIFY(STATUS)

    this%IM_WORLD = COUNTS(1)
    this%JM_WORLD = COUNTS(2)

    allocate(minindex(dimCount,ndes), maxindex(dimCount,ndes), stat=status)
    _VERIFY(STATUS)

! Processors in each direction
!-----------------------------

    call MAPL_DistGridGet(distgrid, &
         minIndex=minindex, &
         maxIndex=maxindex, rc=status)
    _VERIFY(STATUS)

    call MAPL_GetImsJms(Imins=minindex(1,:),Imaxs=maxindex(1,:),&
         Jmins=minindex(2,:),Jmaxs=maxindex(2,:),Ims=ims,Jms=jms,rc=status)
    _VERIFY(STATUS)

    deallocate(maxindex, minindex)

    this%NX = size(ims)
    this%NY = size(jms)
  
! My processor coordinates
!-------------------------

    this%NX0 = mod(this%MYID,this%NX) + 1
    this%NY0 = this%MYID/this%NX + 1


    lgr => logging%get_logger('MAPL.GENERIC')
    call lgr%debug("grid global max= [%3(i0,:,',')~]", WrapArray(counts))
    call lgr%debug(" NX: %i0 ;   NY: %i0", this%nx, this%ny)
    call lgr%debug("NX0: %i0 ;  NY0: %i0", this%nx0, this%ny0)
    call lgr%debug("ims: [%1000(i0,:,',')~]", WrapArray(ims))
    call lgr%debug("jms: [%1000(i0,:,',')~]", WrapArray(jms))
  
    deallocate(jms, ims)

    _RETURN(ESMF_SUCCESS)
 end subroutine set

   
  subroutine MAPL_GridGet(GRID, globalCellCountPerDim, localCellCountPerDim, RC)
      type (ESMF_Grid), intent(IN) :: GRID
      integer, optional, intent(INout) :: globalCellCountPerDim(:)
      integer, optional, intent(INout) :: localCellCountPerDim(:)
      integer, optional, intent(  OUT) :: RC

! local vars
      integer :: status

      integer :: mincounts(ESMF_MAXDIM)
      integer :: maxcounts(ESMF_MAXDIM)
      integer :: gridRank
      integer :: UNGRID
      integer :: sz, tileCount
      logical :: plocal, pglobal, lxtradim
      logical :: isPresent,hasDE

      pglobal = present(globalCellCountPerDim)
      plocal  = present(localCellCountPerDim)

      if (pglobal .or. plocal) then
         call ESMF_GridGet(grid, dimCount=gridRank, rc=status)
         _VERIFY(STATUS)

!ALT kludge
         lxtradim = .false.
         if (gridRank == 1) then
            call ESMF_AttributeGet(grid, name='GRID_EXTRADIM', isPresent=isPresent, rc=status)
            _VERIFY(STATUS)
            if (isPresent) then
               call ESMF_AttributeGet(grid, name='GRID_EXTRADIM', value=UNGRID, rc=status)
               _VERIFY(STATUS)
               lxtradim = .true.
            end if
         else if (gridRank == 2) then
            call ESMF_AttributeGet(grid, name='GRID_LM', isPresent=isPresent, rc=status)
            _VERIFY(STATUS)
            if (isPresent) then
               call ESMF_AttributeGet(grid, name='GRID_LM', value=UNGRID, rc=status)
               _VERIFY(STATUS)
               lxtradim = .true.
            end if
         end if
      end if

      if (pglobal) then

         globalCellCountPerDim = 1
         call ESMF_GridGet(grid, tileCount=tileCount,rc=status)
         _VERIFY(status)

         call ESMF_GridGet(grid, tile=1, staggerLoc=ESMF_STAGGERLOC_CENTER, &
              minIndex=mincounts, &
              maxIndex=maxcounts, &
              rc = status)
         _VERIFY(STATUS)

         sz = min(gridRank, ESMF_MAXDIM, size(globalCellCountPerDim)) 
         globalCellCountPerDim(1:sz) = maxcounts(1:sz)-mincounts(1:sz)+1

         ! kludge for new cube sphere from ESMF
         if (tileCount == 6) then
            if (globalCellCountPerDim(1) /= 1) then ! kludge-on-the-kludge for Single-Column case
               globalCellCountPerDim(2)=globalCellCountPerDim(2)*6
            end if
         end if

         if (lxtradim ) then
            globalCellCountPerDim(gridRank+1) = UNGRID
         end if
      end if

      if (plocal) then
         localCellCountPerDim = 1

         HasDE = MAPL_GridHasDE(grid,rc=status)
         _VERIFY(status)
         if (HasDE) then
            call ESMF_GridGet(GRID, localDE=0, &
                 staggerloc=ESMF_STAGGERLOC_CENTER, &
                 exclusiveCount=localCellCountPerDim, RC=STATUS)
            _VERIFY(STATUS)
         end if

         if (lxtradim ) then
            localCellCountPerDim(gridRank+1) = UNGRID
         end if
      end if

      _RETURN(ESMF_SUCCESS)

    end subroutine MAPL_GridGet

  subroutine MAPL_DistGridGet(distGrid,minIndex,maxIndex,rc)

     type(ESMF_DistGrid), intent(inout) :: distGrid
     integer,             intent(inout) :: MinIndex(:,:)
     integer,             intent(inout) :: MaxIndex(:,:)
     integer, optional,   intent(out  ) :: rc

     integer :: status
     character(len=ESMF_MAXSTR) :: Iam

     integer :: i,tileSize,tileCount,tile,deCount
     logical :: ESMFCubeSphere
     integer, allocatable  :: elementCountPTile(:)
     integer, allocatable :: deToTileMap(:)
     integer, allocatable :: oldMinIndex(:,:),oldMaxIndex(:,:)

     Iam = "MAPL_DistGridGet"

     ESMFCubeSphere = .false.

     call ESMF_DistGridGet(distGrid,tileCount=tileCount,rc=status)
     _VERIFY(STATUS)

     if (tileCount==6) ESMFCubeSphere = .true.

     if (ESMFCubeSphere) then
        allocate(elementCountPTile(tileCount),stat=status)
        _VERIFY(STATUS)
        call ESMF_DistGridGet(distGrid,elementCountPTile=elementCountPTile,rc=status)
        _VERIFY(STATUS)
        ! All tile should have same number of elements
        tileSize = elementCountPTile(1)
        tileSize = SQRT(real(tileSize))
        deallocate(elementCountPTile)

        deCount = size(minIndex,2)

        allocate(deToTileMap(deCount),stat=status)
        _VERIFY(STATUS)
        allocate(oldMinIndex(2,deCount),oldMaxIndex(2,deCount),stat=status)
        _VERIFY(STATUS)
        call ESMF_DistGridGet(distGrid,MaxIndexPDe=oldMaxIndex,MinIndexPDe=oldMinIndex, &
                              deToTileMap=deToTileMap,rc=status)
        _VERIFY(STATUS)
        do i=1,deCount
           tile = deToTileMap(i)
           select case (tile)
              case (1)
                 minIndex(:,i)=oldMinIndex(:,i)
                 maxIndex(:,i)=oldMaxIndex(:,i)
              case (2)
                 minIndex(1,i)=oldMinIndex(1,i) -  tileSize
                 minIndex(2,i)=oldMinIndex(2,i) +  tileSize
                 maxIndex(1,i)=oldMaxIndex(1,i) -  tileSize
                 maxIndex(2,i)=oldMaxIndex(2,i) +  tileSize
              case (3)
                 minIndex(1,i)=oldMinIndex(1,i) -  tileSize
                 minIndex(2,i)=oldMinIndex(2,i) +  tileSize
                 maxIndex(1,i)=oldMaxIndex(1,i) -  tileSize
                 maxIndex(2,i)=oldMaxIndex(2,i) +  tileSize
              case (4)
                 minIndex(1,i)=oldMinIndex(1,i) -2*tileSize
                 minIndex(2,i)=oldMinIndex(2,i) +2*tileSize
                 maxIndex(1,i)=oldMaxIndex(1,i) -2*tileSize
                 maxIndex(2,i)=oldMaxIndex(2,i) +2*tileSize
              case (5)
                 minIndex(1,i)=oldMinIndex(1,i) -2*tileSize
                 minIndex(2,i)=oldMinIndex(2,i) +2*tileSize
                 maxIndex(1,i)=oldMaxIndex(1,i) -2*tileSize
                 maxIndex(2,i)=oldMaxIndex(2,i) +2*tileSize
              case (6)
                 minIndex(1,i)=oldMinIndex(1,i) -3*tileSize
                 minIndex(2,i)=oldMinIndex(2,i) +3*tileSize
                 maxIndex(1,i)=oldMaxIndex(1,i) -3*tileSize
                 maxIndex(2,i)=oldMaxIndex(2,i) +3*tileSize
           end select
        enddo
        deallocate(deToTileMap)
        deallocate(oldMaxIndex,oldMinIndex)

     else

        call ESMF_DistGridGet(distGrid,minIndexPDe=minIndex,maxIndexPDe=maxIndex,rc=status)
        _VERIFY(STATUS)

     end if

 end subroutine MAPL_DistGridGet

  subroutine MAPL_GetImsJms(Imins,Imaxs,Jmins,Jmaxs,Ims,Jms,rc)

!  Given lists of the min and max Is and Js in each processor
!  in a rectangular layout, it computes the number of elements
!  in each of the I columns and the number in each of the j
!  rows of the layout. Pointers Ims and Jms are allocated with the i and j
!  sizes of the layout (nx and ny) and filled with the IMs and JMs.

!  The four input lists must be in the same order, but the order
!  is arbitrary. Nx and Ny can be obtained from the sizes of
!  Ims and Jms, respectively


    use MAPL_SortMod

    integer, dimension(:), intent(IN   ) :: Imins,Imaxs,Jmins,Jmaxs
    integer, pointer                     :: Ims(:),Jms(:)
    integer, optional,   intent(out) :: rc
    
    integer              :: nx, ny, nx0, ny0, nde, k
    integer, allocatable :: Im0(:), Jm0(:)
    integer              :: minI,minJ ! in case the starting index is zero    
    integer              :: status
    
    _ASSERT(.not.associated(Ims), 'Ims is associated and should not be.')
    _ASSERT(.not.associated(Jms), 'Jms is associated and should not be.')
    
!   The original minI and minJ are assumed to be 1
!   The index of EASE grid  is starting from 0
    minI = minval(Imins,DIM=1)
    minJ = minval(Jmins,DIM=1)
    nx = count(Jmins==minJ)
    ny = count(Imins==minI)
    
    allocate(Ims(nx),Jms(ny), stat=STATUS)
    _VERIFY(STATUS)
    allocate(Im0(nx),Jm0(ny), stat=STATUS)
    _VERIFY(STATUS)
    
    nde = size(Imins)
    
    nx0 = 1
    ny0 = 1
    
    do k=1,nde
       if(Imins(k)==minI) then
          Jms(ny0) = Jmaxs(k)-Jmins(k) + 1
          Jm0(ny0) = Jmins(k)
          if(ny0==ny) then
             exit
          else
             ny0 = ny0 + 1
          end if
       end if
    end do
    
    do k=1,nde
       if(Jmins(k)==minJ) then
          Ims(nx0) = Imaxs(k)-Imins(k) + 1
          Im0(nx0) = Imins(k)
          if(nx0==nx) then
             exit
          else
             nx0 = nx0 + 1
          end if
       end if
    end do
    
    call MAPL_Sort(Im0,Ims)
    call MAPL_Sort(Jm0,Jms)
    
    deallocate(Im0,Jm0,stat=status)
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_GetImsJms

  function MAPL_GridHasDE(grid,rc) result(hasDE)
     type(ESMF_Grid), intent(in) :: grid
     integer, intent(out), optional :: rc

     integer :: status
     type(ESMF_DistGrid) :: distGrid
     type(ESMF_DeLayout) :: layout
     integer :: localDECount
     logical :: hasDE

     call ESMF_GridGet    (GRID, distGrid=distGrid, rc=STATUS)
     call ESMF_DistGridGet(distGRID, delayout=layout, rc=STATUS)
     call ESMF_DELayoutGet(layout, localDeCount=localDeCount,rc=status)
     hasDe = (localDECount /=0)
     _RETURN(_SUCCESS)

  end function MAPL_GridHasDE



end module mapl_MaplGrid
