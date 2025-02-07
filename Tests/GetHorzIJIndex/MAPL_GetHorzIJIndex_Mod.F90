#include "MAPL_Generic.h"
#include "MAPL_Exceptions.h"


module MAPL_GetHorzIJIndex_mod

  use ESMF
  use MAPL

  implicit none
  private

  type :: ThreadWorkspace
     integer, allocatable, dimension(:) :: II, JJ
  end type ThreadWorkspace

  type :: GetHorzIJIndex
     integer :: npts
     integer, allocatable, dimension(:) :: II_ref, JJ_ref
     real, allocatable, dimension(:) :: lat, lon
     type(ThreadWorkspace), allocatable :: workspaces(:)
  end type GetHorzIJIndex

   type wrap_
      type (GetHorzIJIndex), pointer     :: PTR !=> null()
   end type wrap_

  public setservices

  contains

  subroutine setservices(gc,rc)

     type(ESMF_GridComp), intent(inout)  :: gc
     integer, optional :: rc

     type (MAPL_MetaComp),       pointer    :: MAPL
     type (ESMF_Config) :: cf
     integer :: status
     logical :: use_threads
     integer :: num_threads
     type(wrap_) :: wrap
     type(GetHorzIJIndex), pointer :: self


!   Wrap internal state for storing in GC
!   -------------------------------------
    allocate (self, _STAT)
    wrap%ptr => self

    call MAPL_GetObjectFromGC (gc, MAPL, _RC)
    call ESMF_GridCompGet(gc, config=cf, _RC)
    call ESMF_ConfigGetAttribute(cf, use_threads, label='use_threads:', default=.FALSE., _RC)
    call MAPL%set_use_threads(use_threads)

    num_threads = MAPL_get_num_threads()
    allocate(self%workspaces(0:num_threads-1), _STAT)

     !   Store internal state in GC
!   --------------------------
     call ESMF_UserCompSetInternalState ( GC, 'GetHorzIJIndex', wrap, _RC )
     call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_INITIALIZE,  initialize, _RC)
     call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_RUN,  run, _RC)
     call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_FINALIZE, finalize, _RC)
     call MAPL_GenericSetServices(gc, _RC)

     _RETURN(_SUCCESS)

  end subroutine setservices


  subroutine initialize(gc, import, export, clock, rc)
     type(ESMF_GridComp), intent(inout) :: gc
     type(ESMF_State), intent(inout) :: import
     type(ESMF_State), intent(inout) :: export
     type(ESMF_Clock), intent(inout) :: clock
     integer, intent(out), optional :: rc

     type (wrap_)                      :: wrap
     type (GetHorzIJIndex), pointer     :: self
     integer :: status
     type(ESMF_VM) :: vm
     integer :: pet, iunit
     character(len=ESMF_MAXSTR) :: fname_indx, fname_lon_lat

     call MAPL_GridCreate(gc, _RC)

      call ESMF_VMGetCurrent(vm, _RC)
      call ESMF_VMGet(vm, localPet=pet, _RC)
      call ESMF_UserCompGetInternalState(GC, 'GetHorzIJIndex', wrap, _RC)
      self => wrap%ptr

      write(fname_indx,'(a,i2.2,a)') 'II_JJ_',pet,'.data'
      write(fname_lon_lat,'(a)') 'lon_lat.data'
      open(newunit=iunit, form='formatted', file=trim(fname_indx))
      read(iunit, *) self%npts
      allocate(self%II_ref(self%npts), self%JJ_ref(self%npts), _STAT)
      allocate(self%lon(self%npts), self%lat(self%npts), _STAT)
      read(iunit, *) self%II_ref
      read(iunit, *) self%JJ_ref
      close(iunit)
      open(newunit=iunit, form='formatted', file=trim(fname_lon_lat))
      read(iunit, *) self%lon
      read(iunit, *) self%lat

     call MAPL_GenericInitialize(gc, import, export, clock, _RC)

     _RETURN(_SUCCESS)

  end subroutine initialize

  subroutine run_check_dim(grid, rc)
     type(ESMF_Grid), intent(in) :: grid
     integer, intent(out), optional :: rc

! locals
     integer :: status
     integer :: dims(3)

      call MAPL_GridGet(grid, globalCellCountPerDim=dims,_RC)

      _ASSERT(dims(1)*6 == dims(2), 'Test global dim failed: Not a cubed sphere grid')

     _RETURN(_SUCCESS)

  end subroutine run_check_dim

  subroutine run(gc, import, export, clock, rc)
     type(ESMF_GridComp), intent(inout) :: gc
     type(ESMF_State), intent(inout) :: import
     type(ESMF_State), intent(inout) :: export
     type(ESMF_Clock), intent(inout) :: clock
     integer, intent(out), optional :: rc

! locals
     type (wrap_)                      :: wrap
     type (GetHorzIJIndex), pointer     :: self
     type (MAPL_MetaComp), pointer     :: mapl
     type (ESMF_Grid)                  :: grid
     integer :: status
     integer :: thread  !, npts, num_threads, thread
     type(ThreadWorkspace), pointer :: workspace
     logical :: isPresent
     integer, allocatable  :: global_grid_info(:)
     integer :: itemCount, bounds_min

      call MAPL_GetObjectFromGC (gc, MAPL, _RC)
      call MAPL_Get (MAPL, grid=grid, _RC)
      call ESMF_GridValidate(grid,_RC)

      call run_check_dim(grid, _RC)

      call ESMF_UserCompGetInternalState(GC, 'GetHorzIJIndex', wrap, _RC)
      self => wrap%ptr

      thread = MAPL_get_current_thread()
      workspace => self%workspaces(thread)
      allocate(workspace%II(self%npts), workspace%JJ(self%npts), _STAT)

      call MAPL_GetHorzIJIndex(self%npts, workspace%II, workspace%JJ,  &
                grid = grid, lon = self%lon, lat = self%lat, _RC)

      ! fetch J starting index for this thread
      bounds_min = 1
      call ESMF_AttributeGet(grid, name="GLOBAL_GRID_INFO", isPresent=isPresent, _RC)
      if (isPresent) then
        call ESMF_AttributeGet(grid, name="GLOBAL_GRID_INFO", itemCount=itemCount, _RC)
        allocate(global_grid_info(itemCount), _STAT)
        call ESMF_AttributeGet(grid, name="GLOBAL_GRID_INFO", valueList=global_grid_info, _RC)
        bounds_min = global_grid_info(itemCount)
        deallocate(global_grid_info, _STAT)
      end if

      ! normalize with offset = starting J index to march baseline (i.e. that is not threaded)
      where(workspace%JJ > 0) 
         workspace%JJ = workspace%JJ + bounds_min - 1
      end where

     _UNUSED_DUMMY(import)
     _UNUSED_DUMMY(export)
     _UNUSED_DUMMY(clock)

     _RETURN(_SUCCESS)

  end subroutine run

  subroutine finalize(gc, import, export, clock, rc)
! !ARGUMENTS:
     type (ESMF_GridComp), intent(inout) :: gc
     type (ESMF_State),    intent(inout) :: import
     type (ESMF_State),    intent(inout) :: export
     type (ESMF_Clock),    intent(inout) :: clock
     integer, optional,    intent(  out) :: rc

! locals
     integer, allocatable, dimension(:) :: II, JJ
     type(wrap_) :: wrap
     type(GetHorzIJIndex), pointer :: self
     integer :: num_threads, i, ith
     integer :: status
 
     call ESMF_UserCompGetInternalState(GC, 'GetHorzIJIndex', wrap, _RC)
     self => wrap%ptr
     num_threads = size(self%workspaces)
     ! merge results from all threads
     allocate(II(self%npts), JJ(self%npts), _STAT)
     do i = 1, self%npts
      II(i) = -1
      JJ(i) = -1
      do ith = 0, num_threads-1
         II(i) = max(II(i), self%workspaces(ith)%II(i)) 
         JJ(i) = max(JJ(i), self%workspaces(ith)%JJ(i)) 
      end do
     end do

    _ASSERT(all(II == self%II_ref), 'Test failed: Some I index disagrees with baseline')
     _ASSERT(all(JJ == self%JJ_ref), 'Test failed: some J index disagrees with baseline')

     deallocate(II, JJ, _STAT)
     do ith = 0, num_threads-1
        deallocate(self%workspaces(ith)%II, _STAT)
        deallocate(self%workspaces(ith)%JJ, _STAT)
     end do
     deallocate(self%II_ref, self%JJ_ref, _STAT)
     deallocate(self%lon, self%lat, _STAT)
     deallocate(self%workspaces, _STAT)

     _UNUSED_DUMMY(import)
     _UNUSED_DUMMY(export)
     _UNUSED_DUMMY(clock)

     _RETURN(_SUCCESS)

  end subroutine finalize

end module MAPL_GetHorzIJIndex_mod
