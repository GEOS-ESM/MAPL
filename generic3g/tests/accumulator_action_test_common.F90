#define _RETURN_(R, S) if(present(R)) R = S; return
#define _RETURN(S) _RETURN_(rc, S)
#define _SUCCESS 0
#include "MAPL_TestErr.h"
module accumulator_action_test_common
   use esmf
   use funit
   use MAPL_FieldUtils
   implicit none

   integer, parameter :: R4 = ESMF_KIND_R4
   integer, parameter :: R8 = ESMF_KIND_R8
   integer, parameter :: I8 = ESMF_KIND_I8
   integer(kind=ESMF_KIND_I4), parameter :: TIME_STEP = 1
   integer(kind=ESMF_KIND_I4), parameter :: START_TIME = 3000
   integer, parameter :: MAX_INDEX(2) = [4, 4]
   real(kind=ESMF_KIND_R8), parameter :: MIN_CORNER_COORD(2) = [0.0_R8, 0.0_R8]
   real(kind=ESMF_KIND_R8), parameter :: MAX_CORNER_COORD(2) = [4.0_R8, 4.0_R8]
   type(ESMF_TypeKind_Flag), parameter :: typekind = ESMF_TYPEKIND_R4

contains

   logical function is_initialized(rc) result(lval)
      integer, optional, intent(out) :: rc
      integer :: status

      lval = ESMF_IsInitialized(_RC)
      _RETURN(_SUCCESS)

   end function is_initialized

   elemental logical function undef(t) result(lval)
      use MAPL_InternalConstantsMod, only: MAPL_UNDEFINED_REAL
      real(kind=ESMF_KIND_R4), intent(in) :: t

      lval = t == MAPL_UNDEFINED_REAL

   end function undef

   elemental subroutine set_undef(t)
      use MAPL_InternalConstantsMod, only: MAPL_UNDEFINED_REAL
      real(kind=ESMF_KIND_R4), intent(inout) :: t

      t = MAPL_UNDEFINED_REAL

   end subroutine set_undef

   subroutine initialize_field(field, typekind, grid, rc)
      type(ESMF_Field), intent(inout) :: field
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      type(ESMF_Grid), optional, intent(inout) :: grid
      integer, optional, intent(out) :: rc
      type(ESMF_Grid) :: grid_
      logical :: grid_created

      integer :: status
      
      grid_created = .FALSE.
      if(present(grid)) then
         grid_created = ESMF_GridIsCreated(grid, _RC)
         if(grid_created) grid_ = grid
      end if

      if(.not. grid_created) then
         grid_ = ESMF_GridCreateNoPeriDimUfrm(maxIndex=MAX_INDEX, &
            & minCornerCoord=MIN_CORNER_COORD, maxCornerCoord=MAX_CORNER_COORD, _RC)
      end if

      field = ESMF_FieldCreate(grid=grid_, typekind=typekind, _RC)
      
      if(present(grid)) grid = grid_
      _RETURN(_SUCCESS)

   end subroutine initialize_field

   subroutine initialize_objects(importState, exportState, clock, typekind, rc) 
      type(ESMF_State), intent(inout) :: importState, exportState
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: importField, exportField
      type(ESMF_Time) :: startTime
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Grid) :: grid

      call ESMF_TimeIntervalSet(timeStep, s=TIME_STEP, _RC)
      call ESMF_TimeSet(startTime, yy=START_TIME, _RC)
      clock = ESMF_ClockCreate(timeStep=timeStep, startTime=startTime, _RC)
      grid = ESMF_GridCreate(regDecomp = [1, 1], maxIndex=MAX_INDEX, _RC)
      importField = ESMF_FieldCreate(grid=grid, typekind=typekind, _RC)
      exportField = ESMF_FieldCreate(grid=grid, typekind=typekind, _RC)
      importState = ESMF_StateCreate(stateIntent=ESMF_STATEINTENT_IMPORT, fieldList=[importField], name='import', _RC)
      exportState = ESMF_StateCreate(stateIntent=ESMF_STATEINTENT_EXPORT, fieldList=[exportField], name='export', _RC)
      _RETURN(_SUCCESS)

   end subroutine initialize_objects

   subroutine get_field(state, field, rc)
      type(ESMF_State), intent(inout) :: state
      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: itemNameList(1)

      call ESMF_StateGet(state, itemNameList=itemNameList, _RC)
      call ESMF_StateGet(state, itemName=itemNameList(1), field=field, _RC)
      _RETURN(_SUCCESS)

   end subroutine get_field
   
   subroutine destroy_objects(importState, exportState, clock, rc)
      type(ESMF_State), intent(inout) :: importState, exportState
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: importField, exportField
      type(ESMF_Grid) :: grid

      call get_field(importState, importField, _RC)
      call get_field(exportState, exportField, _RC)
      call ESMF_StateDestroy(importState, _RC)
      call ESMF_StateDestroy(exportState, _RC)
      call ESMF_FieldGet(importField, grid=grid, _RC)
      call ESMF_FieldDestroy(importField, _RC)
      call ESMF_FieldDestroy(exportField, _RC)
      call ESMF_GridDestroy(grid, _RC)
      call ESMF_ClockDestroy(clock, _RC)
      _RETURN(_SUCCESS)

   end subroutine destroy_objects

end module accumulator_action_test_common
