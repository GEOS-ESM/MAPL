#define _RETURN_(R, S) if(present(R)) R = S; return
#define _RETURN(S) _RETURN_(rc, S)
#define _SUCCESS 0
#define _FAILURE _SUCCESS-1
#include "MAPL_TestErr.h"
module accumulator_transform_test_common
   use esmf
   use funit
   use MAPL_FieldUtils
   implicit none

   integer, parameter :: R4 = ESMF_KIND_R4
   integer, parameter :: R8 = ESMF_KIND_R8
   integer, parameter :: I4 = ESMF_KIND_I4
   integer, parameter :: I8 = ESMF_KIND_I8
   integer(kind=ESMF_KIND_I4), parameter :: TIME_STEP = 1
   integer(kind=ESMF_KIND_I4), parameter :: START_TIME = 3000
   integer, parameter :: MAX_INDEX(2) = [4, 4]
   integer, parameter :: REG_DECOMP(2) = [1, 1]
   type(ESMF_TypeKind_Flag), parameter :: TYPEKIND = ESMF_TYPEKIND_R4

   interface initialize_field
      module procedure :: initialize_field_new
      module procedure :: initialize_field_source
   end interface initialize_field

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

   elemental logical function undef_r8(t) result(lval)
      use MAPL_InternalConstantsMod, only: MAPL_UNDEFINED_REAL64
      real(kind=ESMF_KIND_R8), intent(in) :: t

      lval = t == MAPL_UNDEFINED_REAL64

   end function undef_r8

   elemental subroutine set_undef_r8(t)
      use MAPL_InternalConstantsMod, only: MAPL_UNDEFINED_REAL64
      real(kind=ESMF_KIND_R8), intent(inout) :: t

      t = MAPL_UNDEFINED_REAL64

   end subroutine set_undef_r8

   subroutine create_grid(grid, rc)
      type(ESMF_Grid), optional, intent(inout) :: grid
      integer, optional, intent(out) :: rc
      integer :: status

      grid = ESMF_GridCreate(regDecomp=REG_DECOMP, maxIndex=MAX_INDEX, _RC)
      _RETURN(_SUCCESS)

   end subroutine create_grid

   subroutine initialize_field_new(field, typekind, grid, rc)
      type(ESMF_Field), intent(inout) :: field
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      type(ESMF_Grid), optional, intent(out) :: grid
      integer, optional, intent(out) :: rc

      type(ESMF_Grid) :: grid_
      integer :: status
      
      call create_grid(grid_, _RC)
      field = ESMF_FieldCreate(grid=grid_, typekind=typekind, _RC)
      if(present(grid)) grid=grid_
      _RETURN(_SUCCESS)

   end subroutine initialize_field_new

   subroutine initialize_field_source(field, source, grid, rc)
      type(ESMF_Field), intent(inout) :: field
      type(ESMF_Field), intent(inout) :: source
      type(ESMF_Grid), optional, intent(out) :: grid
      integer, optional, intent(out) :: rc

      type(ESMF_TypeKind_Flag) :: typekind
      type(ESMF_Grid) :: grid_
      integer :: status
      
      call ESMF_FieldGet(source, grid=grid_, typekind=typekind, _RC)
      field = ESMF_FieldCreate(grid=grid_, typekind=typekind, _RC)
      if(present(grid)) grid=grid_
      _RETURN(_SUCCESS)

   end subroutine initialize_field_source

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
      call create_grid(grid, _RC)
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

end module accumulator_transform_test_common
