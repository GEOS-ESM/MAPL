#include "MAPL_TestErr.h"

module Test_InfoUtilities
   use mapl3g_ESMF_info_keys
   use mapl3g_InfoUtilities
   use esmf
   use funit

   implicit none (type, external)

contains

   @test
   subroutine test_set_state()
      type(ESMF_State) :: state
      integer :: status
      character(:), allocatable :: name

      state = ESMF_StateCreate(name='export', _RC)
      call MAPL_InfoSetShared(state, key='component', value='comp_A', _RC)
      call MAPL_InfoGetShared(state, key='component', value=name, _RC)

      @assertEqual('comp_A', name)

      call ESMF_StateDestroy(state, _RC)
   end subroutine test_set_state

   @test
   subroutine test_setShared()
      type(ESMF_State) :: state
      type(ESMF_Field) :: field
      integer :: status
      integer :: i

      state = ESMF_StateCreate(name='export', _RC)

      field = ESMF_FieldEmptyCreate(name='f', _RC)
      call ESMF_StateAdd(state, [field], _RC)

      call MAPL_InfoSetShared(state, short_name='f', key='a', value=1, _RC)
      call MAPL_InfoGetShared(state, short_name='f', key='a', value=i, _RC)

      @assert_that(i, is(1))

      call ESMF_FieldDestroy(field, _RC)
      call ESMF_StateDestroy(state, _RC)

   end subroutine test_setShared

   @test
   subroutine test_setPrivate()
      type(ESMF_State) :: state
      type(ESMF_Field) :: field
      integer :: status
      integer :: i

      state = ESMF_StateCreate(name='import', _RC)
      call MAPL_InfoSetNameSpace(state, namespace='compA', _RC)

      field = ESMF_FieldEmptyCreate(name='f', _RC)
      call ESMF_StateAdd(state, [field], _RC)

      call MAPL_InfoSetPrivate(state, short_name='f', key='a', value=1, _RC)
      call MAPL_InfoGetPrivate(state, short_name='f', key='a', value=i, _RC)

      @assert_that(i, is(1))

      call ESMF_FieldDestroy(field, _RC)
      call ESMF_StateDestroy(state, _RC)

   end subroutine test_setPrivate

   @test
   ! Check that field shared in 2 states does not overwrite info between gridcomps.
   subroutine test_setPrivate_is_private()
      type(ESMF_State) :: state_a
      type(ESMF_State) :: state_b
      type(ESMF_Field) :: field
      integer :: status
      integer :: i_a, i_b

      state_a = ESMF_StateCreate(name='import', _RC)
      call MAPL_InfoSetNameSpace(state_a, namespace='compA', _RC)

      state_b = ESMF_StateCreate(name='import', _RC)
      call MAPL_InfoSetNameSpace(state_b, namespace='compB', _RC)


      field = ESMF_FieldEmptyCreate(name='f', _RC)
      call ESMF_StateAdd(state_a, [field], _RC)
      call ESMF_StateAdd(state_b, [field], _RC)

      call MAPL_InfoSetPrivate(state_a, short_name='f', key='a', value=1, _RC)
      call MAPL_InfoSetPrivate(state_b, short_name='f', key='a', value=2, _RC)

      call MAPL_InfoGetPrivate(state_a, short_name='f', key='a', value=i_a, _RC)
      call MAPL_InfoGetPrivate(state_b, short_name='f', key='a', value=i_b, _RC)

      @assert_that(i_a, is(1))
      @assert_that(i_b, is(2))

      call ESMF_FieldDestroy(field, _RC)
      call ESMF_StateDestroy(state_a, _RC)
      call ESMF_StateDestroy(state_b, _RC)

   end subroutine test_setPrivate_is_private

   @test
   subroutine test_setInternal()
      type(ESMF_State) :: state
      type(ESMF_Field) :: field
      integer :: status
      integer(ESMF_KIND_I4), allocatable :: i(:)

      state = ESMF_StateCreate(name='import', _RC)
      field = ESMF_FieldEmptyCreate(name='f', _RC)
      call ESMF_StateAdd(state, [field], _RC)

      call MAPL_InfoSetInternal(state, short_name='f', key='a', values=[1, 2], _RC)
      call MAPL_InfoGetInternal(state, short_name='f', key='a', values=i, _RC)

      @assert_that(i, is(equal_to([1,2])))

      call ESMF_FieldDestroy(field, _RC)
      call ESMF_StateDestroy(state, _RC)

   end subroutine test_setInternal

end module Test_InfoUtilities
