module Test_NUOPCmap
   use pfunit
   use NUOPCmapMod
   use ESMF

contains
   @test
   subroutine test_add_phase()
      type(NUOPCmap)          :: NUOPC_map
      character(*), parameter :: phase_label = 'test'
      character(*), parameter :: phase_label_bad = 'bad'
      integer                 :: rc

      call NUOPC_map%add_phase(1, phase_label, rc)
      @assert_that(rc, is(equal_to(0)))
      @assert_that(NUOPC_map%NUOPC_map%count(1) > 0, is(true()))
      @assert_that(NUOPC_map%NUOPC_map%at(1), is(equal_to(phase_label)))
      @assert_that(NUOPC_map%NUOPC_map%at(1), is(not(equal_to(phase_label_bad))))

      call NUOPC_map%add_phase(1, phase_label_bad, rc)
      @assert_that(rc, is(equal_to(ESMF_RC_OBJ_BAD)))
      @assert_that(NUOPC_map%NUOPC_map%count(1) > 0, is(true()))
      @assert_that(NUOPC_map%NUOPC_map%at(1), is(not(equal_to(phase_label_bad))))
   end subroutine test_add_phase

   @test
   subroutine test_get_phase()
      type(NUOPCmap)          :: NUOPC_map
      character(:), pointer   :: phase_label
      character(*), parameter :: test_phase_label = 'test'
      integer                 :: rc

      call NUOPC_map%get_phase(1, phase_label, rc)
      @assert_that(rc, is(equal_to(ESMF_RC_OBJ_BAD)))
      @assert_that(associated(phase_label), is(false()))

      call NUOPC_map%add_phase(1, test_phase_label, rc)
      @assert_that(rc, is(equal_to(0)))

      call NUOPC_map%get_phase(1, phase_label, rc)
      @assert_that(rc, is(equal_to(0)))
      @assert_that(associated(phase_label), is(true()))
      @assert_that(phase_label, is(equal_to(test_phase_label)))
   end subroutine test_get_phase
end module Test_NUOPCmap
