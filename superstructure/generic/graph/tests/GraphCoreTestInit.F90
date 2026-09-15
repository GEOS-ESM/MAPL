!------------------------------------------------------------------------------
! Minimal ESMF init/finalize hooks for the GraphCore pFUnit test executable
! (EXTRA_INITIALIZE/EXTRA_FINALIZE/EXTRA_USE), sufficient for GraphStateItem's
! tests to allocate real ESMF_Field/FieldBundle/State handles. Deliberately
! does not depend on the full MAPL library - this repo has no
! GridComp/StateRegistry context in scope (see proposal.md).
!------------------------------------------------------------------------------
module GraphCoreTestInit_mod
   use ESMF, only: ESMF_Initialize, ESMF_Finalize, ESMF_LOGKIND_NONE, ESMF_SUCCESS
   implicit none(type, external)

contains

   subroutine GraphCoreTest_Initialize()
      integer :: status

      call ESMF_Initialize(logkindflag=ESMF_LOGKIND_NONE, rc=status)
      if (status /= ESMF_SUCCESS) error stop 'GraphCoreTestInit_mod: ESMF_Initialize failed'
   end subroutine GraphCoreTest_Initialize

   subroutine GraphCoreTest_Finalize()
      integer :: status

      call ESMF_Finalize(rc=status)
      if (status /= ESMF_SUCCESS) error stop 'GraphCoreTestInit_mod: ESMF_Finalize failed'
   end subroutine GraphCoreTest_Finalize

end module GraphCoreTestInit_mod
