!------------------------------------------------------------------------------
! Minimal ESMF init/finalize hooks for the GraphCore pFUnit test executable
! (EXTRA_INITIALIZE/EXTRA_FINALIZE/EXTRA_USE), sufficient for GraphStateItem's
! tests to allocate real ESMF_Field/FieldBundle/State handles. Deliberately
! does not depend on the full MAPL library - this repo has no
! GridComp/StateRegistry context in scope (see proposal.md).
!------------------------------------------------------------------------------
module GraphCoreTestInit_mod
   use ESMF, only: ESMF_Initialize, ESMF_Finalize, ESMF_LOGKIND_NONE, ESMF_SUCCESS
   use udunits2f, only: UDUNITS_Initialize => Initialize
   implicit none(type, external)

contains

   ! extension-reuse change: also initializes udunits2f (relies on the
   ! UDUNITS2_XML_PATH environment variable, set for this test's ctest
   ! entry - superstructure/generic/graph/tests/CMakeLists.txt - same
   ! convention as mapl/MaplFramework.F90's own initialize_udunits(),
   ! not a new mechanism) so Test_UnitsConvertTransform.pf's real
   ! unit-conversion test can run. Still deliberately does not `use MAPL`
   ! or otherwise pull in GridComp/StateRegistry - udunits2f is a
   ! general-purpose library dependency, not a StateRegistry one.
   subroutine GraphCoreTest_Initialize()
      integer :: status

      call ESMF_Initialize(logkindflag=ESMF_LOGKIND_NONE, rc=status)
      if (status /= ESMF_SUCCESS) error stop 'GraphCoreTestInit_mod: ESMF_Initialize failed'

      call UDUNITS_Initialize(rc=status)
      if (status /= 0) error stop 'GraphCoreTestInit_mod: udunits2f Initialize failed'
   end subroutine GraphCoreTest_Initialize

   subroutine GraphCoreTest_Finalize()
      integer :: status

      call ESMF_Finalize(rc=status)
      if (status /= ESMF_SUCCESS) error stop 'GraphCoreTestInit_mod: ESMF_Finalize failed'
   end subroutine GraphCoreTest_Finalize

end module GraphCoreTestInit_mod
