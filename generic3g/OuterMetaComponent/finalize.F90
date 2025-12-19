#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) finalize_smod

   use mapl3g_GriddedComponentDriverMap
   use mapl3g_GenericPhases
   use mapl_ErrorHandling
   use MAPL_CommsMod, only: MAPL_Am_I_Root
   use MAPL_Profiler, only: ProfileReporter
   use MAPL_Profiler, only: MultiColumn, NameColumn, FormattedTextColumn, PercentageColumn
   use MAPL_Profiler, only: InclusiveColumn, ExclusiveColumn, SeparatorColumn, NumCyclesColumn
   use pflogger, only: logger_t => logger

   implicit none (type, external)

contains

   module recursive subroutine finalize(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(GriddedComponentDriver), pointer :: child
      type(GriddedComponentDriverMapIterator) :: iter
      character(*), parameter :: PHASE_NAME = 'GENERIC::FINALIZE_USER'
      type(StringVector), pointer :: finalize_phases
      logical :: found
      integer :: phase_idx, status
  
      call recurse_finalize_(this, phase_idx=GENERIC_FINALIZE_USER, _RC)

      ! User gridcomp may not have any given phase; not an error condition if not found
      finalize_phases => this%user_phases_map%at(ESMF_METHOD_FINALIZE, _RC)
      phase_idx = get_phase_index(finalize_phases, phase_name=phase_name, found=found)
      _RETURN_UNLESS(found)

      ! Finalize profiler
      call this%profiler%stop(_RC)
      call report_generic_profile(this, _RC)

      call this%run_custom(ESMF_METHOD_FINALIZE, PHASE_NAME, _RC)

      ! TODO - release resources

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine finalize

   recursive subroutine recurse_finalize_(this, phase_idx, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer :: phase_idx
      integer, optional, intent(out) :: rc

      integer :: status
      type(GriddedComponentDriverMapIterator) :: iter
      type(GriddedComponentDriver), pointer :: child

      associate(e => this%children%ftn_end())
        iter = this%children%ftn_begin()
        do while (iter /= e)
           call iter%next()
           child => iter%second()
           call child%finalize(phase_idx=phase_idx, _RC)
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine recurse_finalize_

   subroutine report_generic_profile(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      character(:), allocatable :: report(:)
      type(ProfileReporter) :: reporter
      type(MultiColumn) :: min_multi, mean_multi, max_multi, pe_multi, n_cyc_multi
      type(ESMF_VM) :: vm
      character(1) :: empty(0)
      class(logger_t), pointer :: logger
      integer :: index, status

      logger => this%get_logger()

      ! Generate stats _across_ processes covered by this timer
      ! Requires consistent call trees for now.
      call this%profiler%reduce()

      call ESMF_VmGetCurrent(vm, _RC)
      if  (MAPL_AM_I_Root(vm)) then
         reporter = ProfileReporter(empty)
         call reporter%add_column(NameColumn(25, separator=" "))

         min_multi = MultiColumn(['Min'], separator='=')
         call min_multi%add_column(FormattedTextColumn('   %  ','(f6.2)', 6, PercentageColumn(ExclusiveColumn('MIN')), separator='-'))
         call min_multi%add_column(FormattedTextColumn('inclusive', '(f10.2)', 10, InclusiveColumn('MIN'), separator='-'))
         call min_multi%add_column(FormattedTextColumn('exclusive', '(f10.2)',10, ExclusiveColumn('MIN'), separator='-'))

         mean_multi = MultiColumn(['Mean'], separator='=')
         call mean_multi%add_column(FormattedTextColumn('   %  ','(f6.2)', 6, PercentageColumn(ExclusiveColumn('MEAN')), separator='-'))
         call mean_multi%add_column(FormattedTextColumn('inclusive', '(f10.2)', 10, InclusiveColumn('MEAN'), separator='-'))
         call mean_multi%add_column(FormattedTextColumn('exclusive', '(f10.2)', 10, ExclusiveColumn('MEAN'), separator='-'))

         max_multi = MultiColumn(['Max'], separator='=')
         call max_multi%add_column(FormattedTextColumn('   %  ','(f6.2)', 6, PercentageColumn(ExclusiveColumn('MAX')), separator='-'))
         call max_multi%add_column(FormattedTextColumn('inclusive', '(f10.2)', 10, InclusiveColumn('MAX'), separator='-'))
         call max_multi%add_column(FormattedTextColumn('exclusive', '(f10.2)', 10, ExclusiveColumn('MAX'), separator='-'))

         pe_multi = MultiColumn(['PE'], separator='=')
         call pe_multi%add_column(FormattedTextColumn('max','(1x,i5.5)', 6, ExclusiveColumn('MAX_PE'), separator='-'))
         call pe_multi%add_column(FormattedTextColumn('min','(1x,i5.5)', 6, ExclusiveColumn('MIN_PE'),separator='-'))

         n_cyc_multi = MultiColumn(['# cycles'], separator='=')
         call n_cyc_multi%add_column(FormattedTextColumn('', '(i8.0)', 8, NumCyclesColumn(),separator=' '))

         call reporter%add_column(SeparatorColumn('|'))
         call reporter%add_column(min_multi)
         call reporter%add_column(SeparatorColumn('|'))
         call reporter%add_column(mean_multi)
         call reporter%add_column(SeparatorColumn('|'))
         call reporter%add_column(max_multi)
         call reporter%add_column(SeparatorColumn('|'))
         call reporter%add_column(pe_multi)
         call reporter%add_column(SeparatorColumn('|'))
         call reporter%add_column(n_cyc_multi)

         report = reporter%generate_report(this%profiler)
         call logger%info('')
         call logger%info('Times for component <%a~>', this%user_gc_driver%get_name())
         do index = 1, size(report)
            call logger%info('%a', report(index))
         end do
         call logger%info('')
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine report_generic_profile

end submodule finalize_smod
