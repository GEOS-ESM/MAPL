#include "MAPL.h"
module mapl_ExtDataSample_mod
   use ESMF
   use MAPL
   implicit none
   private

   type, public :: ExtDataSample
      logical :: time_interpolation
      logical :: exact
      type(ESMF_Time), allocatable :: source_time(:)
      character(:), allocatable :: extrap_outside
      character(:), allocatable :: refresh_time
      character(:), allocatable :: refresh_frequency
      character(:), allocatable :: refresh_offset
      contains
         procedure :: set_defaults
   end type

   interface ExtDataSample
      module procedure new_ExtDataSample
   end interface

contains

   function new_ExtDataSample(config,unusable,rc) result(TimeSample)
      type(ESMF_HConfig), intent(in) :: config
      class(mapl_KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ExtDataSample) :: TimeSample
      integer :: status
      _UNUSED_DUMMY(unusable)

      call TimeSample%set_defaults()

      TimeSample%extrap_outside = "none"
      if (ESMF_HConfigIsDefined(config,keyString="extrapolation")) then
         TimeSample%extrap_outside=ESMF_HConfigAsString(config,keyString="extrapolation",_RC)
      end if

      TimeSample%time_interpolation = .true.
      if (ESMF_HConfigIsDefined(config,keyString="time_interpolation")) then
         TimeSample%time_interpolation = ESMF_HConfigAsLogical(config,keyString="time_interpolation",_RC)
      end if

      if (ESMF_HConfigIsDefined(config,keyString="exact")) then
         TimeSample%exact = ESMF_HConfigAsLogical(config,keyString="exact",_RC)
      else
         TimeSample%exact = .false.
      end if

      if (ESMF_HConfigIsDefined(config,keyString="update_reference_time")) then
         TimeSample%refresh_time = ESMF_HConfigAsString(config,keyString="update_reference_time",_RC)
      end if

      if (ESMF_HConfigIsDefined(config,keyString="update_frequency")) then
         TimeSample%refresh_frequency = ESMF_HConfigAsString(config,keyString="update_frequency",_RC)
      end if

      if (ESMF_HConfigIsDefined(config,keyString="update_offset")) then
         TimeSample%refresh_offset = ESMF_HConfigAsString(config,keyString="update_offset",_RC)
      end if

      if (ESMF_HConfigIsDefined(config,keyString="source_time")) then
         if (allocated(TimeSample%source_time)) deallocate(TimeSample%source_time)
         TimeSample%source_time = mapl_HConfigAsTimeRange(config, keyString="source_time", _RC)
      else
         if (.not.allocated(TimeSample%source_time)) allocate(TimeSample%source_time(0))
      end if

      _RETURN(_SUCCESS)

   end function new_ExtDataSample


   subroutine set_defaults(this,unusable,rc)
      class(ExtDataSample), intent(inout), target :: this
      class(mapl_KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      _UNUSED_DUMMY(unusable)
      this%time_interpolation=.true.
      this%extrap_outside='none'
      this%refresh_time="00"
      this%refresh_frequency="PT0S"
      this%refresh_offset="PT0S"
      if (allocated(this%source_time)) then
         deallocate(this%source_time,stat=status)
         _VERIFY(status)
      end if
      allocate(this%source_time(0),stat=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
   end subroutine set_defaults

end module mapl_ExtDataSample_mod
