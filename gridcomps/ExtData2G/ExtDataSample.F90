#include "MAPL_ErrLog.h"
module MAPL_ExtDataTimeSample
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_TimeStringConversion
   implicit none
   private

   type, public :: ExtDataTimeSample
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

   interface ExtDataTimeSample
      module procedure new_ExtDataTimeSample
   end interface

contains

   function new_ExtDataTimeSample(config,unusable,rc) result(TimeSample)
      type(ESMF_HConfig), intent(in) :: config
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ExtDataTimeSample) :: TimeSample
      integer :: status
      character(len=:), allocatable :: source_str
      integer :: idx
      __UNUSED_DUMMY(unusable)

      call TimeSample%set_defaults()

      TimeSample%extrap_outside = "none"
      if (ESMF_HConfigIsDefined(config,keyString="extrapolation")) then
         TimeSample%extrap_outside=ESMF_HConfigAsString(config,keyString="extrapolation",__RC)
      end if

      TimeSample%time_interpolation = .true.
      if (ESMF_HConfigIsDefined(config,keyString="time_interpolation")) then
         TimeSample%time_interpolation = ESMF_HConfigAsLogical(config,keyString="time_interpolation",__RC)
      end if

      if (ESMF_HConfigIsDefined(config,keyString="exact")) then
         TimeSample%exact = ESMF_HConfigAsLogical(config,keyString="exact",__RC)
      else
         TimeSample%exact = .false.
      end if

      if (ESMF_HConfigIsDefined(config,keyString="update_reference_time")) then
         TimeSample%refresh_time = ESMF_HConfigAsString(config,keyString="update_reference_time",__RC)
      end if

      if (ESMF_HConfigIsDefined(config,keyString="update_frequency")) then
         TimeSample%refresh_frequency = ESMF_HConfigAsString(config,keyString="update_frequency",__RC)
      end if

      if (ESMF_HConfigIsDefined(config,keyString="update_offset")) then
         TimeSample%refresh_offset = ESMF_HConfigAsString(config,keyString="update_offset",__RC)
      end if

      if (ESMF_HConfigIsDefined(config,keyString="source_time")) then
         source_str = ESMF_HConfigAsString(config,keyString="source_time",__RC)
         if (allocated(TimeSample%source_time)) deallocate(TimeSample%source_time)
         idx = index(source_str,'/')
         __ASSERT(idx/=0,'invalid specification of source_time')
         allocate(TimeSample%source_time(2))
         TimeSample%source_time(1)=string_to_esmf_time(source_str(:idx-1))
         TimeSample%source_time(2)=string_to_esmf_time(source_str(idx+1:))
      else
         if (.not.allocated(TimeSample%source_time)) allocate(TimeSample%source_time(0))
      end if

      __RETURN(__SUCCESS)

   end function new_ExtDataTimeSample


   subroutine set_defaults(this,unusable,rc)
      class(ExtDataTimeSample), intent(inout), target :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      __UNUSED_DUMMY(unusable)
      this%time_interpolation=.true.
      this%extrap_outside='none'
      this%refresh_time="00"
      this%refresh_frequency="PT0S"
      this%refresh_offset="PT0S"
      if (allocated(this%source_time)) then
         deallocate(this%source_time,stat=status)
         __VERIFY(status)
      end if
      allocate(this%source_time(0),stat=status)
      __VERIFY(status)
      __RETURN(__SUCCESS)
   end subroutine set_defaults

end module MAPL_ExtDataTimeSample

module MAPL_ExtDataTimeSampleMap
   use MAPL_ExtDataTimeSample

#include "types/key_deferredLengthString.inc"
#define _value type(ExtDataTimeSample)
#define _alt

#define _map ExtDataTimeSampleMap
#define _iterator ExtDataTimeSampleMapIterator

#include "templates/map.inc"

#undef _iterator
#undef _map

#undef _alt
#undef _value

end module MAPL_ExtDataTimeSampleMap
