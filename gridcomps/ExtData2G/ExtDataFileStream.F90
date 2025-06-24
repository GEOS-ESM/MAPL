#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_ExtDataFileStream
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_TimeStringConversion
   use MAPL_DataCollectionMod
   use MAPL_CollectionVectorMod
   use MAPL_DataCollectionManagerMod
   use MAPL_FileMetadataUtilsMod
   use MAPL_StringTemplate
   implicit none
   private

   type, public :: ExtDataFileStream
      character(len=:), allocatable :: file_template
      type(ESMF_TimeInterval) :: frequency
      type(ESMF_Time) :: reff_time
      integer :: collection_id
      type(ESMF_Time), allocatable :: valid_range(:)
      type(FileMetaData) :: metadata
      contains
         procedure :: detect_metadata
   end type

    interface ExtDataFileStream
       module procedure new_ExtDataFileStream
    end interface ExtDataFileStream
contains

   function new_ExtDataFileStream(config,current_time,unusable,rc) result(data_set)
      type(ESMF_HConfig), intent(in) :: config
      type(ESMF_Time), intent(in) :: current_time
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ExtDataFileStream) :: data_set
      integer :: status
      integer :: last_token
      integer :: iyy,imm,idd,ihh,imn,isc,idx
      character(len=2) :: token
      character(len=:), allocatable :: file_frequency, file_reff_time,range_str
      logical :: is_present

      __UNUSED_DUMMY(unusable)


      is_present = ESMF_HConfigIsDefined(config,keyString="template",__RC)
      __ASSERT(is_present,"no file template in the collection")

      if (is_present) then
         data_set%file_template = ESMF_HConfigAsString(config,keyString="template",__RC)
         file_frequency = get_string_with_default(config,"freq")
         file_reff_time = get_string_with_default(config,"ref_time")
         range_str = get_string_with_default(config,"valid_range")
      end if

      if (file_frequency /= '') then
         data_set%frequency = string_to_esmf_timeinterval(file_frequency)
      else
         last_token = index(data_set%file_template,'%',back=.true.)
         if (last_token.gt.0) then
            token = data_set%file_template(last_token+1:last_token+2)
            select case(token)
            case("y4")
               call ESMF_TimeIntervalSet(data_set%frequency,yy=1,__RC)
            case("m2")
               call ESMF_TimeIntervalSet(data_set%frequency,mm=1,__RC)
            case("d2")
               call ESMF_TimeIntervalSet(data_set%frequency,d=1,__RC)
            case("h2")
               call ESMF_TimeIntervalSet(data_set%frequency,h=1,__RC)
            case("n2")
               call ESMF_TimeIntervalSet(data_set%frequency,m=1,__RC)
            end select
         else
            ! couldn't find any tokens so all the data must be on one file
            call ESMF_TimeIntervalSet(data_set%frequency,__RC)
         end if
      end if

      if (file_reff_time /= '') then
         data_set%reff_time = string_to_esmf_time(file_reff_time)
      else
         last_token = index(data_set%file_template,'%',back=.true.)
         if (last_token.gt.0) then
            call ESMF_TimeGet(current_time, yy=iyy, mm=imm, dd=idd,h=ihh, m=imn, s=isc  ,__RC)
            token = data_set%file_template(last_token+1:last_token+2)
            select case(token)
            case("y4")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=1,dd=1,h=0,m=0,s=0,__RC)
            case("m2")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=imm,dd=1,h=0,m=0,s=0,__RC)
            case("d2")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=imm,dd=idd,h=0,m=0,s=0,__RC)
            case("h2")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=0,s=0,__RC)
            case("n2")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=imn,s=0,__RC)
            end select
         else
            data_set%reff_time = current_time
         end if
      end if

      if (range_str /= '') then
         idx = index(range_str,'/')
         __ASSERT(idx/=0,'invalid specification of time range')
         if (allocated(data_set%valid_range)) deallocate(data_set%valid_range)
         allocate(data_set%valid_range(2))
         data_set%valid_range(1)=string_to_esmf_time(range_str(:idx-1))
         data_set%valid_range(2)=string_to_esmf_time(range_str(idx+1:))

         last_token = index(data_set%file_template,'%',back=.true.)
         if (last_token.gt.0) then
            call ESMF_TimeGet(data_set%valid_range(1), yy=iyy, mm=imm, dd=idd,h=ihh, m=imn, s=isc  ,__RC)
            token = data_set%file_template(last_token+1:last_token+2)
            select case(token)
            case("y4")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=1,dd=1,h=0,m=0,s=0,__RC)
            case("m2")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=imm,dd=1,h=0,m=0,s=0,__RC)
            case("d2")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=imm,dd=idd,h=0,m=0,s=0,__RC)
            case("h2")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=0,s=0,__RC)
            case("n2")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=imn,s=0,__RC)
            end select
         end if

      end if
      data_set%collection_id = MAPL_DataAddCollection(data_set%file_template)

      __RETURN(__SUCCESS)

      contains

         function get_string_with_default(config,selector) result(string)
            type(ESMF_HConfig), intent(in) :: config
            character(len=*), intent(In) :: selector
            character(len=:), allocatable :: string

            if (ESMF_HConfigIsDefined(config,keyString=selector)) then
               string = ESMF_HConfigAsString(config,keyString=selector,__RC)
            else
               string=''
            end if
         end function

   end function new_ExtDataFileStream

   subroutine detect_metadata(this,metadata_out,time,multi_rule,get_range,rc)
      class(ExtDataFileStream), intent(inout) :: this
      type(FileMetadataUtils), intent(inout) :: metadata_out
      type(ESMF_Time),          intent(in)  :: time
      logical, intent(in)  :: multi_rule
      logical, optional, intent(in)  :: get_range
      integer, optional, intent(out) :: rc

      logical :: get_range_
      type(MAPLDataCollection), pointer :: collection
      type(FileMetadataUtils), pointer :: metadata
      type(ESMF_Time), allocatable :: time_series(:)
      integer :: status

      if (multi_rule) then
         __ASSERT(allocated(this%valid_range),"must use a collection with valid range")
      end if

      if (present(get_range)) then
         get_range_ = get_range
      else
         get_range_ = .false.
      end if

      collection => DataCollections%at(this%collection_id)
      if (get_range_ .and. (.not.allocated(this%valid_range))) then
         if (index('%',this%file_template) == 0) then
            metadata => collection%find(this%file_template)
            call metadata%get_time_info(timeVector=time_series,__RC)
            allocate(this%valid_range(2))
            this%valid_range(1)=time_series(1)
            this%valid_range(2)=time_series(size(time_series))
         end if
      end if

      __RETURN(__SUCCESS)

      __UNUSED_DUMMY(metadata_out)
      __UNUSED_DUMMY(time)

   end subroutine detect_metadata

end module MAPL_ExtDataFileStream

module MAPL_ExtDataFileStreamMap
   use MAPL_ExtDataFileStream

#include "types/key_deferredLengthString.inc"
#define _value type(ExtDataFileStream)
#define _alt

#define _map ExtDataFileStreamMap
#define _iterator ExtDataFileStreamMapIterator

#include "templates/map.inc"

#undef _iterator
#undef _map

#undef _alt
#undef _value

end module MAPL_ExtDataFileStreamMap
