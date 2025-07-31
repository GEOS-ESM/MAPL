#include "MAPL.h"
module mapl3g_ExtDataCollection
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_TimeStringConversion
   use MAPL_StringTemplate
   use pfio_FileMetadataMod
   use mapl3g_AbstractDataSetFileSelector
   use mapl3g_NonClimDataSetFileSelector
   implicit none
   private

   type, public :: ExtDataCollection
      character(len=:), allocatable :: file_template
      type(ESMF_TimeInterval) :: frequency
      type(ESMF_Time), allocatable :: reff_time
      integer :: collection_id
      type(ESMF_Time), allocatable :: valid_range(:)
      contains
         procedure :: get_file_template
         procedure :: get_frequency
         procedure :: get_reff_time
         procedure :: get_collection_id
         procedure :: get_valid_range
         procedure :: is_reff_time_allocated
         procedure :: is_valid_range_allocated
   end type

    interface ExtDataCollection
       module procedure new_ExtDataCollection
    end interface ExtDataCollection
contains

   function new_ExtDataCollection(config,current_time, unusable,rc) result(data_set)
      type(ESMF_HConfig), intent(in) :: config
      type(ESMF_Time), intent(in) :: current_time
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ExtDataCollection) :: data_set
      integer :: status
      integer :: last_token
      integer :: iyy,imm,idd,ihh,imn,isc,idx
      character(len=2) :: token
      character(len=:), allocatable :: file_frequency, file_reff_time,range_str
      logical :: is_present

      is_present = ESMF_HConfigIsDefined(config,keyString="template",_RC)
      _ASSERT(is_present,"no file template in the collection")

      data_set%file_template = ESMF_HConfigAsString(config,keyString="template",_RC)
      file_frequency = get_string_with_default(config,"freq")
      file_reff_time = get_string_with_default(config,"ref_time")
      range_str = get_string_with_default(config,"valid_range")

      if (file_frequency /= '') then
         data_set%frequency = string_to_esmf_timeinterval(file_frequency)
      else
         last_token = index(data_set%file_template,'%',back=.true.)
         if (last_token.gt.0) then
            token = data_set%file_template(last_token+1:last_token+2)
            select case(token)
            case("y4")
               call ESMF_TimeIntervalSet(data_set%frequency,yy=1,_RC)
            case("m2")
               call ESMF_TimeIntervalSet(data_set%frequency,mm=1,_RC)
            case("d2")
               call ESMF_TimeIntervalSet(data_set%frequency,d=1,_RC)
            case("h2")
               call ESMF_TimeIntervalSet(data_set%frequency,h=1,_RC)
            case("n2")
               call ESMF_TimeIntervalSet(data_set%frequency,m=1,_RC)
            case default
               _FAIL("Unsupported token")
            end select
         else
            ! couldn't find any tokens so all the data must be on one file
            call ESMF_TimeIntervalSet(data_set%frequency,_RC)
         end if
      end if

      if (file_reff_time /= '') then
         data_set%reff_time = string_to_esmf_time(file_reff_time)
      else
         last_token = index(data_set%file_template,'%',back=.true.)
         if (last_token.gt.0) then
            call ESMF_TimeGet(current_time, yy=iyy, mm=imm, dd=idd,h=ihh, m=imn, s=isc  ,_RC)
            token = data_set%file_template(last_token+1:last_token+2)
            select case(token)
            case("y4")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=1,dd=1,h=0,m=0,s=0,_RC)
            case("m2")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=imm,dd=1,h=0,m=0,s=0,_RC)
            case("d2")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=imm,dd=idd,h=0,m=0,s=0,_RC)
            case("h2")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=0,s=0,_RC)
            case("n2")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=imn,s=0,_RC)
            case default
               _FAIL("Unsupported token")
            end select
         else
            data_set%reff_time = current_time
         end if
      end if

      if (range_str /= '') then
         idx = index(range_str,'/')
         _ASSERT(idx/=0,'invalid specification of time range')
         if (allocated(data_set%valid_range)) deallocate(data_set%valid_range)
         allocate(data_set%valid_range(2))
         data_set%valid_range(1)=string_to_esmf_time(range_str(:idx-1))
         data_set%valid_range(2)=string_to_esmf_time(range_str(idx+1:))

         last_token = index(data_set%file_template,'%',back=.true.)
         if (last_token.gt.0) then
            call ESMF_TimeGet(data_set%valid_range(1), yy=iyy, mm=imm, dd=idd,h=ihh, m=imn, s=isc  ,_RC)
            token = data_set%file_template(last_token+1:last_token+2)
            select case(token)
            case("y4")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=1,dd=1,h=0,m=0,s=0,_RC)
            case("m2")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=imm,dd=1,h=0,m=0,s=0,_RC)
            case("d2")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=imm,dd=idd,h=0,m=0,s=0,_RC)
            case("h2")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=0,s=0,_RC)
            case("n2")
               call ESMF_TimeSet(data_set%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=imn,s=0,_RC)
            case default
               _FAIL("Unsupported token")
            end select
         end if

      end if

      _UNUSED_DUMMY(unusable)
      _RETURN(_SUCCESS)

      contains

         function get_string_with_default(config,selector) result(string)
            type(ESMF_HConfig), intent(in) :: config
            character(len=*), intent(In) :: selector
            character(len=:), allocatable :: string

           string=''
           if (ESMF_HConfigIsDefined(config,keyString=selector)) then
               string = ESMF_HConfigAsString(config,keyString=selector,_RC)
           end if
         end function

   end function new_ExtDataCollection

   ! file_template accessors
   function get_file_template(this) result(template)
      class(ExtDataCollection), intent(in) :: this
      character(len=:), allocatable :: template
      
      template = ''
      if (allocated(this%file_template)) then
         template = this%file_template
      end if
   end function get_file_template

   ! frequency accessors
   function get_frequency(this) result(freq)
      class(ExtDataCollection), intent(in) :: this
      type(ESMF_TimeInterval) :: freq
      
      freq = this%frequency
   end function get_frequency

   ! reff_time accessors
   subroutine get_reff_time(this, time)
      class(ExtDataCollection), intent(in) :: this
      type(ESMF_Time), intent(out), allocatable :: time
      
      if (allocated(this%reff_time)) then
         time = this%reff_time
      end if
   end subroutine get_reff_time

   ! collection_id accessors
   function get_collection_id(this) result(id)
      class(ExtDataCollection), intent(in) :: this
      integer :: id
      
      id = this%collection_id
   end function get_collection_id

   ! valid_range accessors
   subroutine get_valid_range(this, valid_range)
      class(ExtDataCollection), intent(in) :: this
      type(ESMF_Time), intent(out), allocatable :: valid_range(:)
      
      if (allocated(this%valid_range)) then
         valid_range = this%valid_range
      end if
   end subroutine get_valid_range

   ! Check if reff_time is allocated
   function is_reff_time_allocated(this) result(is_allocated)
      class(ExtDataCollection), intent(in) :: this
      logical :: is_allocated
      
      is_allocated = allocated(this%reff_time)
   end function is_reff_time_allocated

   ! Check if valid_range is allocated
   function is_valid_range_allocated(this) result(is_allocated)
      class(ExtDataCollection), intent(in) :: this
      logical :: is_allocated
      
      is_allocated = allocated(this%valid_range)
   end function is_valid_range_allocated

end module mapl3g_ExtDataCollection
