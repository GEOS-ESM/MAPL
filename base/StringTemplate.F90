#include "MAPL_Exceptions.h"
#include "unused_dummy.H"
module MAPL_StringTemplate
use ESMF
use MAPL_ExceptionHandling
use MAPL_KeywordEnforcerMod

implicit none
private

public fill_grads_template
public StrTemplate

character(len=2), parameter :: valid_tokens(13) = ["y4","y2","m1","m2","mc","Mc","MC","d1","d2","h1","h2","h3","n2"]
character(len=3),parameter :: mon_lc(12) = [&
   'jan','feb','mar','apr','may','jun',   &
   'jul','aug','sep','oct','nov','dec']

contains
   subroutine StrTemplate(str, tmpl, class, xid, nymd, nhms, stat, preserve)
      character(len=*), intent(out) :: str
      character(len=*), intent(in ) :: tmpl

      character(len=*), optional, intent(in ) :: class
      character(len=*), optional, intent(in ) :: xid
      integer,          optional, intent(in ) :: nymd
      integer,          optional, intent(in ) :: nhms
      integer,          optional, intent(out) :: stat
      logical,          optional, intent(in ) :: preserve

      _UNUSED_DUMMY(class)

      call fill_grads_template(str, tmpl, &
            experiment_id=xid, nymd=nymd, nhms=nhms,rc=stat)

      if (present(preserve)) then
         if (preserve) stat = 1
      end if
   end subroutine StrTemplate

   subroutine fill_grads_template(output_string,template,unusable,experiment_id,nymd,nhms,time,rc)
      character(len=*), intent(out) :: output_string
      character(len=*), intent(in)  :: template
      class(keywordEnforcer), optional, intent(in) :: unusable
      character(len=*), intent(in), optional :: experiment_id
      integer, intent(in), optional :: nymd
      integer, intent(in), optional :: nhms
      type(ESMF_Time), intent(in), optional :: time
      integer, intent(out), optional :: rc

      integer :: year,month,day,hour,minute,second,tmpl_length,output_length
      integer  :: i, m, istp, k, kstp, i1, i2
      character(len=1) :: c0,c1,c2
      character(len=4) :: sbuf
      logical :: valid_token
      integer :: status
     
      _UNUSED_DUMMY(unusable)
      year=-1
      month=-1
      day=-1
      hour=-1
      minute=-1
      second=-1
      if (present(time)) then
         _ASSERT(.not.present(nymd),'can not specify an ESMF_Time and an integer time')
         _ASSERT(.not.present(nhms),'can not specify an ESMF_Time and an integer time')
         call ESMF_TimeGet(time,yy=year,mm=month,dd=day,h=hour,m=minute,s=second,rc=status)
         _VERIFY(status)
      end if
      if (present(nymd)) then
         _ASSERT(.not.present(time),'can not specify an ESMF_Time and an integer time')
         year = nymd/10000
         month = mod(nymd/100,100)
         day = mod(nymd,100)
      end if
      if (present(nhms)) then
         _ASSERT(.not.present(time),'can not specify an ESMF_Time and an integer time')
         hour = nhms/10000
         minute = mod(nhms/100,100)
         second = mod(nhms,100)  
      end if
      output_string = ""

      output_length=len(output_string)
      tmpl_length=len_trim(template)
      i=0
      istp=1
      k=1
      kstp=1
      do while(i+istp <= tmpl_length)
         i=i+istp
         c0 = template(i:i)
         if (c0 == "%") then
            i1=i+1
            c1=""
            if (i1<=tmpl_length) c1=template(i1:i1)
            select case(c1)
            case("s")
               if (present(experiment_id)) then
                  istp=2
                  m=min(k+len_trim(experiment_id)-1,output_length)
                  output_string(k:m)=experiment_id
                  k=m+1
                  cycle
               else
                  _ASSERT(.false.,"Using %s token with no experiment id")
               end if
            case("%")
               istp=2
               output_string(k:k)=c1
               k=k+1
               cycle
            case default
               c2=""
               i2=i1+1
               if(i2 <= tmpl_length) c2=template(i2:i2)
               valid_token = check_token(c1//c2) 
               if (valid_token) then
                  istp=3
                  _ASSERT(present(nymd) .or. present(nhms) .or. present(time),'Using token with no time')
                  sbuf = evaluate_token(c1//c2,year,month,day,hour,minute)
                  kstp = len_trim(sbuf)
                  m=k+kstp-1
                  output_string(k:m)=sbuf
                  k=m+1
               else
                  _ASSERT(.false.,"Invalid token in file template: "//c1//c2)
               end if
            end select
         else
            istp=1
            output_string(k:k)=template(i:i)
            k=k+1
         end if
      enddo

   end subroutine fill_grads_template

   function check_token(token) result(is_valid)
      character(len=2), intent(in) :: token
      logical :: is_valid
      integer :: i 
      is_valid = .false.
      do i=1,size(valid_tokens)
         if (token==valid_tokens(i)) then
            is_valid=.true.
            exit
         end if
      enddo
   end function check_token

   function evaluate_token(token,year,month,day,hour,minute) result(buffer)
      character(len=2), intent(in) :: token
      integer, intent(in) :: year,month,day,hour,minute
      character(len=4) :: buffer
      character(len=1) :: c1,c2
      c1=token(1:1)
      c2=token(2:2)
      select case(c1)
      case("y")
         if (c2 == "2" ) then
            write(buffer,'(i2.2)')mod(year,100)
         else if (c2 == "4" ) then
            write(buffer,'(i4.4)')year
         end if
      case("m")
         if (c2 == "c") then
            buffer = mon_lc(month)
         else if (c2 == "1") then
            if (day < 10) then
               write(buffer,'(i1)')month
            else
               write(buffer,'(i2)')month
            end if
         else if (c2 == "2") then
            write(buffer,'(i2.2)')month 
         end if
      case("d")
         if (c2 == "2") then
            write(buffer,'(i2.2)')day
         else if (c1 == "1") then
            if (day < 10) then
               write(buffer,'(i1)')day
            else
               write(buffer,'(i2)')day
            end if
         end if
      case("h")
         if (c2 == "3") then
            write(buffer,'(i3.3)')hour
         else if (c2 == "2") then
            write(buffer,'(i2.2)')hour
         else if (c1 == "1") then
            if (day < 10) then
               write(buffer,'(i1)')hour
            else
               write(buffer,'(i2)')hour
            end if
         end if
      case("n")
         write(buffer,'(i2.2)')minute
      end select

   end function evaluate_token
end module MAPL_StringTemplate
