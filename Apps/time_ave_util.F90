#define I_AM_MAIN
#include "MAPL_Generic.h"

module obs_platform
  type platform
     character (len=ESMF_MAXSTR) :: nc_lon=''
     character (len=ESMF_MAXSTR) :: nc_lat=''
     character (len=ESMF_MAXSTR) :: nc_time=''
     character (len=ESMF_MAXSTR) :: file_name_template=''
     integer :: ngeoval=0
     character (len=ESMF_MAXSTR), allocatable :: field_name(:,:)
  end type platform

  function union_for_field_name(a, b, rc)
    type(platform) :: add_platform
    type(platform), intent(in) :: a
    type(platform), intent(in) :: b
    integer, optional, intent(out) :: rc

    character (len=ESMF_MAXSTR), allocatable :: field_name_loc(:,:)
    integer :: nfield
    integer, allocatable :: tag(:)
    
    union_for_field_name = copy_platform_nckeys(a, _RC)
    nfield = a%ngeoval + b%ngeoval
    allocate (tag(b%ngeoval))

    tag(:)=1    ! true
    k=nfield
    do j=1, b%ngeoval
       do i=1, a%ngeoval
          if ( trim(b%field_name(1,j)) == trim(a%field_name(1,i)) ) then
             tag(j)=0
          endif
       enddo
       if (tag(j)==0) k=k-1
    enddo
    nfield=k
    allocate(union_for_field_name%field_name(4, nfield))
    do i=1, a%ngeoval
       union_for_field_name%field_name(:,i) = a%field_name(:,i)
    enddo
    if (nfield>a%geoval) then
       k = a%geoval
       do j=1, b%ngeoval
          if (tag(j)=1) then
             k = k + 1
             union_for_field_name%field_name(:,k) = b%field_name(:,j)
          end if
       enddo
    end if
    _RETURN(_SUCCESS)

  end function add_platform_for_field_name


  function copy_platform_nckeys(a, rc)
    type(platform) :: copy_platform_nckeys
    type(platform), intent(in) :: a
    integer, optional, intent(out) :: rc
    
    copy_platform_nckeys%nc_lon = a%nc_lon
    copy_platform_nckeys%nc_lat = a%nc_lat
    copy_platform_nckeys%nc_time = a%nc_time
    _RETURN(_SUCCESS)

  end function add_platform
  
    
end module obs_platform

program  read_OBS_PLATFORM

   use ESMF
   use MAPL
   use MPI
   use, intrinsic :: iso_fortran_env, only: int32, int64, int16, real32, real64
   use ieee_arithmetic, only: isnan => ieee_is_nan

   implicit none

   integer  comm,myid,npes,ierror
   integer  imglobal
   integer  jmglobal
   logical  root

   type(ESMF_Config), intent(in)          :: config
    integer, intent(in)                    :: nlist
    character(len=ESMF_MAXSTR), intent(in) :: collections(nlist)
    integer, intent(inout), optional :: rc

    integer n, unitr, unitw
    logical :: match, contLine, con3, count


    
    ! -- note: work on HEAD node
    !
    call ESMF_ConfigGetAttribute(config, value=HIST_CF, &
         label="HIST_CF:", default="HIST.rc", _RC )
    unitr = GETFILE(HIST_CF, FORM='formatted', _RC)


 end program read_OBS_PLATFORM
