#include "MAPL_ErrLog.h"

module MAPL_FileMetadataUtilsMod
   use pFIO
   use MAPL_GridManagerMod
   use MAPL_AbstractGridFactoryMod
   use ESMF
   use MAPL_ExceptionHandling
   use, intrinsic :: iso_fortran_env, only: REAL64,REAL32,INT64,INT32
   
   public :: FileMetadataUtils
   type, extends(Filemetadata) :: FileMetadataUtils

   private
      character(len=:), allocatable :: filename 
   contains
      procedure :: create
      procedure :: get_coordinate_info
      procedure :: get_variable_attribute
      procedure :: get_time_info
      procedure :: get_level_name
      procedure :: is_var_present
      procedure :: get_file_name
   end type FileMetadataUtils

   interface FileMetadataUtils
      module procedure new_FilemetadataUtils
   end interface

   contains

   function new_FilemetadataUtils(metadata,fName) result(metadata_utils)
      type (FileMetadataUtils) :: metadata_utils
      type (FileMetadata), intent(in) :: metadata
      character(len=*), intent(in) :: fName
      metadata_utils%Filemetadata = metadata
      metadata_utils%filename = fName
      
   end function new_FilemetadataUtils

   subroutine create(this,metadata,fname)
      class(FileMetadataUtils), intent(inout) :: this
      type (FileMetadata), intent(in) :: metadata
      character(len=*), intent(in) :: fName
      this%Filemetadata = metadata
      this%filename = fName
   end subroutine create


   subroutine get_time_info(this,startTime,startyear,startmonth,startday,starthour,startmin,startsec,units,timeVector,rc)
      class (FileMetadataUtils), intent(inout) :: this
      type(ESMF_Time), optional, intent(inout) :: startTime
      integer,optional,intent(out) ::        startYear 
      integer,optional,intent(out) ::        startMonth
      integer,optional,intent(out) ::        startDay 
      integer,optional,intent(out) ::        startHour
      integer,optional,intent(out) ::        startMin 
      integer,optional,intent(out) ::        startSec
      type(ESMF_Time), allocatable, optional :: timeVector(:)
      type(ESMF_Time), allocatable :: tVec(:)
      character(len=*), optional, intent(out) :: units
      integer, optional, intent(out) :: rc

      integer :: status
      class(CoordinateVariable), pointer :: var
      type(Attribute), pointer :: attr
      class(*), pointer :: pTimeUnits
      character(len=ESMF_MAXSTR) :: timeUnits,tUnits
      integer :: i,tsize

      integer ypos(2), mpos(2), dpos(2), hpos(2), spos(2)
      integer strlen
      integer firstdash, lastdash
      integer firstcolon, lastcolon
      integer lastspace,since_pos
      integer year,month,day,hour,min,sec
      type(ESMF_Time) :: unmodStartTime
      class(*), pointer :: ptr(:)
      real(REAL64), allocatable :: tr_r64(:)
      type(ESMF_TimeInterval) :: tint

      var => this%get_coordinate_variable('time',rc=status)
      _VERIFY(status)
      attr => var%get_attribute('units')
      ptimeUnits => attr%get_value()
      select type(pTimeUnits)
      type is (character(*))
         timeUnits = pTimeUnits
         strlen = LEN_TRIM (TimeUnits)

         since_pos = index(TimeUnits, 'since')
         tUnits = trim(TimeUnits(:since_pos-1))
         if (present(units)) units = trim(tUnits)

         firstdash = index(TimeUnits, '-')
         lastdash  = index(TimeUnits, '-', BACK=.TRUE.)

         if (firstdash .LE. 0 .OR. lastdash .LE. 0) then
           rc = -1
           return
         endif

         ypos(2) = firstdash - 1
         mpos(1) = firstdash + 1
         ypos(1) = ypos(2) - 3

         mpos(2) = lastdash - 1
         dpos(1) = lastdash + 1
         dpos(2) = dpos(1) + 1

         read ( TimeUnits(ypos(1):ypos(2)), * ) year
         read ( TimeUnits(mpos(1):mpos(2)), * ) month
         read ( TimeUnits(dpos(1):dpos(2)), * ) day

         firstcolon = index(TimeUnits, ':')
         if (firstcolon .LE. 0) then

           ! If no colons, check for hour.

           ! Logic below assumes a null character or something else is after the hour
           ! if we do not find a null character add one so that it correctly parses time
           if (TimeUnits(strlen:strlen) /= char(0)) then
              TimeUnits = trim(TimeUnits)//char(0)
              strlen=len_trim(TimeUnits)
           endif
           lastspace = index(TRIM(TimeUnits), ' ', BACK=.TRUE.)
           if ((strlen-lastspace).eq.2 .or. (strlen-lastspace).eq.3) then
             hpos(1) = lastspace+1
             hpos(2) = strlen-1
             read (TimeUnits(hpos(1):hpos(2)), * ) hour
             min  = 0
             sec  = 0
           else
             hour = 0
             min  = 0
             sec  = 0
           endif

         else
           hpos(1) = firstcolon - 2
           hpos(2) = firstcolon - 1
           lastcolon =  index(TimeUnits, ':', BACK=.TRUE.)
           if ( lastcolon .EQ. firstcolon ) then
             mpos(1) = firstcolon + 1
             mpos(2) = firstcolon + 2
             read (TimeUnits(hpos(1):hpos(2)), * ) hour
             read (TimeUnits(mpos(1):mpos(2)), * ) min
             sec = 0
           else
             mpos(1) = firstcolon + 1
             mpos(2) = lastcolon - 1
             spos(1) = lastcolon + 1
             spos(2) = lastcolon + 2
             read (TimeUnits(hpos(1):hpos(2)), * ) hour
             read (TimeUnits(mpos(1):mpos(2)), * ) min
             read (TimeUnits(spos(1):spos(2)), * ) sec
           endif
         endif
      class default
         _ASSERT(.false.,"Time unit must be character")
      end select
      call ESMF_TimeSet(unmodStartTime,yy=year,mm=month,dd=day,h=hour,m=min,s=sec,rc=status)
      _VERIFY(status)

      call this%get_coordinate_info('time',coordSize=tsize,rc=status)
      _VERIFY(status)
      allocate(tr_r64(tsize))
      allocate(tvec(tsize))
      ptr => var%get_coordinate_data()
      _ASSERT(associated(ptr),"time variable coordinate data not found")
      select type (ptr)
      type is (real(kind=REAL64))
         tr_r64=ptr
      type is (real(kind=REAL32))
         tr_r64=ptr
      type is (integer(kind=INT64))
         tr_r64=ptr
      type is (integer(kind=INT32))
         tr_r64=ptr
      class default
         _ASSERT(.false.,"unsupported time variable type")
      end select
      do i=1,tsize
        select case (trim(tUnits))
        case ("days")
           call ESMF_TimeIntervalSet(tint,d_r8=tr_r64(i),rc=status)
           _VERIFY(status)
           tvec(i)=unmodStartTime+tint
        case ("hours")
           call ESMF_TimeIntervalSet(tint,h_r8=tr_r64(i),rc=status)
           _VERIFY(status)
           tvec(i)=unmodStartTime+tint
        case ("minutes")
           call ESMF_TimeIntervalSet(tint,m_r8=tr_r64(i),rc=status)
           _VERIFY(status)
           tvec(i)=unmodStartTime+tint
        case ("seconds")
           call ESMF_TimeIntervalSet(tint,s_r8=tr_r64(i),rc=status)
           _VERIFY(status)
           tvec(i)=unmodStartTime+tint
        case default
           _ASSERT(.false.,"unsupported time unit")
        end select
      enddo

      call ESMF_TimeGet(tVec(1),yy=year,mm=month,dd=day,h=hour,m=min,s=sec,rc=status)
      _VERIFY(status)
      if (present(startYear)) startYear=year
      if (present(startMonth)) startMonth=month
      if (present(startDay)) startDay=day
      if (present(startHour)) startHour=hour
      if (present(startmin)) startMin=min
      if (present(startsec)) startSec=sec
      if (present(startTime)) then
          startTime=tVec(1)
      end if
      if (present(timeVector)) then
         allocate(timeVector,source=tVec,stat=status)
         _VERIFY(status)
      end if

   end subroutine get_time_info
 
   function is_var_present(this,var_name,rc) result(isPresent)
      class (FileMetadataUtils), intent(inout) :: this
      character(len=*), intent(in) :: var_name
      integer, optional, intent(out) :: rc

      logical :: isPresent
      class(Variable), pointer :: var
      _UNUSED_DUMMY(rc)

      var => this%get_variable(var_name)
      isPresent = associated(var)

   end function is_var_present

   function get_variable_attribute(this,var_name,attr_name,rc) result(units)
      class (FileMetadataUtils), intent(inout) :: this
      character(len=*), intent(in) :: var_name
      character(len=*), intent(in) :: attr_name
      integer, optional, intent(out) :: rc

      character(len=:), pointer :: units 
      type(Attribute), pointer :: attr => null()
      class(Variable), pointer :: var => null()
      class(*), pointer :: vunits
      logical :: isPresent
      integer :: status
    
      var => this%get_variable(var_name,rc=status)
      _VERIFY(status)
      isPresent = var%is_attribute_present(trim(attr_name))
      if (isPresent) then
         attr => var%get_attribute(trim(attr_name))
         vunits => attr%get_value()
         select type(vunits)
         type is (character(*))
            units => vunits
         class default
            _ASSERT(.false.,'units must be string')
         end select
      else
         units => null()
      end if
      _RETURN(_SUCCESS)

   end function get_variable_attribute

   subroutine get_coordinate_info(this,coordinate_name,coordSize,coordUnits,coords,rc)
      class (FileMetadataUtils), intent(inout) :: this
      character(len=*), intent(in) :: coordinate_name
      integer, optional, intent(out) :: coordSize
      character(len=*), optional, intent(out) :: coordUnits
      real, allocatable, optional,  intent(inout) :: coords(:)
      integer, optional, intent(out) :: rc

      integer :: status
      class(CoordinateVariable), pointer :: var
      type(Attribute), pointer :: attr
      character(len=:), pointer :: vdim
      class(*), pointer :: coordUnitPtr
      class(*), pointer :: ptr(:)
 
      var => this%get_coordinate_variable(trim(coordinate_name),rc=status)
      _VERIFY(status)
   
      if (present(coordSize)) then
         vdim => var%get_ith_dimension(1)
         coordSize = this%get_dimension(vdim,rc=status)
      end if

      if (present(coordUnits)) then
         attr => var%get_attribute('units')
         coordUnitPtr => attr%get_value()
         select type(coordUnitPtr)
         type is (character(*))
            coordUnits = trim(coordUnitPtr)
         class default
            _ASSERT(.false.,'units must be string')
         end select
      end if 

      if (present(coords)) then
         ptr => var%get_coordinate_data()
         _ASSERT(associated(ptr),"coord variable coordinate data not found")
         select type (ptr)
         type is (real(kind=REAL64))
            coords=ptr
         type is (real(kind=REAL32))
            coords=ptr
         type is (integer(kind=INT64))
            coords=ptr
         type is (integer(kind=INT32))
            coords=ptr
         class default
            _ASSERT(.false.,"unsupported coordel variable type")
         end select
      end if

   end subroutine get_coordinate_info

   function get_level_name(this,rc) result(lev_name)
      class (FileMetadataUtils), intent(inout) :: this
      integer, optional, intent(out) :: rc

      character(len=:), pointer :: units
      character(len=:), allocatable :: lev_name
      type(CoordinateVariable), pointer :: var
      type (StringVariableMap), pointer :: vars
      type (StringVariableMapIterator) :: var_iter
      character(len=:), pointer :: var_name
      
      vars => this%get_variables()
      var_iter = vars%begin()
      do while(var_iter /=vars%end())
         var_name => var_iter%key()
         var => this%get_coordinate_variable(trim(var_name))
         if (associated(var)) then
            if (index(var_name,'lev') .ne. 0 .or. index(var_name,'height') .ne. 0) then
               lev_name=var_name
               _RETURN(_SUCCESS)
            else
               if (var%is_attribute_present('units')) then
               units => this%get_variable_attribute(var_name,'units')
                  if (trim(units) .eq. 'hPa' .or. trim(units) .eq. 'sigma_level' .or. &
                      trim(units) .eq. 'mb'  .or. trim(units) .eq. 'millibar') then
                     lev_name=var_name
                     _RETURN(_SUCCESS)
                  end if
               end if
            end if
         end if
         call var_iter%next()
      enddo
      lev_name=''
      _RETURN(_SUCCESS)

   end function get_level_name

   function get_file_name(this,rc) result(fname)
      class (FileMetadataUtils), intent(inout) :: this
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: fname

      fname = this%fileName

      _RETURN(_SUCCESS)
   end function get_file_name

end module MAPL_FileMetadataUtilsMod


      
 
