#include "MAPL_ErrLog.h"

module MAPL_FileMetadataUtilsMod
   use pFIO
   use MAPL_GridManagerMod
   use MAPL_AbstractGridFactoryMod
   use Mapl_keywordenforcermod
   use gFTL2_StringIntegerMap
   use ESMF
   use MAPL_ExceptionHandling
   use, intrinsic :: iso_fortran_env, only: REAL64,REAL32,INT64,INT32
   implicit none

   private

   public :: FileMetadataUtils
   type :: FileMetadataUtils
      private
      type(FileMetadata), public :: metadata
      character(len=:), allocatable :: filename
   contains
      procedure :: create
      procedure :: get_coordinate_info
      procedure :: get_variable_attribute
      procedure :: get_time_info
      procedure :: get_level_name
      procedure :: is_var_present
      procedure :: get_file_name
      procedure :: var_get_missing_value
      procedure :: var_has_missing_value
      procedure :: var_has_attr
      procedure :: get_var_attr_real32
      procedure :: get_var_attr_real64
      procedure :: get_var_attr_int32
      procedure :: get_var_attr_int64
      procedure :: get_var_attr_string

      procedure :: get_variable
      procedure :: has_variable
      procedure :: get_coordinate_variable
      procedure :: get_variables
      procedure :: get_dimension
      procedure :: get_dimensions

      procedure :: get_source_file

      procedure :: write_formatted
      generic :: write(formatted) => write_formatted

   end type FileMetadataUtils

   interface FileMetadataUtils
      module procedure new_FilemetadataUtils
   end interface

   contains

   function new_FilemetadataUtils(metadata,fName) result(metadata_utils)
      type (FileMetadataUtils) :: metadata_utils
      type (FileMetadata), intent(in) :: metadata
      character(len=*), intent(in) :: fName
      metadata_utils%metadata = metadata
      metadata_utils%filename = fName

   end function new_FilemetadataUtils

   subroutine create(this,metadata,fname)
      class(FileMetadataUtils), intent(inout) :: this
      type (FileMetadata), intent(in) :: metadata
      character(len=*), intent(in) :: fName
      this%metadata = metadata
      this%filename = fName
   end subroutine create

   function var_get_missing_value(this,var_name,rc) result(missing_value)
      real(REAL32) :: missing_value
      class(FileMetadataUtils), intent(inout) :: this
      character(len=*), intent(in) :: var_name
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: fname
      type(Variable), pointer :: var

      fname = this%get_file_name(_RC)
      var => this%get_variable(var_name,_RC)
      _ASSERT(associated(var),"no variable named "//var_name//" in "//fname)
      ! check _FillValue, we could do more, not sure what to do here like also check for missing_value ...
      if (this%var_has_attr(var_name,"_FillValue")) then
         missing_value = this%get_var_attr_real32(var_name,"_FillValue",_RC)
      end if

      _RETURN(_SUCCESS)
   end function var_get_missing_value

   logical function var_has_missing_value(this,var_name,rc)
      class(FileMetadataUtils), intent(inout) :: this
      character(len=*), intent(in) :: var_name
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: fname
      type(Variable), pointer :: var

      fname = this%get_file_name(_RC)
      var => this%get_variable(var_name,_RC)
      _ASSERT(associated(var),"no variable named "//var_name//" in "//fname)
      var_has_missing_value = var%is_attribute_present("_FillValue")

      _RETURN(_SUCCESS)
   end function var_has_missing_value

   logical function var_has_attr(this,var_name,attr_name,rc)
      class(FileMetadataUtils), intent(inout) :: this
      character(len=*), intent(in) :: var_name
      character(len=*), intent(in) :: attr_name
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: fname
      type(Variable), pointer :: var

      fname = this%get_file_name(_RC)
      var => this%get_variable(var_name,_RC)
      _ASSERT(associated(var),"no variable named "//var_name//" in "//fname)
      var_has_attr = var%is_attribute_present(attr_name)
      _RETURN(_SUCCESS)
   end function var_has_attr

   function get_var_attr_real32(this,var_name,attr_name,rc) result(attr_real32)
      real(REAL32) :: attr_real32
      class(FileMetadataUtils), intent(inout) :: this
      character(len=*), intent(in) :: var_name
      character(len=*), intent(in) :: attr_name
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: fname
      type(Variable), pointer :: var

      fname = this%get_file_name(_RC)
      var => this%get_variable(var_name,_RC)
      _ASSERT(associated(var),"no variable named "//var_name//" in "//fname)
      attr_real32 = var%get_attribute_real32(attr_name, rc=status)
      _ASSERT(status == _SUCCESS, 'failed to get attribute named '//attr_name//' in '//var_name//' in '//fname)

      _RETURN(_SUCCESS)
   end function get_var_attr_real32

   function get_var_attr_real64(this,var_name,attr_name,rc) result(attr_real64)
      real(REAL64) :: attr_real64
      class(FileMetadataUtils), intent(inout) :: this
      character(len=*), intent(in) :: var_name
      character(len=*), intent(in) :: attr_name
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: fname
      type(Variable), pointer :: var

      fname = this%get_file_name(_RC)
      var => this%get_variable(var_name,_RC)
      _ASSERT(associated(var),"no variable named "//var_name//" in "//fname)
      attr_real64 = var%get_attribute_real64(attr_name, rc=status)
      _ASSERT(status == _SUCCESS, 'failed to get attribute named '//attr_name//' in '//var_name//' in '//fname)
      _RETURN(_SUCCESS)

   end function get_var_attr_real64

   function get_var_attr_int32(this,var_name,attr_name,rc) result(attr_int32)
      integer(INT32) :: attr_int32
      class(FileMetadataUtils), intent(inout) :: this
      character(len=*), intent(in) :: var_name
      character(len=*), intent(in) :: attr_name
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: fname
      type(Variable), pointer :: var

      fname = this%get_file_name(_RC)
      var => this%get_variable(var_name,_RC)
      _ASSERT(associated(var),"no variable named "//var_name//" in "//fname)
      attr_int32 = var%get_attribute_int32(attr_name, rc=status)
      _ASSERT(status == _SUCCESS, 'failed to get attribute named '//attr_name//' in '//var_name//' in '//fname)

      _RETURN(_SUCCESS)
   end function get_var_attr_int32

   function get_var_attr_int64(this,var_name,attr_name,rc) result(attr_int64)
      integer(INT64) :: attr_int64
      class(FileMetadataUtils), intent(inout) :: this
      character(len=*), intent(in) :: var_name
      character(len=*), intent(in) :: attr_name
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: fname
      type(Variable), pointer :: var

      fname = this%get_file_name(_RC)
      var => this%get_variable(var_name,_RC)
      _ASSERT(associated(var),"no variable named "//var_name//" in "//fname)
      attr_int64 = var%get_attribute_int64(attr_name, rc=status)
      _ASSERT(status == _SUCCESS, 'failed to get attribute named '//attr_name//' in '//var_name//' in '//fname)

      _RETURN(_SUCCESS)
   end function get_var_attr_int64

   function get_var_attr_string(this,var_name,attr_name,rc) result(attr_string)
      character(len=:), allocatable :: attr_string
      class(FileMetadataUtils), intent(inout) :: this
      character(len=*), intent(in) :: var_name
      character(len=*), intent(in) :: attr_name
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: fname
      type(Variable), pointer :: var

      fname = this%get_file_name(_RC)
      var => this%get_variable(var_name,_RC)
      _ASSERT(associated(var),"no variable named "//var_name//" in "//fname)
      attr_string = var%get_attribute_string(attr_name, rc=status)
      _ASSERT(status == _SUCCESS, 'failed to get attribute named '//attr_name//' in '//var_name//' in '//fname)

      _RETURN(_SUCCESS)
   end function get_var_attr_string

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
      character(:), allocatable :: fname
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

      fname = this%get_file_name(_RC)
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
         _FAIL("Time unit must be character in "//fname)
      end select
      call ESMF_TimeSet(unmodStartTime,yy=year,mm=month,dd=day,h=hour,m=min,s=sec,rc=status)
      _VERIFY(status)

      call this%get_coordinate_info('time',coordSize=tsize,rc=status)
      _VERIFY(status)
      allocate(tr_r64(tsize))
      allocate(tvec(tsize))
      ptr => var%get_coordinate_data()
      _ASSERT(associated(ptr),"time variable coordinate data not found in "//fname)
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
         _FAIL("unsupported time variable type in "//fname)
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
           _FAIL("unsupported time unit in "//fname)
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
      _RETURN(_SUCCESS)

   end subroutine get_time_info

   function is_var_present(this,var_name, rc) result(isPresent)
      class (FileMetadataUtils), intent(inout) :: this
      character(len=*), intent(in) :: var_name
      integer, optional, intent(out) :: rc

      logical :: isPresent

      isPresent = this%metadata%has_variable(var_name)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(rc)
   end function is_var_present

   function get_variable_attribute(this,var_name,attr_name,rc) result(units)
      class (FileMetadataUtils), intent(inout) :: this
      character(len=*), intent(in) :: var_name
      character(len=*), intent(in) :: attr_name
      integer, optional, intent(out) :: rc
      character(:), allocatable :: fname
      character(len=:), pointer :: units
      type(Attribute), pointer :: attr => null()
      class(Variable), pointer :: var => null()
      class(*), pointer :: vunits
      logical :: isPresent
      integer :: status

      fname = this%get_file_name(_RC)
      var => this%get_variable(var_name,_RC)
      isPresent = var%is_attribute_present(trim(attr_name))
      if (isPresent) then
         attr => var%get_attribute(trim(attr_name))
         vunits => attr%get_value()
         select type(vunits)
         type is (character(*))
            units => vunits
         class default
            _FAIL('units must be string for '//var_name//' in '//fname)
         end select
      else
         units => null()
      end if
      _RETURN(_SUCCESS)

   end function get_variable_attribute

   subroutine get_coordinate_info(this,coordinate_name,coordSize,coordUnits,long_name,standard_name,coords,coordinate_attr,rc)
      class (FileMetadataUtils), intent(inout) :: this
      character(len=*), intent(in) :: coordinate_name
      integer, optional, intent(out) :: coordSize
      character(len=*), optional, intent(out) :: coordUnits
      character(len=*), optional, intent(out) :: long_name
      character(len=*), optional, intent(out) :: standard_name
      character(len=*), optional, intent(out) :: coordinate_attr
      real, allocatable, optional,  intent(inout) :: coords(:)
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: fname
      class(CoordinateVariable), pointer :: var
      type(Attribute), pointer :: attr
      character(len=:), pointer :: vdim
      class(*), pointer :: coordUnitPtr
      class(*), pointer :: ptr(:)

      fname = this%get_file_name(_RC)
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
            _FAIL(trim(coordinate_name)//' units must be string in '//fname)
         end select
      end if

      if (present(long_name)) then
         if (this%var_has_attr(coordinate_name,"long_name")) then
            attr => var%get_attribute('long_name')
            coordUnitPtr => attr%get_value()
            select type(coordUnitPtr)
            type is (character(*))
               long_name = trim(coordUnitPtr)
            class default
               _FAIL(trim(coordinate_name)//' long_name must be string in '//fname)
            end select
         else
             long_name = 'not found'
         endif
      end if

      if (present(standard_name)) then
         if (this%var_has_attr(coordinate_name,"standard_name")) then
            attr => var%get_attribute('standard_name')
            coordUnitPtr => attr%get_value()
            select type(coordUnitPtr)
            type is (character(*))
               standard_name = trim(coordUnitPtr)
            class default
               _FAIL(trim(coordinate_name)//' standard_name must be string in '//fname)
            end select
         else
             standard_name = 'not found'
         endif
      end if

      if (present(coordinate_attr)) then
         if (this%var_has_attr(coordinate_name,"coordinate")) then
            attr => var%get_attribute('coordinate')
            coordUnitPtr => attr%get_value()
            select type(coordUnitPtr)
            type is (character(*))
               coordinate_attr = trim(coordUnitPtr)
            class default
               _FAIL(trim(coordinate_name)//' name must be string in '//fname)
            end select
         else
             coordinate_attr = 'not found'
         endif
      end if

      if (present(coords)) then
         ptr => var%get_coordinate_data()
         _ASSERT(associated(ptr),"coord variable coordinate data not found in "//fname)
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
            _FAIL("unsupported coordinate variable type in "//fname)
         end select
      end if
      _RETURN(_SUCCESS)

   end subroutine get_coordinate_info

   function get_level_name(this,rc) result(lev_name)
      class (FileMetadataUtils), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      character(len=:), pointer :: units
      character(len=:), allocatable :: lev_name
      type(CoordinateVariable), pointer :: var
      type (StringVariableMap), pointer :: vars
      type (StringVariableMapIterator) :: var_iter
      character(len=:), pointer :: var_name

      vars => this%get_variables()
      var_iter = vars%ftn_begin()
      do while(var_iter /=vars%ftn_end())
         call var_iter%next()

         var_name => var_iter%first()
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

   function get_variable(this, var_name, unusable, rc) result(var)
      class (Variable), pointer :: var
      class (FileMetadataUtils), target, intent(in) :: this
      character(len=*), intent(in) :: var_name
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      var => this%metadata%get_variable(var_name, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function get_variable

   logical function has_variable(this, var_name, unusable, rc) result(has)
      class (FileMetadataUtils), target, intent(in) :: this
      character(len=*), intent(in) :: var_name
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      class (Variable), pointer :: var

      integer :: status

      has = this%metadata%has_variable(var_name, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function has_variable

   function get_variables(this, rc ) result(variables)
      type (StringVariableMap), pointer :: variables
      class(FileMetadataUtils), target, intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      variables => this%metadata%get_variables(_RC)
      _RETURN(_SUCCESS)
   end function get_variables

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(FileMetadataUtils), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      call this%metadata%write_formatted(unit, iotype, v_list, iostat, iomsg)

   end subroutine write_formatted

   function get_coordinate_variable(this, var_name, unusable, rc) result(var)
      class (CoordinateVariable), pointer :: var
      class (FileMetadataUtils), target, intent(in) :: this
      character(len=*), intent(in) :: var_name
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      var => this%metadata%get_coordinate_variable(var_name, _RC)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function get_coordinate_variable

   function get_dimensions(this) result(dimensions)
      type (StringIntegerMap), pointer :: dimensions
      class (FileMetadataUtils), target, intent(in) :: this

      dimensions => this%metadata%get_dimensions()

   end function get_dimensions

   integer function get_dimension(this, dim_name, unusable, rc) result(extent)
      class (FileMetadataUtils), target, intent(in) :: this
      character(len=*), intent(in) :: dim_name
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      extent = this%metadata%get_dimension(dim_name, _RC)
      _RETURN(_SUCCESS)
   end function get_dimension

   function get_source_file(this,rc) result(source_file)
      character(len=:), allocatable :: source_file
      class (FileMetadataUtils), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      source_file=this%metadata%get_source_file(_RC)
      _RETURN(_SUCCESS)
   end function

end module MAPL_FileMetadataUtilsMod




