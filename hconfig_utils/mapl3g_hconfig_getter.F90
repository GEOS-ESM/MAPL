#include "MAPL_ErrLog.h"
module mapl3g_hconfig_getter
!wdb todo Could this be submodule'd? Probably not, but maybe. Each interface would have 4 arguments.

   use :: esmf, MAXSTRLEN => ESMF_MAXSTR
   use mapl_ErrorHandling

   implicit none
   public :: HConfigGetter
   public :: get_value
   public :: get_value_array
   public :: get_value_i4seq
   public :: MAXSTRLEN

   type :: HConfigGetter
      type(ESMF_HConfig) :: hconfig
      character(len=:), allocatable :: label
      logical :: found = .FALSE.
      character(len=:), allocatable :: typestring
      character(len=:), allocatable :: valuestring
      logical :: value_equals_default = .FALSE.
   end type HConfigGetter

   interface get_value
      module procedure :: get_value_i4
!      module procedure :: get_value_string
   end interface get_value

   interface get_value_array
      module procedure :: get_value_i4seq
   end interface get_value_array

   character(len=*), parameter :: DEFAULT_FORMAT_STRING = '(G0)'
   character(len=*), parameter :: EMPTY_STRING = ''

   interface HConfigGetter
      module procedure :: construct
   end interface HConfigGetter

contains
   
   type(HConfigGetter) function construct(hconfig, label, found)
      type(ESMF_HConfig), intent(in) :: hconfig
      character(len=*), intent(in) :: label
      logical, intent(in) :: found

      construct%hconfig = hconfig
      construct%label = label
      construct%found = found
      construct%typestring = EMPTY_STRING
      construct%valuestring = EMPTY_STRING

   end function construct

#define TYPENUM 4
#include "mapl3g_hconfig_getter_template.h"
#undef TYPENUM

#define TYPENUM 5
#include "mapl3g_hconfig_getter_template.h"
#undef TYPENUM

end module mapl3g_hconfig_getter

!#define TYPENUM 1
!#   define ESMF_HCONFIG_AS ESMF_HConfigAsString
!#   define GET_VALUE_ get_value_string
!#   define VALTYPE character(len=:), allocatable
!#   define DEFTYPE character(len=*)
!#   define TYPESTRING_ 'CH'
!#   define DEFINIT ''
!#include "mapl3g_hconfig_getter_template.h"
!   subroutine get_value_i4(getter, value, default, rc)
!      integer(kind=ESMF_KIND_I4), intent(out) :: value !macro VALTYPE
!      character(len=*), parameter :: fmt_ = DEFAULT_FORMAT_STRING !macro FMTSTR
!      integer(kind=ESMF_KIND_I4) :: default_ !macro VALTYPE
!      type(HConfigGetter), intent(inout) :: getter
!      class(*), optional, intent(in) :: default
!      integer, optional,intent(out) :: rc
!      integer :: status = 0
!      character(len=MAXSTRLEN) :: buffer
!
!      getter%typestring = 'I4' !macro
!      default_ = -huge(1)
!      if (present(default)) then
!         select type(default)
!         type is (integer(kind=ESMF_KIND_I4)) !macro TYPE_
!            default_ = default
!            value = default_
!         class default
!            _FAIL('Illegal type provided for default value for label <'//getter%label//'>')
!         end select
!      end if
!
!      if (getter%found) then
!         value = ESMF_HConfigAsI4(getter%hconfig, keyString=getter%label, _RC) !macro ESMF_HCONFIG_AS
!      end if
!
!      getter%value_equals_default = (value == default_)
!      write(buffer, fmt=fmt_, iostat=status) value
!      _VERIFY(status)
!      getter%valuestring = trim(buffer)
!
!      _RETURN(_SUCCESS)
!
!   end subroutine get_value_i4

!subroutine get_value_i4seq (getter, value, default, rc)
!      integer(kind=ESMF_KIND_I4), allocatable, intent(out) :: value(:)
!      character(len=*), parameter :: fmt_ = '(' // 'G0:", "' // ':", ")'
!      integer(kind=ESMF_KIND_I4), allocatable :: default_(:)
!      type(HConfigGetter), intent(inout) :: getter
!      class(*), optional, intent(in) :: default(:)
!      integer, optional,intent(out) :: rc
!      integer :: status = 0
!      character(len=MAXSTRLEN) :: buffer
!
!      getter%value_equals_default = .FALSE.
!      getter%typestring = 'I4'
!      default_ = [integer(kind=ESMF_KIND_I4) ::]
!      if (present(default)) then
!         select type(default)
!         type is ( integer(kind=ESMF_KIND_I4))
!            default_ = default
!            value = default_
!         class default
!            _FAIL('Illegal type provided for default value for label <'//getter%label//'>')
!         end select
!      end if
!
!      if (getter%found) then
!         value = ESMF_HConfigAsI4Seq (getter%hconfig, keyString=getter%label, _RC)
!      end if
!
!      if(present(default)) then !wdb todo cleanup
!        getter%value_equals_default = product(shape(value)) == product(shape(default_))
!        if(getter%value_equals_default) then
!            getter%value_equals_default = all(value==default_)
!        end if
!      end if
!      write(buffer, fmt=fmt_, iostat=status) value
!      _VERIFY(status)
!      getter%valuestring = trim(buffer)
!
!      _RETURN(_SUCCESS)
!
!end subroutine get_value_i4seq
