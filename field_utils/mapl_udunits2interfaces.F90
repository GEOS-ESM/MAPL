module mapl_udunits2interfaces

   use iso_c_binding, only: c_ptr, c_char, c_int, c_float, c_double
   use mapl_udunits2status
   use mapl_udunits2encoding

   implicit none

   public :: ut_get_status, ut_parse
   public :: ut_read_xml_cptr
   public :: ut_get_converter, ut_are_convertible
   public :: cv_convert_double, cv_convert_float
   public :: cv_convert_doubles, cv_convert_floats
   public :: ut_free, ut_free_system, cv_free

   interface

      ! Procedures that return type(c_ptr) return a C null pointer on failure.
      ! However, checking for the C null pointer IS NOT a good check for status.
      ! ut_get_status is a better check, where UT_SUCCESS indicates success.

      ! Return type(c_ptr) to ut_system units database specified by path
      ! Use ut_get_status to check error condition. 
      ! UT_SUCCESS indicates that the function ran successfully.
      ! Other ut_status codes indicate cause of failure.
      type(c_ptr) function ut_read_xml_cptr(path) bind(c, name='ut_read_xml')
         import :: c_ptr
         type(c_ptr), value :: path
      end function ut_read_xml_cptr

      ! Get status code
      integer(ut_status) function ut_get_status() bind(c, name='ut_get_status')
         import :: ut_status
      end function ut_get_status

      ! Return non-zero value if unit1 can be converted to unit2, otherwise 0
      ! Use ut_get_status to check error condition. 
      ! UT_SUCCESS indicates that the function ran successfully.
      ! Other ut_status codes indicate cause of failure.
      integer(c_int) function ut_are_convertible(unit1, unit2) &
         bind(c, name='ut_are_convertible')
         import :: c_int, c_ptr
         type(c_ptr), value, intent(in) :: unit1, unit2
      end function ut_are_convertible

      ! Return type(c_ptr) to cv_converter
      ! Use ut_get_status to check error condition. 
      ! UT_SUCCESS indicates that the function ran successfully.
      ! Other ut_status codes indicate cause of failure.
      type(c_ptr) function ut_get_converter(from, to) &
         bind(c, name='ut_get_converter')
         import :: c_ptr
         type(c_ptr), value, intent(in) :: from, to
      end function ut_get_converter

      ! Use converter to convert value_
      ! Use ut_get_status to check error condition. 
      ! UT_SUCCESS indicates that the function ran successfully.
      ! Other ut_status codes indicate cause of failure.
      real(c_float) function cv_convert_float(converter, value_) bind(c)
         import :: c_ptr, c_float
         type(c_ptr), value, intent(in) :: converter
         real(c_float), value, intent(in) :: value_
      end function cv_convert_float

      ! Use converter to convert value_
      ! Use ut_get_status to check error condition. 
      ! UT_SUCCESS indicates that the function ran successfully.
      ! Other ut_status codes indicate cause of failure.
      real(c_double) function cv_convert_double(converter, value_) bind(c)
         import :: c_ptr, c_double
         type(c_ptr), value, intent(in) :: converter
         real(c_double), value, intent(in) :: value_
      end function cv_convert_double

      ! Use converter to convert in_ and put it in out_.
      ! Use ut_get_status to check error condition. 
      ! UT_SUCCESS indicates that the function ran successfully.
      ! Other ut_status codes indicate cause of failure.
      subroutine cv_convert_doubles(converter, in_, count_, out_) &
         bind(c, name='cv_convert_doubles')
         import :: c_double, c_int, c_ptr
         type(c_ptr), value, intent(in) :: converter
         real(c_double), intent(in) :: in_(*)
         integer(c_int), value, intent(in) :: count_
         real(c_double), intent(out) :: out_(count_)
      end subroutine cv_convert_doubles

      ! Use converter to convert in_ and put it in out_.
      ! Use ut_get_status to check error condition. 
      ! UT_SUCCESS indicates that the function ran successfully.
      ! Other ut_status codes indicate cause of failure.
      subroutine cv_convert_floats(converter, in_, count_, out_) &
         bind(c, name='cv_convert_floats')
         import :: c_ptr, c_float, c_int
         type(c_ptr), value, intent(in) :: converter
         real(c_float), intent(in) :: in_(*)
         integer(c_int), value, intent(in) :: count_
         real(c_float), intent(out) :: out_(count_)
      end subroutine cv_convert_floats

      ! Return type(c_ptr) to ut_unit
      ! UT_SUCCESS indicates that the function ran successfully.
      ! Other ut_status codes indicate cause of failure.
      ! Use ut_get_status to check error condition. 
      type(c_ptr) function ut_parse(system, string, encoding) &
         bind(c, name='ut_parse')
         import :: c_ptr, c_char, ut_encoding
         type(c_ptr), value, intent(in) :: system
         character(c_char), intent(in) :: string(*)
         integer(ut_encoding), value, intent(in) :: encoding
      end function ut_parse

      ! Free memory for ut_system
      subroutine ut_free_system(system) bind(c, name='ut_free_system')
         import :: c_ptr
         type(c_ptr), value :: system
      end subroutine ut_free_system

      ! Free memory for ut_unit
      subroutine ut_free(unit) bind(c, name='ut_free')
         import :: c_ptr
         type(c_ptr), value :: unit
      end subroutine ut_free

      ! Free memory for cv_converter
      subroutine cv_free(conv) bind(c, name='cv_free')
         import :: c_ptr
         type(c_ptr), value :: conv
      end subroutine cv_free

      ! Set udunits error handler to ut_ignore (do nothing)
      subroutine ut_set_ignore_error_message_handler() &
            bind(c, name='ut_set_ignore_error_message_handler')
      end subroutine ut_set_ignore_error_message_handler

   end interface

end module mapl_udunits2interfaces
