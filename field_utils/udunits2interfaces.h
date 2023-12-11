!============================ PROCEDURE INTERFACES =============================

   interface

      function ut_get_path_xml(path, status) bind(c, name='ut_get_path_xml') result(path_xml)
         import :: c_ptr, ut_status, c_char
         type(c_ptr), intent(in) :: path
         integer(ut_status), intent(out) :: status
         type(c_ptr) :: path_xml
      end function ut_get_path_xml

      ! Get last status
      integer(ut_status) function ut_get_status() &
         bind(c, name='ut_get_status')
         import :: ut_status
      end function ut_get_status
      
      ! Return non-zero value if unit1 can be converted to unit2, otherwise 0
      ! Use ut_get_status to check error condition. 
      ! UT_SUCCESS indicates that the function ran successfully,
      ! not that the units are convertible
      integer(c_int) function ut_are_convertible(unit1, unit2) &
         bind(c, name='ut_are_convertible')
         import :: c_int, c_ptr
         type(c_ptr), intent(in) :: unit1, unit2
      end function ut_are_convertible

      ! Return pointer wrapper for converter, NULL if error.
      ! Use ut_get_status to check error condition. 
      type(c_ptr) function ut_get_converter(from, to) &
         bind(c, name='ut_get_converter')
         import :: c_ptr
         type(c_ptr), intent(in) :: from, to
      end function ut_get_converter

      ! Use converter to convert value_
      real(c_float) function cv_convert_float(converter, value_) bind(c)
         import :: c_ptr, c_float
         type(c_ptr), intent(in) :: converter
         real(c_float), intent(in) :: value_
      end function cv_convert_float

      ! Use converter to convert value_
      real(c_double) function cv_convert_double(converter, value_) bind(c)
         import :: c_ptr, c_double
         type(c_ptr), intent(in) :: converter
         real(c_double), intent(in) :: value_
      end function cv_convert_double

      ! Use converter to convert in_ and put it in out_.
      subroutine cv_convert_doubles(converter, in_, count_, out_) &
         bind(c, name='cv_convert_doubles')
         import :: c_double, c_int, c_ptr
         type(c_ptr), intent(in) :: converter
         real(c_double), intent(in) :: in_(*)
         integer(c_int), value, intent(in) :: count_
         real(c_double), intent(out) :: out_(count_)
      end subroutine cv_convert_doubles

      ! Use converter to convert in_ and put it in out_.
      subroutine cv_convert_floats(converter, in_, count_, out_) &
         bind(c, name='cv_convert_floats')
         import :: c_ptr, c_float, c_int
         type(c_ptr), intent(in) :: converter
         real(c_float), intent(in) :: in_(*)
         integer(c_int), value, intent(in) :: count_
         real(c_float), intent(out) :: out_(count_)
      end subroutine cv_convert_floats

      ! Use ut_get_status to check error condition. 
      type(c_ptr) function ut_read_xml(path_ptr) bind(c, name='ut_read_xml')
         import :: c_ptr
         type(c_ptr), intent(in) :: path_ptr
      end function ut_read_xml

      ! Use ut_get_status to check error condition. 
      type(c_ptr) function ut_parse(system, string, encoding) &
         bind(c, name='ut_parse')
         import :: c_ptr, c_char, ut_encoding
         type(c_ptr), intent(in) :: system
         character(c_char), intent(in) :: string(*)
         integer(ut_encoding), value, intent(in) :: encoding
      end function ut_parse

      subroutine ut_free_system(system) bind(c, name='ut_free_system')
         import :: c_ptr
         type(c_ptr), intent(in) :: system
      end subroutine ut_free_system

      subroutine ut_free(unit) bind(c, name='ut_free')
         import :: c_ptr
         type(c_ptr), intent(in) :: unit
      end subroutine ut_free

      subroutine cv_free(conv) bind(c, name='cv_free')
         import :: c_ptr
         type(c_ptr), intent(in) :: conv
      end subroutine cv_free
         
   end interface

!========================== END PROCEDURE INTERFACES ===========================
! vim: set ft=fortran:
