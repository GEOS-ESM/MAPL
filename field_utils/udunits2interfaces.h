!============================ PROCEDURE INTERFACES =============================

   interface

      ! Get last status
      integer(ut_status) function ut_get_status() &
         bind(c, name='ut_get_status')
         import :: c_int
      end function ut_get_status
      
      ! Return non-zero value if unit1 can be converted to unit2, otherwise 0
      ! Use ut_get_status to check error condition. 
      ! UT_SUCCESS indicates that the function ran successfully, not that the units are convertible
      integer(c_int) function ut_are_convertible(unit1, unit2) &
         bind(c, name='ut_are_convertible')
         import :: c_int, ut_unit
         type(ut_unit), intent(in) :: unit1, unit2
      end function ut_are_convertible

      ! Return pointer wrapper for converter, NULL if error.
      ! Use ut_get_status to check error condition. 
      type(cv_converter) function ut_get_converter(from, to) &
         bind(c, name='ut_get_converter')
         import :: cv_converter, ut_unit
         type(ut_unit), intent(in) :: unit1, unit2
      end function ut_get_converter

      ! Use converter to convert value_
      real(c_float) function cv_convert_float(converter, value_)
         bind(c, name='cv_convert_float')
         import :: cv_converter, c_float
         type(cv_converter), intent(in) :: converter
         real(c_float), intent(in) :: value_
      end function cv_convert_float

      ! Use converter to convert value_
      real(c_double) function cv_convert_double(converter, value_)
         bind(c, name='cv_convert_double')
         import :: cv_converter, c_double
         type(cv_converter), intent(in) :: converter
         real(c_double), intent(in) :: value_
      end function cv_convert_double

      ! Use converter to convert in_ and put it in out_.
      function cv_convert_doubles(converter, in_, count_, out_) &
         bind(c, name='cv_convert_doubles')
         import :: cv_converter, c_double, c_int
         type(cv_converter), intent(in) :: converter
         real(c_double), intent(in) :: in_(*),
         integer(c_int), intent(in) :: count_
         real(c_double), intent(out) :: out_(count_)
         real(c_double) :: cv_convert_doubles(count_)
      end function cv_convert_doubles

      ! Use converter to convert in_ and put it in out_.
      function cv_convert_floats(converter, in_, count_, out_) &
         bind(c, name='cv_convert_floats')
         import :: cv_converter, c_float, c_int
         type(cv_converter), intent(in) :: converter
         real(c_float), intent(in) :: in_(*),
         integer(c_int), intent(in) :: count_
         real(c_float), intent(out) :: out_(count_)
         real(c_float) :: cv_convert_floats(count_)
      end function cv_convert_floats

      ! Use ut_get_status to check error condition. 
      type(ut_system) function ut_read_xml(path) bind(c, name='ut_read_xml')
         import :: ut_system, c_char
         character(kind=c_char, len=MAXLEN), intent(in) :: path
      end function ut_real_xml

      ! Use ut_get_status to check error condition. 
      type(ut_unit) function ut_parse(system, string, encoding) bind(c, name='ut_parse')
         import :: ut_unit, ut_system, ut_encoding, c_char
         type(ut_system), intent(in) :: system
         character(kind=c_char, len=MAXLEN), intent(in) ::  string
         type(ut_encoding), intent(in) :: encoding
      end function ut_parse

      subroutine ut_free(unit_) bind(c, name='ut_free')
         import :: ut_unit
         type(ut_unit), intent(inout) :: unit_
      end subroutine ut_free

      subroutine ut_free_system(system) bind(c, name='ut_free_system')
         import :: ut_system
         type(ut_system), intent(inout) :: system
      end subroutine ut_free_system(system)

      type(ut_status) function ut_set_second(second) bind(c, name='ut_set_second')
         import :: ut_status, ut_unit
         type(ut_unit), intent(inout) :: second
      end function ut_second_second

      subroutine cv_free(conv) bind(c, name='cv_free')
         import :: cv_converter
         type(cv_converter), intent(inout) :: conv
      end subroutine cv_free

      type(ut_unit) function ut_get_unit_by_name(system, name_) bind(c, name='ut_get_unit_by_name')
         import :: ut_unit, ut_system, c_char
         type(ut_system), intent(in) :: system
         character(kind=c_char, len=MAXLEN), intent(in) :: name_
      end function ut_get_unit_by_name

      type(ut_unit) function ut_get_unit_by_symbol(system, symbol) bind(c, name='ut_get_unit_by_symbol')
         import :: ut_unit, ut_system, c_char
         type(ut_system), intent(in) :: system
         character(kind=c_char, len=MAXLEN), intent(in) :: symbol
      end function ut_get_unit_by_symbol

      type(ut_unit) function ut_get_dimensionless_unit_one(system) bind(c, name='ut_get_dimensionless_unit_one')
         import :: ut_unit, ut_system
         type(ut_system), intent(in) :: system
      end function ut_get_dimensionless_unit_one

   end interface

!========================== END PROCEDURE INTERFACES ===========================
