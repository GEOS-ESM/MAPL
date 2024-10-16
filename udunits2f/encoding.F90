! Flags for encodings for unit names and symbols
! The values are the same as the udunits2 utEncoding C enum
module ud2f_encoding
   implicit none (type, external)
   public

   enum, bind(c)
      enumerator :: UT_ASCII = 0
      enumerator :: UT_ISO_8859_1 = 1
      enumerator :: UT_LATIN1 = UT_ISO_8859_1
      enumerator :: UT_UTF8 = 2
      enumerator :: UT_ENCODING_DEFAULT = UT_ASCII
   end enum
   integer, parameter :: ut_encoding = kind(UT_ENCODING_DEFAULT)

end module ud2f_encoding

