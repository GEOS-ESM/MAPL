module pFIO_ConstantsMod
   implicit none
   private

   ! Type/kinds
   public :: pFIO_INT32, pFIO_INT64
   public :: pFIO_REAL32, pFIO_REAL64
   public :: pFIO_LOGICAL
   public :: pFIO_STRING
   public :: pFIO_UNSUPPORTED_TYPE

   ! Misc
   public :: pFIO_UNLIMITED

   ! Return codes
   public :: pFIO_SUCCESS
   public :: pFIO_DIMENSION_NOT_FOUND
   public :: pFIO_ATTRIBUTE_NOT_FOUND
   public :: pFIO_ILLEGAL_DIMENSION_INDEX
   public :: pFIO_ILLEGAL_DIMENSIONS_FORMAT
   public :: pFIO_UNDEFINED_DIMENSION

   ! IO modes
   public :: pFIO_WRITE
   public :: pFIO_READ
   public :: pFIO_s_tag
   public :: pFIO_m_w_tag
   public :: pFIO_w_m_tag

   public :: pFIO_DIMENSION_SEPARATOR

   enum, bind(C)
      enumerator :: pFIO_INT32, pFIO_INT64
      enumerator :: pFIO_REAL32, pFIO_REAL64
      enumerator :: pFIO_LOGICAL
      enumerator :: pFIO_STRING
      enumerator :: pFIO_UNSUPPORTED_TYPE
   end enum


   enum, bind(C)
      enumerator :: pFIO_SUCCESS = 0
      enumerator :: pFIO_DIMENSION_NOT_FOUND
      enumerator :: pFIO_ATTRIBUTE_NOT_FOUND
      enumerator :: pFIO_ILLEGAL_DIMENSION_INDEX
      enumerator :: pFIO_ILLEGAL_DIMENSIONS_FORMAT
      enumerator :: pFIO_UNDEFINED_DIMENSION
   end enum
      
   character, parameter :: pFIO_DIMENSION_SEPARATOR = ','

   integer, parameter :: pFIO_UNLIMITED = 1 - huge(1)

   enum, bind(C)
      enumerator :: pFIO_READ
      enumerator :: pFIO_WRITE
   end enum

   integer, parameter :: pFIO_s_tag   = 9999
   integer, parameter :: pFIO_m_w_tag = 8888
   integer, parameter :: pFIO_w_m_tag = 7777
   

end module pFIO_ConstantsMod
