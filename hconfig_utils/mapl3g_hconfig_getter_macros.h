#if defined TYPENAME
#  undef TYPENAME
#endif

#if defined IS_ARRAY
#   define IS_ARRAY 1
#else
#   define IS_ARRAY 0
#endif

#if TYPE_ == character(len=*) 
#   define TYPENAME String
#   define TYPESTRING_ "CH"
#elif TYPE_ == logical
#   define TYPENAME Logical
#   define TYPESTRING_ "L"
#   define RELOP .eqv.
#elif TYPE_ == real(kind=ESMF_KIND_R4)
#   define TYPENAME R4
#elif TYPE_ == real(kind=ESMF_KIND_R8)
#   define TYPENAME R8
#elif TYPE_ == integer(kind=ESMF_KIND_I4)
#   define TYPENAME I4
#elif TYPE_ == integer(kind=ESMF_KIND_I8)
#   define TYPENAME I8
#endif

#if !defined RELOP
#   define RELOP ==
#endif

#if !defined TYPESTRING_
#   define TYPESTRING_ "##TYPENAME##"
#endif

#if IS_ARRAY
#   define TYPENAME TYPENAME##Seq
#   define RELFCT(A, B) all(A RELOP B)
#   define VALTYPE TYPE_, dimension(:), allocatable
#   define ARGTYPE, dimension(:)
#   define DEFTYPE class(*), dimension(:)
#elif TYPENAME == String
#   define RELFCT(A, B) A RELOP B
#   define VALTYPE character(len=:), allocatable
#   define ARGTYPE character(len=*)
#   define DEFTYPE class(*)
#   define WRITE_STATEMENT(S, V, R) trim(adjustl(V)); R=0
#else
#   define RELFCT(A, B) A RELOP B
#   define VALTYPE TYPE_
#   define ARGTYPE TYPE_
#   define DEFTYPE class(*)
#endif

#if !defined(WRITE_STATEMENT)
#   define WRITE_STATEMENT(S, V, R) write(S, fmt='(G0)', iostat=R) V
#endif

#define SET_VALUE_PROCEDURE set_value_##TYPENAME
#define HANDLE_DEFAULT_PROCEDURE handle_default_##TYPENAME
#define LOG_MESSAGE_PROCEDURE log_message_##TYPENAME
#define ESMF_HCONFIG_AS_PROCEDURE ESMF_HConfigAs##TYPENAME
