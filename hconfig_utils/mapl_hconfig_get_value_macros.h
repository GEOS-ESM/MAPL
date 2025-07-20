#ifndef _MAPL_HCONFIG_MACROS_
#include "mapl_hconfig_macros.h"
#endif

#if defined _TYPE_
#   if _TYPE_ == _LOGICAL_
#       define _TYPECODE_ Logical
#       define _FTYPE_ logical
#       define _ED_ L1
#       define _TYPESTRING_ "L"
#   elif _TYPE_ == _STRING_
#       define _TYPECODE_ String
#       define _FTYPE_ character(len=*)
#       define _ED_ A
#       define _TYPESTRING_ 'CH'
#   elif _TYPE_ == _INT_
#       define _TYPECODE_ _CAT2(I, _KIND_)
#       define _FTYPE_ _PFTYPE(integer, kind, _ESMF_KIND(_TYPECODE_)
#       define _ED_ _EDIT_DESC_DEF_
#   elif _TYPE_ == _REAL_
#       define _TYPECODE_ _CAT2(R, _KIND_)
#       define _FTYPE_ _PFTYPE(integer, kind, _ESMF_KIND(_TYPECODE_)
#       if _KIND_ == 4
#           define _ED_ _CAT2(_EDIT_DESC_DEF_, .7)
#       elif _KIND_ == 8
#           define _ED_ _CAT2(_EDIT_DESC_DEF_, .16)
#       endif
#   endif
#   ifndef _TYPESTRING_
#       define _TYPESTRING_ _QUOTE(_TYPECODE_)
#   endif
#   ifdef _ARRAY_
#       define _VALARG_ _CAT2(_VAL_, _DIMS_)
#       define _DEFARG_ _CAT2(_DEFAULT_, _DIMS_)
#       define _ESMF_FUNC_ _CAT3(ESMF_HConfigAs, _TYPECODE_, _SEQ_)
#   else
#       define _VALARG_ _VAL_
#       define _DEFARG_ _DEFAULT_
#       define _ESMF_FUNC_ _CAT2(ESMF_HConfigAs, _TYPECODE_)
#   endif
#endif
