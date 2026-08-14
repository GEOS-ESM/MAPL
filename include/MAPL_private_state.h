! The macros here are intended to simplify the process of
! accessing the per-gc private state via ESMF.

#ifndef _MAPL_PRIVATE_STATE_ERROR_CODE
#  define _MAPL_PRIVATE_STATE_ERROR_CODE 17
#endif

#ifdef _DECLARE_WRAPPER
#  undef _DECLARE_WRAPPER
#endif

#ifdef _SET_PRIVATE_STATE
#  undef _SET_PRIVATE_STATE
#endif

#ifdef _SET_NAMED_PRIVATE_STATE
#  undef _SET_NAMED_PRIVATE_STATE
#endif

#ifdef _GET_PRIVATE_STATE
#  undef _GET_PRIVATE_STATE
#endif

#ifdef _GET_NAMED_PRIVATE_STATE
#  undef _GET_NAMED_PRIVATE_STATE
#endif

#ifdef _FREE_PRIVATE_STATE
#  undef _FREE_PRIVATE_STATE
#endif

#ifdef _FREE_NAMED_PRIVATE_STATE
#  undef _FREE_NAMED_PRIVATE_STATE
#endif


#define _DECLARE_WRAPPER(T)  \
  type :: PrivateWrapper;    \
    type(T), pointer :: ptr; \
  end type PrivateWrapper


#define _SET_PRIVATE_STATE(gc, T) _SET_NAMED_PRIVATE_STATE(gc, T, "private state")

#define _SET_NAMED_PRIVATE_STATE(gc, T, name)        \
  block;                                             \
    _DECLARE_WRAPPER(T);                               \
    type(PrivateWrapper) :: w;                         \
    allocate(w%ptr);                                           \
    call ESMF_InternalStateAdd(gc, internalState=w, label=name, rc=status);         \
    _ASSERT_CODE_CTX(status==ESMF_SUCCESS, _MAPL_PRIVATE_STATE_ERROR_CODE, name); \
  end block

#define _GET_PRIVATE_STATE(gc, T, private_state) _GET_NAMED_PRIVATE_STATE(gc, T, "private state", private_state)

#define _GET_NAMED_PRIVATE_STATE(gc, T, name, private_state)  \
  block;                                                      \
    _DECLARE_WRAPPER(T);                                        \
    type(PrivateWrapper) :: w;                                  \
    call ESMF_InternalStateGet(gc, internalState=w, label=name, rc=status);         \
    _ASSERT_CODE_CTX(status==ESMF_SUCCESS, _MAPL_PRIVATE_STATE_ERROR_CODE, name); \
    private_state => w%ptr;                         \
  end block

#define _FREE_PRIVATE_STATE(gc, T, private_state) _FREE_NAMED_PRIVATE_STATE(gc, T, "private state", private_state)

#define _FREE_NAMED_PRIVATE_STATE(gc, T, name, private_state)  \
  block;                                                       \
    _DECLARE_WRAPPER(T);                                         \
    type(PrivateWrapper) :: w;                                   \
    call ESMF_InternalStateGet(gc, internalState=w, lable=name, rc=status);         \
    _ASSERT_CODE_CTX(status==ESMF_SUCCESS, _MAPL_PRIVATE_STATE_ERROR_CODE, name); \
    private_state => w%ptr; \
  end block
  
