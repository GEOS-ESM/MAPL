! The macros here are intended to simplify the process of
! accessing the per-gc private state via ESMF.

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


#define _DECLARE_WRAPPER(T) \
  type :: PrivateWrapper;    \
  type(T), pointer :: ptr;   \
  end type PrivateWrapper


#define _SET_PRIVATE_STATE(gc, T) _SET_NAMED_PRIVATE_STATE(gc, T, "private state")

#define _SET_NAMED_PRIVATE_STATE(gc, T, name, private_state)        \
  block;                                             \
    _DECLARE_WRAPPER(T);                               \
    type(PrivateWrapper) :: w;                         \
    allocate(w%ptr);                                           \
    call MAPL_UserCompSetInternalState(gc, name, w, status);         \
    _ASSERT(status==ESMF_SUCCESS, "Private state with name <" //name// "> already created for this gridcomp?"); \
    private_state => w%ptr; \
  end block

#define _GET_PRIVATE_STATE(gc, T, private_state) _GET_NAMED_PRIVATE_STATE(gc, T, "private state", private_state)

#define _GET_NAMED_PRIVATE_STATE(gc, T, name, private_state)  \
  block;                                                      \
    _DECLARE_WRAPPER(T);                                        \
    type(PrivateWrapper) :: w;                                  \
    call MAPL_UserCompGetInternalState(gc, name, w, status);         \
    _ASSERT(status==ESMF_SUCCESS, "Private state with name <" //name// "> not fouund for this gridcomp."); \
    private_state => w%ptr;                         \
  end block

#define _FREE_PRIVATE_STATE(gc, T, private_state) _FREE_NAMED_PRIVATE_STATE(gc, T, "private state", private_state)

#define _FREE_NAMED_PRIVATE_STATE(gc, T, name, private_state)  \
  block;                                                       \
    _DECLARE_WRAPPER(T);                                         \
    type(PrivateWrapper) :: w;                                   \
    call MAPL_UserCompGetInternalState(gc, name, w, rc=status);         \
    _ASSERT(status==ESMF_SUCCESS, "Private state with name <" //name// "> not fouund for this gridcomp."); \
    private_state => w%ptr; \
  end block
  
