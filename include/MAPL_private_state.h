! The macros here are intended to simplify the process of
! accessing the per-gc private state via ESMF.
!
! OpenMP threading note
! ---------------------
! _GET_NAMED_PRIVATE_STATE and _FREE_NAMED_PRIVATE_STATE redirect their
! lookup through mapl_get_owning_gridcomp before calling ESMF.  For a
! normal (non-threaded) gridcomp the function is a no-op.  For a "mini"
! gridcomp created by the threading layer, it walks back to the primary
! user gridcomp that carries the user-visible private state.  This makes
! all private-state accesses transparent to the OpenMP sub-component
! infrastructure without requiring any change in user code.
!
! _SET_NAMED_PRIVATE_STATE is intentionally NOT redirected: user code must
! always set private state on the primary user gridcomp (typically in
! SetServices, before any mini gridcomps exist).  Setting private state on
! a mini gridcomp would silently succeed but be invisible to other threads;
! the existing ESMF_InternalStateAdd assertion catches double-add if the
! caller erroneously holds a mini gridcomp.

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
    _ASSERT(status==ESMF_SUCCESS, "Private state with name <" //name// "> already created for this gridcomp?"); \
  end block

#define _GET_PRIVATE_STATE(gc, T, private_state) _GET_NAMED_PRIVATE_STATE(gc, T, "private state", private_state)

#define _GET_NAMED_PRIVATE_STATE(gc, T, name, private_state)  \
  block;                                                      \
    _DECLARE_WRAPPER(T);                                        \
    type(PrivateWrapper) :: w;                                  \
    type(ESMF_GridComp) :: owner_gc_;                          \
    owner_gc_ = mapl_get_owning_gridcomp(gc, rc=status);      \
    _ASSERT(status==ESMF_SUCCESS, "mapl_get_owning_gridcomp failed for private state <" //name// ">"); \
    call ESMF_InternalStateGet(owner_gc_, internalState=w, label=name, rc=status); \
    _ASSERT(status==ESMF_SUCCESS, "Private state with name <" //name// "> not found for this gridcomp."); \
    private_state => w%ptr;                         \
  end block

#define _FREE_PRIVATE_STATE(gc, T, private_state) _FREE_NAMED_PRIVATE_STATE(gc, T, "private state", private_state)

#define _FREE_NAMED_PRIVATE_STATE(gc, T, name, private_state)  \
  block;                                                       \
    _DECLARE_WRAPPER(T);                                         \
    type(PrivateWrapper) :: w;                                   \
    type(ESMF_GridComp) :: owner_gc_;                           \
    owner_gc_ = mapl_get_owning_gridcomp(gc, rc=status);       \
    _ASSERT(status==ESMF_SUCCESS, "mapl_get_owning_gridcomp failed for private state <" //name// ">"); \
    call ESMF_InternalStateGet(owner_gc_, internalState=w, label=name, rc=status); \
    _ASSERT(status==ESMF_SUCCESS, "Private state with name <" //name// "> not found for this gridcomp."); \
    private_state => w%ptr; \
  end block
  
