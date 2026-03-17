module mapl3g_HistoryConstants

   implicit none(type,external)
   private

   public :: VAR_LIST_KEY
   public :: KEY_TIMESTEP
   public :: KEY_REF_TIME
   public :: KEY_ACCUMULATION_TYPE
   public :: KEY_TIME_SPEC
   public :: KEY_TYPEKIND
   public :: KEY_UNITS
   public :: KEY_EXPRESSION
   public :: KEY_INSTANTANEOUS
   public :: KEY_REGRID

   character(len=*), parameter :: VAR_LIST_KEY = 'var_list'
   character(len=*), parameter :: KEY_TIMESTEP = 'frequency'
   character(len=*), parameter :: KEY_REF_TIME = 'ref_time'
   character(len=*), parameter :: KEY_ACCUMULATION_TYPE = 'mode'
   character(len=*), parameter :: KEY_TIME_SPEC = 'time_spec'
   character(len=*), parameter :: KEY_TYPEKIND = 'typekind'
   character(len=*), parameter :: KEY_UNITS = 'units'
   character(len=*), parameter :: KEY_EXPRESSION = 'expr'
   character(len=*), parameter :: KEY_INSTANTANEOUS = 'instantaneous'
   character(len=*), parameter :: KEY_REGRID = 'regrid'

end module mapl3g_HistoryConstants
