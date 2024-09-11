select case(CASE_)
case(ANALYSIS_ACCUMULATE):
   ACCUMULATE_
case(ANALYSIS_MEAN):
   MEAN_
case(ANALYSIS_MAX):
   MAX_
case(ANALYSIS_MIN):
   MIN_
case default:
   _FAIL('Unsupported analysis procedure')
end select
! vim: ft=fortran
