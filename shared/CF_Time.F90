module CF_Time_mod

   use CF_Time_def_mod, only: CF_Time
   use CF_Time_Integer_mod, only: construct_CF_Time_Integer, CF_Time_Integer
   use CF_Time_Real_mod, only: construct_CF_Time_Real, CF_Time_Real

   implicit none

   private

   public :: CF_Time
   public :: CF_Time_Integer
   public :: CF_Time_Real

   interface CF_Time
      module procedure :: construct_CF_Time_Integer
      module procedure :: construct_CF_Time_Real
   end interface CF_Time
   
end module CF_Time_mod
