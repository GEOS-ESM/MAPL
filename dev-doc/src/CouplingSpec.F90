module oomph_CouplingSpec
   implicit none
   private

   public :: CouplingSpec

   ! In multiples of component heartbeat
   type :: CouplingSpec
      private
      integer :: accumulatate_interval
      integer :: coupling_interval
      integer :: offset
   end type CouplingSpec

   interface CouplingSpec
      module procedure new_CouplingSpec_empty
   end interface CouplingSpec

contains

   pure function new_CouplingSpec_empty() result(coupling_spec)
      type(CouplingSpec) :: coupling_spec

      coupling_spec%accumulatate_interval = 1
      coupling_spec%coupling_interval = 1
      coupling_spec%offset = 0
      
   end function new_CouplingSpec_empty

end module oomph_CouplingSpec
