module mapl_base3g
   use mapl_SimulationTime, only: set_reference_clock, fill_time_dict
   use MAPL_CommsMod, only: mapl_CommsBcast, mapl_CommsScatterV, mapl_CommsGatherV, &
                            mapl_CommsAllGather, mapl_CommsAllGatherV, &
                            mapl_CommsAllReduceMin, mapl_CommsAllReduceMax, &
                            mapl_CommsAllReduceSum, mapl_CommsSend, mapl_CommsRecv, &
                            mapl_CommsSendRecv, mapl_AM_I_ROOT, mapl_AM_I_RANK, &
                            mapl_NPES, ArrayGather, ArrayScatter, MAPL_ROOT, &
                            mapl_CreateRequest, mapl_CommRequest, mapl_ArrayIGather, &
                            mapl_ArrayIScatter, mapl_CollectiveWait, &
                            mapl_CollectiveScatter3D, mapl_CollectiveGather3D, &
                            mapl_RoundRobinPEList, mapl_BcastShared, ArrPtr
   use MAPL_SatVaporMod, only: MAPL_EQsatSET, MAPL_EQsat
   use MAPL_StringTemplate, only: fill_grads_template, StrTemplate, fill_grads_template_esmf
   implicit none(type,external)
   private

   public :: set_reference_clock, fill_time_dict
   public :: mapl_CommsBcast, mapl_CommsScatterV, mapl_CommsGatherV
   public :: mapl_CommsAllGather, mapl_CommsAllGatherV
   public :: mapl_CommsAllReduceMin, mapl_CommsAllReduceMax, mapl_CommsAllReduceSum
   public :: mapl_CommsSend, mapl_CommsRecv, mapl_CommsSendRecv
   public :: mapl_AM_I_ROOT, mapl_AM_I_RANK, mapl_NPES
   public :: ArrayGather, ArrayScatter, MAPL_ROOT
   public :: mapl_CreateRequest, mapl_CommRequest
   public :: mapl_ArrayIGather, mapl_ArrayIScatter, mapl_CollectiveWait
   public :: mapl_CollectiveScatter3D, mapl_CollectiveGather3D
   public :: mapl_RoundRobinPEList, mapl_BcastShared, ArrPtr
   public :: MAPL_EQsatSET, MAPL_EQsat
   public :: fill_grads_template, StrTemplate, fill_grads_template_esmf

end module mapl_base3g
