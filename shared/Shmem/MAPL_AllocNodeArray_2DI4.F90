#define SHM_SUCCESS  0
#include "unused_dummy.H"
#include "MAPL_ErrLog.h"

submodule (MAPL_Shmem) MAPL_AllocNodeArray_2DI4_smod
  use pflogger, only: logging, Logger
  use MAPL_ExceptionHandling
  use MAPL_Constants
  implicit none

contains

    module subroutine MAPL_AllocNodeArray_2DI4(Ptr, Shp, lbd, rc)
       integer, pointer,  intent(INOUT) :: Ptr(:,:)
       integer,           intent(IN   ) :: Shp(2)
       integer, optional, intent(IN   ) :: lbd(2)
       integer, optional, intent(  OUT) :: rc

      type(c_ptr) :: Caddr
      integer len, STATUS

      if(.not.MAPL_ShmInitialized) then
         _RETURN(MAPL_NoShm)
      endif

      len=product(Shp)

      call GetSharedMemory(Caddr, len, rc=STATUS)
      _VERIFY(STATUS)

      call c_f_pointer(Caddr, Ptr, Shp) ! C ptr to Fortran ptr
      _ASSERT(all(shape(Ptr)==Shp),'needs informative message')

      if(present(lbd)) Ptr(lbd(1):,lbd(2):) => Ptr

      _RETURN(SHM_SUCCESS)
    end subroutine MAPL_AllocNodeArray_2DI4

  end submodule MAPL_AllocNodeArray_2DI4_smod
